import Clingo.Types
import Clingo.Ast
import Clingo.Symbol

/-! # AST Conversion: Lean DSL → Opaque AstNode

Converts `Clingo.Ast.Statement` (the Lean DSL representation built at
compile time by `Clingo.Lang`) into opaque `AstNode` values via the
auto-generated variadic builder functions. Each `AstNode` carries its
own `Location`, preserving per-node source location for error reporting.
-/

namespace Clingo.AstConvert

open Clingo.Generated.ClingoBindings

private def unwrap (r : Except (Error × String) AstNode) : IO AstNode :=
  match r with
  | .ok n => pure n
  | .error (_, msg) => throw (IO.userError s!"AST build failed: {msg}")

private def convertLocation (loc : Location) : Location := loc

private def convertSign (s : Ast.Sign) : Sign :=
  match s with
  | .None => .none
  | .Negation => .negation
  | .DoubleNegation => .doubleNegation

private def convertComparisonOp (op : Ast.Comparison.Operator) : ComparisonOperator :=
  match op with
  | .GT  => .gt
  | .LT  => .lt
  | .LEQ => .leq
  | .GEQ => .geq
  | .NEQ => .neq
  | .EQ  => .eq

private def convertUnaryOp (op : Ast.UnaryOperator) : UnaryOperator :=
  match op with
  | .Minus    => .minus
  | .Negation => .negation
  | .Absolute => .absolute

private def convertBinaryOp (op : Ast.BinaryOperator) : BinaryOperator :=
  match op with
  | .XOR            => .xor
  | .OR             => .or
  | .AND            => .and
  | .PLUS           => .plus
  | .MINUS          => .minus
  | .MULTIPLICATION => .multiplication
  | .DIVISION       => .division
  | .MODULO         => .modulo
  | .POWER          => .power

private def convertAggFn (f : Ast.Aggregate.Function) : AggregateFunction :=
  match f with
  | .Count => .count
  | .Sum   => .sum
  | .Sump  => .sump
  | .Min   => .min
  | .Max   => .max

mutual

partial def convertTerm (t : Ast.Term) : IO AstNode := do
  let .mk loc data := t
  match data with
  | .Symbol sym =>
    let s ← Symbol.mk sym
    unwrap (← buildSymbolicTerm loc s)
  | .Variable v =>
    unwrap (← buildVariable loc v)
  | .UnaryOperator op arg =>
    let argN ← convertTerm arg
    unwrap (← buildUnaryOperation loc (convertUnaryOp op) argN)
  | .BinaryOperator op l r =>
    let lN ← convertTerm l
    let rN ← convertTerm r
    unwrap (← buildBinaryOperation loc (convertBinaryOp op) lN rN)
  | .Interval l r =>
    let lN ← convertTerm l
    let rN ← convertTerm r
    unwrap (← buildInterval loc lN rN)
  | .Function name args =>
    let argsN ← args.mapM convertTerm
    unwrap (← buildFunction loc name argsN 0)
  | .ExternalFunction name args =>
    let argsN ← args.mapM convertTerm
    unwrap (← buildFunction loc name argsN 1)
  | .Pool args =>
    let argsN ← args.mapM convertTerm
    unwrap (← buildPool loc argsN)

partial def convertComparison (loc : Location) (cmp : Ast.Comparison) : IO AstNode := do
  let termN ← convertTerm cmp.left
  let rightN ← convertTerm cmp.right
  let guardN ← unwrap (← buildGuard (convertComparisonOp cmp.operator) rightN)
  unwrap (← buildComparison termN #[guardN])

partial def convertLiteralData (loc : Location) (data : Ast.Literal.Data) : IO AstNode := do
  match data with
  | .Boolean b =>
    let boolAtom ← unwrap (← buildBooleanConstant (if b then 1 else 0))
    pure boolAtom
  | .Symbolic t =>
    let tN ← convertTerm t
    unwrap (← buildSymbolicAtom tN)
  | .Comparison cmp =>
    convertComparison loc cmp
  | .Csp _ =>
    throw (IO.userError "CSP literals not supported in AST conversion")

partial def convertLiteral (lit : Ast.Literal) : IO AstNode := do
  let atomN ← convertLiteralData lit.location lit.data
  unwrap (← buildLiteral lit.location (convertSign lit.sign) atomN)

partial def convertBodyLiteral (blit : Ast.BodyLiteral) : IO AstNode := do
  match blit.data with
  | .Literal lit =>
    let atomN ← convertLiteralData lit.location lit.data
    unwrap (← buildLiteral blit.location (convertSign blit.sign) atomN)
  | .Conditional cond =>
    let litN ← convertLiteral cond.literal
    let condN ← cond.condition.mapM convertLiteral
    unwrap (← buildConditionalLiteral blit.location litN condN)
  | .Aggregate agg =>
    let elemsN ← agg.elements.mapM convertConditionalLiteral
    let lgN ← convertAggregateGuard agg.left
    let rgN ← convertAggregateGuard agg.right
    unwrap (← buildAggregate blit.location (some lgN) elemsN (some rgN))
  | .BodyAggregate ba =>
    let elemsN ← ba.elements.mapM convertBodyAggElement
    let lgN ← convertAggregateGuard ba.left
    let rgN ← convertAggregateGuard ba.right
    unwrap (← buildBodyAggregate blit.location (some lgN) (convertAggFn ba.function) elemsN (some rgN))
  | .TheoryAtom _ =>
    throw (IO.userError "Theory atoms not yet supported in body literal conversion")
  | .Disjoint _ =>
    throw (IO.userError "Disjoint literals not yet supported in body literal conversion")

partial def convertConditionalLiteral (cl : Ast.ConditionalLiteral) : IO AstNode := do
  let litN ← convertLiteral cl.literal
  let condN ← cl.condition.mapM convertLiteral
  unwrap (← buildConditionalLiteral cl.literal.location litN condN)

partial def convertAggregateGuard (g : Ast.AggregateGuard) : IO AstNode := do
  let termN ← convertTerm g.term
  unwrap (← buildGuard (convertComparisonOp g.comparison) termN)

partial def convertBodyAggElement (e : Ast.Aggregate.Body.Element) : IO AstNode := do
  let tupleN ← e.tuple.mapM convertTerm
  let condN ← e.condition.mapM convertLiteral
  unwrap (← buildBodyAggregateElement tupleN condN)

partial def convertHeadLiteral (hl : Ast.HeadLiteral) : IO AstNode := do
  match hl.data with
  | .Literal lit =>
    let atomN ← convertLiteralData lit.location lit.data
    unwrap (← buildLiteral hl.location (convertSign lit.sign) atomN)
  | .Disjunction elems =>
    let elemsN ← elems.mapM convertConditionalLiteral
    unwrap (← buildDisjunction hl.location elemsN)
  | .Aggregate agg =>
    let elemsN ← agg.elements.mapM convertConditionalLiteral
    let lgN ← convertAggregateGuard agg.left
    let rgN ← convertAggregateGuard agg.right
    unwrap (← buildAggregate hl.location (some lgN) elemsN (some rgN))
  | .HeadAggregate ha =>
    let elemsN ← ha.elements.mapM convertHeadAggElement
    let lgN ← convertAggregateGuard ha.left
    let rgN ← convertAggregateGuard ha.right
    unwrap (← buildHeadAggregate hl.location (some lgN) (convertAggFn ha.function) elemsN (some rgN))
  | .TheoryAtom _ =>
    throw (IO.userError "Theory atoms not yet supported in head literal conversion")

partial def convertHeadAggElement (e : Ast.Aggregate.Head.Element) : IO AstNode := do
  let tupleN ← e.tuple.mapM convertTerm
  let condN ← convertConditionalLiteral e.conditional_literal
  unwrap (← buildHeadAggregateElement tupleN condN)

end

def convertStatement (stmt : Ast.Statement) : IO AstNode := do
  let loc := stmt.location
  match stmt.data with
  | .Rule head body =>
    let headN ← convertHeadLiteral head
    let bodyN ← body.mapM convertBodyLiteral
    unwrap (← buildRule loc headN bodyN)
  | .Const defn =>
    let valN ← convertTerm defn.value
    unwrap (← buildDefinition loc defn.name valN (if defn.isDefault then 1 else 0))
  | .ShowSignature sig csp =>
    let name := signatureName sig
    let arity := signatureArity sig
    let positive := signatureIsPositive sig
    let _ := csp -- csp flag not directly supported in 5.8 show_signature builder
    unwrap (← buildShowSignature loc name (arity.toNat.toUInt32.toInt32) (if positive then 1 else 0))
  | .ShowTerm term body _ =>
    let termN ← convertTerm term
    let bodyN ← body.mapM convertBodyLiteral
    unwrap (← buildShowTerm loc termN bodyN)
  | .Minimize weight priority tuple body =>
    let wN ← convertTerm weight
    let pN ← convertTerm priority
    let tupleN ← tuple.mapM convertTerm
    let bodyN ← body.mapM convertBodyLiteral
    unwrap (← buildMinimize loc wN pN tupleN bodyN)
  | .Script sty code =>
    let name := match sty with | .Lua => "lua" | .Python => "python"
    unwrap (← buildScript loc name code)
  | .Program name params =>
    let paramsN ← params.mapM fun p => do unwrap (← buildId p.location p.id)
    unwrap (← buildProgram loc name paramsN)
  | .External atom body ty =>
    let atomN ← convertTerm atom
    let atomNodeN ← unwrap (← buildSymbolicAtom atomN)
    let bodyN ← body.mapM convertBodyLiteral
    let tyN ← convertTerm ty
    unwrap (← buildExternal loc atomNodeN bodyN tyN)
  | .Edge u v body =>
    let uN ← convertTerm u
    let vN ← convertTerm v
    let bodyN ← body.mapM convertBodyLiteral
    unwrap (← buildEdge loc uN vN bodyN)
  | .Heuristic atom body bias priority modifier =>
    let atomN ← convertTerm atom
    let atomNodeN ← unwrap (← buildSymbolicAtom atomN)
    let bodyN ← body.mapM convertBodyLiteral
    let biasN ← convertTerm bias
    let prioN ← convertTerm priority
    let modN ← convertTerm modifier
    unwrap (← buildHeuristic loc atomNodeN bodyN biasN prioN modN)
  | .ProjectAtom atom body =>
    let atomN ← convertTerm atom
    let atomNodeN ← unwrap (← buildSymbolicAtom atomN)
    let bodyN ← body.mapM convertBodyLiteral
    unwrap (← buildProjectAtom loc atomNodeN bodyN)
  | .ProjectAtomSignature sig =>
    let name := signatureName sig
    let arity := signatureArity sig
    let positive := signatureIsPositive sig
    unwrap (← buildProjectSignature loc name (arity.toNat.toUInt32.toInt32) (if positive then 1 else 0))
  | .TheoryDefinition name terms atoms =>
    let termsN ← terms.mapM convertTheoryTermDef
    let atomsN ← atoms.mapM convertTheoryAtomDef
    unwrap (← buildTheoryDefinition loc name termsN atomsN)
  | .Defined sig =>
    let name := signatureName sig
    let arity := signatureArity sig
    let positive := signatureIsPositive sig
    unwrap (← buildDefined loc name (arity.toNat.toUInt32.toInt32) (if positive then 1 else 0))
where
  convertTheoryTermDef (td : Ast.Theory.TermDefinition) : IO AstNode := do
    let opsN ← td.operators.mapM fun (op : Ast.Theory.OperatorDefinition) => do
      let opty : Ast.Theory.OperatorType := op.type
      let opTy : TheoryOperatorType := match opty with
        | .Unary       => .unary
        | .BinaryLeft  => .binaryLeft
        | .BinaryRight => .binaryRight
      unwrap (← buildTheoryOperatorDefinition op.location op.name (op.priority.toNat.toUInt32.toInt32) opTy)
    unwrap (← buildTheoryTermDefinition td.location td.name opsN)
  convertTheoryAtomDef (ad : Ast.Theory.AtomDefinition) : IO AstNode := do
    let adty : Ast.Theory.AtomDefinition.Type := ad.type
    let adTy : TheoryAtomDefinitionType := match adty with
      | .Head      => .head
      | .Body      => .body
      | .Any       => .any
      | .Directive => .directive
    let guardN ← convertTheoryGuardDef ad.guard
    unwrap (← buildTheoryAtomDefinition ad.location adTy ad.name (ad.arity.toNat.toUInt32.toInt32) ad.elements (some guardN))
  convertTheoryGuardDef (gd : Ast.Theory.GuardDefinition) : IO AstNode := do
    unwrap (← buildTheoryGuardDefinition gd.operators gd.term)

end Clingo.AstConvert
