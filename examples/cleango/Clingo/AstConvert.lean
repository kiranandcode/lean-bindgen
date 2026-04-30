import Clingo.Ast
import Clingo.Generated.ClingoBindings

/-! Pure Lean conversion from hand-written `Clingo.Ast` types to auto-generated
`Clingo.Generated.ClingoBindings` types. The auto-generated C shim handles
the `ClingoBindings.AstStatement` → C conversion. -/

namespace Clingo.AstConvert

open Clingo.Generated

private def convertSign : Ast.Sign → ClingoBindings.Sign
  | .None           => .none
  | .Negation       => .negation
  | .DoubleNegation => .doubleNegation

private def convertCompOp : Ast.Comparison.Operator → ClingoBindings.ComparisonOperator
  | .GT  => .gt  | .LT  => .lt  | .LEQ => .leq
  | .GEQ => .geq | .NEQ => .neq | .EQ  => .eq

private def convertUnaryOp : Ast.UnaryOperator → ClingoBindings.UnaryOperator
  | .Minus => .minus | .Negation => .negation | .Absolute => .absolute

private def convertBinaryOp : Ast.BinaryOperator → ClingoBindings.BinaryOperator
  | .XOR => .xor | .OR => .or | .AND => .and | .PLUS => .plus
  | .MINUS => .minus | .MULTIPLICATION => .multiplication | .DIVISION => .division
  | .MODULO => .modulo | .POWER => .power

private def convertAggrFn : Ast.Aggregate.Function → ClingoBindings.AggregateFunction
  | .Count => .count | .Sum => .sum | .Sump => .sump | .Min => .min | .Max => .max

private def convertScriptType : Ast.ScriptType → ClingoBindings.ScriptType
  | .Lua => .lua | .Python => .python

private def convertTheoryOpType : Ast.Theory.OperatorType → ClingoBindings.TheoryOperatorType
  | .Unary => .unary | .BinaryLeft => .binaryLeft | .BinaryRight => .binaryRight

private def convertTheoryAtomDefType : Ast.Theory.AtomDefinition.Type → ClingoBindings.TheoryAtomDefinitionType
  | .Head => .head | .Body => .body | .Any => .any | .Directive => .directive

mutual

partial def convertTerm : Ast.Term → IO ClingoBindings.AstTerm
  | .mk loc data => do
    let d ← convertTermData data
    pure ⟨loc, d⟩

partial def convertTermData : Ast.Term.Data → IO ClingoBindings.AstTerm.Data
  | .Symbol sym            => do let s ← Symbol.mk sym; pure (.symbol s)
  | .Variable var          => pure (.variable var)
  | .UnaryOperator op arg  => do
    let a ← convertTerm arg
    pure (.unaryOperation ⟨convertUnaryOp op, a⟩)
  | .BinaryOperator op l r => do
    let l' ← convertTerm l; let r' ← convertTerm r
    pure (.binaryOperation ⟨convertBinaryOp op, l', r'⟩)
  | .Interval l r => do
    let l' ← convertTerm l; let r' ← convertTerm r
    pure (.interval ⟨l', r'⟩)
  | .Function name args => do
    let a ← args.mapM convertTerm
    pure (.function ⟨name, a⟩)
  | .ExternalFunction name args => do
    let a ← args.mapM convertTerm
    pure (.externalFunction ⟨name, a⟩)
  | .Pool args => do
    let a ← args.mapM convertTerm
    pure (.pool ⟨a⟩)

partial def convertTheoryTerm : Ast.Theory.Term → IO ClingoBindings.AstTheoryTerm
  | .mk loc data => do
    let d ← convertTheoryTermData data
    pure ⟨loc, d⟩

partial def convertTheoryUnparsedElem : Ast.Theory.UnparsedTerm → IO ClingoBindings.TheoryUnparsedTermElement
  | .mk _op term => do pure ⟨← convertTheoryTerm term⟩

partial def convertTheoryTermData : Ast.Theory.Term.Data → IO ClingoBindings.AstTheoryTerm.Data
  | .Symbol sym    => do let s ← Symbol.mk sym; pure (.symbol s)
  | .Variable name => pure (.variable name)
  | .Tuple terms   => do pure (.tuple ⟨← terms.mapM convertTheoryTerm⟩)
  | .List terms    => do pure (.list ⟨← terms.mapM convertTheoryTerm⟩)
  | .Set terms     => do pure (.set ⟨← terms.mapM convertTheoryTerm⟩)
  | .Function (.mk name args) => do
    pure (.function ⟨name, ← args.mapM convertTheoryTerm⟩)
  | .UnparsedTerm elts => do
    let es ← elts.mapM convertTheoryUnparsedElem
    pure (.unparsedTerm ⟨es⟩)

end

private def convertComparison (c : Ast.Comparison) : IO ClingoBindings.AstComparison := do
  let l ← convertTerm c.left; let r ← convertTerm c.right
  pure ⟨convertCompOp c.operator, l, r⟩

private def convertCspProductTerm (t : Ast.CSPProductTerm) : IO ClingoBindings.CspProductTerm := do
  let c ← convertTerm t.coefficient; let v ← convertTerm t.variable_
  pure ⟨t.location, c, v⟩

private def convertCspSumTerm (t : Ast.CSPSumTerm) : IO ClingoBindings.CspSumTerm := do
  pure ⟨t.location, ← t.terms.mapM convertCspProductTerm⟩

private def convertCspGuard (g : Ast.CSPGuard) : IO ClingoBindings.CspGuard := do
  pure ⟨convertCompOp g.comparison, ← convertCspSumTerm g.term⟩

private def convertCspLiteral (l : Ast.CSPLiteral) : IO ClingoBindings.CspLiteral := do
  pure ⟨← convertCspSumTerm l.term, ← l.guards.mapM convertCspGuard⟩

private def convertLiteralData : Ast.Literal.Data → IO ClingoBindings.AstLiteral.Data
  | .Boolean b    => pure (.boolean b)
  | .Symbolic t   => do pure (.symbolic (← convertTerm t))
  | .Comparison c => do pure (.comparison (← convertComparison c))
  | .Csp csp      => do pure (.csp (← convertCspLiteral csp))

private def convertLiteral (l : Ast.Literal) : IO ClingoBindings.AstLiteral := do
  pure ⟨l.location, convertSign l.sign, ← convertLiteralData l.data⟩

private def convertConditionalLiteral (cl : Ast.ConditionalLiteral) : IO ClingoBindings.ConditionalLiteral := do
  pure ⟨← convertLiteral cl.literal, ← cl.condition.mapM convertLiteral⟩

private def convertAggregateGuard (g : Ast.AggregateGuard) : IO ClingoBindings.AggregateGuard := do
  pure ⟨convertCompOp g.comparison, ← convertTerm g.term⟩

private def convertAggregate (a : Ast.Aggregate) : IO ClingoBindings.AstAggregate := do
  pure ⟨← a.elements.mapM convertConditionalLiteral,
    ← convertAggregateGuard a.left, ← convertAggregateGuard a.right⟩

private def convertBodyAggregateElement (e : Ast.Aggregate.Body.Element) : IO ClingoBindings.BodyAggregateElement := do
  pure ⟨← e.tuple.mapM convertTerm, ← e.condition.mapM convertLiteral⟩

private def convertBodyAggregate (a : Ast.Aggregate.Body) : IO ClingoBindings.BodyAggregate := do
  pure ⟨convertAggrFn a.function, ← a.elements.mapM convertBodyAggregateElement,
    ← convertAggregateGuard a.left, ← convertAggregateGuard a.right⟩

private def convertHeadAggregateElement (e : Ast.Aggregate.Head.Element) : IO ClingoBindings.HeadAggregateElement := do
  pure ⟨← e.tuple.mapM convertTerm, ← convertConditionalLiteral e.conditional_literal⟩

private def convertHeadAggregate (a : Ast.Aggregate.Head) : IO ClingoBindings.HeadAggregate := do
  pure ⟨convertAggrFn a.function, ← a.elements.mapM convertHeadAggregateElement,
    ← convertAggregateGuard a.left, ← convertAggregateGuard a.right⟩

private def convertTheoryAtomElement (e : Ast.Theory.AtomElement) : IO ClingoBindings.TheoryAtomElement := do
  pure ⟨← e.tuple.mapM convertTheoryTerm, ← e.condition.mapM convertLiteral⟩

private def convertTheoryGuard (g : Ast.Theory.Guard) : IO ClingoBindings.TheoryGuard := do
  pure ⟨g.operatorName, ← convertTheoryTerm g.term⟩

private def convertTheoryAtom (a : Ast.Theory.Atom) : IO ClingoBindings.TheoryAtom := do
  pure ⟨← convertTerm a.term, ← a.elements.mapM convertTheoryAtomElement, ← convertTheoryGuard a.guard⟩

private def convertDisjointElement (e : Ast.DisjointElement) : IO ClingoBindings.DisjointElement := do
  pure ⟨e.location, ← e.tuple.mapM convertTerm, ← convertCspSumTerm e.term, ← e.condition.mapM convertLiteral⟩

private def convertHeadLiteralData : Ast.HeadLiteral.Data → IO ClingoBindings.HeadLiteral.Data
  | .Literal lit      => do pure (.literal (← convertLiteral lit))
  | .Disjunction elts => do pure (.disjunction ⟨← elts.mapM convertConditionalLiteral⟩)
  | .Aggregate agg    => do pure (.aggregate (← convertAggregate agg))
  | .HeadAggregate ha => do pure (.headAggregate (← convertHeadAggregate ha))
  | .TheoryAtom ta    => do pure (.theoryAtom (← convertTheoryAtom ta))

private def convertHeadLiteral (hl : Ast.HeadLiteral) : IO ClingoBindings.HeadLiteral := do
  pure ⟨hl.location, ← convertHeadLiteralData hl.data⟩

private def convertBodyLiteralData : Ast.BodyLiteral.Data → IO ClingoBindings.BodyLiteral.Data
  | .Literal lit      => do pure (.literal (← convertLiteral lit))
  | .Conditional cl   => do pure (.conditional (← convertConditionalLiteral cl))
  | .Aggregate agg    => do pure (.aggregate (← convertAggregate agg))
  | .BodyAggregate ba => do pure (.bodyAggregate (← convertBodyAggregate ba))
  | .TheoryAtom ta    => do pure (.theoryAtom (← convertTheoryAtom ta))
  | .Disjoint elts    => do pure (.disjoint ⟨← elts.mapM convertDisjointElement⟩)

private def convertBodyLiteral (bl : Ast.BodyLiteral) : IO ClingoBindings.BodyLiteral := do
  pure ⟨bl.location, convertSign bl.sign, ← convertBodyLiteralData bl.data⟩

private def convertTheoryOpDef (d : Ast.Theory.OperatorDefinition) : ClingoBindings.TheoryOperatorDefinition :=
  ⟨d.location, d.name, d.priority.toUInt32, convertTheoryOpType d.type⟩

private def convertTheoryTermDef (d : Ast.Theory.TermDefinition) : ClingoBindings.TheoryTermDefinition :=
  ⟨d.location, d.name, d.operators.map convertTheoryOpDef⟩

private def convertTheoryGuardDef (d : Ast.Theory.GuardDefinition) : ClingoBindings.TheoryGuardDefinition :=
  ⟨d.term⟩

private def convertTheoryAtomDef (d : Ast.Theory.AtomDefinition) : ClingoBindings.TheoryAtomDefinition :=
  ⟨d.location, convertTheoryAtomDefType d.type, d.name, d.arity.toUInt32, d.elements, convertTheoryGuardDef d.guard⟩

private def convertStatementData : Ast.Statement.Data → IO ClingoBindings.AstStatement.Data
  | .Rule head body => do
    pure (.rule ⟨← convertHeadLiteral head, ← body.mapM convertBodyLiteral⟩)
  | .Const def_ => do
    pure (.const ⟨def_.name, ← convertTerm def_.value, def_.isDefault⟩)
  | .ShowSignature sig csp =>
    pure (.showSignature ⟨sig, csp⟩)
  | .ShowTerm term body csp => do
    pure (.showTerm ⟨← convertTerm term, ← body.mapM convertBodyLiteral, csp⟩)
  | .Minimize weight priority tuple body => do
    pure (.minimize ⟨← convertTerm weight, ← convertTerm priority,
      ← tuple.mapM convertTerm, ← body.mapM convertBodyLiteral⟩)
  | .Script ty code =>
    pure (.script ⟨convertScriptType ty, code⟩)
  | .Program name params =>
    pure (.program ⟨name, params.map fun p => ⟨p.location, p.id⟩⟩)
  | .External atom body ty => do
    pure (.external ⟨← convertTerm atom, ← body.mapM convertBodyLiteral, ← convertTerm ty⟩)
  | .Edge u v body => do
    pure (.edge ⟨← convertTerm u, ← convertTerm v, ← body.mapM convertBodyLiteral⟩)
  | .Heuristic atom body bias priority modifier => do
    pure (.heuristic ⟨← convertTerm atom, ← body.mapM convertBodyLiteral,
      ← convertTerm bias, ← convertTerm priority, ← convertTerm modifier⟩)
  | .ProjectAtom atom body => do
    pure (.projectAtom ⟨← convertTerm atom, ← body.mapM convertBodyLiteral⟩)
  | .ProjectAtomSignature sig =>
    pure (.projectAtomSignature sig)
  | .TheoryDefinition name terms atoms =>
    pure (.theoryDefinition ⟨name, terms.map convertTheoryTermDef, atoms.map convertTheoryAtomDef⟩)
  | .Defined sig =>
    pure (.defined ⟨sig⟩)

def convertStatement : Ast.Statement → IO ClingoBindings.AstStatement
  | .mk loc data => do pure ⟨loc, ← convertStatementData data⟩

end Clingo.AstConvert
