/-!
# Flat semantic AST for C declarations

This is the AST the parser produces and the codegen consumes — and the AST
we pretty-print back out for shim emission. It is **not** a 1:1 mirror of
the C grammar (precedence-laddered expression nonterminals collapse into
single inductives) and it does **not** model statement bodies — we only
care about what appears in a header.

If you need a full ANSI-C tree, see `reference/c-parser-upstream/`.
-/

namespace LeanBindgen.C

/-- Sign-agnostic primitive scalar kind. -/
inductive ScalarKind
  | char | short | int | long | longLong | float | double | bool
  deriving Repr, DecidableEq, Inhabited

/-- C signedness. `unspecified` corresponds to e.g. plain `int`. -/
inductive Signedness | signed | unsigned | unspecified
  deriving Repr, DecidableEq, Inhabited

mutual

/--
A C type.

Notes on shape:
- Qualifiers (`const`, `volatile`) wrap a type rather than appearing as
  flags on every constructor. So `const char *` is
  `pointer (const (scalar .char .unspecified))`.
- `typedef` references and tagged-type references (`structRef`, etc.) are
  unresolved at parse time — annotation processing or a follow-up pass can
  resolve them against a typedef environment.
- `function` here is the *function type* (used for function-pointer
  typedefs, function prototypes, etc.). Top-level prototype declarations
  are encoded via `CDecl.function`, not as a `variable` of function type.
-/
inductive CType where
  | void
  | scalar  (kind : ScalarKind) (sign : Signedness := .unspecified)
  | typedef (name : String)
  | pointer (to : CType)
  | const   (ty : CType)
  | volatile (ty : CType)
  | array   (elt : CType) (size : Option Nat)
  | structRef (tag : String)
  | unionRef  (tag : String)
  | enumRef   (tag : String)
  | function  (ret : CType) (params : Array CParam) (variadic : Bool := false)
  deriving Inhabited

/-- A function parameter — an optional name plus its type. -/
structure CParam where
  name : Option String
  type : CType
  deriving Inhabited

end

/-- A struct/union field. `bitfield` is the bit width when present. -/
structure CField where
  name     : String
  type     : CType
  bitfield : Option Nat := none
  deriving Inhabited

/--
A top-level declaration appearing in a header.

`function` is a prototype. `variable` is an `extern`-declared global (rare
in headers but appears occasionally). `macroConst` captures values from
`#define X 42` — the value is preserved as the literal text from the
source so we can decide later whether to interpret it as an int / string /
expression.
-/
inductive CDecl where
  | typedef    (name : String) (ty : CType)
  | structDef  (tag : Option String) (fields : Array CField)
  | unionDef   (tag : Option String) (fields : Array CField)
  | enumDef    (tag : Option String) (variants : Array (String × Option Int))
  | function   (name : String) (ret : CType) (params : Array CParam)
               (variadic : Bool := false)
  | variable   (name : String) (ty : CType)
  | macroConst (name : String) (value : String)
  deriving Inhabited

/-- A parsed header, in source order. -/
structure CHeader where
  decls : Array CDecl
  deriving Inhabited

end LeanBindgen.C
