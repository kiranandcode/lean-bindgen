import LeanBindgen.C.Ast

/-!
# Bindgen annotations

The annotations the user writes alongside a C header to drive code
generation. We keep this as a plain data record (no DSL) so the
typechecker enforces shape and Lean autocomplete just works — the
header parser produces a `CHeader`, the annotation file produces a
`Bindings`, and the codegen consumes both.

The MVP covers the two function shapes that dominate the cleango
bindings:

* `direct`: the C function's return value is the Lean function's
  return value. Most accessors fall here (`clingo_signature_arity`,
  `clingo_symbol_type`, …).
* `outParamBoolStatus`: the C function returns `bool` for success,
  writes its real result through an out-pointer, and on failure leaves
  an error message accessible via a thread-local `clingo_error_*`
  function. Most "constructor" / "create" functions in clingo use
  this.

For type bindings the MVP covers:

* `scalarNewtype`: a C typedef of an integer type → a Lean `def Foo
  := UIntXX` (the codegen also emits `deriving Repr`).
* `opaquePointer`: a C `struct foo` accessed only via pointer →
  `opaque Foo : Type`.

Other shapes (struct-of-fields, enum-as-inductive, callback function
pointers, array+size pairs) will land as we encounter them.
-/

namespace LeanBindgen

/-- The Lean scalar a C scalar maps to. -/
inductive ScalarTarget
  | unit
  | uint8 | uint16 | uint32 | uint64
  | int8  | int16  | int32  | int64
  | bool
  | float | double
  | usize | isize
  deriving Repr, BEq, Inhabited

/-- The Lean source-level identifier for the target. -/
def ScalarTarget.toLean : ScalarTarget → String
  | .unit   => "Unit"
  | .uint8  => "UInt8"
  | .uint16 => "UInt16"
  | .uint32 => "UInt32"
  | .uint64 => "UInt64"
  | .int8   => "Int8"
  | .int16  => "Int16"
  | .int32  => "Int32"
  | .int64  => "Int64"
  | .bool   => "Bool"
  | .float  => "Float32"
  | .double => "Float"
  | .usize  => "USize"
  | .isize  => "ISize"

/-- The C type-name (for shim casts). -/
def ScalarTarget.toC : ScalarTarget → String
  | .unit   => "void"
  | .uint8  => "uint8_t"
  | .uint16 => "uint16_t"
  | .uint32 => "uint32_t"
  | .uint64 => "uint64_t"
  | .int8   => "int8_t"
  | .int16  => "int16_t"
  | .int32  => "int32_t"
  | .int64  => "int64_t"
  | .bool   => "_Bool"
  | .float  => "float"
  | .double => "double"
  | .usize  => "size_t"
  | .isize  => "ptrdiff_t"

/-- How a C type is presented in Lean. -/
inductive TypeMapping
  | scalarNewtype (k : ScalarTarget)
  | opaquePointer
  deriving Inhabited

structure TypeAnno where
  /-- The C typedef or tag we're describing. -/
  cName  : String
  /-- The unqualified Lean identifier we emit. -/
  lean   : String
  mapping : TypeMapping
  deriving Inhabited

/-- How a C function's signature maps to Lean's. -/
inductive FunctionStyle
  /-- Lean signature mirrors the C signature directly: return value of
  the C function is the Lean function's return value. -/
  | direct
  /-- C function returns `bool`, true=success. The parameter at
  `outParamIdx` (0-based) is an out-pointer; its pointee becomes the
  Lean function's success value. On failure the result is an error
  string read from `errorMessageFn` (a no-arg C function returning
  `char const *`). The Lean return type becomes `IO (Except String T)`. -/
  | outParamBoolStatus (outParamIdx : Nat) (errorMessageFn : String)
  deriving Inhabited

structure FunctionAnno where
  /-- The C function name. -/
  cName : String
  /-- The fully-qualified Lean name we emit, e.g. `Clingo.Signature.mk`. -/
  lean  : String
  style : FunctionStyle := .direct
  /-- Whether to mark the result as `IO`. `direct` style with this set
  to `false` emits a *pure* opaque (the user is asserting the function
  has no observable side effects beyond reading its inputs). -/
  inIO  : Bool := false
  /-- 0-based indices of parameters that should be passed as `@&`. By
  default, scalar parameters are passed by value; `String` and
  user-defined `opaquePointer` types default to `@&` already, so the
  override here is mostly to *force* by-value or to mark unusual cases. -/
  borrowedParams : List Nat := []
  /-- Override the default extern symbol name (`lean_<cName>`). Useful
  when the bindings author prefers a Lean-side naming convention like
  `lean_<libPrefix>_<short>` (e.g. `lean_clingo_signature_mk`). -/
  externSymbol : Option String := none
  deriving Inhabited

/-- A complete bindgen specification: where the header is, where to
write generated artefacts, and what to map. -/
structure Bindings where
  /-- Path to the C header (relative to the project root). -/
  headerPath  : String
  /-- Lean module to emit, e.g. `Generated.Signature`. The codegen
  writes to `<outDir>/Generated/Signature.lean`. -/
  leanModule  : Lean.Name
  /-- Output directory for generated Lean (relative to project root). -/
  outDir      : String
  /-- Shim source path (relative to project root). -/
  shimPath    : String
  /-- The library name passed to `extern_lib` and used to namespace
  shim symbols (`lean_<libPrefix>_<cName>`). -/
  libPrefix   : String
  types       : Array TypeAnno     := #[]
  functions   : Array FunctionAnno := #[]
  /-- Imports the generated Lean module needs (e.g. `Lean`). -/
  leanImports : Array Lean.Name    := #[]
  deriving Inhabited

end LeanBindgen
