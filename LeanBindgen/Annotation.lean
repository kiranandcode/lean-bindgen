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

/-- Byte size of a Lean ctor scalar field of this type. -/
def ScalarTarget.byteSize : ScalarTarget → Nat
  | .unit                                      => 0
  | .uint8 | .int8 | .bool                     => 1
  | .uint16 | .int16                           => 2
  | .uint32 | .int32 | .float                  => 4
  | .uint64 | .int64 | .double | .usize | .isize => 8

/-- The `lean_ctor_*_<suffix>` family that reads/writes a Lean ctor
scalar field of this type. -/
def ScalarTarget.ctorScalar : ScalarTarget → String
  | .unit                                       => ""
  | .uint8 | .int8 | .bool                      => "uint8"
  | .uint16 | .int16                            => "uint16"
  | .uint32 | .int32                            => "uint32"
  | .uint64 | .int64                            => "uint64"
  | .float                                      => "float32"
  | .double                                     => "float"
  | .usize | .isize                             => "usize"

/-- How a C enum (and its accompanying integer typedef, if any) maps
to a Lean inductive. The `enumTag` names the underlying C enum so the
codegen can look up each variant's integer value from the parsed
header. The `variants` array names the Lean constructors and pairs
each with its source-of-truth C variant. -/
structure EnumMapping where
  /-- The tag of the C `enum` declaration (without the `enum ` prefix),
  e.g. `clingo_error`, `clingo_symbol_type`. -/
  enumTag  : String
  /-- `(cVariant, leanVariant)` for each constructor. -/
  variants : Array (String × String)
  deriving Inhabited

/-- How a C struct maps to a Lean structure. The codegen looks up the
struct's body in the parsed header by tag, then for each field listed
here resolves the C field type to a Lean type and emits both the Lean
`structure` declaration and a pair of `<lean>_to_lean` /
`lean_to_<lean>` shim helpers that translate between the C struct and
the Lean ctor representation.

Fields are emitted in the order given. Lean's struct layout places
all boxed fields first (in declaration order) followed by all scalar
fields (in declaration order, packed by their natural C size with no
reordering). -/
structure StructMapping where
  /-- The tag of the C `struct` declaration, without the `struct `
  prefix (e.g. `clingo_location`). -/
  cStructTag : String
  /-- `(cField, leanField)` pairs in the desired declaration order. -/
  fields : Array (String × String)
  deriving Inhabited

/-- How a C type is presented in Lean. -/
inductive TypeMapping
  /-- Wrap an integer C typedef as a fresh Lean `def`. -/
  | scalarNewtype (k : ScalarTarget)
  /-- An incomplete C struct accessed only by pointer becomes
  `opaque T : Type` plus a `lean_external_class` registration on the
  shim side. `finalizer` names the C function that frees a value
  (e.g. `clingo_control_free`) and is run from the Lean GC when the
  last reference is dropped. -/
  | opaquePointer (finalizer : String)
  /-- A C enum (or `int`-typedef paired with one) becomes a Lean
  `inductive` with one nullary constructor per variant. The codegen
  emits a pair of conversion helpers in the shim that translate
  between the C integer value and Lean's constructor index. -/
  | inductiveEnum (mapping : EnumMapping)
  /-- A C struct with concrete fields becomes a Lean `structure`. The
  codegen emits a Lean structure declaration plus a pair of conversion
  helpers in the shim that move between the C struct and Lean's ctor
  representation. -/
  | structRecord (mapping : StructMapping)
  /-- A C function-pointer typedef (e.g. `clingo_logger_t`) becomes a
  Lean closure type. The Lean type is auto-derived from the C
  function-pointer signature: each non-`void *` parameter is resolved
  via the same rules used for regular function parameters; the
  trailing `void *` is taken as the user-data slot and is *not*
  exposed to Lean. The codegen emits a per-callback trampoline that
  marshals C args back into Lean values and invokes the closure via
  `lean_apply_*`. -/
  | callback
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
  /-- 0-based indices of `void *` parameters that are the user-data
  slots paired with a preceding callback parameter. These are dropped
  from the Lean signature and supplied by the codegen as the boxed
  Lean closure pointer; the preceding callback parameter (which must
  be of a `.callback`-mapped typedef) becomes a single Lean closure. -/
  callbackUserDataParams : List Nat := []
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
