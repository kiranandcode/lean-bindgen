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

Fields are emitted in the order given. Lean's runtime ctor layout
reorders fields: pointer (boxed) fields first (declaration order),
then USize fields (declaration order), then other scalars by
descending byte size (declaration order within same size). -/
structure StructMapping where
  /-- The tag of the C `struct` declaration, without the `struct `
  prefix (e.g. `clingo_location`). -/
  cStructTag : String
  /-- `(cField, leanField)` pairs in the desired declaration order.
  For array-field pairs, include only the *data* field here (the
  size field is hidden from the Lean struct). -/
  fields : Array (String × String)
  /-- Pairs `(dataField, sizeField)` for C array-field pairs within
  the struct. The `dataField` must appear in `fields`; the
  `sizeField` must NOT appear in `fields`. On the Lean side, the
  data field becomes `Array T` (element type resolved from the
  pointer target). -/
  arrayFields : Array (String × String) := #[]
  deriving Inhabited

/-- One variant of a tagged union: associates a C enum value with a
Lean constructor and the union field that carries the payload. -/
structure TaggedVariant where
  /-- The C enum constant (e.g. `clingo_ast_term_type_symbol`). -/
  cTag : String
  /-- The Lean constructor name (e.g. `symbol`). -/
  leanCtor : String
  /-- The union field name in the C struct (e.g. `symbol`). -/
  unionField : String
  /-- Lean type(s) for this variant's payload. If empty, the
  constructor is nullary. Each entry is a `(leanName, leanType)` pair
  where `leanType` is expressed in terms of already-mapped Lean types
  (resolved at codegen time). For single-field variants, a single
  entry suffices; for variants with multiple fields (e.g. a nested
  struct), provide one per payload field.
  If `none`, the payload type is resolved from the union field's C
  type automatically. -/
  payloadOverride : Option (Array (String × String)) := none
  deriving Inhabited

/-- Describes a C tagged-union struct: a struct with a discriminator
enum field and an anonymous union. The Lean codegen emits either a
standalone `inductive` (when `sharedFields` is empty) or a wrapper
`structure` plus an inner `inductive` (when shared fields exist). -/
structure TaggedUnionMapping where
  /-- The tag of the C `struct` declaration. -/
  cStructTag : String
  /-- The C field name of the discriminator (e.g. `type`). -/
  tagField : String
  /-- The C `enum` tag backing the discriminator. -/
  tagEnum : String
  /-- Shared fields (present in all variants) that become fields of
  a wrapper structure. `(cField, leanField)` pairs. -/
  sharedFields : Array (String × String) := #[]
  /-- Per-variant descriptions. -/
  variants : Array TaggedVariant
  deriving Inhabited

/-- A C unsigned/integer typedef where individual bits are meaningful
flags, unpacked into a Lean `structure` of `Bool` fields. Each field
tests `(val & mask) != 0`. The `mask` is a C enum constant looked up
from the parsed header. -/
structure BitfieldMapping where
  /-- The C `enum` tag whose constants name the bit masks (e.g.
  `clingo_solve_result`). -/
  enumTag : String
  /-- `(cEnumConstant, leanFieldName)` pairs, one per Bool field. -/
  fields : Array (String × String)
  deriving Inhabited

/-- How a variant's `void *event` pointer should be interpreted in the
event callback trampoline. -/
inductive EventInterpretation
  /-- Cast `void *` to `T *` and wrap as a Lean external object.
  When `nullable`, wraps in `Option`. -/
  | opaquePtr (typeAnnoLean : String) (nullable : Bool := false)
  /-- Cast `void *` to `T *`, dereference, and convert via the
  type's existing `<type>_to_lean` helper. -/
  | derefMapped (typeAnnoLean : String)
  /-- Cast `void *` to `T **`, index `[0]..[count-1]`, wrap each
  as a Lean external object. The resulting ctor has `count` fields. -/
  | ptrArray (typeAnnoLean : String) (count : Nat)
  deriving Inhabited

/-- One variant of an event callback: associates a C enum value with a
Lean constructor and describes how to interpret the `void *event`. -/
structure EventVariant where
  /-- The C enum constant (e.g. `clingo_solve_event_type_model`). -/
  cEnumValue : String
  /-- The Lean constructor name (e.g. `model`). -/
  leanCtor : String
  /-- How to interpret the `void *event` for this variant. -/
  interpretation : EventInterpretation
  /-- Field names for multi-field variants (e.g. ptrArray). -/
  fieldNames : Array String := #[]
  deriving Inhabited

/-- Describes a polymorphic event callback like
`clingo_solve_event_callback_t` where a single `void *event` carries
different typed data depending on an enum discriminant. The codegen
emits a Lean `inductive` for the event type and a trampoline with
switch-dispatch. -/
structure EventCallbackMapping where
  /-- Lean inductive name for the event type (e.g. `SolveEvent`). -/
  eventTypeName : String
  /-- 0-based param index of the discriminant enum in the C callback. -/
  discriminantIdx : Nat
  /-- 0-based param index of the `void *event` in the C callback. -/
  eventIdx : Nat
  /-- 0-based param index of `void *` user-data. -/
  userDataIdx : Nat
  /-- 0-based indices of out-params (e.g. `bool *goon`). -/
  outParams : Array Nat
  /-- Lean return type of the callback (e.g. `"Bool"`). -/
  leanReturnType : String
  /-- Per-variant descriptions. -/
  variants : Array EventVariant
  /-- The C expression to return on success (default `"1"`). -/
  successReturnValue : String := "1"
  /-- C type for each out-param, for correct unboxing (default all `"bool"`). -/
  outParamTypes : Array String := #[]
  deriving Inhabited

/-- Kind of a mutable struct field for getter/setter generation. -/
inductive MutableFieldKind where
  | scalar
  | stringReadOnly
  /-- Set ptr+size from `@& ByteArray`. `sizeField` names the C size field. -/
  | byteArrayInput (sizeField : String)
  /-- Alloc internal buffer, get output. `sizeField` names the C size field. -/
  | byteArrayOutput (sizeField : String)
  deriving Inhabited

/-- One field of a mutable struct mapping. -/
structure MutableFieldSpec where
  cName    : String
  leanName : String
  kind     : MutableFieldKind := .scalar
  readOnly : Bool := false
  deriving Inhabited

/-- A heap-allocated C struct with mutable field access. The Lean side
gets an opaque type backed by a wrapper struct; the codegen emits
alloc + per-field getters/setters. -/
structure MutableStructMapping where
  cStructTag : String
  cTypedef   : Option String := none
  fields     : Array MutableFieldSpec := #[]
  finalizer  : Option String := none
  deriving Inhabited

/-- A named constant from the C header (`#define` or user-supplied value). -/
structure ConstAnno where
  cName : String
  lean  : String
  type  : String              -- "Int32", "UInt32", etc.
  value : Option String := none -- if none, lookup from parsed CDecl.macroConst
  deriving Inhabited

/-- How the error payload is constructed for `outParamBoolStatus`. -/
inductive ErrorReturn
  /-- Call `errorMessageFn()` → `char const *`, wrap as `Except String T`. -/
  | string (errorMessageFn : String)
  /-- Call `errorCodeFn()` → C enum, convert to Lean enum via helpers,
  wrap as `Except <enumLean> T`. -/
  | enum (errorCodeFn : String) (enumLean : String)
  /-- Build `(enumLean × String)` from both an error code and a message
  function, wrap as `Except (<enumLean> × String) T`. -/
  | tuple (errorCodeFn : String) (enumLean : String) (errorMessageFn : String)
  deriving Inhabited

/-- The kind of an argument to a variadic builder function. -/
inductive BuilderArgKind
  | number | symbol | location | string
  | ast | optionalAst | stringArray | astArray
  deriving Repr, Inhabited

/-- One argument of a variadic builder constructor. -/
structure BuilderArg where
  kind     : BuilderArgKind
  leanName : String
  enumType : Option String := none  -- for .number: Lean enum type name
  deriving Repr, Inhabited

/-- One fixed-arity wrapper around a variadic builder call. -/
structure BuilderConstructor where
  enumValue : String       -- C enum constant (e.g. "clingo_ast_type_rule")
  leanName  : String       -- Lean function name (e.g. "buildRule")
  args      : Array BuilderArg
  deriving Repr, Inhabited

/-- Describes a variadic C function (like `clingo_ast_build`) for which
the codegen emits one fixed-arity C wrapper per constructor variant. -/
structure VariadicBuilderMapping where
  variadicFn     : String          -- e.g. "clingo_ast_build"
  resultType     : String          -- Lean opaque type name (e.g. "AstNode")
  resultCType    : String          -- C type (e.g. "clingo_ast_t")
  locationCType  : String := ""    -- C typedef for location args
  locationLean   : String := ""    -- Lean struct name for location args
  symbolCType    : String := ""    -- C type for symbol args (e.g. "clingo_symbol_t")
  symbolLean     : String := ""    -- Lean type for symbol args (e.g. "Symbol")
  errorReturn    : ErrorReturn
  constructors   : Array BuilderConstructor
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
  /-- A C struct with a tag enum + anonymous union becomes a Lean
  `inductive` (or a wrapper structure + inner inductive when shared
  fields exist). -/
  | taggedUnion (mapping : TaggedUnionMapping)
  /-- A C unsigned/int typedef whose bits are interpreted as flags,
  unpacked into a Lean `structure` of `Bool` fields. -/
  | bitfieldStruct (mapping : BitfieldMapping)
  /-- A polymorphic event callback where `void *event` carries
  different typed data per event type. Emits a Lean inductive for
  the event and a trampoline with switch-dispatch. -/
  | eventCallback (ec : EventCallbackMapping)
  /-- A heap-allocated C struct with mutable field access. The Lean side
  gets an opaque type; the shim allocates a wrapper struct (original
  struct as first member) and provides per-field getters/setters. -/
  | mutableStruct (mapping : MutableStructMapping)
  /-- A variadic C function (e.g. `clingo_ast_build`) for which the
  codegen emits one fixed-arity C wrapper per constructor variant. The
  type annotation itself is a no-op (the result type must be separately
  annotated as `opaquePointer`). -/
  | variadicBuilder (mapping : VariadicBuilderMapping)
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
  described by `error`. The Lean return type becomes
  `IO (Except <ErrorTy> T)`. -/
  | outParamBoolStatus (outParamIdx : Nat) (error : ErrorReturn)
  /-- C function returns `bool`, true=success, but has no out-pointer.
  The Lean return type becomes `IO (Except <ErrorTy> Unit)`. All
  parameters are passed through; the bool return is consumed
  internally. -/
  | boolStatus (error : ErrorReturn)
  /-- C function returns `bool` where `false` means "not applicable"
  (not an error). The parameter at `outParamIdx` is an out-pointer;
  its pointee becomes the Lean function's success value. The Lean
  return type becomes `IO (Option T)` (or `Option T` if `inIO` is
  false). -/
  | optionOutParam (outParamIdx : Nat)
  /-- C function returns `void` and writes its real result through an
  out-pointer at `outParamIdx`. The function always succeeds (no error
  path). The Lean return type is just the pointee type (or `IO T` if
  `inIO` is true). -/
  | voidOutParam (outParamIdx : Nat)
  /-- C function returns `bool` with two out-params forming an array:
  a data pointer at `ptrIdx` and a size at `sizeIdx`. On failure
  (false), returns `Option.none`. On success, builds `Array T` from
  the data/size and returns `Option.some(arr)`. -/
  | optionOutArray (ptrIdx : Nat) (sizeIdx : Nat)
  /-- Two-step caller-allocates pattern: first call `sizeFn` to get the
  buffer size, then malloc and call the main function (`cName`). Both
  must return `bool`. `bufIdx` and `sizeIdx` are the 0-based indices
  of the buffer pointer and size parameters in the main function;
  all other parameters are "shared" (passed to both calls). -/
  | callerAllocates (sizeFn : String) (bufIdx : Nat) (sizeIdx : Nat) (error : ErrorReturn)
      (nullTerminated : Bool := true) (resultKind : Option String := none)
  /-- Void-return function where multiple params are out-pointers. The
  Lean return type is a right-nested `Prod` of the pointee types:
  `(T₁ × T₂ × ... × Tₙ)`. All out-param indices are dropped from the
  visible Lean signature. -/
  | multiOutParam (outParamIndices : Array Nat)
  /-- C function returns `bool`, true=success. Two out-params at
  `ptrIdx` (`uint8_t **`) and `sizeIdx` (`size_t *`) form a byte-buffer
  result. On success, the shim builds a `ByteArray` via `lean_alloc_sarray`.
  The Lean return type is `IO (Except <ErrorTy> ByteArray)`. -/
  | byteArrayOutBoolStatus (ptrIdx : Nat) (sizeIdx : Nat) (error : ErrorReturn)
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
  /-- Pairs `(dataParamIdx, sizeParamIdx)` identifying C parameter
  pairs of the form `(T const *data, size_t data_size)` that should
  be presented as a single `Array T` on the Lean side. Both the data
  pointer and the size parameter are dropped from the Lean signature
  and replaced by a single `Array` argument at the data-pointer's
  position. -/
  arrayPairs : List (Nat × Nat) := []
  /-- Pairs `(dataParamIdx, sizeParamIdx)` identifying C parameter
  pairs of the form `(uint8_t const *data, size_t len)` that should
  be presented as a single `ByteArray` on the Lean side. Like
  `arrayPairs` but uses `lean_sarray_*` (compact scalar array) instead
  of `lean_array_*` (boxed element array). -/
  byteArrayPairs : List (Nat × Nat) := []
  /-- 0-based indices of parameters whose data is retained by the C
  function beyond the call. For these parameters, the shim deep-copies
  (malloc) nested struct payloads but does NOT free them in the postlude
  — ownership is transferred to C. Default empty = C only borrows
  during the call (shim frees deep-copied payloads after C returns). -/
  retainedParams : List Nat := []
  /-- When true, a direct-style function returning `char const *` maps
  to `Option String` instead of `String`: NULL → `Option.none`, non-NULL
  → `Option.some(lean_mk_string(…))`. -/
  nullableReturn : Bool := false
  /-- When true with `outParamBoolStatus`, the out-param pointer may be
  NULL on success (e.g. `clingo_solve_handle_model` returns NULL when no
  model is available). The Lean return type becomes
  `IO (Except ErrorTy (Option T))` instead of `IO (Except ErrorTy T)`. -/
  nullableOutParam : Bool := false
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
  constants   : Array ConstAnno    := #[]
  /-- Imports the generated Lean module needs (e.g. `Lean`). -/
  leanImports : Array Lean.Name    := #[]
  /-- When non-empty, run `cc -E -P <preprocessorArgs> <headerPath>` to
  preprocess the header before parsing. Typically contains `-I` and
  `-D` flags. -/
  preprocessorArgs : Array String  := #[]
  deriving Inhabited

end LeanBindgen
