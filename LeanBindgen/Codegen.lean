import LeanBindgen.Annotation
import LeanBindgen.C.Ast
import LeanBindgen.C.Pretty
import LeanBindgen.Gen.CShim
import LeanBindgen.Gen.CShimPretty
import LeanBindgen.Gen.LeanDecl
import LeanBindgen.Gen.LeanDeclPretty
import Std.Data.HashMap

/-!
# Bindgen codegen

Given a parsed `CHeader` and a user-supplied `Bindings`, emit two
artefacts:

1. **Lean module**: a sequence of `@[extern]` opaque declarations plus
   any `def`/`opaque` type declarations introduced by the
   `TypeMapping`s.
2. **C shim source**: the `lean_<libPrefix>_<cName>` functions that
   marshal between Lean's runtime objects and the underlying C ABI.

Both are produced as `String` and written to disk by the caller (the
`bindgen` executable). Keeping the formatters string-based is fine for
the volume we deal with; we don't need a streaming PrettyPrinter.

The MVP supports two function styles (`direct` and
`outParamBoolStatus`) and two type mappings (`scalarNewtype`,
`opaquePointer`). Each new shape we want to support adds one branch in
the Lean emitter and one in the shim emitter.
-/

namespace LeanBindgen
namespace Codegen

open LeanBindgen.C

/-! ## Resolving C types against Bindings

When emitting a Lean signature for a function, we need to translate
each parameter and the return type from `CType` into Lean source text.
The mapping uses the `TypeAnno`s from `Bindings` plus a small built-in
table for primitive C types and stdint width-typedefs.
-/

/-- How to marshal individual elements of a C array between Lean
boxed objects and C values. Used by `ParamMarshal.array` and
`ReturnMarshal.array`. -/
inductive ArrayElemKind
  /-- Scalar via `lean_unbox_<suffix>` / `lean_box_<suffix>`. `shimTy`
  is the C type (e.g. `"uint64_t"`), `suffix` is the lean_unbox/box
  family suffix (e.g. `"_uint64"` → `lean_unbox_uint64`). -/
  | scalar (shimTy : String) (suffix : String)
  /-- String elements: `lean_string_cstr` / `lean_mk_string`. -/
  | string
  /-- Enum elements via named C helpers. -/
  | enumHelper (toC : String) (toLean : String)
  /-- Struct elements via named C helpers. -/
  | structHelper (toC : String) (toLean : String)
  deriving Inhabited

/-- How a parameter of this resolved type gets transformed in the
shim before being passed to the wrapped C function. -/
inductive ParamMarshal
  /-- Pass through directly — for plain scalars whose Lean and C
  representations coincide. -/
  | passthrough
  /-- `lean_string_cstr` to get a `char const *` from a Lean `String`. -/
  | leanString
  /-- Pass through the named per-enum helper (`lean_to_<enum>`) which
  takes a `uint8_t cidx` and returns the C int value. -/
  | enumHelper (helperFn : String)
  /-- Treat as a Lean external object: extract the payload pointer via
  `lean_get_external_data` and cast to the given C type. -/
  | externalData (cType : String)
  /-- Build a C struct from a Lean ctor via the named helper. The
  `byPointer` flag controls whether the call site passes the local
  by address (`&`) or by value. -/
  | fromLeanStruct (helperFn : String) (cType : String) (byPointer : Bool)
  /-- A Lean closure passed as a callback. The shim takes a single
  `b_lean_obj_arg` (the closure) and must `lean_inc` it before the
  call and `lean_dec` after. At the call site the C ABI takes a
  function pointer (the named trampoline) and a `void *` user-data
  (the closure pointer); the function-shim emitter knows to emit
  *both* and to skip the matching user-data param of the C function. -/
  | callback (trampolineFn : String)
  /-- A Lean `Array T` passed as a `(T const *data, size_t size)` pair
  to the C function. The shim walks the Lean array, `malloc`s a
  temporary buffer, element-marshals each entry, passes the buffer
  and size to the C call, then `free`s the buffer. -/
  | array (cElemTy : String) (elem : ArrayElemKind)
  /-- A Lean `ByteArray` passed as a `(uint8_t const *data, size_t len)`
  pair. The shim extracts the data pointer via `lean_sarray_cptr` and
  the length via `lean_sarray_size`, passing both to the C call. -/
  | byteArray
  deriving Inhabited

/-- How a C return value of this resolved type gets transformed in
the shim before being returned to Lean. -/
inductive ReturnMarshal
  /-- Direct cast to `shimReturn` and return. -/
  | passthrough
  /-- `lean_mk_string` (handles NULL → ""). -/
  | leanString
  /-- Run through the named per-enum helper (`<enum>_to_lean`) which
  yields a `uint8_t cidx`. The IO/non-IO wrapper handles the rest. -/
  | enumHelper (helperFn : String)
  /-- Wrap a raw C pointer as a Lean external object using the named
  class-getter function. -/
  | externalAlloc (classGetter : String)
  /-- Box a C struct value into a Lean ctor via the named helper. -/
  | toLeanStruct (helperFn : String)
  /-- Build a Lean `Array T` from a C `(T *buf, size_t n)` pair.
  Used for out-parameters that return array data. -/
  | array (cElemTy : String) (elem : ArrayElemKind)
  deriving Inhabited

/-- A resolved Lean target for a C type. -/
structure ResolvedType where
  leanType   : String
  /-- C type used in the shim's parameter list. -/
  shimParam  : String
  /-- C type used in the shim's return position. -/
  shimReturn : String
  /-- C type used to declare a stack-local of this type (for example,
  to back an out-parameter). For pointer-typed values this is the
  pointee type (so `&local` has the right kind). -/
  cLocalType : String := ""
  /-- True when the value crosses as a boxed `lean_object *`. -/
  isLeanObj  : Bool := false
  /-- Number of bytes a scalar of this type occupies in a Lean ctor's
  scalar payload. Zero for boxed types (they don't contribute to the
  scalar payload). -/
  cByteSize  : Nat := 0
  /-- The `lean_ctor_*_<suffix>` family used for this type when
  reading/writing it as a struct field's scalar payload. Examples:
  `"uint32"`, `"uint64"`, `"usize"`, `"uint8"`, `"float"`, `"float32"`.
  Empty for boxed types (use `lean_ctor_get`/`lean_ctor_set`). -/
  ctorScalar : String := ""
  paramMarshal  : ParamMarshal := .passthrough
  returnMarshal : ReturnMarshal := .passthrough
  /-- Name of the `free_<type>` helper that recursively frees malloc'd
  nested payloads. `none` when the type has no deep fields. -/
  freeHelperFn : Option String := none
  deriving Inhabited

/-- Whether this type occupies a boxed (`lean_object *`) slot in a
Lean ctor layout. Enum values are boxed (`lean_box(idx)`) even though
they pass as scalars in function signatures. -/
def ResolvedType.isBoxedInCtor (r : ResolvedType) : Bool :=
  r.isLeanObj || match r.returnMarshal with
    | .passthrough => false
    | _ => true

/-- Whether this type is a pointer-to-struct in the C signature (i.e.,
the struct is passed by pointer, so the C field is `T *`). -/
def ResolvedType.isPtrToStruct (r : ResolvedType) : Bool :=
  match r.paramMarshal with
  | .fromLeanStruct _ _ true => true
  | _ => false

/-- Constructor for a "scalar-like" mapping where shim param and
return are the same C type, and the value is *not* a boxed Lean
object. -/
private def scalarRT (lean cTy : String)
    (byteSize : Nat) (ctorScalar : String) : ResolvedType :=
  { leanType := lean, shimParam := cTy, shimReturn := cTy,
    cLocalType := cTy,
    cByteSize := byteSize, ctorScalar := ctorScalar }

/-- Constructor for the Lean `String` mapping (boxed, marshalled via
`lean_string_cstr` / `lean_mk_string`). -/
private def stringRT : ResolvedType :=
  { leanType := "String", shimParam := "b_lean_obj_arg",
    shimReturn := "lean_obj_res", cLocalType := "char const *",
    isLeanObj := true,
    paramMarshal := .leanString, returnMarshal := .leanString }

/-- Build the conversion helper names for an enum mapping. -/
private def enumHelpers (lean : String) : String × String :=
  let snake := lean.foldl (init := "") fun acc c =>
    if c.isUpper && acc ≠ "" then acc ++ "_" ++ String.singleton c.toLower
    else acc ++ String.singleton c.toLower
  (s!"lean_to_{snake}", s!"{snake}_to_lean")

/-- Constructor for a Lean inductive backed by a C int (typedef +
enum). Lean compiles all-nullary inductives to a raw `uint8_t` at the
FFI boundary; the helpers translate cidx ↔ C value. -/
private def enumRT (leanName cTypedef : String) : ResolvedType :=
  let (toC, toLean) := enumHelpers leanName
  { leanType := leanName, shimParam := "uint8_t",
    shimReturn := "uint8_t", cLocalType := cTypedef,
    paramMarshal := .enumHelper toC, returnMarshal := .enumHelper toLean }

/-- Convert a CamelCase Lean name to snake_case (used for shim helper
names). -/
private def toSnake (s : String) : String :=
  s.foldl (init := "") fun acc c =>
    if c.isUpper && acc ≠ "" then acc ++ "_" ++ String.singleton c.toLower
    else acc ++ String.singleton c.toLower

/-- The class-getter name for an opaque-pointer mapping. -/
private def externalClassGetter (lean : String) : String :=
  s!"get_{toSnake lean}_class"

/-- The to/from-Lean helper names for a struct mapping. -/
private def structHelperNames (lean : String) : String × String :=
  let snake := toSnake lean
  (s!"lean_to_{snake}", s!"{snake}_to_lean")

/-- The free-helper name for a struct/tagged-union mapping. -/
private def freeHelperName (lean : String) : String :=
  s!"free_{toSnake lean}"

/-- The trampoline function name for a callback typedef. -/
private def callbackTrampolineName (cTypedef : String) : String :=
  s!"{cTypedef}_trampoline"

/-- The reverse trampoline name (Lean→C wrapper for inner callbacks). -/
private def reverseCallbackTrampolineName (cTypedef : String) : String :=
  s!"{cTypedef}_reverse_trampoline"

/-- Constructor for a Lean structure mapping when the C function uses
the value by-pointer (typical: `T const *` parameter, or `T *out`
out-parameter). -/
private def structByPtrRT (leanName cTypedef : String) : ResolvedType :=
  let (toC, toLean) := structHelperNames leanName
  { leanType := leanName, shimParam := "b_lean_obj_arg",
    shimReturn := "lean_obj_res", cLocalType := cTypedef,
    isLeanObj := true,
    paramMarshal  := .fromLeanStruct toC cTypedef true,
    returnMarshal := .toLeanStruct toLean,
    freeHelperFn  := some (freeHelperName leanName) }

/-- Constructor for a Lean structure mapping when the C function uses
the value by-value. -/
private def structByValRT (leanName cTypedef : String) : ResolvedType :=
  let (toC, toLean) := structHelperNames leanName
  { leanType := leanName, shimParam := "b_lean_obj_arg",
    shimReturn := "lean_obj_res", cLocalType := cTypedef,
    isLeanObj := true,
    paramMarshal  := .fromLeanStruct toC cTypedef false,
    returnMarshal := .toLeanStruct toLean,
    freeHelperFn  := some (freeHelperName leanName) }

/-- Constructor for a bitfield-struct mapping. Uses the same toLean/toC
helpers as regular structs, but no free helper (the C value is a plain
scalar). -/
private def bitfieldRT (leanName cTypedef : String) : ResolvedType :=
  let (toC, toLean) := structHelperNames leanName
  { leanType := leanName, shimParam := "b_lean_obj_arg",
    shimReturn := "lean_obj_res", cLocalType := cTypedef,
    isLeanObj := true,
    paramMarshal  := .fromLeanStruct toC cTypedef false,
    returnMarshal := .toLeanStruct toLean }

/-- Constructor for a callback-typedef mapping. The shim parameter is
the boxed Lean closure; the trampoline is the C function pointer that
gets wired up at the C call site. -/
private def callbackRT (leanName cTypedef : String) : ResolvedType :=
  { leanType := leanName, shimParam := "b_lean_obj_arg",
    shimReturn := "lean_obj_res", cLocalType := cTypedef,
    isLeanObj := true,
    paramMarshal := .callback (callbackTrampolineName cTypedef) }

/-- Derive the `ArrayElemKind` for an element type from its resolved
representation. This tells the array marshaller how to unbox/box each
element. -/
private def arrayElemKindOf (r : ResolvedType) : ArrayElemKind :=
  match r.paramMarshal with
  | .leanString            => .string
  | .enumHelper toC        =>
    match r.returnMarshal with
    | .enumHelper toLean   => .enumHelper toC toLean
    | _                    => .scalar r.shimParam ""
  | .fromLeanStruct toC _ _ =>
    match r.returnMarshal with
    | .toLeanStruct toLean => .structHelper toC toLean
    | _                    => .scalar r.shimParam ""
  | _ =>
    -- Scalar: derive unbox suffix from the ctor-scalar family.
    let suffix := match r.ctorScalar with
      | "uint64"  => "_uint64"
      | "uint32"  => "_uint32"
      | "usize"   => "_usize"
      | "uint8"   => ""       -- lean_unbox / lean_box (small scalars)
      | "uint16"  => ""
      | "float"   => "_float"
      | "float32" => "_float32"
      | _         => ""
    .scalar r.shimParam suffix

/-- Generate the correct `lean_box*` call for a scalar value based on
its ctor-scalar family. For example, Bool/UInt8 → `lean_box`, UInt32 →
`lean_box_uint32`, UInt64 → `lean_box_uint64`, etc. -/
private def boxScalarExpr (r : ResolvedType) (val : String) : String :=
  match r.ctorScalar with
  | "uint64"  => s!"lean_box_uint64((uint64_t){val})"
  | "uint32"  => s!"lean_box_uint32({val})"
  | "usize"   => s!"lean_box_usize({val})"
  | "float"   => s!"lean_box_float({val})"
  | "float32" => s!"lean_box_float32({val})"
  | _         => s!"lean_box({val})"

/-- Constructor for a Lean `Array T` mapping. The shim parameter is
a boxed `lean_object *` (the Lean array); the C-side value is a
`(cElemTy const *, size_t)` pair. -/
private def arrayRT (elemLeanType cElemTy : String) (elemKind : ArrayElemKind) : ResolvedType :=
  { leanType := s!"Array {elemLeanType}", shimParam := "b_lean_obj_arg",
    shimReturn := "lean_obj_res", cLocalType := cElemTy,
    isLeanObj := true,
    paramMarshal := .array cElemTy elemKind,
    returnMarshal := .array cElemTy elemKind }

/-- Constructor for a Lean `ByteArray` mapping. The shim parameter is
a boxed `lean_object *`; the C-side value is a `(uint8_t const *, size_t)`
pair extracted via `lean_sarray_cptr` and `lean_sarray_size`. -/
private def byteArrayRT : ResolvedType :=
  { leanType := "ByteArray", shimParam := "b_lean_obj_arg",
    shimReturn := "lean_obj_res", cLocalType := "uint8_t",
    isLeanObj := true,
    paramMarshal := .byteArray }

/-- Constructor for an opaque-pointer mapping. The C-side value is a
`<typedef> *` wrapped as a Lean external object; the shim extracts via
`lean_get_external_data` (with a cast) and wraps via
`lean_alloc_external` against a per-type class.

`cTypedef` is the typedef name of the *underlying struct* (e.g.,
`clingo_control_t`); the C-side payload type is `cTypedef *`. -/
private def opaqueRT (leanName cTypedef : String) : ResolvedType :=
  let cPtr := cTypedef ++ " *"
  { leanType := leanName, shimParam := "b_lean_obj_arg",
    shimReturn := "lean_obj_res", cLocalType := cPtr,
    isLeanObj := true,
    paramMarshal  := .externalData cPtr,
    returnMarshal := .externalAlloc (externalClassGetter leanName) }

/-- Built-in primitive C → Lean mappings, applied before consulting
the user's `Bindings`. -/
private def primitiveMap (ty : CType) : Option ResolvedType :=
  match ty with
  | .void                       => some (scalarRT "Unit" "void" 0 "")
  | .scalar .bool _             => some (scalarRT "Bool" "uint8_t" 1 "uint8")
  | .scalar .float _            => some (scalarRT "Float32" "float" 4 "float32")
  | .scalar .double _           => some (scalarRT "Float" "double" 8 "float")
  | .scalar .char _             => some (scalarRT "UInt8" "uint8_t" 1 "uint8")
  | .scalar .short .signed      => some (scalarRT "Int16" "int16_t" 2 "uint16")
  | .scalar .short _            => some (scalarRT "UInt16" "uint16_t" 2 "uint16")
  | .scalar .int .signed        => some (scalarRT "Int32" "int32_t" 4 "uint32")
  | .scalar .int _              => some (scalarRT "UInt32" "uint32_t" 4 "uint32")
  | .scalar .long .signed       => some (scalarRT "Int64" "int64_t" 8 "uint64")
  | .scalar .long _             => some (scalarRT "UInt64" "uint64_t" 8 "uint64")
  | .scalar .longLong .signed   => some (scalarRT "Int64" "int64_t" 8 "uint64")
  | .scalar .longLong _         => some (scalarRT "UInt64" "uint64_t" 8 "uint64")
  | _ => none

/-- The fixed-width stdint typedef → Lean scalar map. Only consulted
for typedef references that aren't resolved by the user's `Bindings`. -/
private def stdintMap : Std.HashMap String ResolvedType :=
  ({} : Std.HashMap String ResolvedType)
    |>.insert "int8_t"   (scalarRT "Int8"   "int8_t"   1 "uint8")
    |>.insert "uint8_t"  (scalarRT "UInt8"  "uint8_t"  1 "uint8")
    |>.insert "int16_t"  (scalarRT "Int16"  "int16_t"  2 "uint16")
    |>.insert "uint16_t" (scalarRT "UInt16" "uint16_t" 2 "uint16")
    |>.insert "int32_t"  (scalarRT "Int32"  "int32_t"  4 "uint32")
    |>.insert "uint32_t" (scalarRT "UInt32" "uint32_t" 4 "uint32")
    |>.insert "int64_t"  (scalarRT "Int64"  "int64_t"  8 "uint64")
    |>.insert "uint64_t" (scalarRT "UInt64" "uint64_t" 8 "uint64")
    |>.insert "size_t"   (scalarRT "USize"  "size_t"   8 "usize")
    |>.insert "ssize_t"  (scalarRT "ISize"  "ssize_t"  8 "usize")
    |>.insert "ptrdiff_t" (scalarRT "ISize" "ptrdiff_t" 8 "usize")
    |>.insert "bool"     (scalarRT "Bool"   "uint8_t"  1 "uint8")

/-- Resolve a C type against the user's bindings. Returns an error
message if no mapping exists. -/
partial def resolveType
    (anno : Std.HashMap String TypeAnno) (ty : CType)
    : Except String ResolvedType := do
  match ty with
  -- Strip qualifiers — they don't affect the Lean type.
  | .const t | .volatile t => resolveType anno t
  -- Pointer to void → Lean USize (untyped pointer, passthrough).
  | .pointer inner =>
    let inner' := stripQuals inner
    match inner' with
    | .void => .ok (scalarRT "USize" "void *" 8 "usize")
    -- Pointer to char → Lean String.
    | .scalar .char _ => .ok stringRT
    -- Pointer to pointer-to-char → out-param for string result.
    | .pointer inner2 =>
      let inner2' := stripQuals inner2
      match inner2' with
      | .scalar .char _ =>
        .ok { stringRT with shimParam := "char const **",
                            shimReturn := "char const **" }
      | _ => resolvePointerInner anno inner'
    -- Pointer to typedef: special handling for annotated types.
    | .typedef name => resolvePointerToTypedef anno name
    -- Pointer to anything else: resolve the pointee and add `*`.
    | _ => resolvePointerInner anno inner'
  -- Typedef name: look up in user bindings, then in stdint table.
  | .typedef name =>
    match anno[name]? with
    | some a =>
      match a.mapping with
      | .scalarNewtype k => .ok (scalarRT a.lean k.toC k.byteSize k.ctorScalar)
      | .opaquePointer _ => .ok (opaqueRT a.lean name)
      | .inductiveEnum _ => .ok (enumRT a.lean name)
      | .structRecord _  => .ok (structByValRT a.lean name)
      | .callback        => .ok (callbackRT a.lean name)
      | .taggedUnion _   => .ok (structByValRT a.lean name)
      | .bitfieldStruct _ => .ok (bitfieldRT a.lean name)
      | .eventCallback _ => .ok (callbackRT a.lean name)
    | none =>
      match stdintMap[name]? with
      | some r => .ok r
      | none   => .error s!"unmapped typedef `{name}`"
  | _ =>
    match primitiveMap ty with
    | some r => .ok r
    | none   => .error s!"unmapped C type: {ty.spec}"
where
  /-- Strip const/volatile qualifiers. -/
  stripQuals : CType → CType
    | .const t    => stripQuals t
    | .volatile t => stripQuals t
    | t           => t
  /-- Resolve `T *` for a user-annotated typedef `T`. -/
  resolvePointerToTypedef (anno : Std.HashMap String TypeAnno) (name : String)
      : Except String ResolvedType :=
    match anno[name]? with
    | some a =>
      match a.mapping with
      | .scalarNewtype k =>
        .ok { leanType := a.lean, shimParam := k.toC ++ " *",
              shimReturn := k.toC ++ " *", cLocalType := k.toC }
      | .opaquePointer _ => .ok (opaqueRT a.lean name)
      | .inductiveEnum _ =>
        .ok { leanType := a.lean, shimParam := name ++ " *",
              shimReturn := name ++ " *", cLocalType := name }
      | .structRecord _ => .ok (structByPtrRT a.lean name)
      | .taggedUnion _  => .ok (structByPtrRT a.lean name)
      | .bitfieldStruct _ => .ok (bitfieldRT a.lean name)
      | .callback => .error s!"`{name}` is a callback typedef; pointers to callbacks not supported"
      | .eventCallback _ => .error s!"`{name}` is an event callback typedef; pointers not supported"
    | none =>
      match stdintMap[name]? with
      | some r => .ok { r with shimParam := r.shimParam ++ " *",
                                shimReturn := r.shimReturn ++ " *" }
      | none   => .error s!"unmapped pointer-to-typedef `{name}`"
  /-- Resolve any other `T *` by resolving `T` and appending `*`. -/
  resolvePointerInner (anno : Std.HashMap String TypeAnno) (inner : CType)
      : Except String ResolvedType := do
    -- Try as a primitive.
    match primitiveMap inner with
    | some r => .ok { r with shimParam := r.shimParam ++ " *",
                             shimReturn := r.shimReturn ++ " *" }
    | none   =>
      -- Try resolving the inner type fully.
      match resolveType anno inner with
      | .ok r  => .ok { r with shimParam := r.shimParam ++ " *",
                                shimReturn := r.shimReturn ++ " *" }
      | .error e => .error s!"in pointer type: {e}"

/-! ## Header lookups + per-mapping resolution

These live above the Lean module emitter because the type emitter for
struct/callback mappings needs to resolve types from the parsed
header. -/

/-- Pre-computed indices for O(1) header lookups. -/
structure HeaderIndex where
  structTagMap : Std.HashMap String (Array CField)
  enumTagMap : Std.HashMap String (Array (String × Option Int))
  typedefMap : Std.HashMap String CType
  /-- Anonymous union fields (from `unionDef none`), flattened. -/
  anonUnionFields : Array CField

private def buildHeaderIndex (h : CHeader) : HeaderIndex := Id.run do
  let mut stm : Std.HashMap String (Array CField) := {}
  let mut etm : Std.HashMap String (Array (String × Option Int)) := {}
  let mut tdm : Std.HashMap String CType := {}
  let mut auf : Array CField := #[]
  for d in h.decls do
    match d with
    | .structDef (some t) fields => stm := stm.insert t fields
    | .enumDef (some t) variants => etm := etm.insert t variants
    | .typedef n ty => tdm := tdm.insert n ty
    | .unionDef none uFields => auf := auf ++ uFields
    | _ => pure ()
  return { structTagMap := stm, enumTagMap := etm, typedefMap := tdm, anonUnionFields := auf }

private def lookupEnumTag (h : HeaderIndex) (tag : String) : Option (Array (String × Option Int)) :=
  h.enumTagMap[tag]?

/-- Look up a C enum constant by name across all enum definitions.
Returns the numeric value if found. -/
private def lookupEnumConstant (h : HeaderIndex) (constName : String) : Option Int :=
  h.enumTagMap.toList.findSome? fun (_, variants) =>
    variants.findSome? fun (name, val?) =>
      if name == constName then val? else none

private def lookupStructTag (h : HeaderIndex) (tag : String) : Option (Array CField) :=
  h.structTagMap[tag]?

/-- Look up anonymous union members. When a struct has an anonymous union
field (name="", type=unionRef ""), its members are accessible by name
in C. This helper searches both direct struct fields and anonymous
union members. -/
private def lookupFieldInStruct (h : HeaderIndex) (fields : Array CField) (name : String)
    : Option CField :=
  -- First try direct lookup.
  fields.find? (·.name = name) |>.orElse fun _ =>
    -- Search anonymous unions: fields with name="" and type unionRef "".
    fields.findSome? fun f =>
      if f.name = "" then
        match f.type with
        | .unionRef "" => h.anonUnionFields.find? (·.name = name)
        | _ => none
      else none

/-- Look up the body type of a typedef. -/
private def lookupTypedefBody (h : HeaderIndex) (name : String) : Option CType :=
  h.typedefMap[name]?

private def resolveStructFields
    (h : HeaderIndex) (anno : Std.HashMap String TypeAnno) (sm : StructMapping)
    : Except String (Array (String × String × ResolvedType)) := do
  let some headerFields := lookupStructTag h sm.cStructTag
    | .error s!"struct tag `{sm.cStructTag}` not found in header"
  let arrayDataFields := sm.arrayFields.map Prod.fst
  let mut out : Array (String × String × ResolvedType) := #[]
  for (cField, leanField) in sm.fields do
    let some hf := headerFields.find? (·.name = cField)
      | .error s!"field `{cField}` not found in struct `{sm.cStructTag}`"
    if arrayDataFields.contains cField then
      -- Array data field: strip pointer to get element type, resolve
      -- as Array T.
      let elemCTy := match hf.type with
        | .pointer (.const t) | .pointer t => t
        | t => t
      let elemRes ← resolveType anno elemCTy
      let elemKind := arrayElemKindOf elemRes
      let r := arrayRT elemRes.leanType elemRes.cLocalType elemKind
      out := out.push (cField, leanField, r)
    else
      let r ← resolveType anno hf.type
      out := out.push (cField, leanField, r)
  return out

/-- Decide which (if any) of the function-pointer's params is the
"user-data" slot. By convention this is the *last* parameter when it
is a `void *`. -/
private def callbackUserDataIndex (params : Array CParam) : Option Nat :=
  if h : params.size > 0 then
    match params[params.size - 1].type with
    | .pointer .void => some (params.size - 1)
    | _              => none
  else none

/-- Extract `(retType, params, userDataIdx?)` from a callback typedef. -/
private def callbackSignature (h : HeaderIndex) (typedefName : String)
    : Except String (CType × Array CParam × Option Nat) := do
  let some body := lookupTypedefBody h typedefName
    | .error s!"callback typedef `{typedefName}` not found in header"
  let .pointer (.function ret params _variadic) := body
    | .error s!"callback typedef `{typedefName}` is not a function-pointer"
  return (ret, params, callbackUserDataIndex params)

/-- Check if a CType is a callback-mapped typedef. -/
private def isCallbackTypedef (anno : Std.HashMap String TypeAnno) (ty : CType) : Bool :=
  match ty with
  | .typedef name => match anno[name]? with
    | some a => match a.mapping with
      | .callback => true
      | _ => false
    | _ => false
  | _ => false

/-- Callback parameter layout: identifies the outer user-data slot,
nested callback + user-data pairs, and array (pointer + size_t) pairs.
All indices are 0-based into the original param array. -/
structure CallbackLayout where
  outerUserDataIdx : Nat
  /-- `(callbackIdx, userDataIdx)` for each nested callback param. -/
  nestedCallbacks : Array (Nat × Nat)
  /-- `(dataIdx, sizeIdx)` for each array pair. -/
  arrayPairs : Array (Nat × Nat)
  /-- All hidden indices: outer user-data, nested CB user-data, array sizes. -/
  hiddenIndices : List Nat
  deriving Inhabited

/-- Analyze a callback's params to identify nested callbacks, array pairs,
and the outer user-data slot. -/
private def analyzeCallbackLayout
    (params : Array CParam) (anno : Std.HashMap String TypeAnno)
    : Except String CallbackLayout := do
  let sz := params.size
  -- Step 1: Find nested callback + void* pairs.
  let mut nestedCBs : Array (Nat × Nat) := #[]
  let mut claimed : List Nat := []
  let mut skip := false
  for h_i : i in [:sz] do
    if skip then
      skip := false
      continue
    if isCallbackTypedef anno params[i].type then
      if h2 : i + 1 < sz then
        match params[i + 1].type with
        | .pointer .void =>
          nestedCBs := nestedCBs.push (i, i + 1)
          claimed := claimed ++ [i, i + 1]
          skip := true
          continue
        | _ => pure ()
  -- Step 2: Find the outer user-data (a void* not part of a nested pair).
  let mut outerUD : Option Nat := none
  for h : j in [:sz] do
    if claimed.contains j then continue
    match params[j].type with
    | .pointer .void => outerUD := some j
    | _ => pure ()
  let some outerUDIdx := outerUD
    | .error "callback has no outer user-data void* parameter"
  claimed := claimed ++ [outerUDIdx]
  -- Step 3: Find array pairs (pointer + size_t) among unclaimed params.
  let mut arrPairs : Array (Nat × Nat) := #[]
  let mut skip2 := false
  for h_k : k in [:sz] do
    if skip2 then
      skip2 := false
      continue
    if claimed.contains k then continue
    if h2 : k + 1 < sz then
      if claimed.contains (k + 1) then continue
      let isPtr := match params[k].type with
        | .pointer _ => true
        | _ => false
      let isSizeT := match params[k + 1].type with
        | .typedef "size_t" => true
        | _ => false
      if isPtr && isSizeT then
        arrPairs := arrPairs.push (k, k + 1)
        claimed := claimed ++ [k + 1]
        skip2 := true
  let hiddenIdxs := [outerUDIdx] ++
    nestedCBs.toList.map Prod.snd ++
    arrPairs.toList.map Prod.snd
  return {
    outerUserDataIdx := outerUDIdx
    nestedCallbacks := nestedCBs
    arrayPairs := arrPairs
    hiddenIndices := hiddenIdxs
  }

/-- Derive the Lean arrow type for a callback typedef. Handles nested
callbacks (inner callback + void* → inner Lean callback type), array
pairs (pointer + size_t → Array T), and non-void returns. -/
private partial def deriveCallbackLeanType
    (h : HeaderIndex) (anno : Std.HashMap String TypeAnno) (typedefName : String)
    : Except String String := do
  let (ret, params, _) ← callbackSignature h typedefName
  let layout ← analyzeCallbackLayout params anno
  let nestedCBIdxs := layout.nestedCallbacks.toList.map Prod.fst
  let arrayDataIdxs := layout.arrayPairs.toList.map Prod.fst
  let mut leanParts : Array String := #[]
  for h_i : i in [:params.size] do
    if layout.hiddenIndices.contains i then continue
    let p := params[i]
    -- Nested callback: use the callback's Lean type alias directly.
    if nestedCBIdxs.contains i then
      let r ← resolveType anno p.type
      leanParts := leanParts.push r.leanType
    -- Array data: resolve element type, produce `Array T`.
    else if arrayDataIdxs.contains i then
      let elemCTy := match p.type with
        | .pointer (.const t) | .pointer t => t
        | t => t
      let elemRes ← resolveType anno elemCTy
      leanParts := leanParts.push s!"Array {elemRes.leanType}"
    else
      let r ← resolveType anno p.type
      leanParts := leanParts.push r.leanType
  let retRes ← resolveType anno ret
  let arrow := " → ".intercalate
    (leanParts.toList ++ [s!"IO {retRes.leanType}"])
  return arrow

/-! ## Lean module emitter -/

/-- Render the Lean type expression for a parameter, applying borrowing
where appropriate. `String` and opaque-pointer types are borrowed by
default; scalar-like values are passed by value. -/
private def renderParamType (r : ResolvedType) (forceBorrowed : Bool) : String :=
  let pfx := if forceBorrowed || r.isLeanObj then "@& " else ""
  pfx ++ r.leanType

/-- Build the Lean type signature for one function annotation. Returns
`(typeText, externCSig)` where `typeText` is the Lean source for the
arrow type and `externCSig` is the parameter-and-return shape needed to
emit the matching shim. -/
private def buildFunctionSignature
    (anno : Std.HashMap String TypeAnno)
    (decl : CDecl) (fa : FunctionAnno)
    : Except String (String × Array ResolvedType × ResolvedType × Bool) := do
  let .function _name retCty params _variadic := decl
    | .error s!"function annotation `{fa.cName}` does not match a function decl"
  -- Collect hidden-param indices: callback user-data slots + array
  -- size slots + byteArray size slots. These are invisible in the Lean signature.
  let arraySizeIndices := fa.arrayPairs.map Prod.snd
  let byteArraySizeIndices := fa.byteArrayPairs.map Prod.snd
  let hiddenIndices := fa.callbackUserDataParams ++ arraySizeIndices ++ byteArraySizeIndices
  -- Resolve parameter types. Hidden params get placeholders. Array
  -- data-params get resolved as `Array T`. ByteArray data-params
  -- get resolved as `ByteArray`.
  let arrayDataIndices := fa.arrayPairs.map Prod.fst
  let byteArrayDataIndices := fa.byteArrayPairs.map Prod.fst
  let mut paramRes : Array (Bool × ResolvedType) := #[]
  for h : i in [:params.size] do
    let p := params[i]
    let r ←
      if hiddenIndices.contains i then
        pure (default : ResolvedType)
      else if arrayDataIndices.contains i then do
        -- The data param is a pointer; resolve the pointee as the
        -- element type, then wrap in an array resolved type.
        let .pointer elemTy := p.type
          | .error s!"array data param {i} of `{fa.cName}` is not a pointer"
        let elemTy := match elemTy with | .const t => t | t => t
        let elemRes ← resolveType anno elemTy
        let elemKind := arrayElemKindOf elemRes
        pure (arrayRT elemRes.leanType elemRes.cLocalType elemKind)
      else if byteArrayDataIndices.contains i then
        pure byteArrayRT
      else
        resolveType anno p.type
    let borrow := fa.borrowedParams.contains i
    paramRes := paramRes.push (borrow, r)
  let retRes ← resolveType anno retCty
  -- Apply the function-style transformation.
  -- Filter out hidden params from the Lean signature; they're
  -- entirely synthesised by the codegen.
  let visibleByIdx := paramRes.toList.zipIdx.filter
    (fun (_, i) => !hiddenIndices.contains i)
  match fa.style with
  | .direct =>
    let leanType :=
      let parts := visibleByIdx.map (fun ((b, r), _) => renderParamType r b)
      let retTy := if fa.nullableReturn then s!"Option {retRes.leanType}" else retRes.leanType
      let parts := parts ++ [if fa.inIO then s!"IO ({retTy})" else retTy]
      " → ".intercalate parts
    let allRes := paramRes.map Prod.snd
    return (leanType, allRes, retRes, fa.inIO)
  | .outParamBoolStatus outIdx error =>
    -- Drop the out-parameter (and any callback user-data slots) from
    -- the Lean signature. The pointer target type becomes the success
    -- value of an Except. The bool return is dropped (only used at
    -- the C level for status).
    if h : outIdx < params.size then
      let outParam := params[outIdx]
      let .pointer pointee := outParam.type
        | .error s!"out-param `{outParam.name.getD "?"}` of `{fa.cName}` is not a pointer"
      let pointeeRes ← resolveType anno pointee
      let visibleParams := visibleByIdx.filter (fun (_, i) => i ≠ outIdx)
                                       |>.map (fun (br, _) => br)
      let parts := visibleParams.map (fun (b, r) => renderParamType r b)
      let errorTy := match error with
        | .string _        => "String"
        | .enum _ eLean    => eLean
        | .tuple _ eLean _ => s!"({eLean} × String)"
      let innerTy := if fa.nullableOutParam then s!"(Option {pointeeRes.leanType})"
                     else pointeeRes.leanType
      let leanRet := s!"IO (Except {errorTy} {innerTy})"
      let leanType := " → ".intercalate (parts ++ [leanRet])
      let allRes := paramRes.map Prod.snd
      return (leanType, allRes, pointeeRes, true)
    else
      .error s!"`{fa.cName}` has only {params.size} params but outParamIdx is {outIdx}"
  | .boolStatus error =>
    -- All params are visible; bool return is consumed internally.
    -- Lean return type is IO (Except <ErrorTy> Unit).
    let parts := visibleByIdx.map (fun ((b, r), _) => renderParamType r b)
    let errorTy := match error with
      | .string _        => "String"
      | .enum _ eLean    => eLean
      | .tuple _ eLean _ => s!"({eLean} × String)"
    let leanRet := s!"IO (Except {errorTy} Unit)"
    let leanType := " → ".intercalate (parts ++ [leanRet])
    let allRes := paramRes.map Prod.snd
    let unitRes := scalarRT "Unit" "void" 0 ""
    return (leanType, allRes, unitRes, true)
  | .optionOutParam outIdx =>
    if h : outIdx < params.size then
      let outParam := params[outIdx]
      let .pointer pointee := outParam.type
        | .error s!"out-param `{outParam.name.getD "?"}` of `{fa.cName}` is not a pointer"
      let pointeeRes ← resolveType anno pointee
      let visibleParams := visibleByIdx.filter (fun (_, i) => i ≠ outIdx)
                                       |>.map (fun (br, _) => br)
      let parts := visibleParams.map (fun (b, r) => renderParamType r b)
      let retWrapper := if fa.inIO then s!"IO (Option {pointeeRes.leanType})"
                        else s!"Option {pointeeRes.leanType}"
      let leanType := " → ".intercalate (parts ++ [retWrapper])
      let allRes := paramRes.map Prod.snd
      return (leanType, allRes, pointeeRes, fa.inIO)
    else
      .error s!"`{fa.cName}` has only {params.size} params but outParamIdx is {outIdx}"
  | .voidOutParam outIdx =>
    -- Void-return function with an out-param. Drop the out-param from
    -- the Lean signature; the pointee type becomes the return value.
    if h : outIdx < params.size then
      let outParam := params[outIdx]
      let .pointer pointee := outParam.type
        | .error s!"out-param `{outParam.name.getD "?"}` of `{fa.cName}` is not a pointer"
      let pointeeRes ← resolveType anno pointee
      let visibleParams := visibleByIdx.filter (fun (_, i) => i ≠ outIdx)
                                       |>.map (fun (br, _) => br)
      let parts := visibleParams.map (fun (b, r) => renderParamType r b)
      -- If no visible params, add Unit to avoid generating a bare constant.
      let parts := if parts.isEmpty then ["Unit"] else parts
      let retTy := if fa.inIO then s!"IO {pointeeRes.leanType}" else pointeeRes.leanType
      let leanType := " → ".intercalate (parts ++ [retTy])
      let allRes := paramRes.map Prod.snd
      return (leanType, allRes, pointeeRes, fa.inIO)
    else
      .error s!"`{fa.cName}` has only {params.size} params but outParamIdx is {outIdx}"
  | .optionOutArray ptrIdx sizeIdx =>
    -- Two out-params (data pointer + size) forming an array result.
    if h₁ : ptrIdx < params.size then
      if h₂ : sizeIdx < params.size then
        let ptrParam := params[ptrIdx]
        let .pointer innerPtr := ptrParam.type
          | .error s!"out-array ptr param of `{fa.cName}` is not a pointer"
        let .pointer elemTy := innerPtr
          | .error s!"out-array ptr param of `{fa.cName}` is not a pointer-to-pointer"
        let elemRes ← resolveType anno elemTy
        let visibleParams := visibleByIdx.filter (fun (_, i) => i ≠ ptrIdx && i ≠ sizeIdx)
                                         |>.map (fun (br, _) => br)
        let parts := visibleParams.map (fun (b, r) => renderParamType r b)
        let arrTy := s!"(Array {elemRes.leanType})"
        let retWrapper := if fa.inIO then s!"IO (Option {arrTy})"
                          else s!"Option {arrTy}"
        let leanType := " → ".intercalate (parts ++ [retWrapper])
        let allRes := paramRes.map Prod.snd
        return (leanType, allRes, elemRes, fa.inIO)
      else .error s!"`{fa.cName}` sizeIdx {sizeIdx} out of range"
    else .error s!"`{fa.cName}` ptrIdx {ptrIdx} out of range"
  | .callerAllocates _sizeFn bufIdx sizeIdx error _nullTerm resultKind =>
    -- Two-step caller-allocates: bufIdx and sizeIdx are dropped from
    -- the Lean signature. The result is either String (for char buffers)
    -- or Array T (for typed buffers).
    if h₁ : bufIdx < params.size then
      if h₂ : sizeIdx < params.size then
        let bufParam := params[bufIdx]
        let .pointer elemTy := bufParam.type
          | .error s!"buffer param of `{fa.cName}` is not a pointer"
        let elemRes ← resolveType anno elemTy
        let visibleParams := visibleByIdx.filter (fun (_, i) => i ≠ bufIdx && i ≠ sizeIdx)
                                         |>.map (fun (br, _) => br)
        let parts := visibleParams.map (fun (b, r) => renderParamType r b)
        let resultTy := match resultKind with
          | some rk => rk
          | none    =>
            if elemRes.leanType = "UInt8" then "String"  -- char buffer → String
            else s!"(Array {elemRes.leanType})"
        let errorTy := match error with
          | .string _        => "String"
          | .enum _ eLean    => eLean
          | .tuple _ eLean _ => s!"({eLean} × String)"
        let leanRet := s!"IO (Except {errorTy} {resultTy})"
        let leanType := " → ".intercalate (parts ++ [leanRet])
        let allRes := paramRes.map Prod.snd
        return (leanType, allRes, elemRes, true)
      else .error s!"`{fa.cName}` sizeIdx {sizeIdx} out of range"
    else .error s!"`{fa.cName}` bufIdx {bufIdx} out of range"
  | .multiOutParam outParamIndices =>
    -- Void-return function with multiple out-params. Build a nested
    -- Prod return type: (T₁ × T₂ × ... × Tₙ).
    let mut outRes : Array ResolvedType := #[]
    for idx in outParamIndices do
      if h : idx < params.size then
        let outParam := params[idx]
        let .pointer pointee := outParam.type
          | .error s!"multi-out-param `{outParam.name.getD "?"}` of `{fa.cName}` at index {idx} is not a pointer"
        let pointeeRes ← resolveType anno pointee
        outRes := outRes.push pointeeRes
      else
        .error s!"`{fa.cName}` has only {params.size} params but multiOutParam index {idx} is out of range"
    -- Build the right-nested Prod type: (T₁ × T₂ × T₃) for 3 params
    let prodTy := match outRes.size with
      | 0 => "Unit"
      | 1 => outRes[0]!.leanType
      | _ =>
        let inner := outRes.toList.map (·.leanType)
        -- Right-associate: (A × B × C) = (A × (B × C))
        inner.foldr (init := "") fun ty acc =>
          if acc.isEmpty then ty else s!"({ty} × {acc})"
    -- Drop all out-param indices from visible params.
    let outIdxSet := outParamIndices.toList
    let visibleParams := visibleByIdx.filter (fun (_, i) => !outIdxSet.contains i)
                                     |>.map (fun (br, _) => br)
    let parts := visibleParams.map (fun (b, r) => renderParamType r b)
    -- If no visible params, add Unit to avoid generating a bare constant.
    let parts := if parts.isEmpty then ["Unit"] else parts
    let retTy := if fa.inIO then s!"IO {prodTy}" else prodTy
    let leanType := " → ".intercalate (parts ++ [retTy])
    let allRes := paramRes.map Prod.snd
    -- Return the first out-param's resolved type as retRes (used
    -- minimally by callers; the real return is the Prod).
    let retRes' := if outRes.size > 0 then outRes[0]! else scalarRT "Unit" "void" 0 ""
    return (leanType, allRes, retRes', fa.inIO)
  | .byteArrayOutBoolStatus ptrIdx sizeIdx error =>
    -- Bool return + (uint8_t **, size_t *) out-params → ByteArray.
    -- Both ptrIdx and sizeIdx are dropped from Lean signature.
    if h₁ : ptrIdx < params.size then
      if h₂ : sizeIdx < params.size then
        let visibleParams := visibleByIdx.filter (fun (_, i) => i ≠ ptrIdx && i ≠ sizeIdx)
                                         |>.map (fun (br, _) => br)
        let parts := visibleParams.map (fun (b, r) => renderParamType r b)
        let errorTy := match error with
          | .string _        => "String"
          | .enum _ eLean    => eLean
          | .tuple _ eLean _ => s!"({eLean} × String)"
        let leanRet := s!"IO (Except {errorTy} ByteArray)"
        let leanType := " → ".intercalate (parts ++ [leanRet])
        let allRes := paramRes.map Prod.snd
        let baRes := byteArrayRT
        return (leanType, allRes, baRes, true)
      else .error s!"`{fa.cName}` sizeIdx {sizeIdx} out of range"
    else .error s!"`{fa.cName}` ptrIdx {ptrIdx} out of range"

/-- Resolve the C-side extern symbol for a function annotation.
Default is `lean_<cName>`; the user can override with `externSymbol`. -/
private def externSymbolOf (fa : FunctionAnno) : String :=
  match fa.externSymbol with
  | some s => s
  | none   => s!"lean_{fa.cName}"

/-- Emit a single `@[extern]` opaque declaration. -/
private def emitFunctionDecl
    (b : Bindings)
    (anno : Std.HashMap String TypeAnno)
    (declMap : Std.HashMap String CDecl)
    (fa : FunctionAnno)
    : Except String Gen.LeanDecl := do
  let some decl := declMap[fa.cName]?
    | .error s!"function `{fa.cName}` not found in header"
  let (sig, _, _, _) ← buildFunctionSignature anno decl fa
  let externSym := externSymbolOf fa
  let leanShortName := fa.lean.splitOn "." |>.getLast!
  return .externOpaque externSym leanShortName sig

/-- Emit a single type declaration. The `fieldTypes` map provides the
already-resolved Lean type expression for each (struct's) Lean field
name; only used by the `.structRecord` arm. -/
private def emitTypeDecl
    (ta : TypeAnno) (fieldTypes : Std.HashMap String String := {}) : Array Gen.LeanDecl :=
  match ta.mapping with
  | .scalarNewtype k =>
    #[.defAlias ta.lean k.toLean #["Repr", "Inhabited"]]
  | .opaquePointer _ =>
    #[.opaque_ ta.lean "Type"]
  | .inductiveEnum em =>
    let ctors := em.variants.map fun (_, leanV) => (leanV, none)
    #[.inductive_ ta.lean ctors #["Repr", "Inhabited", "BEq"]]
  | .structRecord sm =>
    let fields := sm.fields.map fun (_, leanF) =>
      (leanF, fieldTypes[leanF]?.getD "Unit")
    #[.structure_ ta.lean fields #["Repr", "Inhabited"]]
  | .callback =>
    let arrow := fieldTypes["__callback__"]?.getD "Unit"
    #[.defAlias ta.lean arrow]
  | .taggedUnion tu =>
    let dataName := if tu.sharedFields.isEmpty then ta.lean else s!"{ta.lean}.Data"
    let ctors := tu.variants.map fun v =>
      let payloadArgs := match fieldTypes[s!"__variant__{v.leanCtor}"]? with
        | some args => if args.isEmpty then none else some args
        | none      => none
      (v.leanCtor, payloadArgs)
    let inductiveDecl : Gen.LeanDecl :=
      .inductive_ dataName ctors #["Repr", "Inhabited"]
    if tu.sharedFields.isEmpty then
      #[inductiveDecl]
    else
      let fields := tu.sharedFields.map fun (_, leanF) =>
        (leanF, fieldTypes[leanF]?.getD "Unit")
      let fields := fields.push ("data", dataName)
      let structDecl : Gen.LeanDecl :=
        .structure_ ta.lean fields #["Repr", "Inhabited"]
      #[inductiveDecl, structDecl]
  | .bitfieldStruct bm =>
    let fields := bm.fields.map fun (_, leanF) => (leanF, "Bool")
    #[.structure_ ta.lean fields #["Repr", "Inhabited"]]
  | .eventCallback ec =>
    -- Emit two declarations:
    -- 1. An inductive for the event type with per-variant ctors
    -- 2. A def alias for the callback type = EventType → IO ReturnType
    let ctors := ec.variants.map fun v =>
      let payloadStr := match v.interpretation with
        | .opaquePtr leanTy true =>
          some s!"(val : Option {leanTy})"
        | .opaquePtr leanTy false =>
          some s!"(val : {leanTy})"
        | .derefMapped leanTy =>
          some s!"(val : {leanTy})"
        | .ptrArray leanTy count =>
          if v.fieldNames.size >= count then
            let fields := (List.range count).map fun i =>
              let nm := v.fieldNames[i]?.getD s!"field{i}"
              s!"({nm} : {leanTy})"
            some (" ".intercalate fields)
          else
            -- Default field names
            let fields := (List.range count).map fun i =>
              s!"(field{i} : {leanTy})"
            some (" ".intercalate fields)
      (v.leanCtor, payloadStr)
    -- Event callback variants may reference opaque types (e.g. Model,
    -- Statistics) that lack Repr, so only derive Inhabited.
    let inductiveDecl : Gen.LeanDecl :=
      .inductive_ ec.eventTypeName ctors #["Inhabited"]
    let callbackDef : Gen.LeanDecl :=
      .defAlias ta.lean s!"{ec.eventTypeName} → IO {ec.leanReturnType}"
    #[inductiveDecl, callbackDef]

/-- Compute which Lean type names a type declaration's body refers to.
Used for building the type-dependency graph for mutual-recursion
detection. -/
private def typeDependencies
    (h : HeaderIndex) (annoMap : Std.HashMap String TypeAnno)
    (ta : TypeAnno) : List String :=
  -- Collect the Lean names of all TypeAnnos referenced from ta's body.
  let cNames : List String := match ta.mapping with
    | .structRecord sm =>
      -- Each struct field type might be a typedef that maps to another
      -- annotated type.
      let headerFields := lookupStructTag h sm.cStructTag
      match headerFields with
      | none => []
      | some fields =>
        sm.fields.toList.filterMap fun (cField, _) =>
          match fields.find? (·.name = cField) with
          | none => none
          | some hf => extractTypedefRef hf.type
    | .callback =>
      -- Callback parameters reference types.
      match lookupTypedefBody h ta.cName with
      | none => []
      | some (.pointer (.function ret params _)) =>
        let paramRefs := params.toList.filterMap (fun p => extractTypedefRef p.type)
        paramRefs ++ (extractTypedefRef ret).toList
      | _ => []
    | .taggedUnion tu =>
      -- Dependencies from shared fields + variant payload (union) fields.
      let headerFields := lookupStructTag h tu.cStructTag
      match headerFields with
      | none => []
      | some fields =>
        let sharedRefs := tu.sharedFields.toList.filterMap fun (cField, _) =>
          match fields.find? (·.name = cField) with
          | none => none
          | some hf => extractTypedefRef hf.type
        let variantRefs := tu.variants.toList.filterMap fun v =>
          match fields.find? (·.name = v.unionField) with
          | none => none
          | some hf => extractTypedefRef hf.type
        sharedRefs ++ variantRefs
    | .eventCallback ec =>
      -- Collect all Lean type names referenced by the variants and
      -- look them up via the annotation map (reverse: Lean name → C name).
      let leanToC : Std.HashMap String String :=
        annoMap.fold (init := ({} : Std.HashMap _ _)) fun m cN ta => m.insert ta.lean cN
      ec.variants.toList.filterMap fun v =>
        let leanTy := match v.interpretation with
          | .opaquePtr ty _ => ty
          | .derefMapped ty => ty
          | .ptrArray ty _  => ty
        leanToC[leanTy]?
    | _ => []
  -- Map C typedef names back to Lean names via the annotation map.
  cNames.filterMap fun cn => (annoMap[cn]?).map (·.lean)
where
  /-- Extract the typedef name (if any) from a CType, stripping
  pointers/const. -/
  extractTypedefRef : CType → Option String
    | .typedef n          => some n
    | .pointer t          => extractTypedefRef t
    | .const t            => extractTypedefRef t
    | .volatile t         => extractTypedefRef t
    | _                   => none

/-- Kosaraju's SCC algorithm on a directed graph given as adjacency
lists. Returns SCCs in reverse topological order (dependencies before
dependents). -/
private def computeSCCs (nodes : Array String)
    (adj : Std.HashMap String (List String))
    : Array (Array String) := Id.run do
  -- First pass: DFS on the forward graph to get finish order.
  let mut visited : Std.HashMap String Bool := {}
  let mut finish  : Array String := #[]
  for n in nodes do
    if visited[n]?.getD false then continue
    -- Iterative DFS to avoid stack overflow.
    let mut stack : List (String × Bool) := [(n, false)]
    while !stack.isEmpty do
      match stack with
      | [] => break
      | (v, processed) :: rest =>
        stack := rest
        if processed then
          finish := finish.push v
          continue
        if visited[v]?.getD false then continue
        visited := visited.insert v true
        stack := (v, true) :: stack
        for w in (adj[v]?.getD []) do
          if !(visited[w]?.getD false) then
            stack := (w, false) :: stack
  -- Build reverse graph.
  let mut radj : Std.HashMap String (List String) := {}
  for n in nodes do
    for w in (adj[n]?.getD []) do
      radj := radj.insert w ((radj[w]?.getD []) ++ [n])
  -- Second pass: DFS on reverse graph in reverse finish order.
  let mut visited2 : Std.HashMap String Bool := {}
  let mut sccs : Array (Array String) := #[]
  for n in finish.reverse do
    if visited2[n]?.getD false then continue
    let mut component : Array String := #[]
    let mut stack : List String := [n]
    while !stack.isEmpty do
      match stack with
      | [] => break
      | v :: rest =>
        stack := rest
        if visited2[v]?.getD false then continue
        visited2 := visited2.insert v true
        component := component.push v
        for w in (radj[v]?.getD []) do
          if !(visited2[w]?.getD false) then
            stack := w :: stack
    sccs := sccs.push component
  return sccs

/-- Generate the entire Lean module text. -/
def emitLeanModule (b : Bindings) (hdr : CHeader) : Except String String := do
  let h := buildHeaderIndex hdr
  -- Build lookup maps.
  let typeAnnoMap : Std.HashMap String TypeAnno :=
    b.types.foldl (fun m a => m.insert a.cName a) ({} : Std.HashMap _ _)
  let declMap : Std.HashMap String CDecl :=
    hdr.decls.foldl (fun m d =>
      match d with
      | .function name .. => m.insert name d
      | .typedef name _   => m.insert name d
      | _ => m) ({} : Std.HashMap _ _)
  -- Type defs. For struct/callback mappings we need extra info
  -- threaded into emitTypeDecl: per-field types for structs, the
  -- derived Lean arrow type for callbacks.
  -- Build a per-TypeAnno rendered decl array map first.
  let mut declsByLean : Std.HashMap String (Array Gen.LeanDecl) := {}
  for ta in b.types do
    let decls ← match ta.mapping with
    | .structRecord sm =>
      let fields ← resolveStructFields h typeAnnoMap sm
      let mut m : Std.HashMap String String := {}
      for (_, leanF, r) in fields do
        m := m.insert leanF r.leanType
      pure (emitTypeDecl ta m)
    | .callback =>
      let arrow ← deriveCallbackLeanType h typeAnnoMap ta.cName
      let m : Std.HashMap String String := ({} : Std.HashMap _ _).insert "__callback__" arrow
      pure (emitTypeDecl ta m)
    | .taggedUnion tu => do
      let some headerFields := lookupStructTag h tu.cStructTag
        | .error s!"struct tag `{tu.cStructTag}` not found in header"
      let mut m : Std.HashMap String String := {}
      -- Resolve shared field types.
      for (cField, leanField) in tu.sharedFields do
        let some hf := lookupFieldInStruct h headerFields cField
          | .error s!"shared field `{cField}` not found in struct `{tu.cStructTag}`"
        let r ← resolveType typeAnnoMap hf.type
        m := m.insert leanField r.leanType
      -- Resolve per-variant payload types from the union fields.
      for v in tu.variants do
        match v.payloadOverride with
        | some pairs =>
          -- User-supplied payload description.
          let args := " ".intercalate (pairs.toList.map fun (nm, ty) => s!"({nm} : {ty})")
          m := m.insert s!"__variant__{v.leanCtor}" args
        | none =>
          -- Resolve from the union field's C type.
          let some hf := lookupFieldInStruct h headerFields v.unionField
            | .error s!"union field `{v.unionField}` not found in struct `{tu.cStructTag}`"
          -- Strip pointer/const from the field type to get the payload.
          let payloadTy := match hf.type with
            | .pointer (.const t) | .pointer t | .const t => t
            | t => t
          let r ← resolveType typeAnnoMap payloadTy
          m := m.insert s!"__variant__{v.leanCtor}" s!"(val : {r.leanType})"
      pure (emitTypeDecl ta m)
    | _ => pure (emitTypeDecl ta)
    declsByLean := declsByLean.insert ta.lean decls
  -- Compute SCCs to detect mutual recursion.
  let typeNodes := b.types.map (·.lean)
  let mut adj : Std.HashMap String (List String) := {}
  for ta in b.types do
    let deps := typeDependencies h typeAnnoMap ta
    adj := adj.insert ta.lean deps
  let sccs := computeSCCs typeNodes adj |>.reverse
  -- Emit each SCC in forward topological order (dependencies first).
  -- Size-1 components are emitted as-is; size > 1 get wrapped in
  -- `mutual ... end`.
  let mut typeDecls : Array Gen.LeanDecl := #[]
  for scc in sccs do
    if scc.size ≤ 1 then
      for name in scc do
        if let some decls := declsByLean[name]? then
          typeDecls := typeDecls ++ decls
    else
      let mut parts : Array Gen.LeanDecl := #[]
      for name in scc do
        if let some decls := declsByLean[name]? then
          parts := parts ++ decls
      typeDecls := typeDecls.push (.mutual_ parts)
  -- Function decls
  let mut fnDecls : Array Gen.LeanDecl := #[]
  for fa in b.functions do
    fnDecls := fnDecls.push (← emitFunctionDecl b typeAnnoMap declMap fa)
  let allDecls := typeDecls ++ fnDecls
  let mod : Gen.LeanModule := {
    headerComment := some "Auto-generated by lean-bindgen. Do not edit."
    imports := b.leanImports.map toString
    namespace_ := some (toString b.leanModule)
    decls := allDecls
  }
  return mod.render

/-! ## Shim emitter -/

/-- Construct a parameter list for the shim's C signature, given the
function annotation and resolved param types. -/
private def shimParamList
    (params : Array CParam) (resolved : Array ResolvedType)
    : Array String :=
  resolved.zipIdx.map fun (r, i) =>
    let baseName := (params[i]?.bind (·.name)).getD s!"arg{i}"
    s!"{r.shimParam} {baseName}"

/-- Render `(prelude, exprs, postlude)` for one shim parameter.
For most kinds `exprs` is a single string; for callbacks it's two
(the trampoline pointer and the boxed closure cast to `void *`),
covering the C function's matching `(callback, user_data)` pair. The
function shim emitter is expected to drop the corresponding user-data
slot from the C call's argument list (using `callbackUserDataParams`).

`postlude` contains cleanup statements that run after the C call —
currently only used to `lean_dec` borrowed callback closures. -/
private def renderParamPass (r : ResolvedType) (nm : String)
    : Array String × Array String × Array String :=
  match r.paramMarshal with
  | .passthrough            => (#[], #[nm], #[])
  | .leanString             =>
      (#[s!"  char const *{nm}_c = lean_string_cstr({nm});"], #[s!"{nm}_c"], #[])
  | .enumHelper fn          =>
      (#[s!"  {r.cLocalType} {nm}_c = {fn}({nm});"], #[s!"{nm}_c"], #[])
  | .externalData cTy       =>
      (#[s!"  {cTy} {nm}_c = ({cTy}) lean_get_external_data({nm});"], #[s!"{nm}_c"], #[])
  | .fromLeanStruct fn cTy byPtr =>
      let prelude := #[s!"  {cTy} {nm}_c = {fn}({nm});"]
      (prelude, #[if byPtr then s!"&{nm}_c" else s!"{nm}_c"], #[])
  | .callback trampoline =>
      (#[s!"  lean_inc({nm});"], #[trampoline, s!"(void*){nm}"],
       #[s!"  lean_dec({nm});"])
  | .array cElemTy elem =>
      -- Emit code to convert a Lean Array into a malloc'd C buffer.
      let unboxExpr := match elem with
        | .scalar shimTy suffix =>
          s!"({shimTy})lean_unbox{suffix}(_arr_obj->m_data[_arr_i])"
        | .string =>
          "lean_string_cstr(_arr_obj->m_data[_arr_i])"
        | .enumHelper toC _ =>
          s!"{toC}((uint8_t)lean_unbox(_arr_obj->m_data[_arr_i]))"
        | .structHelper toC _ =>
          s!"{toC}(_arr_obj->m_data[_arr_i])"
      let prelude := #[
        s!"  lean_array_object *{nm}_arr_obj = lean_to_array({nm});",
        s!"  size_t {nm}_size = {nm}_arr_obj->m_size;",
        s!"  if ({nm}_size > SIZE_MAX / sizeof({cElemTy})) lean_internal_panic(\"lean-bindgen: array size overflow\");",
        s!"  {cElemTy} *{nm}_buf = ({cElemTy} *)malloc(sizeof({cElemTy}) * ({nm}_size > 0 ? {nm}_size : 1));",
        s!"  for (size_t _arr_i = 0; _arr_i < {nm}_size; _arr_i++) \{",
        -- Reuse the variable names with a scoped block inside the loop
        s!"    lean_array_object *_arr_obj = {nm}_arr_obj;",
        s!"    {nm}_buf[_arr_i] = {unboxExpr};",
        s!"  }"
      ]
      -- For struct elements, free each element's deep fields before
      -- freeing the buffer.
      let elemFreeLoop := match elem with
        | .structHelper toC _ =>
          -- Derive the free helper name from the toC helper name.
          -- toC is "lean_to_<snake>"; free is "free_<snake>".
          let snake := toC.drop 8  -- drop "lean_to_" prefix (8 chars)
          let freeFn := "free_" ++ snake
          #[s!"  for (size_t _fi = 0; _fi < {nm}_size; _fi++) \{ {freeFn}(&{nm}_buf[_fi]); }"]
        | _ => #[]
      let postlude := elemFreeLoop ++ #[s!"  free({nm}_buf);"]
      -- Two C call args: the buffer pointer and the size.
      (prelude, #[s!"{nm}_buf", s!"{nm}_size"], postlude)
  | .byteArray =>
      -- ByteArray: extract pointer and size from Lean's compact scalar array.
      let prelude := #[
        s!"  uint8_t const *{nm}_ptr = lean_sarray_cptr({nm});",
        s!"  size_t {nm}_len = lean_sarray_size({nm});"
      ]
      (prelude, #[s!"{nm}_ptr", s!"{nm}_len"], #[])

/-- The C expression that turns the C-call result into a Lean object
(prior to any IO wrapping). For non-IO returns whose Lean type *isn't*
a Lean object (e.g. plain scalar, plain enum cidx), the expression
will be a non-Lean-object value — the caller emits it directly as the
shim's return value. -/
private def renderReturn (retRes : ResolvedType) (callExpr : String) (inIO : Bool)
    (nullable : Bool := false) : String :=
  -- `bare` is the *non-IO* return statement.
  let bare : String :=
    match retRes.returnMarshal with
    | .leanString =>
      if nullable then
        s!"char const *_ret = {callExpr};\n  if (_ret == NULL) return lean_box(0);\n" ++
        s!"  lean_object* some = lean_alloc_ctor(1, 1, 0);\n" ++
        s!"  lean_ctor_set(some, 0, lean_mk_string(_ret));\n  return some;"
      else
        s!"char const *_ret = {callExpr};\n  return lean_mk_string(_ret == NULL ? \"\" : _ret);"
    | .enumHelper fn =>
      s!"return {fn}({callExpr});"
    | .externalAlloc getter =>
      s!"return lean_alloc_external({getter}(), (void *)({callExpr}));"
    | .toLeanStruct fn =>
      s!"return {fn}({callExpr});"
    | .array _cElemTy _elem =>
      s!"/* array return in non-IO context not supported */ {callExpr};"
    | .passthrough =>
      match retRes.leanType with
      | "Unit" => s!"{callExpr};\n  return;"
      | "Bool" => s!"return {callExpr} ? 1 : 0;"
      | _      => s!"return ({retRes.shimReturn})({callExpr});"
  if !inIO then bare else
  -- IO wrap. We need a *Lean object* to give to `lean_io_result_mk_ok`.
  -- Each marshalling kind has a different way of producing one.
  match retRes.returnMarshal with
  | .leanString =>
    if nullable then
      s!"char const *_ret = {callExpr};\n" ++
      s!"  if (_ret == NULL) return lean_io_result_mk_ok(lean_box(0));\n" ++
      s!"  lean_object* some = lean_alloc_ctor(1, 1, 0);\n" ++
      s!"  lean_ctor_set(some, 0, lean_mk_string(_ret));\n" ++
      s!"  return lean_io_result_mk_ok(some);"
    else
      s!"char const *_ret = {callExpr};\n  return lean_io_result_mk_ok(lean_mk_string(_ret == NULL ? \"\" : _ret));"
  | .enumHelper fn =>
    s!"return lean_io_result_mk_ok(lean_box({fn}({callExpr})));"
  | .externalAlloc getter =>
    s!"return lean_io_result_mk_ok(lean_alloc_external({getter}(), (void *)({callExpr})));"
  | .toLeanStruct fn =>
    s!"return lean_io_result_mk_ok({fn}({callExpr}));"
  | .array _cElemTy _elem =>
    s!"#error \"lean-bindgen: array return in IO context unsupported\""
  | .passthrough =>
    match retRes.leanType with
    | "Unit" => s!"{callExpr};\n  return lean_io_result_mk_ok(lean_box(0));"
    | "Bool" => s!"return lean_io_result_mk_ok(lean_box({callExpr} ? 1 : 0));"
    | _      => s!"return lean_io_result_mk_ok({boxScalarExpr retRes callExpr});"

/-- Emit the body of a "direct" shim: marshal each Lean parameter,
call the underlying C function, marshal the result. -/
private def directShimBody
    (decl : CDecl) (fa : FunctionAnno)
    (resolved : Array ResolvedType)
    (retRes : ResolvedType) (cName : String) (inIO : Bool)
    : String := Id.run do
  let .function _name _ret params _variadic := decl
    | return "/* not a function */"
  let mut prelude  : Array String := #[]
  let mut postlude : Array String := #[]
  let mut callArgs : Array String := #[]
  let arraySizeIndices := fa.arrayPairs.map Prod.snd
  let byteArraySizeIndices := fa.byteArrayPairs.map Prod.snd
  let hiddenIndices := fa.callbackUserDataParams ++ arraySizeIndices ++ byteArraySizeIndices
  -- Iterate Lean parameters (which mirror C parameters EXCEPT for
  -- those listed in `callbackUserDataParams` / array-size / byteArray-size slots —
  -- those are filled in automatically).
  for h : i in [:resolved.size] do
    if hiddenIndices.contains i then continue
    let r := resolved[i]
    let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
    let (pre, exprs, post) := renderParamPass r nm
    prelude  := prelude ++ pre
    postlude := postlude ++ post
    -- Free postlude for struct params (unless retained by C).
    if !fa.retainedParams.contains i then
      match r.freeHelperFn with
      | some freeFn =>
        match r.paramMarshal with
        | .fromLeanStruct _ _ _ =>
          postlude := postlude ++ #[s!"  {freeFn}(&{nm}_c);"]
        | _ => pure ()
      | none => pure ()
    callArgs := callArgs ++ exprs
  let callExpr := s!"{cName}({", ".intercalate callArgs.toList})"
  let preludeStr  := if prelude.isEmpty  then "" else "\n".intercalate prelude.toList  ++ "\n"
  let postludeStr := if postlude.isEmpty then "" else "\n" ++ "\n".intercalate postlude.toList
  -- The current renderReturn is statement-level (it includes
  -- `return ...;`) so postlude statements need to be inlined into a
  -- temporary-result pattern when present. For simplicity, when we
  -- have postlude work, we capture the call result, run postlude,
  -- then return.
  if postlude.isEmpty then
    preludeStr ++ "  " ++ renderReturn retRes callExpr inIO fa.nullableReturn
  else
    -- Save call result to a temp, run postlude, then return. This is
    -- safe for void/Unit (we just emit the call, then postlude, then
    -- the void return). For value-returning calls the temporary is
    -- of `retRes.shimReturn`-like type — but for the common
    -- callback case the call returns void/Unit, so this path covers
    -- the only currently-needed case.
    match retRes.leanType with
    | "Unit" =>
      preludeStr ++
      s!"  {callExpr};\n" ++
      postludeStr.trimAscii.toString ++ "\n" ++
      (if inIO then "  return lean_io_result_mk_ok(lean_box(0));" else "  return;")
    | _ =>
      -- Capture the call result in a temp, run postlude, then return.
      let cRetTy := retRes.cLocalType
      preludeStr ++
      s!"  {cRetTy} _tmp = {callExpr};\n" ++
      postludeStr.trimAscii.toString ++ "\n" ++
      "  " ++ renderReturn retRes "_tmp" inIO fa.nullableReturn

/-- Emit one shim function for an annotation. -/
private def emitShimFunction
    (b : Bindings)
    (anno : Std.HashMap String TypeAnno)
    (declMap : Std.HashMap String CDecl)
    (fa : FunctionAnno)
    : Except String Gen.CTopLevel := do
  let some decl := declMap[fa.cName]?
    | .error s!"function `{fa.cName}` not found in header"
  let (_, allRes, retRes, inIO) ← buildFunctionSignature anno decl fa
  let .function _ _ params _ := decl
    | .error "internal: non-function passed to emitShimFunction"
  let externSym := externSymbolOf fa
  match fa.style with
  | .direct =>
    -- Drop hidden params (callback user-data + array size + byteArray size) from the
    -- shim signature.
    let arraySizeIndices := fa.arrayPairs.map Prod.snd
    let byteArraySizeIndices := fa.byteArrayPairs.map Prod.snd
    let hiddenDirect := fa.callbackUserDataParams ++ arraySizeIndices ++ byteArraySizeIndices
    let visiblePairs := allRes.zipIdx.filter (fun (_, i) => !hiddenDirect.contains i)
    let plist := visiblePairs.map (fun (r, i) =>
      let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
      s!"{r.shimParam} {nm}")
    -- Pick shim return type:
    let cRet :=
      if inIO || fa.nullableReturn then "lean_obj_res"
      else
        match retRes.leanType with
        | "Unit"   => "void"
        | "String" => "lean_obj_res"
        | "Bool"   => "uint8_t"
        | _        => retRes.shimReturn
    let body := directShimBody decl fa allRes retRes fa.cName inIO
    return .raw (s!"LEAN_EXPORT {cRet} {externSym}({", ".intercalate plist.toList}) \{\n" ++
           body ++ "\n}")
  | .outParamBoolStatus outIdx error =>
    -- Drop the out-param from the shim signature, allocate a local for
    -- it, call the C function, branch on the bool.
    if h : outIdx < params.size then
      let outName := (params[outIdx].name).getD s!"arg{outIdx}"
      let .pointer pointee := params[outIdx].type
        | .error "out-param is not a pointer (shim)"
      let pointeeRes ← resolveType anno pointee
      -- Build visible params: drop the out-param, callback user-data
      -- slots, and array-size slots.
      let arraySizeIndices := fa.arrayPairs.map Prod.snd
      let byteArraySizeIndices := fa.byteArrayPairs.map Prod.snd
      let hiddenOut := fa.callbackUserDataParams ++ arraySizeIndices ++ byteArraySizeIndices
      let visible := allRes.zipIdx.filter (fun (_, i) =>
        i ≠ outIdx && !hiddenOut.contains i)
      let plist := visible.map (fun (r, i) =>
        let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
        s!"{r.shimParam} {nm}")
      let mut prelude  : Array String := #[]
      let mut postlude : Array String := #[]
      let mut callArgs : Array String := #[]
      for h : i in [:allRes.size] do
        let r := allRes[i]
        let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
        if i = outIdx then
          callArgs := callArgs.push s!"&{outName}"
        else if hiddenOut.contains i then
          -- Filled in by the matching callback/array data param.
          continue
        else
          let (pre, exprs, post) := renderParamPass r nm
          prelude  := prelude ++ pre
          postlude := postlude ++ post
          -- Free postlude for struct params (unless retained by C).
          if !fa.retainedParams.contains i then
            match r.freeHelperFn with
            | some freeFn =>
              match r.paramMarshal with
              | .fromLeanStruct _ _ _ =>
                postlude := postlude ++ #[s!"  {freeFn}(&{nm}_c);"]
              | _ => pure ()
            | none => pure ()
          callArgs := callArgs ++ exprs
      let preludeStr  := "\n".intercalate prelude.toList
      let postludeStr := "\n".intercalate postlude.toList
      -- Local type for the out-param. For an opaque-pointer out-param
      -- the C function expects `clingo_X_t **`, so we declare
      -- `clingo_X_t *<name> = NULL;` and pass `&<name>`.
      let outDecl :=
        match pointeeRes.returnMarshal with
        | .externalAlloc _ => s!"  {pointeeRes.cLocalType} {outName} = NULL;"
        | _                => s!"  {pointeeRes.cLocalType} {outName};"
      -- Build the success-value expression based on the out-param's
      -- return-marshalling kind.
      let valExpr :=
        match pointeeRes.returnMarshal with
        | .externalAlloc getter =>
          s!"lean_alloc_external({getter}(), (void *){outName})"
        | .enumHelper fn =>
          s!"lean_box({fn}({outName}))"
        | .leanString =>
          s!"lean_mk_string({outName} == NULL ? \"\" : {outName})"
        | .toLeanStruct fn =>
          s!"{fn}({outName})"
        | .array _ _ =>
          s!"lean_internal_panic(\"lean-bindgen: array out-param unsupported\")"
        | .passthrough =>
          boxScalarExpr pointeeRes outName
      let postBlock :=
        if postludeStr = "" then "" else postludeStr ++ "\n      "
      let okBlock :=
        if fa.nullableOutParam then
          -- Nullable out-param: success path wraps in Option before Except.ok
          s!"\{\n      {postBlock}lean_object* inner;\n" ++
          s!"      if ({outName} == NULL) \{\n        inner = lean_box(0);\n      } else \{\n" ++
          s!"        lean_object* val = {valExpr};\n        inner = lean_alloc_ctor(1, 1, 0);\n        lean_ctor_set(inner, 0, val);\n      }\n" ++
          s!"      lean_object* ok = lean_alloc_ctor(1, 1, 0);\n      lean_ctor_set(ok, 0, inner);\n      return lean_io_result_mk_ok(ok);\n    }"
        else
          s!"\{\n      {postBlock}lean_object* val = {valExpr};\n      lean_object* ok = lean_alloc_ctor(1, 1, 0);\n      lean_ctor_set(ok, 0, val);\n      return lean_io_result_mk_ok(ok);\n    }"
      let errBlock := match error with
        | .string errMsgFn =>
          s!"\{\n      {postBlock}char const *msg = {errMsgFn}();\n      if (msg == NULL) msg = \"\";\n      lean_object* err = lean_alloc_ctor(0, 1, 0);\n      lean_ctor_set(err, 0, lean_mk_string(msg));\n      return lean_io_result_mk_ok(err);\n    }"
        | .enum codeFn eLean =>
          let (_, toLeanFn) := enumHelpers eLean
          s!"\{\n      {postBlock}lean_object* code = lean_box({toLeanFn}({codeFn}()));\n      lean_object* err = lean_alloc_ctor(0, 1, 0);\n      lean_ctor_set(err, 0, code);\n      return lean_io_result_mk_ok(err);\n    }"
        | .tuple codeFn eLean msgFn =>
          let (_, toLeanFn) := enumHelpers eLean
          s!"\{\n      {postBlock}lean_object* code = lean_box({toLeanFn}({codeFn}()));\n      char const *msg = {msgFn}();\n      if (msg == NULL) msg = \"\";\n      lean_object* pair = lean_alloc_ctor(0, 2, 0);\n      lean_ctor_set(pair, 0, code);\n      lean_ctor_set(pair, 1, lean_mk_string(msg));\n      lean_object* err = lean_alloc_ctor(0, 1, 0);\n      lean_ctor_set(err, 0, pair);\n      return lean_io_result_mk_ok(err);\n    }"
      return .raw (s!"LEAN_EXPORT lean_obj_res {externSym}({", ".intercalate plist.toList}) \{\n" ++
             outDecl ++ "\n" ++
             (if preludeStr = "" then "" else preludeStr ++ "\n") ++
             s!"  if ({fa.cName}({", ".intercalate callArgs.toList})) " ++ okBlock ++ " else " ++ errBlock ++ "\n}")
    else
      .error s!"`{fa.cName}` has only {params.size} params but outParamIdx is {outIdx}"
  | .boolStatus error =>
    -- All params are visible in the shim; bool return → Except Unit.
    let arraySizeIndices := fa.arrayPairs.map Prod.snd
    let byteArraySizeIndices := fa.byteArrayPairs.map Prod.snd
    let hiddenOut := fa.callbackUserDataParams ++ arraySizeIndices ++ byteArraySizeIndices
    let visible := allRes.zipIdx.filter (fun (_, i) => !hiddenOut.contains i)
    let plist := visible.map (fun (r, i) =>
      let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
      s!"{r.shimParam} {nm}")
    let mut prelude  : Array String := #[]
    let mut postlude : Array String := #[]
    let mut callArgs : Array String := #[]
    for h : i in [:allRes.size] do
      let r := allRes[i]
      let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
      if hiddenOut.contains i then continue
      let (pre, exprs, post) := renderParamPass r nm
      prelude  := prelude ++ pre
      postlude := postlude ++ post
      if !fa.retainedParams.contains i then
        match r.freeHelperFn with
        | some freeFn =>
          match r.paramMarshal with
          | .fromLeanStruct _ _ _ =>
            postlude := postlude ++ #[s!"  {freeFn}(&{nm}_c);"]
          | _ => pure ()
        | none => pure ()
      callArgs := callArgs ++ exprs
    let preludeStr  := "\n".intercalate prelude.toList
    let postludeStr := "\n".intercalate postlude.toList
    let postBlock :=
      if postludeStr = "" then "" else postludeStr ++ "\n      "
    let okBlock :=
      s!"\{\n      {postBlock}lean_object* ok = lean_alloc_ctor(1, 1, 0);\n      lean_ctor_set(ok, 0, lean_box(0));\n      return lean_io_result_mk_ok(ok);\n    }"
    let errBlock := match error with
      | .string errMsgFn =>
        s!"\{\n      {postBlock}char const *msg = {errMsgFn}();\n      if (msg == NULL) msg = \"\";\n      lean_object* err = lean_alloc_ctor(0, 1, 0);\n      lean_ctor_set(err, 0, lean_mk_string(msg));\n      return lean_io_result_mk_ok(err);\n    }"
      | .enum codeFn eLean =>
        let (_, toLeanFn) := enumHelpers eLean
        s!"\{\n      {postBlock}lean_object* code = lean_box({toLeanFn}({codeFn}()));\n      lean_object* err = lean_alloc_ctor(0, 1, 0);\n      lean_ctor_set(err, 0, code);\n      return lean_io_result_mk_ok(err);\n    }"
      | .tuple codeFn eLean msgFn =>
        let (_, toLeanFn) := enumHelpers eLean
        s!"\{\n      {postBlock}lean_object* code = lean_box({toLeanFn}({codeFn}()));\n      char const *msg = {msgFn}();\n      if (msg == NULL) msg = \"\";\n      lean_object* pair = lean_alloc_ctor(0, 2, 0);\n      lean_ctor_set(pair, 0, code);\n      lean_ctor_set(pair, 1, lean_mk_string(msg));\n      lean_object* err = lean_alloc_ctor(0, 1, 0);\n      lean_ctor_set(err, 0, pair);\n      return lean_io_result_mk_ok(err);\n    }"
    return .raw (s!"LEAN_EXPORT lean_obj_res {externSym}({", ".intercalate plist.toList}) \{\n" ++
           (if preludeStr = "" then "" else preludeStr ++ "\n") ++
           s!"  if ({fa.cName}({", ".intercalate callArgs.toList})) " ++ okBlock ++ " else " ++ errBlock ++ "\n}")
  | .optionOutParam outIdx =>
    -- Same structure as outParamBoolStatus but wraps in Option instead
    -- of Except.  Option.some = ctor 1 (1 field), Option.none = lean_box(0).
    if h : outIdx < params.size then
      let outName := (params[outIdx].name).getD s!"arg{outIdx}"
      let .pointer pointee := params[outIdx].type
        | .error "out-param is not a pointer (shim)"
      let pointeeRes ← resolveType anno pointee
      let arraySizeIndices := fa.arrayPairs.map Prod.snd
      let byteArraySizeIndices := fa.byteArrayPairs.map Prod.snd
      let hiddenOut := fa.callbackUserDataParams ++ arraySizeIndices ++ byteArraySizeIndices
      let visible := allRes.zipIdx.filter (fun (_, i) =>
        i ≠ outIdx && !hiddenOut.contains i)
      let plist := visible.map (fun (r, i) =>
        let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
        s!"{r.shimParam} {nm}")
      let mut prelude  : Array String := #[]
      let mut postlude : Array String := #[]
      let mut callArgs : Array String := #[]
      for h : i in [:allRes.size] do
        let r := allRes[i]
        let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
        if i = outIdx then
          callArgs := callArgs.push s!"&{outName}"
        else if hiddenOut.contains i then
          continue
        else
          let (pre, exprs, post) := renderParamPass r nm
          prelude  := prelude ++ pre
          postlude := postlude ++ post
          if !fa.retainedParams.contains i then
            match r.freeHelperFn with
            | some freeFn =>
              match r.paramMarshal with
              | .fromLeanStruct _ _ _ =>
                postlude := postlude ++ #[s!"  {freeFn}(&{nm}_c);"]
              | _ => pure ()
            | none => pure ()
          callArgs := callArgs ++ exprs
      let preludeStr  := "\n".intercalate prelude.toList
      let postludeStr := "\n".intercalate postlude.toList
      let outDecl :=
        match pointeeRes.returnMarshal with
        | .externalAlloc _ => s!"  {pointeeRes.cLocalType} {outName} = NULL;"
        | _                => s!"  {pointeeRes.cLocalType} {outName};"
      let valExpr :=
        match pointeeRes.returnMarshal with
        | .externalAlloc getter =>
          s!"lean_alloc_external({getter}(), (void *){outName})"
        | .enumHelper fn =>
          s!"lean_box({fn}({outName}))"
        | .leanString =>
          s!"lean_mk_string({outName} == NULL ? \"\" : {outName})"
        | .toLeanStruct fn =>
          s!"{fn}({outName})"
        | .array _ _ =>
          s!"lean_internal_panic(\"lean-bindgen: array out-param unsupported\")"
        | .passthrough =>
          boxScalarExpr pointeeRes outName
      let postBlock :=
        if postludeStr = "" then "" else postludeStr ++ "\n      "
      let someBlock :=
        s!"\{\n      {postBlock}lean_object* val = {valExpr};\n      lean_object* some = lean_alloc_ctor(1, 1, 0);\n      lean_ctor_set(some, 0, val);\n" ++
        (if fa.inIO then s!"      return lean_io_result_mk_ok(some);\n    }"
         else s!"      return some;\n    }")
      let noneBlock :=
        s!"\{\n      {postBlock}" ++
        (if fa.inIO then s!"return lean_io_result_mk_ok(lean_box(0));\n    }"
         else s!"return lean_box(0);\n    }")
      let cRet := if fa.inIO then "lean_obj_res" else "lean_obj_res"
      return .raw (s!"LEAN_EXPORT {cRet} {externSym}({", ".intercalate plist.toList}) \{\n" ++
             outDecl ++ "\n" ++
             (if preludeStr = "" then "" else preludeStr ++ "\n") ++
             s!"  if ({fa.cName}({", ".intercalate callArgs.toList})) " ++ someBlock ++ " else " ++ noneBlock ++ "\n}")
    else
      .error s!"`{fa.cName}` has only {params.size} params but outParamIdx is {outIdx}"
  | .voidOutParam outIdx =>
    -- Void-returning function with an out-param. No error path.
    if h : outIdx < params.size then
      let outName := (params[outIdx].name).getD s!"arg{outIdx}"
      let .pointer pointee := params[outIdx].type
        | .error "out-param is not a pointer (shim)"
      let pointeeRes ← resolveType anno pointee
      let arraySizeIndices := fa.arrayPairs.map Prod.snd
      let byteArraySizeIndices := fa.byteArrayPairs.map Prod.snd
      let hiddenOut := fa.callbackUserDataParams ++ arraySizeIndices ++ byteArraySizeIndices
      let visible := allRes.zipIdx.filter (fun (_, i) =>
        i ≠ outIdx && !hiddenOut.contains i)
      let plist := visible.map (fun (r, i) =>
        let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
        s!"{r.shimParam} {nm}")
      -- If no visible params, add a dummy Unit parameter to match the Lean signature.
      let plist := if plist.isEmpty then #["lean_obj_arg _unit"] else plist
      let mut prelude  : Array String := #[]
      let mut callArgs : Array String := #[]
      for h : i in [:allRes.size] do
        let r := allRes[i]
        let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
        if i = outIdx then
          callArgs := callArgs.push s!"&{outName}"
        else if hiddenOut.contains i then
          continue
        else
          let (pre, exprs, _) := renderParamPass r nm
          prelude  := prelude ++ pre
          callArgs := callArgs ++ exprs
      let preludeStr := "\n".intercalate prelude.toList
      let outDecl :=
        match pointeeRes.returnMarshal with
        | .externalAlloc _ => s!"  {pointeeRes.cLocalType} {outName} = NULL;"
        | _                => s!"  {pointeeRes.cLocalType} {outName};"
      let valExpr :=
        match pointeeRes.returnMarshal with
        | .externalAlloc getter =>
          s!"lean_alloc_external({getter}(), (void *){outName})"
        | .enumHelper fn =>
          s!"lean_box({fn}({outName}))"
        | .leanString =>
          s!"lean_mk_string({outName} == NULL ? \"\" : {outName})"
        | .toLeanStruct fn =>
          s!"{fn}({outName})"
        | .array _ _ =>
          s!"lean_internal_panic(\"lean-bindgen: array out-param unsupported\")"
        | .passthrough =>
          boxScalarExpr pointeeRes outName
      let cRet := if fa.inIO then "lean_obj_res" else pointeeRes.shimReturn
      let retStmt :=
        if fa.inIO then s!"  return lean_io_result_mk_ok({valExpr});"
        else
          -- For pure passthrough, return the raw C value (not boxed).
          match pointeeRes.returnMarshal with
          | .passthrough => s!"  return ({pointeeRes.cLocalType}){outName};"
          | _ => s!"  return {valExpr};"
      return .raw (s!"LEAN_EXPORT {cRet} {externSym}({", ".intercalate plist.toList}) \{\n" ++
             outDecl ++ "\n" ++
             (if preludeStr = "" then "" else preludeStr ++ "\n") ++
             s!"  {fa.cName}({", ".intercalate callArgs.toList});\n" ++
             retStmt ++ "\n}")
    else
      .error s!"`{fa.cName}` has only {params.size} params but outParamIdx is {outIdx}"
  | .optionOutArray ptrIdx sizeIdx =>
    -- Two out-params (data pointer + size) → Option (Array T).
    if h₁ : ptrIdx < params.size then
      if h₂ : sizeIdx < params.size then
        let ptrName := (params[ptrIdx].name).getD s!"arg{ptrIdx}"
        let sizeName := (params[sizeIdx].name).getD s!"arg{sizeIdx}"
        let .pointer innerPtr := params[ptrIdx].type
          | .error "out-array ptr is not a pointer (shim)"
        let .pointer elemTy := innerPtr
          | .error "out-array ptr is not a pointer-to-pointer (shim)"
        let elemRes ← resolveType anno elemTy
        let arraySizeIndices := fa.arrayPairs.map Prod.snd
        let hiddenOut := fa.callbackUserDataParams ++ arraySizeIndices
        let visible := allRes.zipIdx.filter (fun (_, i) =>
          i ≠ ptrIdx && i ≠ sizeIdx && !hiddenOut.contains i)
        let plist := visible.map (fun (r, i) =>
          let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
          s!"{r.shimParam} {nm}")
        let mut prelude  : Array String := #[]
        let mut callArgs : Array String := #[]
        for h : i in [:allRes.size] do
          let r := allRes[i]
          let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
          if i = ptrIdx then
            callArgs := callArgs.push s!"&{ptrName}"
          else if i = sizeIdx then
            callArgs := callArgs.push s!"&{sizeName}"
          else if hiddenOut.contains i then
            continue
          else
            let (pre, exprs, _) := renderParamPass r nm
            prelude  := prelude ++ pre
            callArgs := callArgs ++ exprs
        let preludeStr := "\n".intercalate prelude.toList
        -- Build the array loop expression for the element type.
        let boxElem :=
          match elemRes.returnMarshal with
          | .passthrough => s!"({boxScalarExpr elemRes (s!"{ptrName}[i]")})"
          | .enumHelper fn => s!"lean_box({fn}({ptrName}[i]))"
          | .leanString => s!"lean_mk_string({ptrName}[i])"
          | .toLeanStruct fn => s!"{fn}({ptrName}[i])"
          | .externalAlloc getter => s!"lean_alloc_external({getter}(), (void *){ptrName}[i])"
          | .array _ _ => s!"lean_internal_panic(\"lean-bindgen: nested array unsupported\")"
        let cRet := "lean_obj_res"
        let someBlock :=
          s!"    lean_object *arr = lean_mk_empty_array();\n" ++
          s!"    for (size_t i = 0; i < {sizeName}; i++) \{\n" ++
          s!"      arr = lean_array_push(arr, {boxElem});\n" ++
          s!"    }\n" ++
          s!"    lean_object* some = lean_alloc_ctor(1, 1, 0);\n" ++
          s!"    lean_ctor_set(some, 0, arr);\n" ++
          (if fa.inIO then s!"    return lean_io_result_mk_ok(some);"
           else s!"    return some;")
        let noneBlock :=
          if fa.inIO then s!"    return lean_io_result_mk_ok(lean_box(0));"
          else s!"    return lean_box(0);"
        return .raw (s!"LEAN_EXPORT {cRet} {externSym}({", ".intercalate plist.toList}) \{\n" ++
               s!"  {elemRes.cLocalType} const *{ptrName} = NULL;\n" ++
               s!"  size_t {sizeName} = 0;\n" ++
               (if preludeStr = "" then "" else preludeStr ++ "\n") ++
               s!"  if ({fa.cName}({", ".intercalate callArgs.toList})) \{\n" ++
               someBlock ++ "\n  } else {\n" ++
               noneBlock ++ "\n  }\n}")
      else .error s!"`{fa.cName}` sizeIdx out of range"
    else .error s!"`{fa.cName}` ptrIdx out of range"
  | .callerAllocates sizeFn bufIdx sizeIdx error nullTerm resultKind =>
    -- Two-step: call sizeFn for size, malloc, call mainFn, build result.
    if h₁ : bufIdx < params.size then
      if h₂ : sizeIdx < params.size then
        let .pointer elemTy := params[bufIdx].type
          | .error "buffer param is not a pointer (shim)"
        let elemRes ← resolveType anno elemTy
        let arraySizeIndices := fa.arrayPairs.map Prod.snd
        let hiddenOut := fa.callbackUserDataParams ++ arraySizeIndices
        -- Shared params: everything except bufIdx and sizeIdx.
        let visible := allRes.zipIdx.filter (fun (_, i) =>
          i ≠ bufIdx && i ≠ sizeIdx && !hiddenOut.contains i)
        let plist := visible.map (fun (r, i) =>
          let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
          s!"{r.shimParam} {nm}")
        -- Build prelude + call args for shared params.
        let mut prelude  : Array String := #[]
        let mut sharedCallArgs : Array String := #[]
        for h : i in [:allRes.size] do
          let r := allRes[i]
          let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
          if i = bufIdx || i = sizeIdx || hiddenOut.contains i then
            continue
          else
            let (pre, exprs, _) := renderParamPass r nm
            prelude := prelude ++ pre
            sharedCallArgs := sharedCallArgs ++ exprs
        let preludeStr := "\n".intercalate prelude.toList
        -- Build sizeFn call args: shared params + &_size (in order).
        let mut sizeCallArgs : Array String := #[]
        for h : i in [:allRes.size] do
          let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
          if i = bufIdx then continue  -- sizeFn doesn't have the buffer param
          else if i = sizeIdx then sizeCallArgs := sizeCallArgs.push "&_size"
          else if hiddenOut.contains i then continue
          else
            let r := allRes[i]
            let (_, exprs, _) := renderParamPass r nm
            sizeCallArgs := sizeCallArgs ++ exprs
        -- Build mainFn call args: shared + _buf at bufIdx + _size at sizeIdx.
        let mut mainCallArgs : Array String := #[]
        for h : i in [:allRes.size] do
          let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
          if i = bufIdx then mainCallArgs := mainCallArgs.push "_buf"
          else if i = sizeIdx then mainCallArgs := mainCallArgs.push "_size"
          else if hiddenOut.contains i then continue
          else
            let r := allRes[i]
            let (_, exprs, _) := renderParamPass r nm
            mainCallArgs := mainCallArgs ++ exprs
        -- Determine if result is String or Array.
        let isString := match resultKind with
          | some "String" => true
          | some _        => false
          | none          => elemRes.leanType = "UInt8"  -- char buffer heuristic
        let sizeExpr := if nullTerm then "_size > 0 ? _size - 1 : 0" else "_size"
        let resultExpr :=
          if isString then
            s!"lean_mk_string_from_bytes((char const *)_buf, {sizeExpr})"
          else
            let boxElem :=
              match elemRes.returnMarshal with
              | .passthrough => boxScalarExpr elemRes "_buf[i]"
              | .enumHelper fn => s!"lean_box({fn}(_buf[i]))"
              | .leanString => s!"lean_mk_string(_buf[i])"
              | .toLeanStruct fn => s!"{fn}(_buf[i])"
              | .externalAlloc getter => s!"lean_alloc_external({getter}(), (void *)_buf[i])"
              | .array _ _ => s!"lean_internal_panic(\"lean-bindgen: nested array unsupported\")"
            s!"/* array result built in okBody */ lean_box(0)"
        -- Error block helper.
        let mkErrBlock (postExtra : String) := match error with
          | .string errMsgFn =>
            s!"  {postExtra}char const *msg = {errMsgFn}();\n    if (msg == NULL) msg = \"\";\n    lean_object* err = lean_alloc_ctor(0, 1, 0);\n    lean_ctor_set(err, 0, lean_mk_string(msg));\n    return lean_io_result_mk_ok(err);"
          | .enum codeFn eLean =>
            let (_, toLeanFn) := enumHelpers eLean
            s!"  {postExtra}lean_object* code = lean_box({toLeanFn}({codeFn}()));\n    lean_object* err = lean_alloc_ctor(0, 1, 0);\n    lean_ctor_set(err, 0, code);\n    return lean_io_result_mk_ok(err);"
          | .tuple codeFn eLean msgFn =>
            let (_, toLeanFn) := enumHelpers eLean
            s!"  {postExtra}lean_object* code = lean_box({toLeanFn}({codeFn}()));\n    char const *msg = {msgFn}();\n    if (msg == NULL) msg = \"\";\n    lean_object* pair = lean_alloc_ctor(0, 2, 0);\n    lean_ctor_set(pair, 0, code);\n    lean_ctor_set(pair, 1, lean_mk_string(msg));\n    lean_object* err = lean_alloc_ctor(0, 1, 0);\n    lean_ctor_set(err, 0, pair);\n    return lean_io_result_mk_ok(err);"
        let errBlock1 := mkErrBlock ""
        let errBlock2 := mkErrBlock "free(_buf);\n    "
        -- For non-string array results, we need to build the array
        -- differently (can't use a statement-expression in all compilers).
        let okBody :=
          if isString then
            s!"  lean_object* val = {resultExpr};\n" ++
            s!"  free(_buf);\n" ++
            s!"  lean_object* ok = lean_alloc_ctor(1, 1, 0);\n" ++
            s!"  lean_ctor_set(ok, 0, val);\n" ++
            s!"  return lean_io_result_mk_ok(ok);"
          else
            let boxElem :=
              match elemRes.returnMarshal with
              | .passthrough => boxScalarExpr elemRes "_buf[i]"
              | .enumHelper fn => s!"lean_box({fn}(_buf[i]))"
              | .leanString => s!"lean_mk_string(_buf[i])"
              | .toLeanStruct fn => s!"{fn}(_buf[i])"
              | .externalAlloc getter => s!"lean_alloc_external({getter}(), (void *)_buf[i])"
              | .array _ _ => s!"lean_internal_panic(\"lean-bindgen: nested array unsupported\")"
            s!"  lean_object *arr = lean_mk_empty_array();\n" ++
            s!"  for (size_t i = 0; i < _size; i++) \{\n" ++
            s!"    arr = lean_array_push(arr, {boxElem});\n" ++
            s!"  }\n" ++
            s!"  free(_buf);\n" ++
            s!"  lean_object* ok = lean_alloc_ctor(1, 1, 0);\n" ++
            s!"  lean_ctor_set(ok, 0, arr);\n" ++
            s!"  return lean_io_result_mk_ok(ok);"
        return .raw (s!"LEAN_EXPORT lean_obj_res {externSym}({", ".intercalate plist.toList}) \{\n" ++
               (if preludeStr = "" then "" else preludeStr ++ "\n") ++
               s!"  size_t _size = 0;\n" ++
               s!"  if (!{sizeFn}({", ".intercalate sizeCallArgs.toList})) \{\n" ++
               errBlock1 ++ "\n  }\n" ++
               s!"  if (_size > SIZE_MAX / sizeof({elemRes.cLocalType})) lean_internal_panic(\"lean-bindgen: array size overflow\");\n" ++
               s!"  {elemRes.cLocalType} *_buf = ({elemRes.cLocalType} *)malloc(sizeof({elemRes.cLocalType}) * (_size > 0 ? _size : 1));\n" ++
               s!"  if (!{fa.cName}({", ".intercalate mainCallArgs.toList})) \{\n" ++
               errBlock2 ++ "\n  }\n" ++
               okBody ++ "\n}")
      else .error s!"`{fa.cName}` sizeIdx out of range"
    else .error s!"`{fa.cName}` bufIdx out of range"
  | .multiOutParam outParamIndices =>
    -- Void-return function with multiple out-params. Allocate a local
    -- for each, call the C function, build right-nested Prod.mk ctors.
    let outIdxSet := outParamIndices.toList
    let arraySizeIndices := fa.arrayPairs.map Prod.snd
    let byteArraySizeIndices := fa.byteArrayPairs.map Prod.snd
    let hiddenOut := fa.callbackUserDataParams ++ arraySizeIndices ++ byteArraySizeIndices
    let visible := allRes.zipIdx.filter (fun (_, i) =>
      !outIdxSet.contains i && !hiddenOut.contains i)
    let plist := visible.map (fun (r, i) =>
      let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
      s!"{r.shimParam} {nm}")
    -- If no visible params, add a dummy Unit parameter to match the Lean signature.
    let plist := if plist.isEmpty then #["lean_obj_arg _unit"] else plist
    -- Resolve each out-param's pointee type.
    let mut outInfos : Array (String × ResolvedType) := #[]
    for idx in outParamIndices do
      if h : idx < params.size then
        let outName := (params[idx].name).getD s!"arg{idx}"
        let .pointer pointee := params[idx].type
          | .error "multi-out-param is not a pointer (shim)"
        let pointeeRes ← resolveType anno pointee
        outInfos := outInfos.push (outName, pointeeRes)
    -- Build prelude + call args (including &outName for each out-param).
    let mut prelude  : Array String := #[]
    let mut callArgs : Array String := #[]
    for h : i in [:allRes.size] do
      let r := allRes[i]
      let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
      if outIdxSet.contains i then
        callArgs := callArgs.push s!"&{nm}"
      else if hiddenOut.contains i then
        continue
      else
        let (pre, exprs, _) := renderParamPass r nm
        prelude  := prelude ++ pre
        callArgs := callArgs ++ exprs
    let preludeStr := "\n".intercalate prelude.toList
    -- Out-param local declarations.
    let mut outDecls : Array String := #[]
    for (outName, pointeeRes) in outInfos do
      outDecls := outDecls.push s!"  {pointeeRes.cLocalType} {outName};"
    -- Box each result.
    let mut boxExprs : Array String := #[]
    for (outName, pointeeRes) in outInfos do
      let boxed := match pointeeRes.returnMarshal with
        | .externalAlloc getter =>
          s!"lean_alloc_external({getter}(), (void *){outName})"
        | .enumHelper fn =>
          s!"lean_box({fn}({outName}))"
        | .leanString =>
          s!"lean_mk_string({outName} == NULL ? \"\" : {outName})"
        | .toLeanStruct fn =>
          s!"{fn}({outName})"
        | _ => boxScalarExpr pointeeRes outName
      boxExprs := boxExprs.push boxed
    -- Build right-nested Prod.mk: lean_alloc_ctor(0, 2, 0) is Prod.mk.
    -- For n out-params: (a, (b, (c, d))) built bottom-up.
    let mut prodLines : Array String := #[]
    if boxExprs.size == 0 then
      prodLines := prodLines.push s!"  lean_object* _result = lean_box(0);"
    else if boxExprs.size == 1 then
      prodLines := prodLines.push s!"  lean_object* _result = {boxExprs[0]!};"
    else
      -- Build from right to left: the last element is the innermost.
      -- For [a, b, c]: first box c as _p2, then pair (b, _p2) as _p1,
      -- then pair (a, _p1) as _result.
      let n := boxExprs.size
      -- The rightmost element.
      prodLines := prodLines.push s!"  lean_object* _p{n - 1} = {boxExprs[n - 1]!};"
      -- Build pairs from right to left.
      let mut i := n - 2
      while i > 0 do
        prodLines := prodLines ++ #[
          s!"  lean_object* _t{i} = lean_alloc_ctor(0, 2, 0);",
          s!"  lean_ctor_set(_t{i}, 0, {boxExprs[i]!});",
          s!"  lean_ctor_set(_t{i}, 1, _p{i + 1});",
          s!"  lean_object* _p{i} = _t{i};"
        ]
        i := i - 1
      -- Outermost pair.
      prodLines := prodLines ++ #[
        s!"  lean_object* _result = lean_alloc_ctor(0, 2, 0);",
        s!"  lean_ctor_set(_result, 0, {boxExprs[0]!});",
        s!"  lean_ctor_set(_result, 1, _p1);"
      ]
    let cRet := if fa.inIO then "lean_obj_res" else "lean_obj_res"
    let retStmt :=
      if fa.inIO then "  return lean_io_result_mk_ok(_result);"
      else "  return _result;"
    return .raw (s!"LEAN_EXPORT {cRet} {externSym}({", ".intercalate plist.toList}) \{\n" ++
           "\n".intercalate outDecls.toList ++ "\n" ++
           (if preludeStr = "" then "" else preludeStr ++ "\n") ++
           s!"  {fa.cName}({", ".intercalate callArgs.toList});\n" ++
           "\n".intercalate prodLines.toList ++ "\n" ++
           retStmt ++ "\n}")
  | .byteArrayOutBoolStatus ptrIdx sizeIdx error =>
    -- Bool return + (uint8_t **, size_t *) out-params → ByteArray.
    if h₁ : ptrIdx < params.size then
      if h₂ : sizeIdx < params.size then
        let ptrName := (params[ptrIdx].name).getD s!"arg{ptrIdx}"
        let sizeName := (params[sizeIdx].name).getD s!"arg{sizeIdx}"
        let arraySizeIndices := fa.arrayPairs.map Prod.snd
        let byteArraySizeIndices := fa.byteArrayPairs.map Prod.snd
        let hiddenOut := fa.callbackUserDataParams ++ arraySizeIndices ++ byteArraySizeIndices
        let visible := allRes.zipIdx.filter (fun (_, i) =>
          i ≠ ptrIdx && i ≠ sizeIdx && !hiddenOut.contains i)
        let plist := visible.map (fun (r, i) =>
          let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
          s!"{r.shimParam} {nm}")
        let mut prelude  : Array String := #[]
        let mut postlude : Array String := #[]
        let mut callArgs : Array String := #[]
        for h : i in [:allRes.size] do
          let r := allRes[i]
          let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
          if i = ptrIdx then
            callArgs := callArgs.push s!"&{ptrName}"
          else if i = sizeIdx then
            callArgs := callArgs.push s!"&{sizeName}"
          else if hiddenOut.contains i then
            continue
          else
            let (pre, exprs, post) := renderParamPass r nm
            prelude  := prelude ++ pre
            postlude := postlude ++ post
            if !fa.retainedParams.contains i then
              match r.freeHelperFn with
              | some freeFn =>
                match r.paramMarshal with
                | .fromLeanStruct _ _ _ =>
                  postlude := postlude ++ #[s!"  {freeFn}(&{nm}_c);"]
                | _ => pure ()
              | none => pure ()
            callArgs := callArgs ++ exprs
        let preludeStr  := "\n".intercalate prelude.toList
        let postludeStr := "\n".intercalate postlude.toList
        let postBlock :=
          if postludeStr = "" then "" else postludeStr ++ "\n      "
        let okBlock :=
          s!"\{\n      {postBlock}lean_object* ba = lean_alloc_sarray(1, {sizeName}, {sizeName});\n" ++
          s!"      memcpy(lean_sarray_cptr(ba), {ptrName}, {sizeName});\n" ++
          s!"      free({ptrName});\n" ++
          s!"      lean_object* ok = lean_alloc_ctor(1, 1, 0);\n" ++
          s!"      lean_ctor_set(ok, 0, ba);\n" ++
          s!"      return lean_io_result_mk_ok(ok);\n    }"
        let errBlock := match error with
          | .string errMsgFn =>
            s!"\{\n      {postBlock}char const *msg = {errMsgFn}();\n      if (msg == NULL) msg = \"\";\n      lean_object* err = lean_alloc_ctor(0, 1, 0);\n      lean_ctor_set(err, 0, lean_mk_string(msg));\n      return lean_io_result_mk_ok(err);\n    }"
          | .enum codeFn eLean =>
            let (_, toLeanFn) := enumHelpers eLean
            s!"\{\n      {postBlock}lean_object* code = lean_box({toLeanFn}({codeFn}()));\n      lean_object* err = lean_alloc_ctor(0, 1, 0);\n      lean_ctor_set(err, 0, code);\n      return lean_io_result_mk_ok(err);\n    }"
          | .tuple codeFn eLean msgFn =>
            let (_, toLeanFn) := enumHelpers eLean
            s!"\{\n      {postBlock}lean_object* code = lean_box({toLeanFn}({codeFn}()));\n      char const *msg = {msgFn}();\n      if (msg == NULL) msg = \"\";\n      lean_object* pair = lean_alloc_ctor(0, 2, 0);\n      lean_ctor_set(pair, 0, code);\n      lean_ctor_set(pair, 1, lean_mk_string(msg));\n      lean_object* err = lean_alloc_ctor(0, 1, 0);\n      lean_ctor_set(err, 0, pair);\n      return lean_io_result_mk_ok(err);\n    }"
        return .raw (s!"LEAN_EXPORT lean_obj_res {externSym}({", ".intercalate plist.toList}) \{\n" ++
               s!"  uint8_t *{ptrName} = NULL;\n" ++
               s!"  size_t {sizeName} = 0;\n" ++
               (if preludeStr = "" then "" else preludeStr ++ "\n") ++
               s!"  if ({fa.cName}({", ".intercalate callArgs.toList})) " ++ okBlock ++ " else " ++ errBlock ++ "\n}")
      else .error s!"`{fa.cName}` sizeIdx out of range"
    else .error s!"`{fa.cName}` ptrIdx out of range"

/-- Emit the pair of static helper functions converting between a
Lean inductive-encoded enum and the underlying C int. -/
private def emitEnumHelpers
    (h : HeaderIndex) (ta : TypeAnno) (em : EnumMapping)
    : Except String (Array Gen.CTopLevel) := do
  let some headerVariants := lookupEnumTag h em.enumTag
    | .error s!"enum tag `{em.enumTag}` not found in header"
  -- Build a name → C-int map from the parsed header.
  let mut valueByName : Std.HashMap String Int := {}
  let mut running : Int := 0
  for (n, v?) in headerVariants do
    let v := v?.getD running
    valueByName := valueByName.insert n v
    running := v + 1
  -- Sanity-check the user's variant list.
  let mut pairs : Array (String × String × Int) := #[]
  for (cV, leanV) in em.variants do
    let some cVal := valueByName[cV]?
      | .error s!"variant `{cV}` not found in enum `{em.enumTag}`"
    pairs := pairs.push (cV, leanV, cVal)
  let (toC, toLean) := enumHelpers ta.lean
  -- toC: uint8_t cidx → C enum value
  let toCCases := pairs.zipIdx.map fun ((cV, _, _), i) =>
    (.intLit i, #[.ret (some (.var cV))])
  let toCDefault := #[.ret (some (.cast ta.cName (.intLit 0)))]
  let toCDef : Gen.CTopLevel := .funcDef .static_ ta.cName toC
    #[⟨"uint8_t", "cidx"⟩]
    #[.switch (.var "cidx") toCCases toCDefault]
  -- toLean: C enum value → uint8_t cidx
  let toLeanCases := pairs.zipIdx.map fun ((cV, _, _), i) =>
    (.var cV, #[.ret (some (.intLit i))])
  let toLeanDefault := #[.ret (some (.intLit 0))]
  let toLeanDef : Gen.CTopLevel := .funcDef .static_ "uint8_t" toLean
    #[⟨ta.cName, "v"⟩]
    #[.switch (.var "v") toLeanCases toLeanDefault]
  return #[toCDef, toLeanDef]

/-- Reorder resolved fields to match Lean's runtime ctor layout:
1. Pointer (boxed) fields — declaration order
2. USize scalar fields — declaration order
3. Other scalar fields — descending `cByteSize` (stable within same size)

This is needed because Lean's compiler reorders structure fields in
memory regardless of source declaration order. The C shim must match
the runtime layout to produce correct `lean_ctor_get/set` offsets. -/
private def reorderForCtorLayout
    (fields : Array (String × String × ResolvedType))
    : Array (String × String × ResolvedType) :=
  let boxed  := fields.filter (·.2.2.isBoxedInCtor)
  let usize  := fields.filter (fun f => !f.2.2.isBoxedInCtor && f.2.2.ctorScalar == "usize")
  let others := fields.filter (fun f => !f.2.2.isBoxedInCtor && f.2.2.ctorScalar != "usize")
  -- Sort other scalars by descending byte size (stable: preserves
  -- declaration order within same size).
  let othersSorted := others.qsort (fun a b => a.2.2.cByteSize > b.2.2.cByteSize)
  boxed ++ usize ++ othersSorted

/-- Emit a pair of static helper functions converting between a C
struct value and a Lean ctor.

Layout assumption: 64-bit (`sizeof(void*) = sizeof(size_t) = 8`). The
helpers mirror Lean's compiled struct ABI: boxed (pointer) fields
first, then USize scalars, then other scalars by descending size. -/
private def emitStructHelpers
    (h : HeaderIndex) (anno : Std.HashMap String TypeAnno)
    (ta : TypeAnno) (sm : StructMapping)
    : Except String (Array Gen.CTopLevel) := do
  let fields ← resolveStructFields h anno sm
  let layoutFields := reorderForCtorLayout fields
  let boxedCount := layoutFields.filter (·.2.2.isBoxedInCtor) |>.size
  let usizeCount := layoutFields.filter (fun f => !f.2.2.isBoxedInCtor && f.2.2.ctorScalar == "usize") |>.size
  let scalarBytes : Nat := layoutFields.foldl (init := 0) (· + ·.2.2.cByteSize)
  -- Validate lean_alloc_ctor limits.
  if boxedCount >= 256 then
    .error s!"struct `{ta.cName}`: num_objs ({boxedCount}) exceeds lean_alloc_ctor limit of 255"
  if scalarBytes >= 1024 then
    .error s!"struct `{ta.cName}`: scalar_sz ({scalarBytes}) exceeds lean_alloc_ctor limit of 1023"
  let (toC, toLean) := structHelperNames ta.lean
  let cTypedef := ta.cName
  let mut toLeanStmts : Array Gen.CStmt := #[
    .varDecl "lean_object*" "o" (some (.call "lean_alloc_ctor"
      #[.intLit 0, .intLit boxedCount, .intLit scalarBytes]))
  ]
  let mut toCStmts : Array Gen.CStmt := #[.varDecl cTypedef "v"]
  let mut boxedIdx : Nat := 0
  let mut usizeIdx : Nat := 0
  let mut scalarOff : Nat := (boxedCount + usizeCount) * 8
  for (cField, _leanField, r) in layoutFields do
    if r.isBoxedInCtor then
      -- toLean: boxed field
      match r.returnMarshal with
      | .array _cElemTy elem =>
        let sizeField := (sm.arrayFields.find? (·.1 = cField)).map (·.2) |>.getD "size"
        let boxExpr := match elem with
          | .scalar _ suffix => s!"lean_box{suffix}(v.{cField}[_i])"
          | .string => s!"lean_mk_string(v.{cField}[_i] == NULL ? \"\" : v.{cField}[_i])"
          | .enumHelper _ toLeanFn => s!"lean_box({toLeanFn}(v.{cField}[_i]))"
          | .structHelper _ toLeanFn => s!"{toLeanFn}(v.{cField}[_i])"
        toLeanStmts := toLeanStmts.push (.block #[
          .varDecl "lean_object*" "_arr" (some (.call "lean_mk_empty_array_with_capacity"
            #[.call "lean_usize_to_nat" #[.raw s!"v.{sizeField}"]])),
          .forLoop "size_t _i = 0" s!"_i < v.{sizeField}" "_i++" #[
            .assign (.var "_arr") (.call "lean_array_push" #[.var "_arr", .raw boxExpr])
          ],
          .expr (.call "lean_ctor_set" #[.var "o", .intLit boxedIdx, .var "_arr"])
        ])
      | _ =>
        let toLeanExpr := match r.returnMarshal with
          | .leanString       => s!"lean_mk_string(v.{cField} == NULL ? \"\" : v.{cField})"
          | .toLeanStruct fn  =>
            if r.isPtrToStruct then s!"{fn}(*v.{cField})" else s!"{fn}(v.{cField})"
          | .enumHelper fn    => s!"lean_box({fn}(v.{cField}))"
          | .externalAlloc gt => s!"lean_alloc_external({gt}(), (void *)(v.{cField}))"
          | _                 => s!"/* unsupported boxed field {cField} of type {r.leanType} */ NULL"
        toLeanStmts := toLeanStmts.push (.expr (.call "lean_ctor_set"
          #[.var "o", .intLit boxedIdx, .raw toLeanExpr]))
      -- toC: boxed field
      match r.paramMarshal with
      | .array cElemTy elem =>
        let sizeField := (sm.arrayFields.find? (·.1 = cField)).map (·.2) |>.getD "size"
        let unboxExpr := match elem with
          | .scalar shimTy suffix => s!"({shimTy})lean_unbox{suffix}(_arr_obj->m_data[_i])"
          | .string => s!"lean_string_cstr(_arr_obj->m_data[_i])"
          | .enumHelper toCFn _ => s!"{toCFn}((uint8_t)lean_unbox(_arr_obj->m_data[_i]))"
          | .structHelper toCFn _ => s!"{toCFn}(_arr_obj->m_data[_i])"
        toCStmts := toCStmts.push (.block #[
          .varDecl "lean_array_object *" "_arr_obj" (some (.call "lean_to_array"
            #[.call "lean_ctor_get" #[.var "obj", .intLit boxedIdx]])),
          .raw s!"v.{sizeField} = _arr_obj->m_size;",
          .raw s!"if (v.{sizeField} > SIZE_MAX / sizeof({cElemTy})) lean_internal_panic(\"lean-bindgen: array size overflow\");",
          .raw s!"v.{cField} = ({cElemTy} *)malloc(sizeof({cElemTy}) * (v.{sizeField} > 0 ? v.{sizeField} : 1));",
          .forLoop "size_t _i = 0" s!"_i < v.{sizeField}" "_i++" #[
            .raw s!"(({cElemTy} *)v.{cField})[_i] = {unboxExpr};"
          ]
        ])
      | .fromLeanStruct fn cTy true =>
        toCStmts := toCStmts.push (.raw s!"v.{cField} = ({cTy} *)malloc(sizeof({cTy}));")
        toCStmts := toCStmts.push (.raw s!"*({cTy} *)v.{cField} = {fn}(lean_ctor_get(obj, {boxedIdx}));")
      | .leanString =>
        toCStmts := toCStmts.push (.raw s!"v.{cField} = strdup(lean_string_cstr(lean_ctor_get(obj, {boxedIdx})));")
      | .fromLeanStruct fn _ false =>
        toCStmts := toCStmts.push (.raw s!"v.{cField} = {fn}(lean_ctor_get(obj, {boxedIdx}));")
      | .enumHelper fn =>
        toCStmts := toCStmts.push (.raw s!"v.{cField} = {fn}((uint8_t)lean_unbox(lean_ctor_get(obj, {boxedIdx})));")
      | .externalData cTy =>
        toCStmts := toCStmts.push (.raw s!"v.{cField} = ({cTy}) lean_get_external_data(lean_ctor_get(obj, {boxedIdx}));")
      | _ =>
        toCStmts := toCStmts.push (.raw s!"v.{cField} = /* unsupported boxed field {cField} */ NULL;")
      boxedIdx := boxedIdx + 1
    else if r.ctorScalar == "usize" then
      let slotIdx := boxedCount + usizeIdx
      toLeanStmts := toLeanStmts.push (.expr (.call s!"lean_ctor_set_usize"
        #[.var "o", .intLit slotIdx, .cast "size_t" (.field (.var "v") cField)]))
      toCStmts := toCStmts.push (.raw s!"v.{cField} = ({r.shimReturn})lean_ctor_get_usize(obj, {slotIdx});")
      usizeIdx := usizeIdx + 1
    else
      let suffix := r.ctorScalar
      toLeanStmts := toLeanStmts.push (.expr (.call s!"lean_ctor_set_{suffix}"
        #[.var "o", .intLit scalarOff, .cast r.shimReturn (.field (.var "v") cField)]))
      toCStmts := toCStmts.push (.raw s!"v.{cField} = ({r.shimReturn})lean_ctor_get_{suffix}(obj, {scalarOff});")
      scalarOff := scalarOff + r.cByteSize
  toLeanStmts := toLeanStmts.push (.ret (some (.var "o")))
  toCStmts := toCStmts.push (.ret (some (.var "v")))
  let toLeanFn : Gen.CTopLevel := .funcDef .plain "lean_object*" toLean
    #[⟨cTypedef, "v"⟩] toLeanStmts
  let toCFn : Gen.CTopLevel := .funcDef .plain cTypedef toC
    #[⟨"b_lean_obj_arg", "obj"⟩] toCStmts
  return #[toLeanFn, toCFn]

/-- Emit the pair of conversion helpers for a tagged union: a C→Lean
function that switches on the tag field and builds the appropriate
Lean inductive ctor, and a Lean→C function that reads
`lean_ptr_tag(obj)` and sets the C tag + union field. When the
mapping has shared fields, the helpers also marshal those. -/
private def emitTaggedUnionHelpers
    (h : HeaderIndex) (anno : Std.HashMap String TypeAnno)
    (ta : TypeAnno) (tu : TaggedUnionMapping)
    : Except String String := do
  let some headerFields := lookupStructTag h tu.cStructTag
    | .error s!"struct tag `{tu.cStructTag}` not found in header"
  -- Look up the discriminator enum values.
  let some enumVariants := lookupEnumTag h tu.tagEnum
    | .error s!"enum tag `{tu.tagEnum}` not found in header"
  let mut tagValueByName : Std.HashMap String Int := {}
  let mut running : Int := 0
  for (n, v?) in enumVariants do
    let v := v?.getD running
    tagValueByName := tagValueByName.insert n v
    running := v + 1
  let (toC, toLean) := structHelperNames ta.lean
  let cTypedef := ta.cName
  -- Resolve shared fields.
  let mut sharedResolved : Array (String × String × ResolvedType) := #[]
  for (cField, leanField) in tu.sharedFields do
    let some hf := lookupFieldInStruct h headerFields cField
      | .error s!"shared field `{cField}` not found in struct `{tu.cStructTag}`"
    let r ← resolveType anno hf.type
    sharedResolved := sharedResolved.push (cField, leanField, r)
  -- Resolve per-variant payloads.
  let mut variantInfo : Array (String × String × String × Option ResolvedType) := #[]
  for v in tu.variants do
    let some _tagVal := tagValueByName[v.cTag]?
      | .error s!"tag `{v.cTag}` not found in enum `{tu.tagEnum}`"
    -- Resolve the payload type from the union field.
    -- We resolve the *original* field type (including pointer) so that
    -- pointer-to-struct fields get `structByPtrRT` (isPtrToStruct=true).
    -- For non-pointer scalars/enums the original type works fine too.
    -- We only strip `const` qualifiers since resolveType handles those.
    let payloadRes? ← match lookupFieldInStruct h headerFields v.unionField with
      | some hf => do
        let r ← resolveType anno hf.type
        pure (some r)
      | none => pure none
    variantInfo := variantInfo.push (v.cTag, v.leanCtor, v.unionField, payloadRes?)
  -- Validate lean_alloc_ctor limits for variant tags.
  if variantInfo.size > 244 then
    .error s!"tagged union `{ta.cName}`: {variantInfo.size} variants exceeds lean_alloc_ctor tag limit of 243"
  let hasShared := !tu.sharedFields.isEmpty
  -- == C→Lean (_to_lean) ==
  -- The result is a Lean inductive. If shared fields exist, we wrap
  -- it in a structure: `lean_alloc_ctor(0, nBoxed, nScalar)` with
  -- shared fields + a "data" field (the inductive).
  let mut toLeanCases : Array String := #[]
  for ((cTag, _leanCtor, unionField, payloadRes?), varIdx) in variantInfo.toList.zipIdx do
    let payloadExpr := match payloadRes? with
      | none => ""
      | some r => match r.returnMarshal with
        | .leanString      => s!"\n      lean_ctor_set(data, 0, lean_mk_string(v.{unionField} == NULL ? \"\" : v.{unionField}));"
        | .toLeanStruct fn =>
          if r.isPtrToStruct
          then s!"\n      lean_ctor_set(data, 0, {fn}(*v.{unionField}));"
          else s!"\n      lean_ctor_set(data, 0, {fn}(v.{unionField}));"
        | .enumHelper fn   => s!"\n      lean_ctor_set(data, 0, lean_box({fn}(v.{unionField})));"
        | .externalAlloc g => s!"\n      lean_ctor_set(data, 0, lean_alloc_external({g}(), (void *)(v.{unionField})));"
        | .passthrough     =>
          match r.ctorScalar with
          | "uint64"  => s!"\n      lean_ctor_set(data, 0, lean_box_uint64(v.{unionField}));"
          | "uint32"  => s!"\n      lean_ctor_set(data, 0, lean_box_uint32(v.{unionField}));"
          | _         => s!"\n      lean_ctor_set(data, 0, lean_box((size_t)v.{unionField}));"
        | _ => s!"\n      /* unsupported variant payload for {unionField} */"
    let nBoxed := if payloadRes?.isSome then 1 else 0
    let allocExpr := s!"lean_alloc_ctor({varIdx}, {nBoxed}, 0)"
    toLeanCases := toLeanCases.push
      (s!"    case {cTag}: \{\n      lean_object* data = {allocExpr};" ++
       payloadExpr ++
       s!"\n      result = data;\n      break;\n    }")
  let switchBody := "\n".intercalate toLeanCases.toList
  let toLeanBody ← if !hasShared then pure (
    s!"lean_object* {toLean}({cTypedef} v) \{\n" ++
    s!"  lean_object* result = NULL;\n" ++
    s!"  switch (v.{tu.tagField}) \{\n" ++ switchBody ++ "\n" ++
    s!"    default: result = lean_alloc_ctor(0, 0, 0); break;\n" ++
    s!"  }\n  return result;\n}")
  else do
    -- Reorder shared fields for Lean's ctor layout, then add "data"
    -- (always boxed) at the end of the pointer group.
    let layoutShared := reorderForCtorLayout sharedResolved
    let sharedBoxed := layoutShared.filter (·.2.2.isBoxedInCtor) |>.size
    let totalBoxed := sharedBoxed + 1  -- +1 for the data field
    let sharedUSize := layoutShared.filter (fun f => !f.2.2.isBoxedInCtor && f.2.2.ctorScalar == "usize") |>.size
    let sharedScalar := layoutShared.foldl (init := 0) (· + ·.2.2.cByteSize)
    let mut setLines : Array String := #[]
    let mut boxIdx := 0
    let mut usizeIdx := 0
    let mut scOff := (totalBoxed + sharedUSize) * 8
    for (cField, _, r) in layoutShared do
      if r.isBoxedInCtor then
        let expr := match r.returnMarshal with
          | .leanString       => s!"lean_mk_string(v.{cField} == NULL ? \"\" : v.{cField})"
          | .toLeanStruct fn  =>
            if r.isPtrToStruct
            then s!"{fn}(*v.{cField})"
            else s!"{fn}(v.{cField})"
          | .enumHelper fn    => s!"lean_box({fn}(v.{cField}))"
          | .externalAlloc gt => s!"lean_alloc_external({gt}(), (void *)(v.{cField}))"
          | _                 => s!"/* unsupported */ NULL"
        setLines := setLines.push s!"  lean_ctor_set(o, {boxIdx}, {expr});"
        boxIdx := boxIdx + 1
      else if r.ctorScalar == "usize" then
        let slotIdx := totalBoxed + usizeIdx
        setLines := setLines.push s!"  lean_ctor_set_usize(o, {slotIdx}, (size_t)v.{cField});"
        usizeIdx := usizeIdx + 1
      else
        let suffix := r.ctorScalar
        setLines := setLines.push s!"  lean_ctor_set_{suffix}(o, {scOff}, ({r.shimReturn})v.{cField});"
        scOff := scOff + r.cByteSize
    pure (
    s!"lean_object* {toLean}({cTypedef} v) \{\n" ++
    s!"  lean_object* result = NULL;\n" ++
    s!"  switch (v.{tu.tagField}) \{\n" ++ switchBody ++ "\n" ++
    s!"    default: result = lean_alloc_ctor(0, 0, 0); break;\n" ++
    s!"  }\n" ++
    s!"  lean_object* o = lean_alloc_ctor(0, {totalBoxed}, {sharedScalar});\n" ++
    "\n".intercalate setLines.toList ++ "\n" ++
    s!"  lean_ctor_set(o, {boxIdx}, result);\n" ++
    s!"  return o;\n}")
  -- == Lean→C (lean_to_) ==
  let mut toCCases : Array String := #[]
  for ((cTag, _leanCtor, unionField, payloadRes?), varIdx) in variantInfo.toList.zipIdx do
    let setPayload := match payloadRes? with
      | none => ""
      | some r => match r.paramMarshal with
        | .leanString           => s!"\n      v.{unionField} = lean_string_cstr(lean_ctor_get(data, 0));"
        | .fromLeanStruct fn cTy true =>
          -- Pointer-to-struct: malloc + assign through pointer
          s!"\n      v.{unionField} = ({cTy} *)malloc(sizeof({cTy}));" ++
          s!"\n      *({cTy} *)v.{unionField} = {fn}(lean_ctor_get(data, 0));"
        | .fromLeanStruct fn _ false => s!"\n      v.{unionField} = {fn}(lean_ctor_get(data, 0));"
        | .enumHelper fn        => s!"\n      v.{unionField} = {fn}((uint8_t)lean_unbox(lean_ctor_get(data, 0)));"
        | .externalData cTy     => s!"\n      v.{unionField} = ({cTy}) lean_get_external_data(lean_ctor_get(data, 0));"
        | .passthrough          =>
          match r.ctorScalar with
          | "uint64"  => s!"\n      v.{unionField} = ({r.shimReturn})lean_unbox_uint64(lean_ctor_get(data, 0));"
          | "uint32"  => s!"\n      v.{unionField} = ({r.shimReturn})lean_unbox_uint32(lean_ctor_get(data, 0));"
          | _         => s!"\n      v.{unionField} = ({r.shimReturn})lean_unbox(lean_ctor_get(data, 0));"
        | _ => s!"\n      /* unsupported variant payload for {unionField} */"
    toCCases := toCCases.push
      (s!"    case {varIdx}: \{\n      v.{tu.tagField} = {cTag};" ++
       setPayload ++ s!"\n      break;\n    }")
  let toCSwitch := "\n".intercalate toCCases.toList
  let toCBody ← if !hasShared then
    pure (s!"{cTypedef} {toC}(b_lean_obj_arg obj) \{\n" ++
    s!"  {cTypedef} v;\n  memset(&v, 0, sizeof(v));\n" ++
    s!"  lean_object* data = obj;\n" ++
    s!"  unsigned cidx = lean_ptr_tag(data);\n" ++
    s!"  switch (cidx) \{\n" ++ toCSwitch ++ "\n" ++
    s!"    default: break;\n" ++
    s!"  }\n  return v;\n}")
  else do
    -- Read shared fields from wrapper (using layout-reordered fields),
    -- then read data from the last boxed field.
    let layoutShared := reorderForCtorLayout sharedResolved
    let sharedBoxed := layoutShared.filter (·.2.2.isBoxedInCtor) |>.size
    let sharedUSize := layoutShared.filter (fun f => !f.2.2.isBoxedInCtor && f.2.2.ctorScalar == "usize") |>.size
    let dataBoxIdx := sharedBoxed  -- data field is after shared boxed fields
    let mut readLines : Array String := #[]
    let mut boxIdx := 0
    let mut usizeIdx := 0
    let mut scOff := (sharedBoxed + 1 + sharedUSize) * 8
    for (cField, _, r) in layoutShared do
      if r.isBoxedInCtor then
        match r.paramMarshal with
        | .fromLeanStruct fn cTy true =>
          readLines := readLines.push s!"  v.{cField} = ({cTy} *)malloc(sizeof({cTy}));"
          readLines := readLines.push s!"  *({cTy} *)v.{cField} = {fn}(lean_ctor_get(obj, {boxIdx}));"
        | _ =>
          let expr := match r.paramMarshal with
            | .leanString           => s!"lean_string_cstr(lean_ctor_get(obj, {boxIdx}))"
            | .fromLeanStruct fn _ false => s!"{fn}(lean_ctor_get(obj, {boxIdx}))"
            | .enumHelper fn        => s!"{fn}((uint8_t)lean_unbox(lean_ctor_get(obj, {boxIdx})))"
            | .externalData cTy     => s!"({cTy})lean_get_external_data(lean_ctor_get(obj, {boxIdx}))"
            | _                     => s!"/* unsupported */ NULL"
          readLines := readLines.push s!"  v.{cField} = {expr};"
        boxIdx := boxIdx + 1
      else if r.ctorScalar == "usize" then
        let slotIdx := sharedBoxed + 1 + usizeIdx
        readLines := readLines.push s!"  v.{cField} = ({r.shimReturn})lean_ctor_get_usize(obj, {slotIdx});"
        usizeIdx := usizeIdx + 1
      else
        let suffix := r.ctorScalar
        readLines := readLines.push s!"  v.{cField} = ({r.shimReturn})lean_ctor_get_{suffix}(obj, {scOff});"
        scOff := scOff + r.cByteSize
    pure (s!"{cTypedef} {toC}(b_lean_obj_arg obj) \{\n" ++
    s!"  {cTypedef} v;\n  memset(&v, 0, sizeof(v));\n" ++
    "\n".intercalate readLines.toList ++ "\n" ++
    s!"  lean_object* data = lean_ctor_get(obj, {dataBoxIdx});\n" ++
    s!"  unsigned cidx = lean_ptr_tag(data);\n" ++
    s!"  switch (cidx) \{\n" ++ toCSwitch ++ "\n" ++
    s!"    default: break;\n" ++
    s!"  }\n  return v;\n}")
  return toLeanBody ++ "\n\n" ++ toCBody

/-- C expression that boxes a C value of the given resolved type into
a `lean_object*`. Used by trampolines that hand C-side values back to
a Lean closure. -/
private def marshalCToLean (r : ResolvedType) (cVar : String) : String :=
  match r.returnMarshal with
  | .leanString          => s!"lean_mk_string({cVar} == NULL ? \"\" : {cVar})"
  | .enumHelper fn       => s!"lean_box({fn}({cVar}))"
  | .externalAlloc getter => s!"lean_alloc_external({getter}(), (void *)({cVar}))"
  | .toLeanStruct fn     => s!"{fn}({cVar})"
  | .array _cElemTy _elem =>
    s!"lean_internal_panic(\"lean-bindgen: array C-to-Lean in callback context unsupported\")"
  | .passthrough =>
    match r.ctorScalar with
    | "uint8"   => s!"lean_box((uint8_t)({cVar}))"
    | "uint16"  => s!"lean_box((uint16_t)({cVar}))"
    | "uint32"  => s!"lean_box_uint32({cVar})"
    | "uint64"  => s!"lean_box_uint64({cVar})"
    | "usize"   => s!"lean_box_usize({cVar})"
    | "float"   => s!"lean_box_float({cVar})"
    | "float32" => s!"lean_box_float32({cVar})"
    | _         => s!"lean_box(0) /* unsupported {r.leanType} */"

/-- C expression that unboxes a Lean object into a C value.
Inverse of `marshalCToLean`. Used by reverse trampolines. -/
private def marshalLeanToC (r : ResolvedType) (leanVar : String) : String :=
  match r.paramMarshal with
  | .passthrough =>
    match r.ctorScalar with
    | "uint8"   => s!"(uint8_t)lean_unbox({leanVar})"
    | "uint16"  => s!"(uint16_t)lean_unbox({leanVar})"
    | "uint32"  => s!"lean_unbox_uint32({leanVar})"
    | "uint64"  => s!"lean_unbox_uint64({leanVar})"
    | "usize"   => s!"(size_t)lean_unbox_usize({leanVar})"
    | "float"   => s!"lean_unbox_float({leanVar})"
    | "float32" => s!"lean_unbox_float32({leanVar})"
    | _         => s!"lean_unbox({leanVar})"
  | .leanString       => s!"lean_string_cstr({leanVar})"
  | .enumHelper fn    => s!"{fn}((uint8_t)lean_unbox({leanVar}))"
  | .fromLeanStruct fn _ _ => s!"{fn}({leanVar})"
  | .externalData cTy => s!"({cTy})lean_get_external_data({leanVar})"
  | .array _ _        => s!"/* array Lean→C: handled inline */ NULL"
  | .byteArray        => s!"/* byteArray Lean→C: handled inline */ NULL"
  | .callback _       => s!"/* nested callback: handled inline */ NULL"

/-- Emit the forward trampoline function for a callback typedef (C→Lean).
The trampoline mirrors the C function-pointer's signature, treats the
outer `void *` as a `lean_object *` (the Lean closure), marshals each
remaining C argument into a Lean object, then invokes the closure via
`lean_apply_*` with an IO world token.

Handles:
- Non-void returns (extracts value from `lean_io_result_get_value`)
- Array pairs (pointer + size_t → Lean Array)
- Nested callbacks (inner callback + void* → Lean closure via reverse
  trampoline) -/
private def emitCallbackTrampoline
    (h : HeaderIndex) (anno : Std.HashMap String TypeAnno) (ta : TypeAnno)
    : Except String Gen.CTopLevel := do
  let (ret, params, _) ← callbackSignature h ta.cName
  let layout ← analyzeCallbackLayout params anno
  let nestedCBIdxs := layout.nestedCallbacks.toList.map Prod.fst
  let nestedCBUDIdxs := layout.nestedCallbacks.toList.map Prod.snd
  let arrayDataIdxs := layout.arrayPairs.toList.map Prod.fst
  let arraySizeIdxs := layout.arrayPairs.toList.map Prod.snd
  let retRes ← resolveType anno ret
  let isVoid := match ret with | .void => true | _ => false
  let trampolineName := callbackTrampolineName ta.cName
  -- Build C signature: use original C types for all params, except the
  -- outer user-data slot is declared as `void *`.
  let cSig := params.toList.zipIdx.map fun (p, i) =>
    let nm := p.name.getD s!"_arg{i}"
    if i = layout.outerUserDataIdx then s!"void *{nm}"
    else CType.declarator p.type nm
  let userDataName := (params[layout.outerUserDataIdx]?.bind (·.name)).getD s!"_arg{layout.outerUserDataIdx}"
  -- C return type.
  let cRetTy := if isVoid then "void" else CType.declarator ret ""
  -- Marshal each visible param from C to Lean.
  let mut marshalLines : Array String := #[]
  let mut leanArgs : Array String := #[]
  for h_i : i in [:params.size] do
    -- Skip hidden params.
    if i = layout.outerUserDataIdx then continue
    if nestedCBUDIdxs.contains i then continue
    if arraySizeIdxs.contains i then continue
    let p := params[i]
    let nm := p.name.getD s!"_arg{i}"
    let leanArgVar := s!"_lean_{i}"
    if nestedCBIdxs.contains i then
      -- Nested callback: wrap the C fn ptr + its paired void* into a
      -- Lean closure pointing at the reverse trampoline.
      let cbTypedefName := match p.type with
        | .typedef name => name | _ => "unknown"
      let udIdx := (layout.nestedCallbacks.find? (·.1 = i)).map (·.2) |>.getD (i + 1)
      let udNm := (params[udIdx]?.bind (·.name)).getD s!"_arg{udIdx}"
      -- Compute the inner callback's visible param count for the
      -- reverse trampoline arity (captured 2 + visible + 1 IO world).
      let (_, innerParams, _) ← callbackSignature h cbTypedefName
      let innerLayout ← analyzeCallbackLayout innerParams anno
      let innerVisibleCount := innerParams.size - innerLayout.hiddenIndices.length
      let totalArity := 2 + innerVisibleCount + 1
      let reverseName := reverseCallbackTrampolineName cbTypedefName
      marshalLines := marshalLines ++ #[
        s!"  lean_object* _inner_fn_{i} = lean_alloc_external(get_callback_wrapper_class(), (void *){nm});",
        s!"  lean_object* _inner_ud_{i} = lean_alloc_external(get_callback_wrapper_class(), {udNm});",
        s!"  lean_object* {leanArgVar} = lean_alloc_closure((void *)&{reverseName}, {totalArity}, 2);",
        s!"  lean_closure_set({leanArgVar}, 0, _inner_fn_{i});",
        s!"  lean_closure_set({leanArgVar}, 1, _inner_ud_{i});"
      ]
      leanArgs := leanArgs.push leanArgVar
    else if arrayDataIdxs.contains i then
      -- Array param: build Lean Array from C buffer + size.
      let sizeIdx := (layout.arrayPairs.find? (·.1 = i)).map (·.2) |>.getD (i + 1)
      let sizeNm := (params[sizeIdx]?.bind (·.name)).getD s!"_arg{sizeIdx}"
      let elemCTy := match p.type with
        | .pointer (.const t) | .pointer t => t | t => t
      let elemRes ← resolveType anno elemCTy
      let boxExpr := marshalCToLean elemRes s!"_arr_elem"
      marshalLines := marshalLines ++ #[
        s!"  lean_object* {leanArgVar} = lean_mk_empty_array_with_capacity(lean_usize_to_nat({sizeNm}));",
        s!"  for (size_t _ai = 0; _ai < {sizeNm}; _ai++) \{",
        s!"    {elemRes.cLocalType} _arr_elem = {nm}[_ai];",
        s!"    {leanArgVar} = lean_array_push({leanArgVar}, {boxExpr});",
        s!"  }"
      ]
      leanArgs := leanArgs.push leanArgVar
    else
      -- Regular param: simple C→Lean marshal.
      -- If the C param is a pointer to struct/tagged-union, we need to
      -- dereference it since the toLean helper takes by value.
      let r ← resolveType anno p.type
      let cExpr := if r.isPtrToStruct then s!"*{nm}" else nm
      marshalLines := marshalLines.push
        s!"  lean_object* {leanArgVar} = {marshalCToLean r cExpr};"
      leanArgs := leanArgs.push leanArgVar
  let arity := leanArgs.size + 1  -- +1 for the IO world
  if arity > 16 then
    .error s!"callback `{ta.cName}` has arity {arity} (max 16 for lean_apply)"
  let applyArgs := leanArgs.toList ++ ["lean_io_mk_world()"]
  -- Return handling.
  let returnBlock :=
    if isVoid then
      s!"  lean_dec(_result);\n"
    else
      let unbox := marshalLeanToC retRes "_ret_val"
      s!"  lean_object* _ret_val = lean_io_result_get_value(_result);\n" ++
      s!"  {retRes.cLocalType} _ret = {unbox};\n" ++
      s!"  lean_dec(_result);\n" ++
      s!"  return ({cRetTy.trimAscii.toString}){if retRes.leanType == "Bool" then "_ret ? 1 : 0" else "_ret"};\n"
  let body :=
    s!"{cRetTy.trimAscii.toString} {trampolineName}({", ".intercalate cSig}) \{\n" ++
    s!"  lean_object* closure = (lean_object*){userDataName};\n" ++
    "\n".intercalate marshalLines.toList ++
    (if marshalLines.isEmpty then "" else "\n") ++
    s!"  lean_inc(closure);\n" ++
    s!"  lean_object* _result = lean_apply_{arity}(closure, {", ".intercalate applyArgs});\n" ++
    returnBlock ++
    "}"
  return .raw body

/-- Emit a reverse trampoline for a callback typedef used as a nested
callback (Lean→C). When Lean invokes an inner callback closure, this
function extracts the captured C function pointer and user-data,
unmarshals each Lean argument back to C, calls the C function pointer,
and marshals the return to Lean.

Returns `none` if this callback typedef is never used as a nested
callback (i.e., never appears as a param in another callback). -/
private def emitReverseTrampoline
    (h : HeaderIndex) (anno : Std.HashMap String TypeAnno) (ta : TypeAnno)
    : Except String Gen.CTopLevel := do
  let (ret, params, _) ← callbackSignature h ta.cName
  let layout ← analyzeCallbackLayout params anno
  let arrayDataIdxs := layout.arrayPairs.toList.map Prod.fst
  let arraySizeIdxs := layout.arrayPairs.toList.map Prod.snd
  let retRes ← resolveType anno ret
  let isVoid := match ret with | .void => true | _ => false
  let reverseName := reverseCallbackTrampolineName ta.cName
  -- The reverse trampoline's Lean-side params: captured (fn_ptr, user_data)
  -- plus one lean_object* per visible param plus IO world.
  let mut visibleCount := 0
  for h_i : i in [:params.size] do
    if !layout.hiddenIndices.contains i then
      visibleCount := visibleCount + 1
  -- Build the C function signature.
  let mut sigParams : Array String := #[
    "lean_object *_fn_ptr_obj",
    "lean_object *_user_data_obj"
  ]
  for h_i : i in [:params.size] do
    if layout.hiddenIndices.contains i then continue
    sigParams := sigParams.push s!"lean_object *_arg{i}"
  sigParams := sigParams.push "lean_object *_world"
  -- Body: extract fn_ptr and user_data.
  let mut bodyLines : Array String := #[
    s!"  {ta.cName} _fn_ptr = ({ta.cName})lean_get_external_data(_fn_ptr_obj);",
    s!"  void *_user_data = lean_get_external_data(_user_data_obj);"
  ]
  -- Unmarshal each visible param and build the C call args in original order.
  -- Also track cleanup lines for array buffers.
  let mut callArgByIdx : Std.HashMap Nat String := {}
  let mut cleanupLines : Array String := #[]
  -- Set user-data in call args.
  callArgByIdx := callArgByIdx.insert layout.outerUserDataIdx "_user_data"
  for h_i : i in [:params.size] do
    if layout.hiddenIndices.contains i then continue
    if arrayDataIdxs.contains i then
      -- Array param: unmarshal Lean Array to (buf, size).
      let sizeIdx := (layout.arrayPairs.find? (·.1 = i)).map (·.2) |>.getD (i + 1)
      let p := params[i]
      let elemCTy := match p.type with
        | .pointer (.const t) | .pointer t => t | t => t
      let elemRes ← resolveType anno elemCTy
      let unboxExpr := marshalLeanToC elemRes s!"_arr_obj_{i}->m_data[_ai]"
      bodyLines := bodyLines ++ #[
        s!"  lean_array_object *_arr_obj_{i} = lean_to_array(_arg{i});",
        s!"  size_t _arr_sz_{i} = _arr_obj_{i}->m_size;",
        s!"  if (_arr_sz_{i} > SIZE_MAX / sizeof({elemRes.cLocalType})) lean_internal_panic(\"lean-bindgen: array size overflow\");",
        s!"  {elemRes.cLocalType} *_arr_buf_{i} = ({elemRes.cLocalType} *)malloc(sizeof({elemRes.cLocalType}) * (_arr_sz_{i} > 0 ? _arr_sz_{i} : 1));",
        s!"  for (size_t _ai = 0; _ai < _arr_sz_{i}; _ai++) \{",
        s!"    _arr_buf_{i}[_ai] = {unboxExpr};",
        s!"  }"
      ]
      callArgByIdx := callArgByIdx.insert i s!"_arr_buf_{i}"
      callArgByIdx := callArgByIdx.insert sizeIdx s!"_arr_sz_{i}"
      cleanupLines := cleanupLines.push s!"  free(_arr_buf_{i});"
    else
      -- Regular param: unmarshal.
      let r ← resolveType anno params[i].type
      let cExpr := marshalLeanToC r s!"_arg{i}"
      bodyLines := bodyLines.push s!"  {r.cLocalType} _c_{i} = {cExpr};"
      callArgByIdx := callArgByIdx.insert i s!"_c_{i}"
  -- Build the C call in original param order.
  let mut callArgs : Array String := #[]
  for h_i : i in [:params.size] do
    match callArgByIdx[i]? with
    | some arg => callArgs := callArgs.push arg
    | none     => pure ()  -- should not happen
  let callExpr := s!"_fn_ptr({", ".intercalate callArgs.toList})"
  -- Call and return.
  if isVoid then
    bodyLines := bodyLines.push s!"  {callExpr};"
    bodyLines := bodyLines ++ cleanupLines
    bodyLines := bodyLines.push s!"  return lean_io_result_mk_ok(lean_box(0));"
  else
    bodyLines := bodyLines.push s!"  {retRes.cLocalType} _ret = {callExpr};"
    bodyLines := bodyLines ++ cleanupLines
    let boxed := marshalCToLean retRes "_ret"
    bodyLines := bodyLines.push s!"  return lean_io_result_mk_ok({boxed});"
  return .raw (s!"LEAN_EXPORT lean_obj_res {reverseName}({", ".intercalate sigParams.toList}) \{\n" ++
         "\n".intercalate bodyLines.toList ++ "\n}")

/-- Check whether a callback typedef is referenced as a nested callback
inside any other callback typedef. -/
private def isNestedCallback
    (h : HeaderIndex) (anno : Std.HashMap String TypeAnno)
    (ta : TypeAnno) : Bool :=
  -- Check if any other callback's params reference this typedef.
  -- We look for params of type `.typedef ta.cName` in other callbacks.
  let found := anno.toList.any fun (_, other) =>
    match other.mapping with
    | .callback =>
      if other.cName == ta.cName then false else
      match lookupTypedefBody h other.cName with
      | some (.pointer (.function _ params _)) =>
        params.any fun p => match p.type with
          | .typedef name => name == ta.cName
          | _ => false
      | _ => false
    | _ => false
  found

/-- Emit toLean / toC helpers for a bitfield struct mapping.
The toLean function unpacks individual bits into a Lean struct of Bools;
the toC function packs them back. -/
private def emitBitfieldHelpers
    (h : HeaderIndex) (ta : TypeAnno) (bm : BitfieldMapping)
    : Except String (Array Gen.CTopLevel) := do
  let (toC, toLean) := structHelperNames ta.lean
  let numFields := bm.fields.size
  -- Look up the enum to resolve mask values.
  let enumVariants ← match lookupEnumTag h bm.enumTag with
    | some v => .ok v
    | none   => .error s!"bitfield enum `{bm.enumTag}` not found in header"
  let resolveVal (cConst : String) : Except String String := do
    match enumVariants.find? (·.1 == cConst) with
    | some (_, some v) => .ok (toString v)
    | some (_, none)   => .ok cConst
    | none             => .error s!"bitfield constant `{cConst}` not found in enum `{bm.enumTag}`"
  -- toLean: C unsigned → Lean struct
  let mut toLeanStmts : Array Gen.CStmt := #[
    .varDecl "lean_object *" "obj" (some (.call "lean_alloc_ctor" #[.intLit 0, .intLit 0, .intLit numFields])),
    .varDecl "uint8_t *" "sp" (some (.call "lean_ctor_scalar_cptr" #[.var "obj"]))
  ]
  for (cConst, _) in bm.fields, i in List.range numFields do
    let mask ← resolveVal cConst
    toLeanStmts := toLeanStmts.push
      (.assign (.subscript (.var "sp") (.intLit i))
               (.binop "!=" (.paren (.binop "&" (.var "v") (.raw mask))) (.intLit 0)))
  toLeanStmts := toLeanStmts.push (.ret (some (.var "obj")))
  let toLeanDef : Gen.CTopLevel := .funcDef .plain "lean_object*" toLean
    #[⟨ta.cName, "v"⟩] toLeanStmts
  -- toC: Lean struct → C unsigned
  let mut toCStmts : Array Gen.CStmt := #[
    .varDecl "uint8_t *" "sp" (some (.call "lean_ctor_scalar_cptr" #[.var "obj"])),
    .varDecl ta.cName "v" (some (.intLit 0))
  ]
  for (cConst, _) in bm.fields, i in List.range numFields do
    let mask ← resolveVal cConst
    toCStmts := toCStmts.push
      (.ifElse (.subscript (.var "sp") (.intLit i))
               #[.assignOp "|" (.var "v") (.raw mask)] #[])
  toCStmts := toCStmts.push (.ret (some (.var "v")))
  let toCDef : Gen.CTopLevel := .funcDef .plain ta.cName toC
    #[⟨"b_lean_obj_arg", "obj"⟩] toCStmts
  .ok #[toLeanDef, toCDef]

/-- Emit the per-opaque-type external-class boilerplate: a static
class pointer, a lazy getter, the finalizer wrapper, and the foreach
no-op. -/
private def emitOpaqueClass (ta : TypeAnno) (finalizer : String) : Array Gen.CTopLevel :=
  let getter := externalClassGetter ta.lean
  let cTypedef := ta.cName
  let snake := ta.lean.foldl (init := "") fun acc c =>
    if c.isUpper && acc ≠ "" then acc ++ "_" ++ String.singleton c.toLower
    else acc ++ String.singleton c.toLower
  let finalizerWrapper := s!"finalize_{snake}"
  let classGlobal := s!"g_{snake}_class"
  let finalizerBody : Gen.CStmt :=
    if finalizer == "" then .raw "(void)ptr;"
    else .expr (.call finalizer #[.cast s!"{cTypedef} *" (.var "ptr")])
  let finalizerFn : Gen.CTopLevel := .funcDef .static_ "void" finalizerWrapper
    #[⟨"void *", "ptr"⟩] #[finalizerBody]
  let globalDef : Gen.CTopLevel :=
    .globalVar "static lean_external_class *" classGlobal (some "NULL")
  let onceName := s!"g_{snake}_once"
  let initName := s!"init_{snake}_class"
  let onceDef : Gen.CTopLevel :=
    .raw s!"static pthread_once_t {onceName} = PTHREAD_ONCE_INIT;"
  let initFn : Gen.CTopLevel := .funcDef .static_ "void" initName
    #[] #[
      .assign (.var classGlobal) (.call "lean_register_external_class"
        #[.raw s!"&{finalizerWrapper}", .raw "&noop_foreach"])
    ]
  -- Class getter is *not* `static` so hand-written helpers in
  -- separate translation units can box raw C pointers as the same
  -- Lean opaque type.
  let getterFn : Gen.CTopLevel := .funcDef .plain "lean_external_class *" getter
    #[] #[
      .expr (.call "pthread_once" #[.raw s!"&{onceName}", .var initName]),
      .ret (some (.var classGlobal))
    ]
  #[finalizerFn, globalDef, onceDef, initFn, getterFn]

/-- Emit a `free_<type>` function that recursively frees malloc'd
nested payloads of a C struct. Takes a pointer to the struct to free
(so it can be called on both stack locals via `&v` and heap-allocated
via the pointer). Does NOT free the struct itself — only its deep
fields. -/
private def emitFreeHelper
    (h : HeaderIndex) (anno : Std.HashMap String TypeAnno)
    (ta : TypeAnno) (sm : StructMapping)
    : Except String Gen.CTopLevel := do
  let fields ← resolveStructFields h anno sm
  let freeName := freeHelperName ta.lean
  let cTypedef := ta.cName
  let mut stmts : Array Gen.CStmt := #[]
  for (cField, _, r) in fields do
    match r.paramMarshal with
    | .array cElemTy elem =>
      let sizeField := (sm.arrayFields.find? (·.1 = cField)).map (·.2) |>.getD "size"
      let elemFreeLoop := match elem with
        | .structHelper toC _ =>
          let snake := toC.drop 8
          let freeFn := s!"free_{snake}"
          #[Gen.CStmt.forLoop "size_t _i = 0" s!"_i < p->{sizeField}" "_i++"
            #[.expr (.call freeFn #[.cast s!"{cElemTy} *" (.raw s!"&p->{cField}[_i]")])]]
        | _ => #[]
      stmts := stmts.push (.ifElse (.arrow (.var "p") cField)
        (elemFreeLoop ++ #[.expr (.call "free" #[.cast "void *" (.arrow (.var "p") cField)])]) #[])
    | .fromLeanStruct _ cTy true =>
      match r.freeHelperFn with
      | some nestedFree =>
        stmts := stmts.push (.ifElse (.arrow (.var "p") cField) #[
          .expr (.call nestedFree #[.cast s!"{cTy} *" (.arrow (.var "p") cField)]),
          .expr (.call "free" #[.cast "void *" (.arrow (.var "p") cField)])
        ] #[])
      | none =>
        stmts := stmts.push (.ifElse (.arrow (.var "p") cField)
          #[.expr (.call "free" #[.cast "void *" (.arrow (.var "p") cField)])] #[])
    | .fromLeanStruct _ _ false =>
      match r.freeHelperFn with
      | some nestedFree =>
        stmts := stmts.push (.expr (.call nestedFree #[.addrOf (.arrow (.var "p") cField)]))
      | none => pure ()
    | .leanString =>
      stmts := stmts.push (.ifElse (.arrow (.var "p") cField)
        #[.expr (.call "free" #[.cast "void *" (.arrow (.var "p") cField)])] #[])
    | _ => pure ()
  return .funcDef .static_ "void" freeName #[⟨s!"{cTypedef} *", "p"⟩] stmts

/-- Emit a `free_<type>` function for a tagged union. Switches on the
tag, frees variant-specific payloads, then frees shared fields. -/
private def emitTaggedUnionFreeHelper
    (h : HeaderIndex) (anno : Std.HashMap String TypeAnno)
    (ta : TypeAnno) (tu : TaggedUnionMapping)
    : Except String Gen.CTopLevel := do
  let some headerFields := lookupStructTag h tu.cStructTag
    | .error s!"struct tag `{tu.cStructTag}` not found in header"
  let freeName := freeHelperName ta.lean
  let cTypedef := ta.cName
  let mut stmts : Array Gen.CStmt := #[]
  -- Free variant-specific payloads via switch.
  let mut switchCases : Array (Gen.CExpr × Array Gen.CStmt) := #[]
  for v in tu.variants do
    let some hf := lookupFieldInStruct h headerFields v.unionField
      | continue
    let payloadTy := match hf.type with
      | .pointer (.const t) | .pointer t | .const t => t
      | t => t
    let r ← resolveType anno payloadTy
    let isPtr := match hf.type with | .pointer _ => true | _ => false
    match r.freeHelperFn with
    | some nestedFree =>
      let body := if isPtr then #[
        .ifElse (.arrow (.var "p") v.unionField) #[
          .expr (.call nestedFree #[.cast s!"{r.cLocalType} *" (.arrow (.var "p") v.unionField)]),
          .expr (.call "free" #[.cast "void *" (.arrow (.var "p") v.unionField)])
        ] #[],
        .raw "break;"
      ] else #[
        .expr (.call nestedFree #[.addrOf (.arrow (.var "p") v.unionField)]),
        .raw "break;"
      ]
      switchCases := switchCases.push (.var v.cTag, body)
    | none => pure ()
  if !switchCases.isEmpty then
    stmts := stmts.push (.switch (.arrow (.var "p") tu.tagField) switchCases
      #[.raw "break;"])
  -- Free shared fields.
  for (cField, _) in tu.sharedFields do
    let some hf := lookupFieldInStruct h headerFields cField | continue
    let r ← resolveType anno hf.type
    match r.paramMarshal with
    | .fromLeanStruct _ cTy true =>
      match r.freeHelperFn with
      | some nestedFree =>
        stmts := stmts.push (.ifElse (.arrow (.var "p") cField) #[
          .expr (.call nestedFree #[.cast s!"{cTy} *" (.arrow (.var "p") cField)]),
          .expr (.call "free" #[.cast "void *" (.arrow (.var "p") cField)])
        ] #[])
      | none =>
        stmts := stmts.push (.ifElse (.arrow (.var "p") cField)
          #[.expr (.call "free" #[.cast "void *" (.arrow (.var "p") cField)])] #[])
    | .fromLeanStruct _ _ false =>
      match r.freeHelperFn with
      | some nestedFree =>
        stmts := stmts.push (.expr (.call nestedFree #[.addrOf (.arrow (.var "p") cField)]))
      | none => pure ()
    | _ => pure ()
  return .funcDef .static_ "void" freeName #[⟨s!"{cTypedef} *", "p"⟩] stmts

/-- Emit the trampoline for an event callback typedef. Generates a
C function with switch-dispatch on the discriminant enum, marshalling
`void *event` differently per variant. -/
private def emitEventCallbackTrampoline
    (h : HeaderIndex) (anno : Std.HashMap String TypeAnno) (ta : TypeAnno) (ec : EventCallbackMapping)
    : Except String Gen.CTopLevel := do
  -- Look up the C signature of the callback typedef.
  let some body := lookupTypedefBody h ta.cName
    | .error s!"event callback typedef `{ta.cName}` not found in header"
  let .pointer (.function ret params _variadic) := body
    | .error s!"event callback typedef `{ta.cName}` is not a function-pointer"
  let trampolineName := callbackTrampolineName ta.cName
  -- Build C param signature.
  let cSig := params.toList.zipIdx.map fun (p, i) =>
    let nm := p.name.getD s!"_arg{i}"
    CType.declarator p.type nm
  let retTy := CType.declarator ret ""
  -- Param names.
  let discrimName := (params[ec.discriminantIdx]?.bind (·.name)).getD s!"_arg{ec.discriminantIdx}"
  let eventName := (params[ec.eventIdx]?.bind (·.name)).getD s!"_arg{ec.eventIdx}"
  let userData := (params[ec.userDataIdx]?.bind (·.name)).getD s!"_arg{ec.userDataIdx}"
  let outParamNames := ec.outParams.map fun idx =>
    (params[idx]?.bind (·.name)).getD s!"_arg{idx}"
  -- Build per-variant switch cases.
  let mut switchCases : Array String := #[]
  for (v, varIdx) in ec.variants.toList.zipIdx do
    let mut caseLines : Array String := #[]
    match v.interpretation with
    | .opaquePtr leanTy nullable =>
      -- Find the opaque class getter for this Lean type.
      let getter := externalClassGetter leanTy
      if nullable then
        caseLines := caseLines ++ #[
          s!"      if ({eventName} == NULL) \{",
          s!"        eventObj = lean_alloc_ctor({varIdx}, 1, 0);",
          s!"        lean_ctor_set(eventObj, 0, lean_box(0));",
          s!"      } else \{",
          s!"        lean_object* inner = lean_alloc_external({getter}(), {eventName});",
          s!"        lean_object* some = lean_alloc_ctor(1, 1, 0);",
          s!"        lean_ctor_set(some, 0, inner);",
          s!"        eventObj = lean_alloc_ctor({varIdx}, 1, 0);",
          s!"        lean_ctor_set(eventObj, 0, some);",
          s!"      }"
        ]
      else
        caseLines := caseLines ++ #[
          s!"      lean_object* val = lean_alloc_external({getter}(), {eventName});",
          s!"      eventObj = lean_alloc_ctor({varIdx}, 1, 0);",
          s!"      lean_ctor_set(eventObj, 0, val);"
        ]
    | .derefMapped leanTy =>
      -- Find the toLean helper for this mapped type.
      let (_, toLeanFn) := structHelperNames leanTy
      -- Look up the C typedef name from annotation.
      let cTypedefName := anno.toList.findSome? fun (cN, a) =>
        if a.lean == leanTy then some cN else none
      let cTy := cTypedefName.getD leanTy
      caseLines := caseLines ++ #[
        s!"      {cTy} *_deref_ptr = ({cTy} *){eventName};",
        s!"      lean_object* val = {toLeanFn}(*_deref_ptr);",
        s!"      eventObj = lean_alloc_ctor({varIdx}, 1, 0);",
        s!"      lean_ctor_set(eventObj, 0, val);"
      ]
    | .ptrArray leanTy count =>
      let getter := externalClassGetter leanTy
      -- Cast to T**, index each element.
      let cTypedefName := anno.toList.findSome? fun (cN, a) =>
        if a.lean == leanTy then some cN else none
      let cTy := cTypedefName.getD leanTy
      caseLines := caseLines.push s!"      {cTy} **_ptrs = ({cTy} **){eventName};"
      for i in List.range count do
        caseLines := caseLines.push
          s!"      lean_object* _ev{i} = lean_alloc_external({getter}(), (void *)_ptrs[{i}]);"
      caseLines := caseLines.push s!"      eventObj = lean_alloc_ctor({varIdx}, {count}, 0);"
      for i in List.range count do
        caseLines := caseLines.push s!"      lean_ctor_set(eventObj, {i}, _ev{i});"
    let caseBody := "\n".intercalate caseLines.toList
    -- Use numeric value if we can resolve it from the header; otherwise
    -- fall back to the symbolic constant (works when compiling against
    -- the same header the annotation was written for).
    let caseLabel := match lookupEnumConstant h v.cEnumValue with
      | some n => s!"{n}"
      | none   => v.cEnumValue
    switchCases := switchCases.push
      (s!"    case {caseLabel}: \{\n{caseBody}\n      break;\n    }")
  let switchBody := "\n".intercalate switchCases.toList
  -- Build the return handling (reads out-params from the Lean closure result).
  let mut outParamWrites : Array String := #[]
  for ((outName, _idx), opIdx) in (outParamNames.toList.zip ec.outParams.toList).zipIdx do
    let outCTy := if h : opIdx < ec.outParamTypes.size then ec.outParamTypes[opIdx] else "bool"
    let unboxExpr := match outCTy with
      | "bool" => "lean_unbox(lean_io_result_get_value(_result)) ? 1 : 0"
      | "int"  => "(int)lean_unbox_uint32(lean_io_result_get_value(_result))"
      | _      => s!"({outCTy})lean_unbox(lean_io_result_get_value(_result))"
    outParamWrites := outParamWrites.push s!"    *{outName} = {unboxExpr};"
  let outParamBlock := "\n".intercalate outParamWrites.toList
  let successRet := ec.successReturnValue
  let isVoid := match ret with | .void => true | _ => false
  let returnBlock := if isVoid then
    s!"  lean_dec(_result);\n"
  else if ec.outParams.isEmpty then
    s!"  lean_dec(_result);\n  return {successRet};\n"
  else
    s!"  if (lean_io_result_is_ok(_result)) \{\n{outParamBlock}\n  } else \{\n" ++
    (outParamNames.toList.map (s!"    *{·} = 0;") |> "\n".intercalate) ++
    s!"\n  }\n  lean_dec(_result);\n  return {successRet};\n"
  let body :=
    s!"{retTy.trimAscii.toString} {trampolineName}({", ".intercalate cSig}) \{\n" ++
    s!"  lean_object *closure = (lean_object *){userData};\n" ++
    s!"  lean_object *eventObj;\n" ++
    s!"  switch ({discrimName}) \{\n" ++ switchBody ++ "\n" ++
    -- For unknown event types (e.g. clingo 5.8 unsat events), skip the
    -- callback entirely: set out-params to defaults and return success.
    s!"    default:\n" ++
    (outParamNames.toList.map (s!"      *{·} = 1;") |> "\n".intercalate) ++
    (if outParamNames.isEmpty then "" else "\n") ++
    s!"      return {ec.successReturnValue};\n" ++
    s!"  }\n" ++
    s!"  lean_inc(closure);\n" ++
    s!"  lean_object *_result = lean_apply_2(closure, eventObj, lean_io_mk_world());\n" ++
    returnBlock ++
    "}"
  return .raw body

/-- Generate the entire shim source text. -/
def emitShim (b : Bindings) (hdr : CHeader) : Except String String := do
  let h := buildHeaderIndex hdr
  let typeAnnoMap : Std.HashMap String TypeAnno :=
    b.types.foldl (fun m a => m.insert a.cName a) ({} : Std.HashMap _ _)
  let declMap : Std.HashMap String CDecl :=
    hdr.decls.foldl (fun m d =>
      match d with
      | .function name .. => m.insert name d
      | .typedef name _   => m.insert name d
      | _ => m) ({} : Std.HashMap _ _)
  -- Per-type setup: enum helpers, opaque-class boilerplate, struct
  -- to/from converters, callback trampolines, reverse trampolines.
  let mut forwardDecls    : Array Gen.CTopLevel := #[]
  let mut enumHelpers     : Array Gen.CTopLevel := #[]
  let mut opaqueHelpers   : Array Gen.CTopLevel := #[]
  let mut freeHelpers     : Array Gen.CTopLevel := #[]
  let mut structHelpers   : Array Gen.CTopLevel := #[]
  let mut reverseCallbacks : Array Gen.CTopLevel := #[]
  let mut callbacks       : Array Gen.CTopLevel := #[]
  let mut hasNestedCallbacks := false
  for ta in b.types do
    match ta.mapping with
    | .inductiveEnum em =>
      let tops ← emitEnumHelpers h ta em
      enumHelpers := enumHelpers ++ tops
    | .opaquePointer fin =>
      opaqueHelpers := opaqueHelpers ++ emitOpaqueClass ta fin
    | .structRecord sm =>
      let (toC, toLean) := structHelperNames ta.lean
      let freeFn := freeHelperName ta.lean
      forwardDecls := forwardDecls ++ #[
        .forwardDecl s!"static void {freeFn}({ta.cName} *p)",
        .forwardDecl s!"lean_object* {toLean}({ta.cName} v)",
        .forwardDecl s!"{ta.cName} {toC}(b_lean_obj_arg obj)"
      ]
      freeHelpers := freeHelpers.push (← emitFreeHelper h typeAnnoMap ta sm)
      structHelpers := structHelpers ++ (← emitStructHelpers h typeAnnoMap ta sm)
    | .callback =>
      let cbTop ← emitCallbackTrampoline h typeAnnoMap ta
      callbacks := callbacks.push cbTop
      if isNestedCallback h typeAnnoMap ta then
        hasNestedCallbacks := true
        reverseCallbacks := reverseCallbacks.push (← emitReverseTrampoline h typeAnnoMap ta)
        let reverseName := reverseCallbackTrampolineName ta.cName
        let (_, innerParams, _) ← callbackSignature h ta.cName
        let innerLayout ← analyzeCallbackLayout innerParams typeAnnoMap
        let mut sigParts : Array String := #[
          "lean_object *", "lean_object *"
        ]
        for h_i : i in [:innerParams.size] do
          if !innerLayout.hiddenIndices.contains i then
            sigParts := sigParts.push "lean_object *"
        sigParts := sigParts.push "lean_object *"
        forwardDecls := forwardDecls.push
          (.forwardDecl s!"LEAN_EXPORT lean_obj_res {reverseName}({", ".intercalate sigParts.toList})")
    | .taggedUnion tu =>
      let (toC, toLean) := structHelperNames ta.lean
      let freeFn := freeHelperName ta.lean
      forwardDecls := forwardDecls ++ #[
        .forwardDecl s!"static void {freeFn}({ta.cName} *p)",
        .forwardDecl s!"lean_object* {toLean}({ta.cName} v)",
        .forwardDecl s!"{ta.cName} {toC}(b_lean_obj_arg obj)"
      ]
      freeHelpers := freeHelpers.push (← emitTaggedUnionFreeHelper h typeAnnoMap ta tu)
      structHelpers := structHelpers.push (.raw (← emitTaggedUnionHelpers h typeAnnoMap ta tu))
    | .bitfieldStruct bm =>
      let (toC, toLean) := structHelperNames ta.lean
      forwardDecls := forwardDecls ++ #[
        .forwardDecl s!"lean_object* {toLean}({ta.cName} v)",
        .forwardDecl s!"{ta.cName} {toC}(b_lean_obj_arg obj)"
      ]
      structHelpers := structHelpers ++ (← emitBitfieldHelpers h ta bm)
    | .eventCallback ec =>
      let cbTop ← emitEventCallbackTrampoline h typeAnnoMap ta ec
      callbacks := callbacks.push cbTop
    | _ => pure ()
  let hasCallbacks := !callbacks.isEmpty
  let needsForeach := !opaqueHelpers.isEmpty || hasCallbacks
  let hasTaggedUnion := b.types.any (fun ta => match ta.mapping with | .taggedUnion _ => true | _ => false)
  let hasStructOrTU := b.types.any (fun ta => match ta.mapping with
    | .structRecord _ | .taggedUnion _ => true | _ => false)
  let hasByteArrayOut := b.functions.any (fun fa => match fa.style with
    | .byteArrayOutBoolStatus _ _ _ => true | _ => false)
  let needsStdlib := hasStructOrTU || hasNestedCallbacks || hasByteArrayOut
  -- Assemble the CShimFile in order.
  let mut file : Gen.CShimFile := #[]
  file := file.push (.comment "Auto-generated by lean-bindgen. Do not edit.")
  file := file.push (.include "lean/lean.h" false)
  if !opaqueHelpers.isEmpty || hasNestedCallbacks then
    file := file.push (.include "pthread.h")
  if needsStdlib then file := file.push (.include "stdlib.h")
  if hasTaggedUnion || hasStructOrTU || hasByteArrayOut then file := file.push (.include "string.h")
  file := file.push (.include (b.headerPath.splitOn "/" |>.getLast!) false)
  if needsForeach then
    file := file.push (.raw "static void noop_foreach(void *mod, b_lean_obj_arg fn) { (void)mod; (void)fn; }")
  if hasNestedCallbacks then
    file := file ++ #[
      .raw "static void noop_finalize(void *p) { (void)p; }",
      .raw "static lean_external_class *g_callback_wrapper_class = NULL;",
      .raw "static pthread_once_t g_callback_wrapper_once = PTHREAD_ONCE_INIT;",
      .raw ("static void init_callback_wrapper_class(void) {\n" ++
            "  g_callback_wrapper_class = lean_register_external_class(&noop_finalize, &noop_foreach);\n" ++
            "}"),
      .raw ("lean_external_class *get_callback_wrapper_class() {\n" ++
            "  pthread_once(&g_callback_wrapper_once, init_callback_wrapper_class);\n" ++
            "  return g_callback_wrapper_class;\n}")
    ]
  if !forwardDecls.isEmpty then
    file := file.push (.comment "Forward declarations for recursive helpers.")
    file := file ++ forwardDecls
  file := file ++ enumHelpers ++ opaqueHelpers ++ freeHelpers ++ structHelpers
               ++ reverseCallbacks ++ callbacks
  for fa in b.functions do
    file := file.push (← emitShimFunction b typeAnnoMap declMap fa)
  return Gen.CShimFile.render file ++ "\n"

end Codegen
end LeanBindgen
