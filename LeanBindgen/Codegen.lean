import LeanBindgen.Annotation
import LeanBindgen.C.Ast
import LeanBindgen.C.Pretty
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
  | "float32" => s!"lean_box_float({val})"
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

private def lookupEnumTag (h : CHeader) (tag : String) : Option (Array (String × Option Int)) :=
  h.decls.findSome? fun
    | .enumDef (some t) variants => if t = tag then some variants else none
    | _ => none

private def lookupStructTag (h : CHeader) (tag : String) : Option (Array CField) :=
  h.decls.findSome? fun
    | .structDef (some t) fields => if t = tag then some fields else none
    | _ => none

/-- Look up anonymous union members. When a struct has an anonymous union
field (name="", type=unionRef ""), its members are accessible by name
in C. This helper searches both direct struct fields and anonymous
union members. -/
private def lookupFieldInStruct (h : CHeader) (fields : Array CField) (name : String)
    : Option CField :=
  -- First try direct lookup.
  fields.find? (·.name = name) |>.orElse fun _ =>
    -- Search anonymous unions: fields with name="" and type unionRef "".
    fields.findSome? fun f =>
      if f.name = "" then
        match f.type with
        | .unionRef "" =>
          -- Find the anonymous union decl (most recent unionDef with tag=none).
          h.decls.findSome? fun
            | .unionDef none uFields => uFields.find? (·.name = name)
            | _ => none
        | _ => none
      else none

/-- Look up the body type of a typedef. -/
private def lookupTypedefBody (h : CHeader) (name : String) : Option CType :=
  h.decls.findSome? fun
    | .typedef n ty => if n = name then some ty else none
    | _ => none

private def resolveStructFields
    (h : CHeader) (anno : Std.HashMap String TypeAnno) (sm : StructMapping)
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
private def callbackSignature (h : CHeader) (typedefName : String)
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
    (h : CHeader) (anno : Std.HashMap String TypeAnno) (typedefName : String)
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
  -- size slots. These are invisible in the Lean signature.
  let arraySizeIndices := fa.arrayPairs.map Prod.snd
  let hiddenIndices := fa.callbackUserDataParams ++ arraySizeIndices
  -- Resolve parameter types. Hidden params get placeholders. Array
  -- data-params get resolved as `Array T`.
  let arrayDataIndices := fa.arrayPairs.map Prod.fst
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
  | .callerAllocates _sizeFn bufIdx sizeIdx error =>
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
        let resultTy :=
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
    : Except String String := do
  let some decl := declMap[fa.cName]?
    | .error s!"function `{fa.cName}` not found in header"
  let (sig, _, _, _) ← buildFunctionSignature anno decl fa
  let externSym := externSymbolOf fa
  let leanShortName := fa.lean.splitOn "." |>.getLast!
  return s!"@[extern \"{externSym}\"]\nopaque {leanShortName} : {sig}"

/-- Emit a single type declaration. The `fieldTypes` map provides the
already-resolved Lean type expression for each (struct's) Lean field
name; only used by the `.structRecord` arm. -/
private def emitTypeDecl
    (ta : TypeAnno) (fieldTypes : Std.HashMap String String := {}) : String :=
  match ta.mapping with
  | .scalarNewtype k =>
    s!"def {ta.lean} := {k.toLean}\n  deriving Repr, Inhabited"
  | .opaquePointer _ =>
    s!"opaque {ta.lean} : Type"
  | .inductiveEnum em =>
    let ctors := "\n".intercalate
      (em.variants.toList.map (fun (_, leanV) => s!"  | {leanV}"))
    s!"inductive {ta.lean} where\n{ctors}\n  deriving Repr, Inhabited, BEq"
  | .structRecord sm =>
    let fields := "\n".intercalate
      (sm.fields.toList.map (fun (_, leanF) =>
        let ty := fieldTypes[leanF]?.getD "Unit"
        s!"  {leanF} : {ty}"))
    s!"structure {ta.lean} where\n{fields}\n  deriving Repr, Inhabited"
  | .callback =>
    -- The Lean arrow type goes here; the caller threads it in via
    -- `fieldTypes` with the synthetic key `"__callback__"` (a hack to
    -- avoid changing every `emitTypeDecl` call site).
    let arrow := fieldTypes["__callback__"]?.getD "Unit"
    s!"def {ta.lean} := {arrow}"
  | .taggedUnion tu =>
    -- If there are shared fields, emit a wrapper structure plus
    -- an inner inductive `<Name>.Data`. Otherwise, just the inductive.
    let dataName := if tu.sharedFields.isEmpty then ta.lean else s!"{ta.lean}.Data"
    let ctors := "\n".intercalate
      (tu.variants.toList.map (fun v =>
        -- Payload types come from `fieldTypes` (populated by the caller
        -- from resolved union field types).
        let payloadArgs := match fieldTypes[s!"__variant__{v.leanCtor}"]? with
          | some args => if args.isEmpty then "" else s!" ({args})"
          | none      => ""
        s!"  | {v.leanCtor}{payloadArgs}"))
    let inductiveDecl :=
      s!"inductive {dataName} where\n{ctors}\n  deriving Repr, Inhabited"
    if tu.sharedFields.isEmpty then
      inductiveDecl
    else
      let fields := "\n".intercalate
        (tu.sharedFields.toList.map (fun (_, leanF) =>
          let ty := fieldTypes[leanF]?.getD "Unit"
          s!"  {leanF} : {ty}") ++
        [s!"  data : {dataName}"])
      let structDecl :=
        s!"structure {ta.lean} where\n{fields}\n  deriving Repr, Inhabited"
      inductiveDecl ++ "\n\n" ++ structDecl
  | .bitfieldStruct bm =>
    let fields := "\n".intercalate
      (bm.fields.toList.map (fun (_, leanF) => s!"  {leanF} : Bool"))
    s!"structure {ta.lean} where\n{fields}\n  deriving Repr, Inhabited"

/-- Compute which Lean type names a type declaration's body refers to.
Used for building the type-dependency graph for mutual-recursion
detection. -/
private def typeDependencies
    (h : CHeader) (annoMap : Std.HashMap String TypeAnno)
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
def emitLeanModule (b : Bindings) (h : CHeader) : Except String String := do
  -- Build lookup maps.
  let typeAnnoMap : Std.HashMap String TypeAnno :=
    b.types.foldl (fun m a => m.insert a.cName a) ({} : Std.HashMap _ _)
  let declMap : Std.HashMap String CDecl :=
    h.decls.foldl (fun m d =>
      match d with
      | .function name .. => m.insert name d
      | .typedef name _   => m.insert name d
      | _ => m) ({} : Std.HashMap _ _)
  let header :=
    s!"-- Auto-generated by lean-bindgen. Do not edit.\n"
  let imports :=
    "\n".intercalate (b.leanImports.toList.map (s!"import {·}"))
  let nsOpen :=
    s!"\nnamespace {b.leanModule}\n"
  -- Type defs. For struct/callback mappings we need extra info
  -- threaded into emitTypeDecl: per-field types for structs, the
  -- derived Lean arrow type for callbacks.
  -- Build a per-TypeAnno rendered string map first.
  let mut renderedByLean : Std.HashMap String String := {}
  for ta in b.types do
    let rendered ← match ta.mapping with
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
    renderedByLean := renderedByLean.insert ta.lean rendered
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
  let mut typeBlocks : Array String := #[]
  for scc in sccs do
    if scc.size ≤ 1 then
      for name in scc do
        if let some rendered := renderedByLean[name]? then
          typeBlocks := typeBlocks.push rendered
    else
      let mut parts : Array String := #[]
      for name in scc do
        if let some rendered := renderedByLean[name]? then
          parts := parts.push rendered
      typeBlocks := typeBlocks.push
        ("mutual\n\n" ++ "\n\n".intercalate parts.toList ++ "\n\nend")
  let typeBlock := "\n\n".intercalate typeBlocks.toList
  -- Function decls
  let mut fnBlock := #[]
  for fa in b.functions do
    fnBlock := fnBlock.push (← emitFunctionDecl b typeAnnoMap declMap fa)
  let fnText := "\n\n".intercalate fnBlock.toList
  let nsClose := s!"\n\nend {b.leanModule}\n"
  return header ++ imports ++ nsOpen ++ "\n" ++ typeBlock ++
         (if typeBlock = "" then "" else "\n\n") ++
         fnText ++ nsClose

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
    s!"/* TODO: array return in IO context */ {callExpr};"
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
  let hiddenIndices := fa.callbackUserDataParams ++ arraySizeIndices
  -- Iterate Lean parameters (which mirror C parameters EXCEPT for
  -- those listed in `callbackUserDataParams` / array-size slots —
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
    preludeStr ++
    s!"  {callExpr};\n" ++
    postludeStr.trimAscii.toString ++ "\n" ++
    (match retRes.leanType with
     | "Unit" => if inIO then "  return lean_io_result_mk_ok(lean_box(0));" else "  return;"
     | _      => "  /* TODO: postlude with non-void return */")

/-- Emit one shim function for an annotation. -/
private def emitShimFunction
    (b : Bindings)
    (anno : Std.HashMap String TypeAnno)
    (declMap : Std.HashMap String CDecl)
    (fa : FunctionAnno)
    : Except String String := do
  let some decl := declMap[fa.cName]?
    | .error s!"function `{fa.cName}` not found in header"
  let (_, allRes, retRes, inIO) ← buildFunctionSignature anno decl fa
  let .function _ _ params _ := decl
    | .error "internal: non-function passed to emitShimFunction"
  let externSym := externSymbolOf fa
  match fa.style with
  | .direct =>
    -- Drop hidden params (callback user-data + array size) from the
    -- shim signature.
    let arraySizeIndices := fa.arrayPairs.map Prod.snd
    let hiddenDirect := fa.callbackUserDataParams ++ arraySizeIndices
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
    return s!"LEAN_EXPORT {cRet} {externSym}({", ".intercalate plist.toList}) \{\n" ++
           body ++ "\n}"
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
      let hiddenOut := fa.callbackUserDataParams ++ arraySizeIndices
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
          s!"/* TODO: array out-param */ lean_box(0)"
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
      return s!"LEAN_EXPORT lean_obj_res {externSym}({", ".intercalate plist.toList}) \{\n" ++
             outDecl ++ "\n" ++
             (if preludeStr = "" then "" else preludeStr ++ "\n") ++
             s!"  if ({fa.cName}({", ".intercalate callArgs.toList})) " ++ okBlock ++ " else " ++ errBlock ++ "\n}"
    else
      .error s!"`{fa.cName}` has only {params.size} params but outParamIdx is {outIdx}"
  | .boolStatus error =>
    -- All params are visible in the shim; bool return → Except Unit.
    let arraySizeIndices := fa.arrayPairs.map Prod.snd
    let hiddenOut := fa.callbackUserDataParams ++ arraySizeIndices
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
    return s!"LEAN_EXPORT lean_obj_res {externSym}({", ".intercalate plist.toList}) \{\n" ++
           (if preludeStr = "" then "" else preludeStr ++ "\n") ++
           s!"  if ({fa.cName}({", ".intercalate callArgs.toList})) " ++ okBlock ++ " else " ++ errBlock ++ "\n}"
  | .optionOutParam outIdx =>
    -- Same structure as outParamBoolStatus but wraps in Option instead
    -- of Except.  Option.some = ctor 1 (1 field), Option.none = lean_box(0).
    if h : outIdx < params.size then
      let outName := (params[outIdx].name).getD s!"arg{outIdx}"
      let .pointer pointee := params[outIdx].type
        | .error "out-param is not a pointer (shim)"
      let pointeeRes ← resolveType anno pointee
      let arraySizeIndices := fa.arrayPairs.map Prod.snd
      let hiddenOut := fa.callbackUserDataParams ++ arraySizeIndices
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
          s!"/* TODO: array out-param */ lean_box(0)"
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
      return s!"LEAN_EXPORT {cRet} {externSym}({", ".intercalate plist.toList}) \{\n" ++
             outDecl ++ "\n" ++
             (if preludeStr = "" then "" else preludeStr ++ "\n") ++
             s!"  if ({fa.cName}({", ".intercalate callArgs.toList})) " ++ someBlock ++ " else " ++ noneBlock ++ "\n}"
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
      let hiddenOut := fa.callbackUserDataParams ++ arraySizeIndices
      let visible := allRes.zipIdx.filter (fun (_, i) =>
        i ≠ outIdx && !hiddenOut.contains i)
      let plist := visible.map (fun (r, i) =>
        let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
        s!"{r.shimParam} {nm}")
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
          s!"/* TODO: array out-param */ lean_box(0)"
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
      return s!"LEAN_EXPORT {cRet} {externSym}({", ".intercalate plist.toList}) \{\n" ++
             outDecl ++ "\n" ++
             (if preludeStr = "" then "" else preludeStr ++ "\n") ++
             s!"  {fa.cName}({", ".intercalate callArgs.toList});\n" ++
             retStmt ++ "\n}"
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
          | .array _ _ => s!"lean_box(0) /* TODO */"
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
        return s!"LEAN_EXPORT {cRet} {externSym}({", ".intercalate plist.toList}) \{\n" ++
               s!"  {elemRes.cLocalType} const *{ptrName} = NULL;\n" ++
               s!"  size_t {sizeName} = 0;\n" ++
               (if preludeStr = "" then "" else preludeStr ++ "\n") ++
               s!"  if ({fa.cName}({", ".intercalate callArgs.toList})) \{\n" ++
               someBlock ++ "\n  } else {\n" ++
               noneBlock ++ "\n  }\n}"
      else .error s!"`{fa.cName}` sizeIdx out of range"
    else .error s!"`{fa.cName}` ptrIdx out of range"
  | .callerAllocates sizeFn bufIdx sizeIdx error =>
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
        let isString := elemRes.leanType = "UInt8"  -- char buffer
        let resultExpr :=
          if isString then
            s!"lean_mk_string_from_bytes((char const *)_buf, _size > 0 ? _size - 1 : 0)"
          else
            let boxElem :=
              match elemRes.returnMarshal with
              | .passthrough => boxScalarExpr elemRes "_buf[i]"
              | .enumHelper fn => s!"lean_box({fn}(_buf[i]))"
              | .leanString => s!"lean_mk_string(_buf[i])"
              | .toLeanStruct fn => s!"{fn}(_buf[i])"
              | .externalAlloc getter => s!"lean_alloc_external({getter}(), (void *)_buf[i])"
              | .array _ _ => s!"lean_box(0) /* TODO */"
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
              | .array _ _ => s!"lean_box(0)"
            s!"  lean_object *arr = lean_mk_empty_array();\n" ++
            s!"  for (size_t i = 0; i < _size; i++) \{\n" ++
            s!"    arr = lean_array_push(arr, {boxElem});\n" ++
            s!"  }\n" ++
            s!"  free(_buf);\n" ++
            s!"  lean_object* ok = lean_alloc_ctor(1, 1, 0);\n" ++
            s!"  lean_ctor_set(ok, 0, arr);\n" ++
            s!"  return lean_io_result_mk_ok(ok);"
        return s!"LEAN_EXPORT lean_obj_res {externSym}({", ".intercalate plist.toList}) \{\n" ++
               (if preludeStr = "" then "" else preludeStr ++ "\n") ++
               s!"  size_t _size = 0;\n" ++
               s!"  if (!{sizeFn}({", ".intercalate sizeCallArgs.toList})) \{\n" ++
               errBlock1 ++ "\n  }\n" ++
               s!"  {elemRes.cLocalType} *_buf = ({elemRes.cLocalType} *)malloc(sizeof({elemRes.cLocalType}) * (_size > 0 ? _size : 1));\n" ++
               s!"  if (!{fa.cName}({", ".intercalate mainCallArgs.toList})) \{\n" ++
               errBlock2 ++ "\n  }\n" ++
               okBody ++ "\n}"
      else .error s!"`{fa.cName}` sizeIdx out of range"
    else .error s!"`{fa.cName}` bufIdx out of range"

/-- Emit the pair of static helper functions converting between a
Lean inductive-encoded enum and the underlying C int. -/
private def emitEnumHelpers
    (h : CHeader) (ta : TypeAnno) (em : EnumMapping)
    : Except String String := do
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
  let toCcases := "\n".intercalate
    (pairs.toList.zipIdx.map fun ((cV, _, _), i) =>
      s!"    case {i}: return {cV};")
  let toCDef :=
    s!"static {ta.cName} {toC}(uint8_t cidx) \{\n" ++
    s!"  switch (cidx) \{\n" ++
    toCcases ++ "\n" ++
    s!"    default: return ({ta.cName})0;\n" ++
    "  }\n}"
  let toLeanCases := "\n".intercalate
    (pairs.toList.zipIdx.map fun ((cV, _, _), i) =>
      s!"    case {cV}: return {i};")
  let toLeanDef :=
    s!"static uint8_t {toLean}({ta.cName} v) \{\n" ++
    s!"  switch (v) \{\n" ++
    toLeanCases ++ "\n" ++
    s!"    default: return 0;\n" ++
    "  }\n}"
  return toCDef ++ "\n\n" ++ toLeanDef

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
    (h : CHeader) (anno : Std.HashMap String TypeAnno)
    (ta : TypeAnno) (sm : StructMapping)
    : Except String String := do
  let fields ← resolveStructFields h anno sm
  -- Reorder to match Lean's runtime ctor layout.
  let layoutFields := reorderForCtorLayout fields
  let boxedCount := layoutFields.filter (·.2.2.isBoxedInCtor) |>.size
  let usizeCount := layoutFields.filter (fun f => !f.2.2.isBoxedInCtor && f.2.2.ctorScalar == "usize") |>.size
  let scalarBytes := layoutFields.foldl (init := 0) (· + ·.2.2.cByteSize)
  let (toC, toLean) := structHelperNames ta.lean
  let cTypedef := ta.cName
  -- Walk in layout order: boxed → USize → other scalars.
  let mut toLeanLines : Array String := #[]
  let mut toCLines    : Array String := #[]
  let mut boxedIdx : Nat := 0
  let mut usizeIdx : Nat := 0
  -- Non-USize scalar byte offset starts after boxed + USize slots.
  let mut scalarOff : Nat := (boxedCount + usizeCount) * 8
  for (cField, _leanField, r) in layoutFields do
    if r.isBoxedInCtor then
      match r.returnMarshal with
      | .array cElemTy elem =>
        -- Array field: build Lean Array from C array.
        let sizeField := (sm.arrayFields.find? (·.1 = cField)).map (·.2) |>.getD "size"
        let boxExpr := match elem with
          | .scalar _ suffix => s!"lean_box{suffix}(v.{cField}[_i])"
          | .string => s!"lean_mk_string(v.{cField}[_i] == NULL ? \"\" : v.{cField}[_i])"
          | .enumHelper _ toLeanFn => s!"lean_box({toLeanFn}(v.{cField}[_i]))"
          | .structHelper _ toLeanFn => s!"{toLeanFn}(v.{cField}[_i])"
        toLeanLines := toLeanLines.push s!"  \{"
        toLeanLines := toLeanLines.push s!"    lean_object* _arr = lean_mk_empty_array_with_capacity(lean_usize_to_nat(v.{sizeField}));"
        toLeanLines := toLeanLines.push s!"    for (size_t _i = 0; _i < v.{sizeField}; _i++) \{"
        toLeanLines := toLeanLines.push s!"      _arr = lean_array_push(_arr, {boxExpr});"
        toLeanLines := toLeanLines.push s!"    }"
        toLeanLines := toLeanLines.push s!"    lean_ctor_set(o, {boxedIdx}, _arr);"
        toLeanLines := toLeanLines.push s!"  }"
      | _ =>
        let toLeanExpr := match r.returnMarshal with
          | .leanString       => s!"lean_mk_string(v.{cField} == NULL ? \"\" : v.{cField})"
          | .toLeanStruct fn  =>
            if r.isPtrToStruct
            then s!"{fn}(*v.{cField})"    -- dereference pointer-to-struct
            else s!"{fn}(v.{cField})"     -- by-value
          | .enumHelper fn    => s!"lean_box({fn}(v.{cField}))"
          | .externalAlloc gt => s!"lean_alloc_external({gt}(), (void *)(v.{cField}))"
          | _                 => s!"/* unsupported boxed field {cField} of type {r.leanType} */ NULL"
        toLeanLines := toLeanLines.push s!"  lean_ctor_set(o, {boxedIdx}, {toLeanExpr});"
      match r.paramMarshal with
      | .array cElemTy elem =>
        -- Array field: build C array from Lean Array.
        let sizeField := (sm.arrayFields.find? (·.1 = cField)).map (·.2) |>.getD "size"
        let unboxExpr := match elem with
          | .scalar shimTy suffix => s!"({shimTy})lean_unbox{suffix}(_arr_obj->m_data[_i])"
          | .string => s!"lean_string_cstr(_arr_obj->m_data[_i])"
          | .enumHelper toCFn _ => s!"{toCFn}((uint8_t)lean_unbox(_arr_obj->m_data[_i]))"
          | .structHelper toCFn _ => s!"{toCFn}(_arr_obj->m_data[_i])"
        toCLines := toCLines.push s!"  \{"
        toCLines := toCLines.push s!"    lean_array_object *_arr_obj = lean_to_array(lean_ctor_get(obj, {boxedIdx}));"
        toCLines := toCLines.push s!"    v.{sizeField} = _arr_obj->m_size;"
        toCLines := toCLines.push s!"    v.{cField} = ({cElemTy} *)malloc(sizeof({cElemTy}) * (v.{sizeField} > 0 ? v.{sizeField} : 1));"
        toCLines := toCLines.push s!"    for (size_t _i = 0; _i < v.{sizeField}; _i++) \{"
        toCLines := toCLines.push s!"      (({cElemTy} *)v.{cField})[_i] = {unboxExpr};"
        toCLines := toCLines.push s!"    }"
        toCLines := toCLines.push s!"  }"
      | .fromLeanStruct fn cTy true =>
        -- Pointer-to-struct: malloc + assign through pointer
        toCLines := toCLines.push s!"  v.{cField} = ({cTy} *)malloc(sizeof({cTy}));"
        toCLines := toCLines.push s!"  *({cTy} *)v.{cField} = {fn}(lean_ctor_get(obj, {boxedIdx}));"
      | .leanString =>
        toCLines := toCLines.push s!"  v.{cField} = lean_string_cstr(lean_ctor_get(obj, {boxedIdx}));"
      | .fromLeanStruct fn _ false =>
        toCLines := toCLines.push s!"  v.{cField} = {fn}(lean_ctor_get(obj, {boxedIdx}));"
      | .enumHelper fn =>
        toCLines := toCLines.push s!"  v.{cField} = {fn}((uint8_t)lean_unbox(lean_ctor_get(obj, {boxedIdx})));"
      | .externalData cTy =>
        toCLines := toCLines.push s!"  v.{cField} = ({cTy}) lean_get_external_data(lean_ctor_get(obj, {boxedIdx}));"
      | _ =>
        toCLines := toCLines.push s!"  v.{cField} = /* unsupported boxed field {cField} */ NULL;"
      boxedIdx := boxedIdx + 1
    else if r.ctorScalar == "usize" then
      -- USize fields use slot index: lean_ctor_{get,set}_usize(o, num_objs + j)
      let slotIdx := boxedCount + usizeIdx
      toLeanLines := toLeanLines.push
        s!"  lean_ctor_set_usize(o, {slotIdx}, (size_t)v.{cField});"
      toCLines := toCLines.push
        s!"  v.{cField} = ({r.shimReturn})lean_ctor_get_usize(obj, {slotIdx});"
      usizeIdx := usizeIdx + 1
    else
      -- Other scalars use byte offset starting after boxed + USize area.
      let suffix := r.ctorScalar
      toLeanLines := toLeanLines.push
        s!"  lean_ctor_set_{suffix}(o, {scalarOff}, ({r.shimReturn})v.{cField});"
      toCLines := toCLines.push
        s!"  v.{cField} = ({r.shimReturn})lean_ctor_get_{suffix}(obj, {scalarOff});"
      scalarOff := scalarOff + r.cByteSize
  let toLeanFn :=
    s!"lean_object* {toLean}({cTypedef} v) \{\n" ++
    s!"  lean_object* o = lean_alloc_ctor(0, {boxedCount}, {scalarBytes});\n" ++
    "\n".intercalate toLeanLines.toList ++ "\n" ++
    s!"  return o;\n}"
  let toCFn :=
    s!"{cTypedef} {toC}(b_lean_obj_arg obj) \{\n" ++
    s!"  {cTypedef} v;\n" ++
    "\n".intercalate toCLines.toList ++ "\n" ++
    s!"  return v;\n}"
  return toLeanFn ++ "\n\n" ++ toCFn

/-- Emit the pair of conversion helpers for a tagged union: a C→Lean
function that switches on the tag field and builds the appropriate
Lean inductive ctor, and a Lean→C function that reads
`lean_ptr_tag(obj)` and sets the C tag + union field. When the
mapping has shared fields, the helpers also marshal those. -/
private def emitTaggedUnionHelpers
    (h : CHeader) (anno : Std.HashMap String TypeAnno)
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
    s!"/* TODO: array C→Lean in callback context */ lean_box(0)"
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
    (h : CHeader) (anno : Std.HashMap String TypeAnno) (ta : TypeAnno)
    : Except String String := do
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
  return body

/-- Emit a reverse trampoline for a callback typedef used as a nested
callback (Lean→C). When Lean invokes an inner callback closure, this
function extracts the captured C function pointer and user-data,
unmarshals each Lean argument back to C, calls the C function pointer,
and marshals the return to Lean.

Returns `none` if this callback typedef is never used as a nested
callback (i.e., never appears as a param in another callback). -/
private def emitReverseTrampoline
    (h : CHeader) (anno : Std.HashMap String TypeAnno) (ta : TypeAnno)
    : Except String String := do
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
  return s!"LEAN_EXPORT lean_obj_res {reverseName}({", ".intercalate sigParams.toList}) \{\n" ++
         "\n".intercalate bodyLines.toList ++ "\n}"

/-- Check whether a callback typedef is referenced as a nested callback
inside any other callback typedef. -/
private def isNestedCallback
    (h : CHeader) (anno : Std.HashMap String TypeAnno)
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
    (h : CHeader) (ta : TypeAnno) (bm : BitfieldMapping)
    : Except String String := do
  let (toC, toLean) := structHelperNames ta.lean
  let numFields := bm.fields.size
  -- Look up the enum to resolve mask values.
  let enumVariants ← match lookupEnumTag h bm.enumTag with
    | some v => .ok v
    | none   => .error s!"bitfield enum `{bm.enumTag}` not found in header"
  let resolveVal (cConst : String) : Except String String := do
    match enumVariants.find? (·.1 == cConst) with
    | some (_, some v) => .ok (toString v)
    | some (_, none)   => .ok cConst  -- no explicit value; use the constant name
    | none             => .error s!"bitfield constant `{cConst}` not found in enum `{bm.enumTag}`"
  -- toLean: C unsigned → Lean struct
  let mut toLeanBody := s!"lean_object* {toLean}({ta.cName} v) \{\n"
  toLeanBody := toLeanBody ++ s!"  lean_object *obj = lean_alloc_ctor(0, 0, {numFields});\n"
  toLeanBody := toLeanBody ++ "  uint8_t *sp = lean_ctor_scalar_cptr(obj);\n"
  for (cConst, _) in bm.fields, i in List.range numFields do
    let mask ← resolveVal cConst
    toLeanBody := toLeanBody ++ s!"  sp[{i}] = (v & {mask}) != 0;\n"
  toLeanBody := toLeanBody ++ "  return obj;\n}"
  -- toC: Lean struct → C unsigned
  let mut toCBody := s!"{ta.cName} {toC}(b_lean_obj_arg obj) \{\n"
  toCBody := toCBody ++ s!"  uint8_t *sp = lean_ctor_scalar_cptr(obj);\n"
  toCBody := toCBody ++ s!"  {ta.cName} v = 0;\n"
  for (cConst, _) in bm.fields, i in List.range numFields do
    let mask ← resolveVal cConst
    toCBody := toCBody ++ s!"  if (sp[{i}]) v |= {mask};\n"
  toCBody := toCBody ++ "  return v;\n}"
  .ok (toLeanBody ++ "\n\n" ++ toCBody)

/-- Emit the per-opaque-type external-class boilerplate: a static
class pointer, a lazy getter, the finalizer wrapper, and the foreach
no-op. -/
private def emitOpaqueClass (ta : TypeAnno) (finalizer : String) : String :=
  let getter := externalClassGetter ta.lean
  let cTypedef := ta.cName
  let finalizerWrapper :=
    let snake := ta.lean.foldl (init := "") fun acc c =>
      if c.isUpper && acc ≠ "" then acc ++ "_" ++ String.singleton c.toLower
      else acc ++ String.singleton c.toLower
    s!"finalize_{snake}"
  let classGlobal :=
    let snake := ta.lean.foldl (init := "") fun acc c =>
      if c.isUpper && acc ≠ "" then acc ++ "_" ++ String.singleton c.toLower
      else acc ++ String.singleton c.toLower
    s!"g_{snake}_class"
  -- Class getter is *not* `static` so hand-written helpers in
  -- separate translation units (e.g. user-supplied test shims) can
  -- box raw C pointers as the same Lean opaque type.
  let finalizerBody := if finalizer == "" then "(void)ptr;"
                       else s!"{finalizer}(({cTypedef} *)ptr);"
  s!"static void {finalizerWrapper}(void *ptr) \{ {finalizerBody} }\n" ++
  s!"static lean_external_class *{classGlobal} = NULL;\n" ++
  s!"lean_external_class *{getter}() \{\n" ++
  s!"  if ({classGlobal} == NULL) \{\n" ++
  s!"    {classGlobal} = lean_register_external_class(&{finalizerWrapper}, &noop_foreach);\n" ++
  s!"  }\n  return {classGlobal};\n}"

/-- Emit a `free_<type>` function that recursively frees malloc'd
nested payloads of a C struct. Takes a pointer to the struct to free
(so it can be called on both stack locals via `&v` and heap-allocated
via the pointer). Does NOT free the struct itself — only its deep
fields. -/
private def emitFreeHelper
    (h : CHeader) (anno : Std.HashMap String TypeAnno)
    (ta : TypeAnno) (sm : StructMapping)
    : Except String String := do
  let fields ← resolveStructFields h anno sm
  let freeName := freeHelperName ta.lean
  let cTypedef := ta.cName
  let mut lines : Array String := #[]
  for (cField, _, r) in fields do
    match r.paramMarshal with
    | .array cElemTy elem =>
      -- Array field: free each element's deep fields, then free buffer.
      let sizeField := (sm.arrayFields.find? (·.1 = cField)).map (·.2) |>.getD "size"
      let elemFreeLine := match elem with
        | .structHelper toC _ =>
          let snake := toC.drop 8  -- drop "lean_to_" prefix
          some s!"free_{snake}"
        | _ => none
      lines := lines.push s!"  if (p->{cField}) \{"
      match elemFreeLine with
      | some freeFn =>
        lines := lines.push s!"    for (size_t _i = 0; _i < p->{sizeField}; _i++) \{"
        lines := lines.push s!"      {freeFn}(({cElemTy} *)&p->{cField}[_i]);"
        lines := lines.push s!"    }"
      | none => pure ()
      lines := lines.push s!"    free((void *)p->{cField});"
      lines := lines.push s!"  }"
    | .fromLeanStruct _ cTy true =>
      -- Pointer-to-struct field: free nested, then free the pointer
      match r.freeHelperFn with
      | some nestedFree =>
        lines := lines.push s!"  if (p->{cField}) \{"
        lines := lines.push s!"    {nestedFree}(({cTy} *)p->{cField});"
        lines := lines.push s!"    free((void *)p->{cField});"
        lines := lines.push s!"  }"
      | none =>
        lines := lines.push s!"  if (p->{cField}) free((void *)p->{cField});"
    | .fromLeanStruct _ _ false =>
      -- By-value nested struct: free its deep fields in-place
      match r.freeHelperFn with
      | some nestedFree =>
        lines := lines.push s!"  {nestedFree}(&p->{cField});"
      | none => pure ()
    | _ => pure ()
  let body := "\n".intercalate lines.toList
  return s!"static void {freeName}({cTypedef} *p) \{\n{body}\n}"

/-- Emit a `free_<type>` function for a tagged union. Switches on the
tag, frees variant-specific payloads, then frees shared fields. -/
private def emitTaggedUnionFreeHelper
    (h : CHeader) (anno : Std.HashMap String TypeAnno)
    (ta : TypeAnno) (tu : TaggedUnionMapping)
    : Except String String := do
  let some headerFields := lookupStructTag h tu.cStructTag
    | .error s!"struct tag `{tu.cStructTag}` not found in header"
  let freeName := freeHelperName ta.lean
  let cTypedef := ta.cName
  -- Free variant-specific payloads.
  let mut casesLines : Array String := #[]
  for v in tu.variants do
    let some hf := lookupFieldInStruct h headerFields v.unionField
      | continue  -- skip unknown fields
    let payloadTy := match hf.type with
      | .pointer (.const t) | .pointer t | .const t => t
      | t => t
    let r ← resolveType anno payloadTy
    let isPtr := match hf.type with
      | .pointer _ => true
      | _          => false
    match r.freeHelperFn with
    | some nestedFree =>
      if isPtr then
        casesLines := casesLines.push s!"    case {v.cTag}:"
        casesLines := casesLines.push s!"      if (p->{v.unionField}) \{"
        casesLines := casesLines.push s!"        {nestedFree}(({r.cLocalType} *)p->{v.unionField});"
        casesLines := casesLines.push s!"        free((void *)p->{v.unionField});"
        casesLines := casesLines.push s!"      }"
        casesLines := casesLines.push s!"      break;"
      else
        casesLines := casesLines.push s!"    case {v.cTag}:"
        casesLines := casesLines.push s!"      {nestedFree}(&p->{v.unionField});"
        casesLines := casesLines.push s!"      break;"
    | none => pure ()
  -- Free shared fields.
  let mut sharedLines : Array String := #[]
  for (cField, _) in tu.sharedFields do
    let some hf := lookupFieldInStruct h headerFields cField
      | continue
    let r ← resolveType anno hf.type
    match r.paramMarshal with
    | .fromLeanStruct _ cTy true =>
      match r.freeHelperFn with
      | some nestedFree =>
        sharedLines := sharedLines.push s!"  if (p->{cField}) \{"
        sharedLines := sharedLines.push s!"    {nestedFree}(({cTy} *)p->{cField});"
        sharedLines := sharedLines.push s!"    free((void *)p->{cField});"
        sharedLines := sharedLines.push s!"  }"
      | none =>
        sharedLines := sharedLines.push s!"  if (p->{cField}) free((void *)p->{cField});"
    | .fromLeanStruct _ _ false =>
      match r.freeHelperFn with
      | some nestedFree =>
        sharedLines := sharedLines.push s!"  {nestedFree}(&p->{cField});"
      | none => pure ()
    | _ => pure ()
  let switchBlock :=
    if casesLines.isEmpty then ""
    else
      s!"  switch (p->{tu.tagField}) \{\n" ++
      "\n".intercalate casesLines.toList ++ "\n" ++
      s!"    default: break;\n  }\n"
  let sharedBlock := "\n".intercalate sharedLines.toList
  return s!"static void {freeName}({cTypedef} *p) \{\n{switchBlock}{sharedBlock}\n}"

/-- Generate the entire shim source text. -/
def emitShim (b : Bindings) (h : CHeader) : Except String String := do
  let typeAnnoMap : Std.HashMap String TypeAnno :=
    b.types.foldl (fun m a => m.insert a.cName a) ({} : Std.HashMap _ _)
  let declMap : Std.HashMap String CDecl :=
    h.decls.foldl (fun m d =>
      match d with
      | .function name .. => m.insert name d
      | .typedef name _   => m.insert name d
      | _ => m) ({} : Std.HashMap _ _)
  -- Per-type setup: enum helpers, opaque-class boilerplate, struct
  -- to/from converters, callback trampolines, reverse trampolines.
  let mut enumHelpersArr    : Array String := #[]
  let mut opaqueClassArr    : Array String := #[]
  let mut forwardDeclArr    : Array String := #[]
  let mut freeHelpersArr    : Array String := #[]
  let mut structHelpersArr  : Array String := #[]
  let mut reverseCallbackArr : Array String := #[]
  let mut callbackArr       : Array String := #[]
  let mut hasNestedCallbacks := false
  for ta in b.types do
    match ta.mapping with
    | .inductiveEnum em =>
      enumHelpersArr := enumHelpersArr.push (← emitEnumHelpers h ta em)
    | .opaquePointer fin =>
      opaqueClassArr := opaqueClassArr.push (emitOpaqueClass ta fin)
    | .structRecord sm =>
      -- Forward declarations for recursive helper calls.
      let (toC, toLean) := structHelperNames ta.lean
      let freeFn := freeHelperName ta.lean
      forwardDeclArr := forwardDeclArr.push
        s!"static void {freeFn}({ta.cName} *p);"
      forwardDeclArr := forwardDeclArr.push
        s!"lean_object* {toLean}({ta.cName} v);"
      forwardDeclArr := forwardDeclArr.push
        s!"{ta.cName} {toC}(b_lean_obj_arg obj);"
      freeHelpersArr := freeHelpersArr.push
        (← emitFreeHelper h typeAnnoMap ta sm)
      structHelpersArr := structHelpersArr.push
        (← emitStructHelpers h typeAnnoMap ta sm)
    | .callback =>
      -- Forward trampoline (C→Lean).
      callbackArr := callbackArr.push (← emitCallbackTrampoline h typeAnnoMap ta)
      -- Reverse trampoline (Lean→C) if this callback is used as a
      -- nested callback inside another callback.
      if isNestedCallback h typeAnnoMap ta then
        hasNestedCallbacks := true
        let reverseTramp ← emitReverseTrampoline h typeAnnoMap ta
        reverseCallbackArr := reverseCallbackArr.push reverseTramp
        -- Forward-declare the reverse trampoline.
        let reverseName := reverseCallbackTrampolineName ta.cName
        -- Build the forward declaration signature.
        let (_, innerParams, _) ← callbackSignature h ta.cName
        let innerLayout ← analyzeCallbackLayout innerParams typeAnnoMap
        let mut sigParts : Array String := #[
          "lean_object *", "lean_object *"
        ]
        for h_i : i in [:innerParams.size] do
          if !innerLayout.hiddenIndices.contains i then
            sigParts := sigParts.push "lean_object *"
        sigParts := sigParts.push "lean_object *"
        forwardDeclArr := forwardDeclArr.push
          s!"LEAN_EXPORT lean_obj_res {reverseName}({", ".intercalate sigParts.toList});"
    | .taggedUnion tu =>
      let (toC, toLean) := structHelperNames ta.lean
      let freeFn := freeHelperName ta.lean
      forwardDeclArr := forwardDeclArr.push
        s!"static void {freeFn}({ta.cName} *p);"
      forwardDeclArr := forwardDeclArr.push
        s!"lean_object* {toLean}({ta.cName} v);"
      forwardDeclArr := forwardDeclArr.push
        s!"{ta.cName} {toC}(b_lean_obj_arg obj);"
      freeHelpersArr := freeHelpersArr.push
        (← emitTaggedUnionFreeHelper h typeAnnoMap ta tu)
      structHelpersArr := structHelpersArr.push
        (← emitTaggedUnionHelpers h typeAnnoMap ta tu)
    | .bitfieldStruct bm =>
      let (toC, toLean) := structHelperNames ta.lean
      forwardDeclArr := forwardDeclArr.push
        s!"lean_object* {toLean}({ta.cName} v);"
      forwardDeclArr := forwardDeclArr.push
        s!"{ta.cName} {toC}(b_lean_obj_arg obj);"
      structHelpersArr := structHelpersArr.push
        (← emitBitfieldHelpers h ta bm)
    | _ => pure ()
  let hasCallbacks := !callbackArr.isEmpty
  let needsForeach := !opaqueClassArr.isEmpty || hasCallbacks
  let foreachDef :=
    if needsForeach then
      "static void noop_foreach(void *mod, b_lean_obj_arg fn) { (void)mod; (void)fn; }\n\n"
    else ""
  -- Callback wrapper external class (shared, no-op finalizer) for wrapping
  -- inner callback fn-ptrs and user-data as Lean external objects.
  let callbackWrapperClassDef :=
    if hasNestedCallbacks then
      "static void noop_finalize(void *p) { (void)p; }\n" ++
      "static lean_external_class *g_callback_wrapper_class = NULL;\n" ++
      "lean_external_class *get_callback_wrapper_class() {\n" ++
      "  if (g_callback_wrapper_class == NULL) {\n" ++
      "    g_callback_wrapper_class = lean_register_external_class(&noop_finalize, &noop_foreach);\n" ++
      "  }\n  return g_callback_wrapper_class;\n}\n\n"
    else ""
  let forwardBlock :=
    if forwardDeclArr.isEmpty then ""
    else "// Forward declarations for recursive helpers.\n" ++
         "\n".intercalate forwardDeclArr.toList ++ "\n\n"
  let helperBlock :=
    let parts := (enumHelpersArr ++ opaqueClassArr ++ freeHelpersArr ++ structHelpersArr
                  ++ reverseCallbackArr ++ callbackArr).toList
    if parts.isEmpty then "" else forwardBlock ++ "\n\n".intercalate parts ++ "\n\n"
  let hasTaggedUnion := b.types.any (fun ta => match ta.mapping with | .taggedUnion _ => true | _ => false)
  let hasStructOrTU := b.types.any (fun ta => match ta.mapping with
    | .structRecord _ | .taggedUnion _ => true | _ => false)
  let needsStdlib := hasStructOrTU || hasNestedCallbacks
  let header :=
    s!"// Auto-generated by lean-bindgen. Do not edit.\n" ++
    s!"#include \"lean/lean.h\"\n" ++
    (if needsStdlib then "#include <stdlib.h>\n" else "") ++
    (if hasTaggedUnion then "#include <string.h>\n" else "") ++
    s!"#include \"{b.headerPath.splitOn "/" |>.getLast!}\"\n\n"
  let mut block := #[]
  for fa in b.functions do
    block := block.push (← emitShimFunction b typeAnnoMap declMap fa)
  return header ++ foreachDef ++ callbackWrapperClassDef ++ helperBlock ++
         "\n\n".intercalate block.toList ++ "\n"

end Codegen
end LeanBindgen
