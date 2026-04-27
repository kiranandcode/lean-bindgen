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
  paramMarshal  : ParamMarshal := .passthrough
  returnMarshal : ReturnMarshal := .passthrough
  deriving Inhabited

/-- Constructor for a "scalar-like" mapping where shim param and
return are the same C type, and the value is *not* a boxed Lean
object. -/
private def scalarRT (lean cTy : String) : ResolvedType :=
  { leanType := lean, shimParam := cTy, shimReturn := cTy,
    cLocalType := cTy }

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

/-- The class-getter name for an opaque-pointer mapping. -/
private def externalClassGetter (lean : String) : String :=
  let snake := lean.foldl (init := "") fun acc c =>
    if c.isUpper && acc ≠ "" then acc ++ "_" ++ String.singleton c.toLower
    else acc ++ String.singleton c.toLower
  s!"get_{snake}_class"

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
  | .void                       => some (scalarRT "Unit" "void")
  | .scalar .bool _             => some (scalarRT "Bool" "uint8_t")
  | .scalar .float _            => some (scalarRT "Float32" "float")
  | .scalar .double _           => some (scalarRT "Float" "double")
  | .scalar .char _             => some (scalarRT "UInt8" "uint8_t")
  | .scalar .short .signed      => some (scalarRT "Int16" "int16_t")
  | .scalar .short _            => some (scalarRT "UInt16" "uint16_t")
  | .scalar .int .signed        => some (scalarRT "Int32" "int32_t")
  | .scalar .int _              => some (scalarRT "UInt32" "uint32_t")
  | .scalar .long .signed       => some (scalarRT "Int64" "int64_t")
  | .scalar .long _             => some (scalarRT "UInt64" "uint64_t")
  | .scalar .longLong .signed   => some (scalarRT "Int64" "int64_t")
  | .scalar .longLong _         => some (scalarRT "UInt64" "uint64_t")
  | _ => none

/-- The fixed-width stdint typedef → Lean scalar map. Only consulted
for typedef references that aren't resolved by the user's `Bindings`. -/
private def stdintMap : Std.HashMap String ResolvedType :=
  ({} : Std.HashMap String ResolvedType)
    |>.insert "int8_t"   (scalarRT "Int8" "int8_t")
    |>.insert "uint8_t"  (scalarRT "UInt8" "uint8_t")
    |>.insert "int16_t"  (scalarRT "Int16" "int16_t")
    |>.insert "uint16_t" (scalarRT "UInt16" "uint16_t")
    |>.insert "int32_t"  (scalarRT "Int32" "int32_t")
    |>.insert "uint32_t" (scalarRT "UInt32" "uint32_t")
    |>.insert "int64_t"  (scalarRT "Int64" "int64_t")
    |>.insert "uint64_t" (scalarRT "UInt64" "uint64_t")
    |>.insert "size_t"   (scalarRT "USize" "size_t")
    |>.insert "ssize_t"  (scalarRT "ISize" "ssize_t")
    |>.insert "ptrdiff_t" (scalarRT "ISize" "ptrdiff_t")
    |>.insert "bool"     (scalarRT "Bool" "uint8_t")

/-- Resolve a C type against the user's bindings. Returns an error
message if no mapping exists. -/
partial def resolveType
    (anno : Std.HashMap String TypeAnno) (ty : CType)
    : Except String ResolvedType := do
  match ty with
  -- Strip qualifiers — they don't affect the Lean type.
  | .const t | .volatile t => resolveType anno t
  -- Pointer to char/const char → Lean String.
  | .pointer (.const (.scalar .char _))
  | .pointer (.scalar .char _) =>
      .ok stringRT
  -- Typedef name: look up in user bindings, then in stdint table.
  | .typedef name =>
    match anno[name]? with
    | some a =>
      match a.mapping with
      | .scalarNewtype k     => .ok (scalarRT a.lean k.toC)
      | .opaquePointer _     => .ok (opaqueRT a.lean name)
      | .inductiveEnum _     => .ok (enumRT a.lean name)
    | none =>
      match stdintMap[name]? with
      | some r => .ok r
      | none   => .error s!"unmapped typedef `{name}`"
  -- Pointer to user-annotated type — typically the C signature shape
  -- for an opaque type (`clingo_control_t *`) in a parameter or out-
  -- pointer position. We resolve this to the same Lean opaque the
  -- typedef itself maps to.
  | .pointer (.typedef name) =>
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
    | none => .error s!"unmapped pointer-to-typedef `{name}`"
  | _ =>
    match primitiveMap ty with
    | some r => .ok r
    | none   => .error s!"unmapped C type: {ty.spec}"

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
  -- Resolve parameter types.
  let mut paramRes : Array (Bool × ResolvedType) := #[]   -- borrow flag × resolved
  for h : i in [:params.size] do
    let p := params[i]
    let r ← resolveType anno p.type
    let borrow := fa.borrowedParams.contains i
    paramRes := paramRes.push (borrow, r)
  let retRes ← resolveType anno retCty
  -- Apply the function-style transformation.
  match fa.style with
  | .direct =>
    let leanType :=
      let parts := paramRes.toList.map (fun (b, r) => renderParamType r b)
      let parts := parts ++ [if fa.inIO then s!"IO {retRes.leanType}" else retRes.leanType]
      " → ".intercalate parts
    let allRes := paramRes.map Prod.snd
    return (leanType, allRes, retRes, fa.inIO)
  | .outParamBoolStatus outIdx _errMsgFn =>
    -- Drop the out-parameter from the Lean signature. The pointer
    -- target type becomes the success value of an Except. The bool
    -- return is dropped (only used at the C level for status).
    if h : outIdx < params.size then
      let outParam := params[outIdx]
      let .pointer pointee := outParam.type
        | .error s!"out-param `{outParam.name.getD "?"}` of `{fa.cName}` is not a pointer"
      let pointeeRes ← resolveType anno pointee
      let visibleParams := paramRes.toList.zipIdx.filter (fun (_, i) => i ≠ outIdx)
                                         |>.map (fun (br, _) => br)
      let parts := visibleParams.map (fun (b, r) => renderParamType r b)
      let leanRet := s!"IO (Except String {pointeeRes.leanType})"
      let leanType := " → ".intercalate (parts ++ [leanRet])
      let allRes := paramRes.map Prod.snd
      return (leanType, allRes, pointeeRes, true)
    else
      .error s!"`{fa.cName}` has only {params.size} params but outParamIdx is {outIdx}"

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

/-- Emit a single type declaration. -/
private def emitTypeDecl (ta : TypeAnno) : String :=
  match ta.mapping with
  | .scalarNewtype k =>
    s!"def {ta.lean} := {k.toLean}\n  deriving Repr, Inhabited"
  | .opaquePointer _ =>
    s!"opaque {ta.lean} : Type"
  | .inductiveEnum em =>
    let ctors := "\n".intercalate
      (em.variants.toList.map (fun (_, leanV) => s!"  | {leanV}"))
    s!"inductive {ta.lean} where\n{ctors}\n  deriving Repr, Inhabited"

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
  -- Type defs
  let typeBlock :=
    "\n\n".intercalate (b.types.toList.map emitTypeDecl)
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

/-- Render `(prelude, expr)` for one shim parameter, dispatching on
its `paramMarshal`. The `prelude` lines (if any) declare locals; the
`expr` is what gets passed to the underlying C call. -/
private def renderParamPass (r : ResolvedType) (nm : String)
    : Array String × String :=
  match r.paramMarshal with
  | .passthrough            => (#[], nm)
  | .leanString             =>
      (#[s!"  char const *{nm}_c = lean_string_cstr({nm});"], s!"{nm}_c")
  | .enumHelper fn          =>
      (#[s!"  {r.cLocalType} {nm}_c = {fn}({nm});"], s!"{nm}_c")
  | .externalData cTy       =>
      (#[s!"  {cTy} {nm}_c = ({cTy}) lean_get_external_data({nm});"], s!"{nm}_c")

/-- The C expression that turns the C-call result into a Lean object
(prior to any IO wrapping). For non-IO returns whose Lean type *isn't*
a Lean object (e.g. plain scalar, plain enum cidx), the expression
will be a non-Lean-object value — the caller emits it directly as the
shim's return value. -/
private def renderReturn (retRes : ResolvedType) (callExpr : String) (inIO : Bool)
    : String :=
  -- `bare` is the *non-IO* return statement.
  let bare : String :=
    match retRes.returnMarshal with
    | .leanString =>
      s!"char const *_ret = {callExpr};\n  return lean_mk_string(_ret == NULL ? \"\" : _ret);"
    | .enumHelper fn =>
      s!"return {fn}({callExpr});"
    | .externalAlloc getter =>
      s!"return lean_alloc_external({getter}(), (void *)({callExpr}));"
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
    s!"char const *_ret = {callExpr};\n  return lean_io_result_mk_ok(lean_mk_string(_ret == NULL ? \"\" : _ret));"
  | .enumHelper fn =>
    s!"return lean_io_result_mk_ok(lean_box({fn}({callExpr})));"
  | .externalAlloc getter =>
    s!"return lean_io_result_mk_ok(lean_alloc_external({getter}(), (void *)({callExpr})));"
  | .passthrough =>
    match retRes.leanType with
    | "Unit" => s!"{callExpr};\n  return lean_io_result_mk_ok(lean_box(0));"
    | "Bool" => s!"return lean_io_result_mk_ok(lean_box({callExpr} ? 1 : 0));"
    | _      => s!"return lean_io_result_mk_ok(lean_box_uint64((uint64_t)({callExpr})));"

/-- Emit the body of a "direct" shim: marshal each parameter, call the
underlying C function, marshal the result. -/
private def directShimBody
    (decl : CDecl) (resolved : Array ResolvedType)
    (retRes : ResolvedType) (cName : String) (inIO : Bool)
    : String := Id.run do
  let .function _name _ret params _variadic := decl
    | return "/* not a function */"
  let mut prelude : Array String := #[]
  let mut callArgs : Array String := #[]
  for h : i in [:resolved.size] do
    let r := resolved[i]
    let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
    let (pre, expr) := renderParamPass r nm
    prelude := prelude ++ pre
    callArgs := callArgs.push expr
  let callExpr := s!"{cName}({", ".intercalate callArgs.toList})"
  let preludeStr := if prelude.isEmpty then "" else "\n".intercalate prelude.toList ++ "\n"
  preludeStr ++ "  " ++ renderReturn retRes callExpr inIO

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
    let plist := shimParamList params allRes
    -- Pick shim return type:
    let cRet :=
      if inIO then "lean_obj_res"
      else
        match retRes.leanType with
        | "Unit"   => "void"
        | "String" => "lean_obj_res"
        | "Bool"   => "uint8_t"
        | _        => retRes.shimReturn
    let body := directShimBody decl allRes retRes fa.cName inIO
    return s!"LEAN_EXPORT {cRet} {externSym}({", ".intercalate plist.toList}) \{\n" ++
           body ++ "\n}"
  | .outParamBoolStatus outIdx errMsgFn =>
    -- Drop the out-param from the shim signature, allocate a local for
    -- it, call the C function, branch on the bool.
    if h : outIdx < params.size then
      let outName := (params[outIdx].name).getD s!"arg{outIdx}"
      let .pointer pointee := params[outIdx].type
        | .error "out-param is not a pointer (shim)"
      let pointeeRes ← resolveType anno pointee
      -- Build visible params.
      let visible := allRes.zipIdx.filter (fun (_, i) => i ≠ outIdx)
      let plist := visible.map (fun (r, i) =>
        let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
        s!"{r.shimParam} {nm}")
      let mut prelude : Array String := #[]
      let mut callArgs : Array String := #[]
      for h : i in [:allRes.size] do
        let r := allRes[i]
        let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
        if i = outIdx then
          callArgs := callArgs.push s!"&{outName}"
        else
          let (pre, expr) := renderParamPass r nm
          prelude := prelude ++ pre
          callArgs := callArgs.push expr
      let preludeStr := "\n".intercalate prelude.toList
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
        | .passthrough =>
          s!"lean_box_uint64((uint64_t){outName})"
      let okBlock :=
        s!"\{\n      lean_object* val = {valExpr};\n      lean_object* ok = lean_alloc_ctor(1, 1, 0);\n      lean_ctor_set(ok, 0, val);\n      return lean_io_result_mk_ok(ok);\n    }"
      let errBlock :=
        s!"\{\n      char const *msg = {errMsgFn}();\n      if (msg == NULL) msg = \"\";\n      lean_object* err = lean_alloc_ctor(0, 1, 0);\n      lean_ctor_set(err, 0, lean_mk_string(msg));\n      return lean_io_result_mk_ok(err);\n    }"
      return s!"LEAN_EXPORT lean_obj_res {externSym}({", ".intercalate plist.toList}) \{\n" ++
             outDecl ++ "\n" ++
             (if preludeStr = "" then "" else preludeStr ++ "\n") ++
             s!"  if ({fa.cName}({", ".intercalate callArgs.toList})) " ++ okBlock ++ " else " ++ errBlock ++ "\n}"
    else
      .error s!"`{fa.cName}` has only {params.size} params but outParamIdx is {outIdx}"

/-- Walk the parsed header for an enum decl whose tag matches and
return its variants. -/
private def lookupEnumTag (h : CHeader) (tag : String) : Option (Array (String × Option Int)) :=
  h.decls.findSome? fun
    | .enumDef (some t) variants => if t = tag then some variants else none
    | _ => none

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
  s!"static void {finalizerWrapper}(void *ptr) \{ {finalizer}(({cTypedef} *)ptr); }\n" ++
  s!"static lean_external_class *{classGlobal} = NULL;\n" ++
  s!"static lean_external_class *{getter}() \{\n" ++
  s!"  if ({classGlobal} == NULL) \{\n" ++
  s!"    {classGlobal} = lean_register_external_class(&{finalizerWrapper}, &noop_foreach);\n" ++
  s!"  }\n  return {classGlobal};\n}"

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
  -- Per-type setup: enum helpers and opaque-class boilerplate.
  let mut enumHelpersArr : Array String := #[]
  let mut opaqueClassArr : Array String := #[]
  for ta in b.types do
    match ta.mapping with
    | .inductiveEnum em =>
      enumHelpersArr := enumHelpersArr.push (← emitEnumHelpers h ta em)
    | .opaquePointer fin =>
      opaqueClassArr := opaqueClassArr.push (emitOpaqueClass ta fin)
    | _ => pure ()
  let needsForeach := !opaqueClassArr.isEmpty
  let foreachDef :=
    if needsForeach then
      "static void noop_foreach(void *mod, b_lean_obj_arg fn) { (void)mod; (void)fn; }\n\n"
    else ""
  let helperBlock :=
    let parts := (enumHelpersArr ++ opaqueClassArr).toList
    if parts.isEmpty then "" else "\n\n".intercalate parts ++ "\n\n"
  let header :=
    s!"// Auto-generated by lean-bindgen. Do not edit.\n" ++
    s!"#include \"lean/lean.h\"\n" ++
    s!"#include \"{b.headerPath.splitOn "/" |>.getLast!}\"\n\n"
  let mut block := #[]
  for fa in b.functions do
    block := block.push (← emitShimFunction b typeAnnoMap declMap fa)
  return header ++ foreachDef ++ helperBlock ++
         "\n\n".intercalate block.toList ++ "\n"

end Codegen
end LeanBindgen
