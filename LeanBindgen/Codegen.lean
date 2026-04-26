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

/-- A resolved Lean target for a C type. Carries both the Lean
identifier (for emitting Lean code) and a description of how the value
crosses the FFI boundary (for emitting shim code). -/
structure ResolvedType where
  leanType   : String
  shimParam  : String
  shimReturn : String
  /-- True when the value crosses as a boxed `lean_object *`. -/
  isLeanObj  : Bool := false
  /-- If set, the shim must pass each parameter of this type through
  this helper to obtain the underlying C value before calling the
  wrapped function (Lean → C). -/
  shimUnbox  : Option String := none
  /-- If set, the C return value of this type must be passed through
  this helper to obtain the Lean object to return (C → Lean). -/
  shimBox    : Option String := none
  /-- C type name to use for a stack-local of this type when we need
  to declare one (e.g. for an out-param). For boxed types this is
  `lean_object *`; for scalars it's `int`/`uint64_t`/etc. -/
  shimLocal  : String := ""
  deriving Inhabited

/-- Constructor for a "scalar-like" mapping where shim param and
return are the same C type, and the value is *not* a boxed Lean
object. -/
private def scalarRT (lean cTy : String) : ResolvedType :=
  { leanType := lean, shimParam := cTy, shimReturn := cTy,
    isLeanObj := false, shimLocal := cTy }

/-- Constructor for a Lean-object-typed mapping (e.g. `String`,
opaque pointers): boxed across the FFI. -/
private def boxedRT (lean : String) : ResolvedType :=
  { leanType := lean, shimParam := "b_lean_obj_arg",
    shimReturn := "lean_obj_res", isLeanObj := true,
    shimLocal := "lean_object *" }

/-- Build the conversion helper names for an enum mapping. -/
private def enumHelpers (lean : String) : String × String :=
  let snake := lean.foldl (init := "") fun acc c =>
    if c.isUpper && acc ≠ "" then acc ++ "_" ++ String.singleton c.toLower
    else acc ++ String.singleton c.toLower
  (s!"lean_to_{snake}", s!"{snake}_to_lean")

/-- Constructor for a Lean inductive backed by a C int (typedef +
enum). Lean compiles all-nullary inductives to a *raw* `uint8_t` (the
constructor index) at the FFI boundary, so we cross as a scalar; the
per-enum helpers translate between cidx and the C enum value. -/
private def enumRT (leanName cTypedef : String) : ResolvedType :=
  let (toC, toLean) := enumHelpers leanName
  { leanType := leanName, shimParam := "uint8_t",
    shimReturn := "uint8_t", isLeanObj := false,
    shimLocal := cTypedef,
    shimUnbox := some toC, shimBox := some toLean }

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
      .ok (boxedRT "String")
  -- Typedef name: look up in user bindings, then in stdint table.
  | .typedef name =>
    match anno[name]? with
    | some a =>
      match a.mapping with
      | .scalarNewtype k     => .ok (scalarRT a.lean k.toC)
      | .opaquePointer       => .ok (boxedRT a.lean)
      | .inductiveEnum _     => .ok (enumRT a.lean name)
    | none =>
      match stdintMap[name]? with
      | some r => .ok r
      | none   => .error s!"unmapped typedef `{name}`"
  -- Pointer to user-annotated type. Usually shows up as a bool-status
  -- function's out-param; the call site strips it before reaching this
  -- branch. Surface it as an explicit pointer so the shim emitter has
  -- something to use.
  | .pointer (.typedef name) =>
    match anno[name]? with
    | some a =>
      match a.mapping with
      | .scalarNewtype k =>
        .ok { leanType := a.lean, shimParam := k.toC ++ " *",
              shimReturn := k.toC ++ " *", isLeanObj := false,
              shimLocal := k.toC }
      | .opaquePointer   => .ok (boxedRT a.lean)
      | .inductiveEnum _ =>
        .ok { leanType := a.lean, shimParam := name ++ " *",
              shimReturn := name ++ " *", isLeanObj := false,
              shimLocal := name }
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
  | .opaquePointer =>
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

/-- Build the C-side argument expression for one shim parameter.
For most types this is just the parameter name; `String` parameters
are unboxed via `lean_string_cstr` into a local; enum-typed
parameters are passed through the per-enum helper. Returns the
prelude statements (in declaration order) and the expression to use
in the call. -/
private def renderParamPass (r : ResolvedType) (nm : String)
    : Array String × String :=
  if r.leanType = "String" then
    (#[s!"  char const *{nm}_c = lean_string_cstr({nm});"], s!"{nm}_c")
  else if let some unbox := r.shimUnbox then
    (#[s!"  {r.shimLocal} {nm}_c = {unbox}({nm});"], s!"{nm}_c")
  else
    (#[], nm)

/-- Render the C statement(s) that produce the shim's return value
from the result of the C call. -/
private def renderReturn (retRes : ResolvedType) (callExpr : String) (inIO : Bool)
    : String :=
  let raw : String :=
    match retRes.leanType with
    | "Unit"   => s!"{callExpr};\n  return lean_box(0);"
    | "String" =>
      s!"char const *_ret = {callExpr};\n  return lean_mk_string(_ret == NULL ? \"\" : _ret);"
    | "Bool"   => s!"return {callExpr} ? 1 : 0;"
    | _ =>
      match retRes.shimBox with
      | some boxFn => s!"return {boxFn}({callExpr});"
      | none       => s!"return ({retRes.shimReturn})({callExpr});"
  if inIO then
    -- Wrap in lean_io_result_mk_ok. Different return shapes need
    -- different surface — Unit is `lean_box(0)`, String is the result
    -- of `lean_mk_string`, plain scalars use `lean_box_uint64`, and
    -- enum-typed returns are first run through `shimBox` (which gives
    -- a uint8_t cidx) and then `lean_box`'d.
    match retRes.leanType with
    | "Unit"   => s!"{callExpr};\n  return lean_io_result_mk_ok(lean_box(0));"
    | "String" => s!"char const *_ret = {callExpr};\n  return lean_io_result_mk_ok(lean_mk_string(_ret == NULL ? \"\" : _ret));"
    | "Bool"   => s!"return lean_io_result_mk_ok(lean_box({callExpr} ? 1 : 0));"
    | _ =>
      match retRes.shimBox with
      | some boxFn => s!"return lean_io_result_mk_ok(lean_box({boxFn}({callExpr})));"
      | none       => s!"return lean_io_result_mk_ok(lean_box_uint64((uint64_t)({callExpr})));"
  else
    raw

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
      let pointeeC := match pointee with
        | .typedef n => n
        | _          => "void *"  -- unexpected; surface below
      -- Build visible params.
      let visible := allRes.zipIdx.filter (fun (_, i) => i ≠ outIdx)
      let plist := visible.map (fun (r, i) =>
        let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
        s!"{r.shimParam} {nm}")
      let mut prelude := #[]
      let mut callArgs := #[]
      for h : i in [:allRes.size] do
        let r := allRes[i]
        let nm := (params[i]?.bind (·.name)).getD s!"arg{i}"
        if i = outIdx then
          callArgs := callArgs.push s!"&{outName}"
        else if r.leanType = "String" then
          prelude := prelude.push s!"  char const *{nm}_c = lean_string_cstr({nm});"
          callArgs := callArgs.push s!"{nm}_c"
        else
          callArgs := callArgs.push nm
      let preludeStr := "\n".intercalate prelude.toList
      let outDecl := s!"  {pointeeC} {outName};"
      let okMarshal :=
        s!"    return lean_io_result_mk_ok(lean_alloc_ctor(1, 1, 0)); /* Except.ok */"
      -- Build "Except.ok(value)" properly:
      let valExpr := s!"lean_box_uint64((uint64_t){outName})"
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
  let toLeanCases := "\n".intercalate
    (pairs.toList.zipIdx.map fun ((cV, _, _), i) =>
      s!"    case {cV}: return lean_box({i});")
  let toCDef :=
    s!"static {ta.cName} {toC}(uint8_t cidx) \{\n" ++
    s!"  switch (cidx) \{\n" ++
    toCcases ++ "\n" ++
    s!"    default: return ({ta.cName})0;\n" ++
    "  }\n}"
  let toLeanCases' := "\n".intercalate
    (pairs.toList.zipIdx.map fun ((cV, _, _), i) =>
      s!"    case {cV}: return {i};")
  let toLeanDef :=
    s!"static uint8_t {toLean}({ta.cName} v) \{\n" ++
    s!"  switch (v) \{\n" ++
    toLeanCases' ++ "\n" ++
    s!"    default: return 0;\n" ++
    "  }\n}"
  return toCDef ++ "\n\n" ++ toLeanDef

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
  -- Emit conversion helpers once per enum-typed annotation.
  let mut helpers : Array String := #[]
  for ta in b.types do
    match ta.mapping with
    | .inductiveEnum em => helpers := helpers.push (← emitEnumHelpers h ta em)
    | _ => pure ()
  let helperBlock :=
    if helpers.isEmpty then "" else "\n\n".intercalate helpers.toList ++ "\n\n"
  let header := s!"// Auto-generated by lean-bindgen. Do not edit.\n#include \"lean/lean.h\"\n#include \"{b.headerPath.splitOn "/" |>.getLast!}\"\n\n"
  let mut block := #[]
  for fa in b.functions do
    block := block.push (← emitShimFunction b typeAnnoMap declMap fa)
  return header ++ helperBlock ++ "\n\n".intercalate block.toList ++ "\n"

end Codegen
end LeanBindgen
