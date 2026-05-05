import LeanBindgen.Annotation
import Lean

/-!
# Binding Declaration DSL

A macro-based DSL that expands to the existing `Bindings`/`TypeAnno`/`FunctionAnno`/`ConstAnno`
record types. No runtime changes — purely syntactic sugar.
-/

open Lean

namespace LeanBindgen.DSL

-- ============================================================
-- Syntax categories
-- ============================================================

declare_syntax_cat cbindVariant
declare_syntax_cat cbindError
declare_syntax_cat cbindFnMod
declare_syntax_cat cbindTUItem
declare_syntax_cat cbindItem

-- ============================================================
-- Variant syntax: | cName => leanName
-- ============================================================

scoped syntax (name := variantPair) "| " ident " => " ident : cbindVariant

-- ============================================================
-- Error syntax
-- ============================================================

scoped syntax (name := errString) "on_error" "string" ident : cbindError
scoped syntax (name := errEnum) "on_error" "enum" ident ident : cbindError
scoped syntax (name := errTuple) "on_error" "tuple" ident ident ident : cbindError

-- ============================================================
-- Function modifiers
-- ============================================================

scoped syntax (name := fmodIo) "+io" : cbindFnMod
scoped syntax (name := fmodNullRet) "+nullable_return" : cbindFnMod
scoped syntax (name := fmodNullOut) "+nullable_out" : cbindFnMod
scoped syntax (name := fmodCbData) "callback_data" "[" num,* "]" : cbindFnMod
scoped syntax (name := fmodArrPairs) "array_pairs" "[" "(" num "," num ")" ,* "]" : cbindFnMod
scoped syntax (name := fmodBytePairs) "byte_pairs" "[" "(" num "," num ")" ,* "]" : cbindFnMod
scoped syntax (name := fmodRetained) "retained_params" "[" num,* "]" : cbindFnMod
scoped syntax (name := fmodBorrowed) "borrowed_params" "[" num,* "]" : cbindFnMod
scoped syntax (name := fmodExtern) "extern_sym" str : cbindFnMod

-- ============================================================
-- Tagged union sub-items
-- ============================================================

scoped syntax (name := tuShared) "shared" cbindVariant* : cbindTUItem
scoped syntax (name := tuVariantField) "| " ident " => " ident "field" ident : cbindTUItem
scoped syntax (name := tuVariantPlain) "| " ident " => " ident : cbindTUItem

-- ============================================================
-- Item syntax (header fields, types, functions, constants)
-- ============================================================

-- Header fields
scoped syntax (name := cbindHeader) "header" str : cbindItem
scoped syntax (name := cbindModule) "module" ident : cbindItem
scoped syntax (name := cbindOutDir) "out_dir" str : cbindItem
scoped syntax (name := cbindShim) "shim" str : cbindItem
scoped syntax (name := cbindLib) "lib" str : cbindItem
scoped syntax (name := cbindPreprocessor) "preprocessor" "[" str,* "]" : cbindItem
scoped syntax (name := cbindImports) "imports" "[" ident,* "]" : cbindItem

-- Type items
scoped syntax (name := cbindScalar) "scalar" ident "=>" ident ":" ident : cbindItem
scoped syntax (name := cbindOpaqueFree) "opaque" ident "=>" ident "freed_by" ident : cbindItem
scoped syntax (name := cbindOpaqueBorrowed) "opaque" ident "=>" ident "borrowed" : cbindItem
scoped syntax (name := cbindEnum) "enum" ident "=>" ident "tag" ident cbindVariant* : cbindItem
scoped syntax (name := cbindCallback) "callback" ident "=>" ident : cbindItem
scoped syntax (name := cbindStruct) "struct" ident "=>" ident "tag" ident cbindVariant*
    ("arrays" "| " ident "," ident)* : cbindItem
scoped syntax (name := cbindBitfield) "bitfield" ident "=>" ident "tag" ident cbindVariant* : cbindItem
scoped syntax (name := cbindTaggedUnion) "tagged_union" ident "=>" ident
    "tag" ident "." ident "enum" ident
    cbindTUItem* : cbindItem
scoped syntax (name := cbindTypeRaw) "type_raw" term : cbindItem

-- Function items
scoped syntax (name := cbindFnDirect) (priority := low) "cfn" ident "=>" ident cbindFnMod* : cbindItem
scoped syntax (name := cbindFnOut) "cfn" ident "=>" ident
    "out" "[" num "]" cbindError cbindFnMod* : cbindItem
scoped syntax (name := cbindFnVoidOut) "cfn" ident "=>" ident
    "void_out" "[" num "]" cbindFnMod* : cbindItem
scoped syntax (name := cbindFnOptionOut) "cfn" ident "=>" ident
    "option_out" "[" num "]" cbindFnMod* : cbindItem
scoped syntax (name := cbindFnOptionOutArray) "cfn" ident "=>" ident
    "option_out_array" "[" num "," num "]" cbindFnMod* : cbindItem
scoped syntax (name := cbindFnMultiOut) "cfn" ident "=>" ident
    "multi_out" "[" num,* "]" cbindFnMod* : cbindItem
scoped syntax (name := cbindFnBoolStatus) "cfn" ident "=>" ident
    "bool_status" cbindError cbindFnMod* : cbindItem
scoped syntax (name := cbindFnCallerAlloc) "cfn" ident "=>" ident
    "caller_alloc" str "[" num "," num "]" cbindError cbindFnMod* : cbindItem
scoped syntax (name := cbindFnByteOut) "cfn" ident "=>" ident
    "byte_out" "[" num "," num "]" cbindError cbindFnMod* : cbindItem
scoped syntax (name := cbindFnRaw) "fn_raw" term : cbindItem

-- Constant items
scoped syntax (name := cbindConstVal) "cconst" ident ":" ident ":=" str : cbindItem
scoped syntax (name := cbindConstLookup) "cconst" ident ":" ident : cbindItem

-- ============================================================
-- Top-level syntax
-- ============================================================

scoped syntax (name := cBindings) "c_bindings" "{" cbindItem* "}" : term

-- ============================================================
-- Macro helpers
-- ============================================================

private def identToStr (id : TSyntax `ident) : String :=
  id.getId.toString (escape := false)

private def mkStrLitT (s : String) : TSyntax `str :=
  Syntax.mkStrLit s

private def expandScalarTarget (ty : TSyntax `ident) : MacroM (TSyntax `term) := do
  let s := identToStr ty
  match s with
  | "Unit"    => `(ScalarTarget.unit)
  | "UInt8"   => `(ScalarTarget.uint8)
  | "UInt16"  => `(ScalarTarget.uint16)
  | "UInt32"  => `(ScalarTarget.uint32)
  | "UInt64"  => `(ScalarTarget.uint64)
  | "Int8"    => `(ScalarTarget.int8)
  | "Int16"   => `(ScalarTarget.int16)
  | "Int32"   => `(ScalarTarget.int32)
  | "Int64"   => `(ScalarTarget.int64)
  | "Bool"    => `(ScalarTarget.bool)
  | "Float32" => `(ScalarTarget.float)
  | "Float"   => `(ScalarTarget.double)
  | "USize"   => `(ScalarTarget.usize)
  | "ISize"   => `(ScalarTarget.isize)
  | _         => Macro.throwError s!"Unknown scalar target: {s}"

private def expandError (e : TSyntax `cbindError) : MacroM (TSyntax `term) := do
  let node := e.raw
  let kind := node.getKind
  if kind == ``errString then
    let args := node.getArgs
    let errFn := mkStrLitT (identToStr ⟨args[2]!⟩)
    `(ErrorReturn.string $errFn)
  else if kind == ``errEnum then
    let args := node.getArgs
    let codeFn := mkStrLitT (identToStr ⟨args[2]!⟩)
    let enumLn := mkStrLitT (identToStr ⟨args[3]!⟩)
    `(ErrorReturn.enum $codeFn $enumLn)
  else if kind == ``errTuple then
    let args := node.getArgs
    let codeFn := mkStrLitT (identToStr ⟨args[2]!⟩)
    let enumLn := mkStrLitT (identToStr ⟨args[3]!⟩)
    let msgFn := mkStrLitT (identToStr ⟨args[4]!⟩)
    `(ErrorReturn.tuple $codeFn $enumLn $msgFn)
  else
    Macro.throwError s!"Unknown error kind"

private def expandVariantPair (v : TSyntax `cbindVariant) : MacroM (TSyntax `term) := do
  let args := v.raw.getArgs
  -- args[0] = "|", args[1] = cName, args[2] = "=>", args[3] = leanName
  let cStr := mkStrLitT (identToStr ⟨args[1]!⟩)
  let lStr := mkStrLitT (identToStr ⟨args[3]!⟩)
  `(($cStr, $lStr))

/-- Process function modifiers, returning all fields as a record-like structure. -/
private def expandFnMods (mods : Array (TSyntax `cbindFnMod))
    : MacroM (Bool × Bool × Bool × Array (TSyntax `term) × Array (TSyntax `term) ×
              Array (TSyntax `term) × Array (TSyntax `term) × Array (TSyntax `term) ×
              Option (TSyntax `term)) := do
  let mut ioFlag := false
  let mut nrFlag := false
  let mut noFlag := false
  let mut cbData : Array (TSyntax `term) := #[]
  let mut arrPairs : Array (TSyntax `term) := #[]
  let mut bPairs : Array (TSyntax `term) := #[]
  let mut retP : Array (TSyntax `term) := #[]
  let mut borP : Array (TSyntax `term) := #[]
  let mut extSym : Option (TSyntax `term) := none
  for m in mods do
    let node := m.raw
    let kind := node.getKind
    if kind == ``fmodIo then ioFlag := true
    else if kind == ``fmodNullRet then nrFlag := true
    else if kind == ``fmodNullOut then noFlag := true
    else if kind == ``fmodCbData then
      let nums := node.getArgs[2]!.getSepArgs
      for n in nums do
        let lit : TSyntax `num := ⟨n⟩
        cbData := cbData.push (← `($lit))
    else if kind == ``fmodArrPairs then
      -- Tokens are flattened: "array_pairs" "[" "(" num "," num ")" ... "]"
      -- Extract num pairs by scanning for num-kind tokens
      let allArgs := node.getArgs
      let mut nums : Array (TSyntax `num) := #[]
      for i in [:allArgs.size] do
        if allArgs[i]!.isOfKind `num then
          nums := nums.push ⟨allArgs[i]!⟩
      -- Pair consecutive nums
      let mut j := 0
      while _h : j + 1 < nums.size do
        arrPairs := arrPairs.push (← `(($(nums[j]!), $(nums[j+1]!))))
        j := j + 2
    else if kind == ``fmodBytePairs then
      let allArgs := node.getArgs
      let mut nums : Array (TSyntax `num) := #[]
      for i in [:allArgs.size] do
        if allArgs[i]!.isOfKind `num then
          nums := nums.push ⟨allArgs[i]!⟩
      let mut j := 0
      while _h : j + 1 < nums.size do
        bPairs := bPairs.push (← `(($(nums[j]!), $(nums[j+1]!))))
        j := j + 2
    else if kind == ``fmodRetained then
      let nums := node.getArgs[2]!.getSepArgs
      for n in nums do
        let lit : TSyntax `num := ⟨n⟩
        retP := retP.push (← `($lit))
    else if kind == ``fmodBorrowed then
      let nums := node.getArgs[2]!.getSepArgs
      for n in nums do
        let lit : TSyntax `num := ⟨n⟩
        borP := borP.push (← `($lit))
    else if kind == ``fmodExtern then
      let s : TSyntax `str := ⟨node.getArgs[1]!⟩
      extSym := some (← `(some $s))
    else
      Macro.throwError s!"Unknown function modifier"
  return (ioFlag, nrFlag, noFlag, cbData, arrPairs, bPairs, retP, borP, extSym)

private def mkFnAnno (cName leanName : TSyntax `ident) (style : TSyntax `term)
    (mods : Array (TSyntax `cbindFnMod)) : MacroM (TSyntax `term) := do
  let cStr := mkStrLitT (identToStr cName)
  let lStr := mkStrLitT (identToStr leanName)
  let (ioFlag, nrFlag, noFlag, cbData, arrPairs, bPairs, retP, borP, extSym) ← expandFnMods mods
  let ioLit ← if ioFlag then `(true) else `(false)
  let nrLit ← if nrFlag then `(true) else `(false)
  let noLit ← if noFlag then `(true) else `(false)
  let cdList ← `([$cbData,*])
  let apList ← `([$arrPairs,*])
  let bpList ← `([$bPairs,*])
  let rtList ← `([$retP,*])
  let brList ← `([$borP,*])
  let extTerm ← match extSym with
    | some e => pure e
    | none => `((none : Option String))
  `(FunctionAnno.mk $cStr $lStr $style $ioLit $brList $extTerm $cdList $apList $bpList $rtList $nrLit $noLit)

-- ============================================================
-- Main macro expansion
-- ============================================================

macro_rules
  | `(c_bindings { $items:cbindItem* }) => do
    -- Default header fields
    let mut headerPath : TSyntax `str := Syntax.mkStrLit ""
    let mut leanModule : TSyntax `term ← `(`Anonymous)
    let mut outDir : TSyntax `str := Syntax.mkStrLit ""
    let mut shimPath : TSyntax `str := Syntax.mkStrLit ""
    let mut libPrefix : TSyntax `str := Syntax.mkStrLit ""
    let mut ppArgs : Array (TSyntax `str) := #[]
    let mut leanImps : Array (TSyntax `term) := #[]

    -- Collected items
    let mut types : Array (TSyntax `term) := #[]
    let mut fns : Array (TSyntax `term) := #[]
    let mut consts : Array (TSyntax `term) := #[]

    for item in items do
      let node := item.raw
      let kind := node.getKind
      -- Header fields
      if kind == ``cbindHeader then
        headerPath := ⟨node.getArgs[1]!⟩
      else if kind == ``cbindModule then
        let modId : TSyntax `ident := ⟨node.getArgs[1]!⟩
        leanModule ← `($(quote modId.getId))
      else if kind == ``cbindOutDir then
        outDir := ⟨node.getArgs[1]!⟩
      else if kind == ``cbindShim then
        shimPath := ⟨node.getArgs[1]!⟩
      else if kind == ``cbindLib then
        libPrefix := ⟨node.getArgs[1]!⟩
      else if kind == ``cbindPreprocessor then
        let strs := node.getArgs[2]!.getSepArgs
        ppArgs := strs.map (⟨·⟩)
      else if kind == ``cbindImports then
        let ids := node.getArgs[2]!.getSepArgs
        for id in ids do
          leanImps := leanImps.push (← `($(quote id.getId)))

      -- Type: scalar
      else if kind == ``cbindScalar then
        let args := node.getArgs
        let cStr := mkStrLitT (identToStr ⟨args[1]!⟩)
        let lStr := mkStrLitT (identToStr ⟨args[3]!⟩)
        let target ← expandScalarTarget ⟨args[5]!⟩
        types := types.push (← `(⟨$cStr, $lStr, TypeMapping.scalarNewtype $target⟩))

      -- Type: opaque freed_by
      else if kind == ``cbindOpaqueFree then
        let args := node.getArgs
        let cStr := mkStrLitT (identToStr ⟨args[1]!⟩)
        let lStr := mkStrLitT (identToStr ⟨args[3]!⟩)
        let finStr := mkStrLitT (identToStr ⟨args[5]!⟩)
        types := types.push (← `(⟨$cStr, $lStr, TypeMapping.opaquePointer $finStr⟩))

      -- Type: opaque borrowed
      else if kind == ``cbindOpaqueBorrowed then
        let args := node.getArgs
        let cStr := mkStrLitT (identToStr ⟨args[1]!⟩)
        let lStr := mkStrLitT (identToStr ⟨args[3]!⟩)
        let emptyS := mkStrLitT ""
        types := types.push (← `(⟨$cStr, $lStr, TypeMapping.opaquePointer $emptyS⟩))

      -- Type: enum
      else if kind == ``cbindEnum then
        let args := node.getArgs
        let cStr := mkStrLitT (identToStr ⟨args[1]!⟩)
        let lStr := mkStrLitT (identToStr ⟨args[3]!⟩)
        let tagStr := mkStrLitT (identToStr ⟨args[5]!⟩)
        let mut vTerms : Array (TSyntax `term) := #[]
        let variants := args[6]!.getArgs
        for v in variants do
          vTerms := vTerms.push (← expandVariantPair ⟨v⟩)
        let vArr ← `(#[$vTerms,*])
        let mp ← `(TypeMapping.inductiveEnum { enumTag := $tagStr, variants := $vArr })
        types := types.push (← `(⟨$cStr, $lStr, $mp⟩))

      -- Type: callback
      else if kind == ``cbindCallback then
        let args := node.getArgs
        let cStr := mkStrLitT (identToStr ⟨args[1]!⟩)
        let lStr := mkStrLitT (identToStr ⟨args[3]!⟩)
        types := types.push (← `(⟨$cStr, $lStr, TypeMapping.callback⟩))

      -- Type: struct
      else if kind == ``cbindStruct then
        let args := node.getArgs
        let cStr := mkStrLitT (identToStr ⟨args[1]!⟩)
        let lStr := mkStrLitT (identToStr ⟨args[3]!⟩)
        let tagStr := mkStrLitT (identToStr ⟨args[5]!⟩)
        -- args[6] = cbindVariant* (null wrapper), args[7] = ("arrays" ...)* (null wrapper)
        let mut fieldTerms : Array (TSyntax `term) := #[]
        let mut arrFieldTerms : Array (TSyntax `term) := #[]
        let variants := args[6]!.getArgs
        for v in variants do
          fieldTerms := fieldTerms.push (← expandVariantPair ⟨v⟩)
        let arrGroups := args[7]!.getArgs
        for g in arrGroups do
          -- Each group: "arrays" "|" ident "," ident
          let gArgs := g.getArgs
          let dStr := mkStrLitT (identToStr ⟨gArgs[2]!⟩)
          let sStr := mkStrLitT (identToStr ⟨gArgs[4]!⟩)
          arrFieldTerms := arrFieldTerms.push (← `(($dStr, $sStr)))
        let fArr ← `(#[$fieldTerms,*])
        let afArr ← `(#[$arrFieldTerms,*])
        let mp ← `(TypeMapping.structRecord { cStructTag := $tagStr, fields := $fArr, arrayFields := $afArr })
        types := types.push (← `(⟨$cStr, $lStr, $mp⟩))

      -- Type: bitfield
      else if kind == ``cbindBitfield then
        let args := node.getArgs
        let cStr := mkStrLitT (identToStr ⟨args[1]!⟩)
        let lStr := mkStrLitT (identToStr ⟨args[3]!⟩)
        let tagStr := mkStrLitT (identToStr ⟨args[5]!⟩)
        let mut fTerms : Array (TSyntax `term) := #[]
        let bfVariants := args[6]!.getArgs
        for v in bfVariants do
          fTerms := fTerms.push (← expandVariantPair ⟨v⟩)
        let fArr ← `(#[$fTerms,*])
        let mp ← `(TypeMapping.bitfieldStruct { enumTag := $tagStr, fields := $fArr })
        types := types.push (← `(⟨$cStr, $lStr, $mp⟩))

      -- Type: tagged_union
      else if kind == ``cbindTaggedUnion then
        let args := node.getArgs
        -- tagged_union cName => lName tag structTag . tagField enum tagEnum tuItem*
        let cStr := mkStrLitT (identToStr ⟨args[1]!⟩)
        let lStr := mkStrLitT (identToStr ⟨args[3]!⟩)
        let sTag := mkStrLitT (identToStr ⟨args[5]!⟩)
        let tField := mkStrLitT (identToStr ⟨args[7]!⟩)
        let tEnum := mkStrLitT (identToStr ⟨args[9]!⟩)
        let mut sharedTerms : Array (TSyntax `term) := #[]
        let mut tuVarTerms : Array (TSyntax `term) := #[]
        let tuItems := args[10]!.getArgs
        for child in tuItems do
          let ck := child.getKind
          if ck == ``tuShared then
            -- shared cbindVariant*
            let sArgs := child.getArgs
            let sharedVars := sArgs[1]!.getArgs
            for sv in sharedVars do
              sharedTerms := sharedTerms.push (← expandVariantPair ⟨sv⟩)
          else if ck == ``tuVariantField then
            -- | cTag => lCtor field uField
            let vArgs := child.getArgs
            let cTag := mkStrLitT (identToStr ⟨vArgs[1]!⟩)
            let lCtor := mkStrLitT (identToStr ⟨vArgs[3]!⟩)
            let uField := mkStrLitT (identToStr ⟨vArgs[5]!⟩)
            tuVarTerms := tuVarTerms.push (← `(TaggedVariant.mk $cTag $lCtor $uField none))
          else if ck == ``tuVariantPlain then
            -- | cTag => lCtor (unionField = lCtor)
            let vArgs := child.getArgs
            let cTag := mkStrLitT (identToStr ⟨vArgs[1]!⟩)
            let lCtor := mkStrLitT (identToStr ⟨vArgs[3]!⟩)
            tuVarTerms := tuVarTerms.push (← `(TaggedVariant.mk $cTag $lCtor $lCtor none))
          else pure ()
        let sfArr ← `(#[$sharedTerms,*])
        let tvArr ← `(#[$tuVarTerms,*])
        let mp ← `(TypeMapping.taggedUnion (TaggedUnionMapping.mk $sTag $tField $tEnum $sfArr $tvArr))
        types := types.push (← `(⟨$cStr, $lStr, $mp⟩))

      -- Type: raw
      else if kind == ``cbindTypeRaw then
        types := types.push ⟨node.getArgs[1]!⟩

      -- Function: direct
      else if kind == ``cbindFnDirect then
        let args := node.getArgs
        let cId : TSyntax `ident := ⟨args[1]!⟩
        let lId : TSyntax `ident := ⟨args[3]!⟩
        let mods : Array (TSyntax `cbindFnMod) := args[4]!.getArgs.map fun a => (⟨a⟩ : TSyntax `cbindFnMod)
        let style ← `(FunctionStyle.direct)
        fns := fns.push (← mkFnAnno cId lId style mods)

      -- Function: out[N] on_error
      else if kind == ``cbindFnOut then
        let args := node.getArgs
        let cId : TSyntax `ident := ⟨args[1]!⟩
        let lId : TSyntax `ident := ⟨args[3]!⟩
        -- cfn id => id "out" "[" num "]" error mods...
        let idx : TSyntax `num := ⟨args[6]!⟩
        let errTerm ← expandError ⟨args[8]!⟩
        let style ← `(FunctionStyle.outParamBoolStatus $idx $errTerm)
        let mods : Array (TSyntax `cbindFnMod) := args[9]!.getArgs.map fun a => (⟨a⟩ : TSyntax `cbindFnMod)
        fns := fns.push (← mkFnAnno cId lId style mods)

      -- Function: void_out[N]
      else if kind == ``cbindFnVoidOut then
        let args := node.getArgs
        let cId : TSyntax `ident := ⟨args[1]!⟩
        let lId : TSyntax `ident := ⟨args[3]!⟩
        let idx : TSyntax `num := ⟨args[6]!⟩
        let style ← `(FunctionStyle.voidOutParam $idx)
        let mods : Array (TSyntax `cbindFnMod) := args[8]!.getArgs.map fun a => (⟨a⟩ : TSyntax `cbindFnMod)
        fns := fns.push (← mkFnAnno cId lId style mods)

      -- Function: option_out[N]
      else if kind == ``cbindFnOptionOut then
        let args := node.getArgs
        let cId : TSyntax `ident := ⟨args[1]!⟩
        let lId : TSyntax `ident := ⟨args[3]!⟩
        let idx : TSyntax `num := ⟨args[6]!⟩
        let style ← `(FunctionStyle.optionOutParam $idx)
        let mods : Array (TSyntax `cbindFnMod) := args[8]!.getArgs.map fun a => (⟨a⟩ : TSyntax `cbindFnMod)
        fns := fns.push (← mkFnAnno cId lId style mods)

      -- Function: option_out_array[N, M]
      else if kind == ``cbindFnOptionOutArray then
        let args := node.getArgs
        let cId : TSyntax `ident := ⟨args[1]!⟩
        let lId : TSyntax `ident := ⟨args[3]!⟩
        let n : TSyntax `num := ⟨args[6]!⟩
        let mm : TSyntax `num := ⟨args[8]!⟩
        let style ← `(FunctionStyle.optionOutArray $n $mm)
        let mods : Array (TSyntax `cbindFnMod) := args[10]!.getArgs.map fun a => (⟨a⟩ : TSyntax `cbindFnMod)
        fns := fns.push (← mkFnAnno cId lId style mods)

      -- Function: multi_out[n, ...]
      else if kind == ``cbindFnMultiOut then
        let args := node.getArgs
        let cId : TSyntax `ident := ⟨args[1]!⟩
        let lId : TSyntax `ident := ⟨args[3]!⟩
        let nums := args[6]!.getSepArgs
        let mut numTerms : Array (TSyntax `term) := #[]
        for n in nums do
          let lit : TSyntax `num := ⟨n⟩
          numTerms := numTerms.push (← `($lit))
        let numArr ← `(#[$numTerms,*])
        let style ← `(FunctionStyle.multiOutParam $numArr)
        let mods : Array (TSyntax `cbindFnMod) := args[8]!.getArgs.map fun a => (⟨a⟩ : TSyntax `cbindFnMod)
        fns := fns.push (← mkFnAnno cId lId style mods)

      -- Function: bool_status on_error
      else if kind == ``cbindFnBoolStatus then
        let args := node.getArgs
        let cId : TSyntax `ident := ⟨args[1]!⟩
        let lId : TSyntax `ident := ⟨args[3]!⟩
        let errTerm ← expandError ⟨args[5]!⟩
        let style ← `(FunctionStyle.boolStatus $errTerm)
        let mods : Array (TSyntax `cbindFnMod) := args[6]!.getArgs.map fun a => (⟨a⟩ : TSyntax `cbindFnMod)
        fns := fns.push (← mkFnAnno cId lId style mods)

      -- Function: caller_alloc "sizeFn" [buf, size] on_error
      else if kind == ``cbindFnCallerAlloc then
        let args := node.getArgs
        let cId : TSyntax `ident := ⟨args[1]!⟩
        let lId : TSyntax `ident := ⟨args[3]!⟩
        let sizeFn : TSyntax `str := ⟨args[5]!⟩
        let bufIdx : TSyntax `num := ⟨args[7]!⟩
        let sizeIdx : TSyntax `num := ⟨args[9]!⟩
        let errTerm ← expandError ⟨args[11]!⟩
        let style ← `(FunctionStyle.callerAllocates $sizeFn $bufIdx $sizeIdx $errTerm)
        let mods : Array (TSyntax `cbindFnMod) := args[12]!.getArgs.map fun a => (⟨a⟩ : TSyntax `cbindFnMod)
        fns := fns.push (← mkFnAnno cId lId style mods)

      -- Function: byte_out[N, M] on_error
      else if kind == ``cbindFnByteOut then
        let args := node.getArgs
        let cId : TSyntax `ident := ⟨args[1]!⟩
        let lId : TSyntax `ident := ⟨args[3]!⟩
        let n : TSyntax `num := ⟨args[6]!⟩
        let mm : TSyntax `num := ⟨args[8]!⟩
        let errTerm ← expandError ⟨args[10]!⟩
        let style ← `(FunctionStyle.byteArrayOutBoolStatus $n $mm $errTerm)
        let mods : Array (TSyntax `cbindFnMod) := args[11]!.getArgs.map fun a => (⟨a⟩ : TSyntax `cbindFnMod)
        fns := fns.push (← mkFnAnno cId lId style mods)

      -- Function: raw
      else if kind == ``cbindFnRaw then
        fns := fns.push ⟨node.getArgs[1]!⟩

      -- Constant with value
      else if kind == ``cbindConstVal then
        let args := node.getArgs
        let cStr := mkStrLitT (identToStr ⟨args[1]!⟩)
        let tyStr := mkStrLitT (identToStr ⟨args[3]!⟩)
        let valStr : TSyntax `str := ⟨args[5]!⟩
        consts := consts.push (← `(ConstAnno.mk $cStr $cStr $tyStr (some $valStr)))

      -- Constant lookup
      else if kind == ``cbindConstLookup then
        let args := node.getArgs
        let cStr := mkStrLitT (identToStr ⟨args[1]!⟩)
        let tyStr := mkStrLitT (identToStr ⟨args[3]!⟩)
        consts := consts.push (← `(ConstAnno.mk $cStr $cStr $tyStr none))

      else
        Macro.throwError s!"Unknown c_bindings item kind: {kind}"

    -- Build the final Bindings struct
    let typesArr ← `(#[$types,*])
    let fnsArr ← `(#[$fns,*])
    let constsArr ← `(#[$consts,*])
    let ppArr ← `(#[$ppArgs,*])
    let importsArr ← `(#[$leanImps,*])

    `(Bindings.mk $headerPath $leanModule $outDir $shimPath $libPrefix $typesArr $fnsArr $constsArr $importsArr $ppArr)

end LeanBindgen.DSL
