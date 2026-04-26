import LeanBindgen.C.Ast
import LeanBindgen.C.Token
import Std.Data.HashSet

/-!
# C header parser (MVP)

Recursive-descent over the token array produced by `Token.tokenize`.
Handles the declaration-level subset of C — typedefs (incl. function-
pointer typedefs), struct/union/enum definitions, function prototypes,
and extern variable declarations. No statement bodies.

Maintains a typedef-name set so identifiers introduced by `typedef`
become legal type specifiers in subsequent declarations. Preprocessor
lines are surfaced as `CDecl.macroConst` for `#define X val`; other
forms are currently skipped (a richer pass lives in `Preprocessor.lean`).

This file is intentionally short — it covers what's needed to validate
against `clingo.h` declarations and is meant to grow incrementally as
new forms are encountered.
-/

namespace LeanBindgen.C

/-! ## Keyword sets -/

private def storageKeywords   : List String :=
  ["typedef", "extern", "static", "inline", "auto", "register"]
private def qualifierKeywords : List String := ["const", "volatile", "restrict"]
private def signKeywords      : List String := ["signed", "unsigned"]
private def builtinTypeKwds   : List String :=
  ["void", "char", "short", "int", "long", "float", "double", "_Bool"]
private def aggregateKwds     : List String := ["struct", "union", "enum"]
private def typeSpecKwds      : List String :=
  builtinTypeKwds ++ signKeywords ++ aggregateKwds

/-! ## State + monad -/

/-- Typedefs assumed to be in scope from the standard headers we don't
actually preprocess (`<stdint.h>`, `<stddef.h>`, `<stdbool.h>`,
`<sys/types.h>`). Until we add a real `#include` resolver, these stand
in so headers using fixed-width integer types still parse. -/
def standardTypedefs : List String := [
  -- stdint.h
  "int8_t", "int16_t", "int32_t", "int64_t",
  "uint8_t", "uint16_t", "uint32_t", "uint64_t",
  "int_least8_t", "int_least16_t", "int_least32_t", "int_least64_t",
  "uint_least8_t", "uint_least16_t", "uint_least32_t", "uint_least64_t",
  "int_fast8_t", "int_fast16_t", "int_fast32_t", "int_fast64_t",
  "uint_fast8_t", "uint_fast16_t", "uint_fast32_t", "uint_fast64_t",
  "intptr_t", "uintptr_t", "intmax_t", "uintmax_t",
  -- stddef.h
  "size_t", "ssize_t", "ptrdiff_t", "wchar_t", "max_align_t",
  -- sys/types.h (a small common subset)
  "off_t", "pid_t", "uid_t", "gid_t", "mode_t", "time_t", "clock_t",
  -- stdbool.h: `bool` is a macro for `_Bool`, but we accept it as
  -- a typedef since some headers `#include` it and use plain `bool`.
  "bool",
  -- C11 native flavours
  "char16_t", "char32_t"
]

structure ParserState where
  tokens   : Array Token
  pos      : Nat
  typedefs : Std.HashSet String
  /-- Names of `#define` macros we've seen. When such a name appears
  in a decl-spec context, we treat it as a transparent annotation and
  skip it (covers `CLINGO_VISIBILITY_DEFAULT`, `CLINGO_DEPRECATED`, etc.). -/
  macros   : Std.HashSet String
  decls    : Array CDecl

def ParserState.mk' (tokens : Array Token) : ParserState :=
  let typedefs := standardTypedefs.foldl (·.insert ·) ({} : Std.HashSet String)
  { tokens, pos := 0, typedefs, macros := {}, decls := #[] }

abbrev ParserM := EStateM String ParserState

namespace ParserM

@[inline] def peek? (off : Nat := 0) : ParserM (Option Token) := do
  let s ← get; return s.tokens[s.pos + off]?

@[inline] def peek! : ParserM Token := do
  match (← peek?) with
  | some t => pure t
  | none   => throw "unexpected EOF"

@[inline] def advance : ParserM Unit :=
  modify fun s => { s with pos := s.pos + 1 }

def err (msg : String) : ParserM α := do
  match (← peek?) with
  | some t => throw s!"{t.pos}: {msg}"
  | none   => throw s!"<eof>: {msg}"

def isPunct (s : String) : ParserM Bool := do
  match (← peek?) with
  | some ⟨.punct s', _⟩ => return s = s'
  | _                   => return false

def isIdent (s : String) : ParserM Bool := do
  match (← peek?) with
  | some ⟨.ident s', _⟩ => return s = s'
  | _                   => return false

def consumePunct (s : String) : ParserM Unit := do
  if ← isPunct s then advance else err s!"expected `{s}`"

def expectIdentName : ParserM String := do
  match (← peek!).kind with
  | .ident s => advance; return s
  | _        => err "expected identifier"

def isKnownTypedef (s : String) : ParserM Bool := do
  return (← get).typedefs.contains s

def isKnownMacro (s : String) : ParserM Bool := do
  return (← get).macros.contains s

end ParserM

open ParserM

/-! ## Decl-spec accumulation

We collect tokens that contribute to the *type specifier* of a
declaration, then materialise them into a `CType` once we hit the
declarator boundary.
-/

private structure DeclSpec where
  storage    : Option String  := none
  isConst    : Bool           := false
  isVolatile : Bool           := false
  base       : Option CType   := none
  signed?    : Option Bool    := none
  intWidth   : Option ScalarKind := none
  longCount  : Nat            := 0

private def DeclSpec.materialise (s : DeclSpec) : ParserM CType := do
  let sign : Signedness :=
    match s.signed? with
    | some true  => .signed
    | some false => .unsigned
    | none       => .unspecified
  let core : CType ←
    match s.base, s.intWidth with
    | some t, none =>
      -- Apply sign to typedef-named integer scalars only when explicit
      -- (`unsigned int32_t` is illegal anyway). Otherwise return as-is.
      pure t
    | none, some w =>
      if w = .long && s.longCount = 2 then pure (.scalar .longLong sign)
      else pure (.scalar w sign)
    | some t, some _ => pure t
    | none, none =>
      if s.signed?.isSome then pure (.scalar .int sign)
      else err "missing type specifier"
  let core := if s.isVolatile then CType.volatile core else core
  let core := if s.isConst    then CType.const    core else core
  return core

private def isReservedDecl (s : String) : Bool :=
  storageKeywords.contains s
  || qualifierKeywords.contains s
  || typeSpecKwds.contains s

/-- Consume tokens until the running paren-balance hits 0. Used for
function-like macro annotations (`__attribute__((...))` shapes). The
caller is expected to have already consumed the opening `(`, so the
initial depth is 1. -/
partial def skipBalancedParens (depth : Nat) : ParserM Unit := do
  if depth = 0 then return
  match (← ParserM.peek?) with
  | none                  => ParserM.err "unbalanced `(`"
  | some ⟨.punct "(", _⟩  => ParserM.advance; skipBalancedParens (depth + 1)
  | some ⟨.punct ")", _⟩  => ParserM.advance; skipBalancedParens (depth - 1)
  | _                     => ParserM.advance; skipBalancedParens depth

/-! ## Declarator wrapping

A declarator describes how a variable's type wraps its base type. For
`int *x[5]` the base is `int` and the wrapper is `λty => array (pointer ty) 5`.
We represent the wrapper as a `CType → CType` function and compose
pieces in order: pointer-prefix is applied *innermost*, suffixes
(arrays, function parameters) wrap the result *outermost*. Parens
group: `int (*x)[5]` reorders so the array suffix applies before the
pointer wrap, yielding `pointer (array int 5)`.

The CPS form is necessary because we don't know the eventual base
until after we've parsed the entire decl-spec; a parens-grouped
sub-declarator needs to be parsed structurally and only later get its
base from the outer context.
-/

abbrev TypeWrap := CType → CType

private def composeWrap (outer inner : TypeWrap) : TypeWrap :=
  fun ty => outer (inner ty)

/-! ## Mutual recursion: type/decl parsers -/

mutual

/-- Parse and accumulate decl-specifier tokens. Stops once the next
token is no longer part of a decl-spec. -/
partial def parseDeclSpec : ParserM DeclSpec :=
  go {}
where
  go (acc : DeclSpec) : ParserM DeclSpec := do
    match (← peek?) with
    | none => return acc
    | some t =>
      match t.kind with
      | .ident kw =>
        if storageKeywords.contains kw then
          if acc.storage.isSome then err s!"duplicate storage class `{kw}`"
          advance; go { acc with storage := some kw }
        else if qualifierKeywords.contains kw then
          advance
          let acc := if kw = "const"    then { acc with isConst := true } else acc
          let acc := if kw = "volatile" then { acc with isVolatile := true } else acc
          go acc
        else if signKeywords.contains kw then
          advance; go { acc with signed? := some (kw = "signed") }
        else if kw = "long" then
          advance; go { acc with longCount := acc.longCount + 1, intWidth := some .long }
        else if kw = "short" then
          advance; go { acc with intWidth := some .short }
        else if kw = "char" then
          advance; go { acc with intWidth := some .char, base := some (.scalar .char) }
        else if kw = "int" then
          advance; go { acc with intWidth := some (acc.intWidth.getD .int) }
        else if kw = "void" then
          advance; go { acc with base := some .void }
        else if kw = "float" then
          advance; go { acc with base := some (.scalar .float) }
        else if kw = "double" then
          advance; go { acc with base := some (.scalar .double) }
        else if kw = "_Bool" then
          advance; go { acc with base := some (.scalar .bool) }
        else if kw = "struct" || kw = "union" then
          advance
          let tag? ← match (← peek?) with
            | some ⟨.ident t, _⟩ => advance; pure (some t)
            | _ => pure none
          if ← isPunct "{" then
            let fields ← parseFieldBlock
            let decl := if kw = "struct" then CDecl.structDef tag? fields
                                          else CDecl.unionDef  tag? fields
            modify fun s => { s with decls := s.decls.push decl }
          let ref := if kw = "struct" then CType.structRef (tag?.getD "")
                                       else CType.unionRef  (tag?.getD "")
          go { acc with base := some ref }
        else if kw = "enum" then
          advance
          let tag? ← match (← peek?) with
            | some ⟨.ident t, _⟩ => advance; pure (some t)
            | _ => pure none
          if ← isPunct "{" then
            let variants ← parseEnumBody
            modify fun s => { s with decls := s.decls.push (CDecl.enumDef tag? variants) }
          go { acc with base := some (.enumRef (tag?.getD "")) }
        else
          -- Possibly a typedef-name. Only consume it as a type
          -- specifier if no base type has been set yet.
          if (← isKnownTypedef kw) && acc.base.isNone && acc.intWidth.isNone then
            advance; go { acc with base := some (.typedef kw) }
          else if (← isKnownMacro kw) then
            -- Annotation-like macro (e.g. CLINGO_VISIBILITY_DEFAULT,
            -- CLINGO_DEPRECATED). Skip it. If it's followed by `(`...`)`
            -- we also need to swallow that — handles the rare case
            -- where the macro is function-like.
            advance
            if ← isPunct "(" then
              advance
              skipBalancedParens 1
            go acc
          else
            return acc
      | _ => return acc

/-- Parse `{ field_decl... }` — a struct/union body. The opening `{` is
expected to be the *current* token (we don't consume it before calling). -/
partial def parseFieldBlock : ParserM (Array CField) := do
  consumePunct "{"
  let mut fields : Array CField := #[]
  while !(← isPunct "}") do
    let spec ← parseDeclSpec
    let baseTy ← spec.materialise
    let mut moreDeclarators := true
    while moreDeclarators do
      let (name, wrap) ← parseDeclarator
      let ty := wrap baseTy
      let bits? ← if ← isPunct ":" then do
          advance
          match (← peek!).kind with
          | .intLit raw => advance; pure (some raw.toNat!)
          | _ => err "expected bitfield width"
        else pure none
      fields := fields.push { name, type := ty, bitfield := bits? }
      if ← isPunct "," then advance else moreDeclarators := false
    consumePunct ";"
  consumePunct "}"
  return fields

/-- Parse an enum body `{ NAME, NAME = N, ... }`. Opening `{` is current. -/
partial def parseEnumBody : ParserM (Array (String × Option Int)) := do
  consumePunct "{"
  let mut variants : Array (String × Option Int) := #[]
  if ← isPunct "}" then
    advance; return #[]
  let mut more := true
  while more do
    let name ← expectIdentName
    let val? ← if ← isPunct "=" then do
        advance
        let neg ← if ← isPunct "-" then advance; pure true else pure false
        match (← peek!).kind with
        | .intLit raw =>
          advance
          let n : Int := raw.toNat!
          pure (some (if neg then -n else n))
        | _ => err "expected integer enum value"
      else pure none
    variants := variants.push (name, val?)
    if ← isPunct "," then
      advance
      if ← isPunct "}" then more := false
    else
      more := false
  consumePunct "}"
  return variants

/-- Parse a declarator and return `(name, wrap)` such that the final
type is `wrap base` once the base type is known. -/
partial def parseDeclarator : ParserM (String × TypeWrap) := do
  let ptrWrap ← parsePointerPrefix
  let (name, dirWrap) ← parseDirectDeclarator
  -- `dirWrap` (suffixes / inner declarator) wraps OUTSIDE the pointer
  -- prefix: `int *x[5]` is `array (pointer int) 5`, not `pointer (array int 5)`.
  return (name, composeWrap dirWrap ptrWrap)

/-- Consume zero or more `*` (each optionally followed by qualifiers
that bind to the pointer itself). Returns the wrapping function. -/
partial def parsePointerPrefix : ParserM TypeWrap := do
  if ← isPunct "*" then
    advance
    let mut single : TypeWrap := CType.pointer
    let mut moreQuals := true
    while moreQuals do
      if ← isIdent "const" then
        advance; single := composeWrap CType.const single
      else if ← isIdent "volatile" then
        advance; single := composeWrap CType.volatile single
      else moreQuals := false
    let rest ← parsePointerPrefix
    -- rest wraps OUTSIDE single (further-out pointers wrap inner ones).
    return composeWrap rest single
  else
    pure id

/-- direct-declarator: identifier, parenthesised sub-declarator, plus
trailing `[...]` / `(...)` suffixes. -/
partial def parseDirectDeclarator : ParserM (String × TypeWrap) := do
  let (name, innerWrap) ←
    if ← isPunct "(" then do
      advance
      if ← startsParamList then
        -- Abstract function-declarator: no inner declarator, just params.
        let (params, variadic) ← parseParamList
        consumePunct ")"
        pure ("", fun ty => CType.function ty params variadic)
      else
        let nt ← parseDeclarator
        consumePunct ")"
        pure nt
    else
      match (← peek?) with
      | some ⟨.ident s, _⟩ =>
        if isReservedDecl s then pure ("", id)
        else advance; pure (s, id)
      | _ => pure ("", id)
  let suffixWrap ← parseSuffixes
  -- Suffixes apply to the BASE first, then we wrap with the inner
  -- declarator's structure (pointers introduced inside parens).
  return (name, composeWrap innerWrap suffixWrap)

/-- Collect all `[...]` and `(...)` suffixes and return a single
wrapping function that applies them in the correct order. -/
partial def parseSuffixes : ParserM TypeWrap := do
  if ← isPunct "[" then
    advance
    let size? ← match (← peek!).kind with
      | .punct "]" => pure none
      | .intLit r  => advance; pure (some r.toNat!)
      | _ => err "expected array size or `]`"
    consumePunct "]"
    let rest ← parseSuffixes
    -- For `[3][5]`, the LEFT suffix is the OUTER wrapper. So `rest`
    -- (which represents the trailing `[5]`) applies first.
    return fun ty => CType.array (rest ty) size?
  else if ← isPunct "(" then
    advance
    let (params, variadic) ← parseParamList
    consumePunct ")"
    let rest ← parseSuffixes
    return fun ty => CType.function (rest ty) params variadic
  else
    pure id

partial def parseParamList : ParserM (Array CParam × Bool) := do
  if ← isPunct ")" then return (#[], false)
  -- explicit `void` for empty list:
  if (← isIdent "void") then
    if let some ⟨.punct ")", _⟩ := (← peek? 1) then
      advance; return (#[], false)
  let mut params : Array CParam := #[]
  let mut variadic := false
  let mut more := true
  while more do
    if ← isPunct "..." then
      advance; variadic := true; more := false
    else
      let spec ← parseDeclSpec
      let baseTy ← spec.materialise
      let (name, wrap) ← parseDeclarator
      let ty := wrap baseTy
      let nameOpt := if name = "" then none else some name
      params := params.push { name := nameOpt, type := ty }
      if ← isPunct "," then advance else more := false
  return (params, variadic)

/-- Lookahead: is the next token a parameter-decl-specifier? -/
partial def startsParamList : ParserM Bool := do
  match (← peek?) with
  | none                  => return false
  | some ⟨.punct ")", _⟩  => return true
  | some ⟨.punct "...", _⟩=> return true
  | some ⟨.ident s, _⟩    =>
    if typeSpecKwds.contains s then return true
    if storageKeywords.contains s   then return true
    if qualifierKeywords.contains s then return true
    isKnownTypedef s
  | _ => return false

end

/-! ## Top level -/

/-- Parse a single top-level declaration (one decl-spec + one or more
declarators terminated by `;`). -/
partial def parseTopDecl : ParserM Unit := do
  let spec ← parseDeclSpec
  if ← isPunct ";" then
    advance; return  -- e.g. a bare `struct foo { ... };` (already pushed)
  let baseTy ← spec.materialise
  let mut more := true
  while more do
    let (name, wrap) ← parseDeclarator
    let ty := wrap baseTy
    if name = "" then err "declaration without a name"
    let decl ←
      match spec.storage with
      | some "typedef" =>
        modify fun s => { s with typedefs := s.typedefs.insert name }
        pure (CDecl.typedef name ty)
      | _ =>
        match ty with
        | .function ret params variadic =>
          pure (CDecl.function name ret params variadic)
        | _ =>
          pure (CDecl.variable name ty)
    modify fun s => { s with decls := s.decls.push decl }
    if ← isPunct "," then advance else more := false
  consumePunct ";"

private def trimBoth (s : String) : String :=
  s.trimAsciiStart.trimAsciiEnd.toString

/-- Recognise a small subset of `#…` lines. -/
private def handlePPLine (raw : String) : ParserM Unit := do
  let trimmed := trimBoth raw
  if trimmed.startsWith "define " then
    let rest := trimBoth (trimmed.drop 7).toString
    let nameChars := rest.toList.takeWhile (fun c => c.isAlphanum || c = '_')
    if !nameChars.isEmpty then
      let name := String.ofList nameChars
      let value := trimBoth (rest.drop name.length).toString
      modify fun s => { s with
        macros := s.macros.insert name,
        decls  := s.decls.push (.macroConst name value) }

/-- Iterate over top-level forms. `allowEndBrace` is set to `true` when
we're inside an `extern "C" { ... }` block; in that case a `}` exits
the loop (the caller consumes it). At the true top level, `}` is an
error. -/
private partial def parseHeaderLoop (allowEndBrace : Bool) : ParserM Unit := do
  match (← ParserM.peek?) with
  | none                  => return
  | some ⟨.eof, _⟩         => return
  | some ⟨.punct "}", _⟩   =>
    if allowEndBrace then return else err "unexpected `}`"
  | some ⟨.ppLine raw, _⟩  =>
    ParserM.advance; handlePPLine raw; parseHeaderLoop allowEndBrace
  | some ⟨.punct ";", _⟩   =>
    ParserM.advance; parseHeaderLoop allowEndBrace
  | some ⟨.ident "extern", _⟩ =>
    -- extern "C" { ... } — a C++ linkage spec we treat as transparent.
    match (← ParserM.peek? 1) with
    | some ⟨.strLit _, _⟩ =>
      ParserM.advance; ParserM.advance  -- consume `extern` and the literal
      if ← isPunct "{" then
        ParserM.advance
        parseHeaderLoop true
        consumePunct "}"
      -- else: it's a single linkage-specified declaration; fall through
      -- to parseTopDecl with the rest of the line. We'll have already
      -- consumed `extern "C"`, so the next token starts a normal decl.
      parseHeaderLoop allowEndBrace
    | _ =>
      parseTopDecl; parseHeaderLoop allowEndBrace
  | _ => parseTopDecl; parseHeaderLoop allowEndBrace

/-- Parse an entire token stream into a `CHeader`. -/
def parseHeader (tokens : Array Token) : Except String CHeader :=
  match (parseHeaderLoop false).run (ParserState.mk' tokens) with
  | .ok _ s    => .ok { decls := s.decls }
  | .error e _ => .error e

end LeanBindgen.C
