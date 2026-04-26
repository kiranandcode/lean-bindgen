/-!
# C tokenizer

Hand-rolled tokenizer for the subset of C we care about (headers,
declarations only). Skips whitespace and comments, preserves source
position for diagnostics, and surfaces preprocessor lines verbatim so
the preprocessor pass can dispatch on them before the grammar parser
sees them.
-/

namespace LeanBindgen.C

/-- A 1-indexed source position. -/
structure SrcPos where
  line : Nat
  col  : Nat
  deriving Repr, Inhabited, BEq

instance : ToString SrcPos where
  toString p := s!"{p.line}:{p.col}"

/-- Token payload. Most punctuation and operators are coalesced into
`punct` with their literal text; the parser dispatches on the string. -/
inductive TokKind where
  /-- Identifier or keyword. The parser distinguishes keywords by string. -/
  | ident   (s : String)
  /-- Integer literal, in decimal/hex/octal; raw text preserved. -/
  | intLit  (raw : String)
  /-- Floating literal, raw text preserved. -/
  | floatLit (raw : String)
  /-- String literal *contents* (escapes resolved). -/
  | strLit  (s : String)
  /-- Char literal, as the literal source text (e.g. `'\n'`). -/
  | charLit (raw : String)
  /-- Punctuation/operator token, as literal text. -/
  | punct   (s : String)
  /-- A preprocessor directive line (everything from `#` to the end of
  line, *without* the leading `#`). The preprocessor pass interprets it
  before the grammar parser ever sees it. -/
  | ppLine  (s : String)
  | eof
  deriving Repr, Inhabited

structure Token where
  kind : TokKind
  pos  : SrcPos
  deriving Repr, Inhabited

/-- C punctuators, sorted *longest first* so the matcher prefers `<<=`
over `<<` over `<`. Kept short — we don't care about `<:` digraphs. -/
private def punctuators : List String := [
  "...", "<<=", ">>=",
  "==", "!=", "<=", ">=", "&&", "||", "<<", ">>",
  "->", "++", "--", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=",
  "(", ")", "{", "}", "[", "]", ",", ";", ":", "?", "~", "!",
  "+", "-", "*", "/", "%", "&", "|", "^", "=", "<", ">", "."
]

private def isIdStart (c : Char) : Bool := c.isAlpha || c == '_'
private def isIdCont  (c : Char) : Bool := c.isAlphanum || c == '_'

private structure St where
  src  : String
  pos  : Nat
  line : Nat
  col  : Nat
  deriving Inhabited

private def St.rawPos (s : St) : String.Pos.Raw := ⟨s.pos⟩
private def St.atEnd  (s : St) : Bool := s.rawPos.atEnd s.src
private def St.peek?  (s : St) : Option Char :=
  if s.atEnd then none else some (s.rawPos.get s.src)
private def St.peekAt? (s : St) (off : Nat) : Option Char :=
  let p := { s with pos := s.pos + off }
  p.peek?

/-- Advance the cursor by one Unicode scalar, updating line/col. -/
private def St.advance (s : St) : St :=
  match s.peek? with
  | none      => s
  | some '\n' => { s with pos := (s.rawPos.next s.src).byteIdx, line := s.line + 1, col := 1 }
  | some _    => { s with pos := (s.rawPos.next s.src).byteIdx, col := s.col + 1 }

private def St.advanceN (s : St) : Nat → St
  | 0     => s
  | n + 1 => s.advance.advanceN n

private def St.srcPos (s : St) : SrcPos := { line := s.line, col := s.col }
private def St.extract (s : St) (start : Nat) : String :=
  String.Pos.Raw.extract s.src ⟨start⟩ s.rawPos

/-- Skip whitespace AND C comments (`// ...` and `/* ... */`).
Stops at a `\n` if `stopOnNewline` is set — used by the preprocessor-
line scanner. -/
private partial def skipWS (s : St) (stopOnNewline := false) : St :=
  match s.peek? with
  | none => s
  | some c =>
    if c == '\n' && stopOnNewline then s
    else if c.isWhitespace then skipWS s.advance stopOnNewline
    else if c == '/' then
      match s.peekAt? 1 with
      | some '/' =>
        let s := skipLineComment (s.advanceN 2)
        skipWS s stopOnNewline
      | some '*' =>
        let s := skipBlockComment (s.advanceN 2)
        skipWS s stopOnNewline
      | _ => s
    else
      s
where
  skipLineComment (s : St) : St :=
    match s.peek? with
    | none      => s
    | some '\n' => s
    | some _    => skipLineComment s.advance
  skipBlockComment (s : St) : St :=
    match s.peek? with
    | none      => s
    | some '*'  =>
      match s.peekAt? 1 with
      | some '/' => s.advanceN 2
      | _        => skipBlockComment s.advance
    | some _    => skipBlockComment s.advance

/-- Consume a continuous run of chars matching `p`, returning the run as
a string. -/
private partial def takeWhile (s : St) (p : Char → Bool) : St × String :=
  let start := s.pos
  let rec go (s : St) : St :=
    match s.peek? with
    | some c => if p c then go s.advance else s
    | none   => s
  let s' := go s
  (s', s'.extract start)

/-- Read up to (but not including) the next newline; return the run.
Used for preprocessor lines. Backslash-newline continuations are joined. -/
private partial def takeLine (s : St) : St × String :=
  let rec go (s : St) (acc : String) : St × String :=
    match s.peek? with
    | none      => (s, acc)
    | some '\n' => (s, acc)
    | some '\\' =>
      match s.peekAt? 1 with
      | some '\n' =>
        -- line continuation: skip both and continue
        go (s.advanceN 2) (acc ++ " ")
      | _ =>
        go s.advance (acc.push '\\')
    | some c    => go s.advance (acc.push c)
  go s ""

/-- Read a string literal body (after the opening `"`); return the
escaped contents and the state past the closing `"`. -/
private partial def readStrLit (s : St) : Except String (St × String) :=
  let rec go (s : St) (acc : String) : Except String (St × String) :=
    match s.peek? with
    | none      => .error s!"unterminated string literal at {s.srcPos}"
    | some '"'  => .ok (s.advance, acc)
    | some '\\' =>
      match s.peekAt? 1 with
      | some 'n'  => go (s.advanceN 2) (acc.push '\n')
      | some 't'  => go (s.advanceN 2) (acc.push '\t')
      | some 'r'  => go (s.advanceN 2) (acc.push '\r')
      | some '"'  => go (s.advanceN 2) (acc.push '"')
      | some '\\' => go (s.advanceN 2) (acc.push '\\')
      | some '0'  => go (s.advanceN 2) (acc.push (Char.ofNat 0))
      | some c    => go (s.advanceN 2) (acc.push c)
      | none      => .error "unterminated string literal at backslash"
    | some c    => go s.advance (acc.push c)
  go s ""

/-- Read a char literal, returning the raw source text including quotes. -/
private partial def readCharLit (s : St) : Except String (St × String) := do
  -- caller already consumed the opening '
  let start := s.pos
  let rec go (s : St) : Except String St :=
    match s.peek? with
    | none      => .error s!"unterminated char literal at {s.srcPos}"
    | some '\\' => go (s.advanceN 2)
    | some '\'' => .ok s.advance
    | _         => go s.advance
  let s' ← go s
  let inner := String.Pos.Raw.extract s.src ⟨start⟩ ⟨s'.pos - 1⟩
  .ok (s', "'" ++ inner ++ "'")

/-- Try to match the longest punctuator at the current position. -/
private def tryPunct (s : St) : Option (St × String) :=
  let rest := String.Pos.Raw.extract s.src s.rawPos ⟨s.src.utf8ByteSize⟩
  punctuators.foldl (init := none) fun acc tok =>
    match acc with
    | some _ => acc
    | none   => if rest.startsWith tok then some (s.advanceN tok.length, tok) else none

/-- Match a numeric literal (integer or float). Conservative: detects
the common forms — decimal, hex (`0x...`), octal-style leading-zero,
and a trailing `.fraction` / `eExp` for floats. Type-suffixes (`u`,
`UL`, `LL`, `f`) are kept in the raw text. -/
private partial def readNumber (s : St) : St × TokKind :=
  let start := s.pos
  let isHex :=
    s.peek? == some '0' && (s.peekAt? 1 == some 'x' || s.peekAt? 1 == some 'X')
  if isHex then
    let s := s.advanceN 2
    let (s, _) := takeWhile s (fun c => c.isDigit ||
                                          ('a' ≤ c && c ≤ 'f') ||
                                          ('A' ≤ c && c ≤ 'F'))
    let (s, _) := takeWhile s (fun c => "uUlL".contains c)
    (s, .intLit (s.extract start))
  else
    let (s, _) := takeWhile s Char.isDigit
    let (s, isFloat) :=
      match s.peek?, s.peekAt? 1 with
      | some '.', some d2 =>
        if d2.isDigit then
          let s := s.advance
          let (s, _) := takeWhile s Char.isDigit
          (s, true)
        else (s, false)
      | _, _ => (s, false)
    let (s, isFloat) :=
      match s.peek? with
      | some 'e' | some 'E' =>
        let s := s.advance
        let s := match s.peek? with | some '+' | some '-' => s.advance | _ => s
        let (s, _) := takeWhile s Char.isDigit
        (s, true)
      | _ => (s, isFloat)
    let (s, _) := takeWhile s (fun c => "uUlLfF".contains c)
    let raw := s.extract start
    if isFloat then (s, .floatLit raw) else (s, .intLit raw)

/-- Single-token step. Returns the next `Token` and the advanced state.
On unrecognised input, produces a `punct` of length 1 to avoid getting
stuck — the parser will surface a better diagnostic. -/
private def nextToken (s : St) : Except String (St × Token) := do
  let s := skipWS s
  let pos := s.srcPos
  match s.peek? with
  | none =>
    .ok (s, ⟨.eof, pos⟩)
  | some '#' =>
    -- preprocessor line: capture from after # to end of line
    let (s, line) := takeLine s.advance
    .ok (s, ⟨.ppLine line, pos⟩)
  | some '"' =>
    let (s', body) ← readStrLit s.advance
    .ok (s', ⟨.strLit body, pos⟩)
  | some '\'' =>
    let (s', raw) ← readCharLit s.advance
    .ok (s', ⟨.charLit raw, pos⟩)
  | some c =>
    if c.isDigit then
      let (s', kind) := readNumber s
      .ok (s', ⟨kind, pos⟩)
    else if isIdStart c then
      let (s', name) := takeWhile s isIdCont
      .ok (s', ⟨.ident name, pos⟩)
    else
      match tryPunct s with
      | some (s', tok) => .ok (s', ⟨.punct tok, pos⟩)
      | none =>
        .error s!"unexpected character `{c}` at {pos}"

/-- Tokenize an entire source string. Yields tokens up to and including
a final `.eof` token. -/
partial def tokenize (src : String) : Except String (Array Token) :=
  let init : St := { src, pos := 0, line := 1, col := 1 }
  go init #[]
where
  go (s : St) (acc : Array Token) : Except String (Array Token) := do
    let (s', t) ← nextToken s
    let acc := acc.push t
    match t.kind with
    | .eof => .ok acc
    | _    => go s' acc

end LeanBindgen.C
