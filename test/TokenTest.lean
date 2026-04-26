import LeanBindgen.C.Token

open LeanBindgen.C

private def renderToken : Token → String
  | ⟨.ident s,    _⟩ => s!"id({s})"
  | ⟨.intLit r,   _⟩ => s!"int({r})"
  | ⟨.floatLit r, _⟩ => s!"flt({r})"
  | ⟨.strLit s,   _⟩ => s!"str({s})"
  | ⟨.charLit r,  _⟩ => s!"chr({r})"
  | ⟨.punct s,    _⟩ => s!"`{s}`"
  | ⟨.ppLine s,   _⟩ => s!"#[{s}]"
  | ⟨.eof,        _⟩ => "<eof>"

private def assertEq (label got expected : String) : IO Unit := do
  if got = expected then IO.println s!"  ✓ {label}"
  else
    IO.eprintln s!"  ✗ {label}"
    IO.eprintln s!"      expected: {expected}"
    IO.eprintln s!"      got:      {got}"

private def go (src : String) : IO String := do
  match tokenize src with
  | .error e => return s!"ERR: {e}"
  | .ok toks => return " ".intercalate (toks.toList.map renderToken)

def main : IO Unit := do
  IO.println "Tokenizer smoke tests"
  -- 1. punctuation + identifier
  assertEq "simple decl"
    (← go "int x;")
    "id(int) id(x) `;` <eof>"
  -- 2. line + block comments
  assertEq "comments"
    (← go "// hello\nint /* block */ y ;")
    "id(int) id(y) `;` <eof>"
  -- 3. string + char literals + escapes
  assertEq "literals"
    (← go "\"hello\\n\" 'a' '\\t'")
    "str(hello\n) chr('a') chr('\\t') <eof>"
  -- 4. numeric literals
  assertEq "numbers"
    (← go "0 42 0xFF 0xDEADbeef 3.14 1.0e-5 5UL")
    "int(0) int(42) int(0xFF) int(0xDEADbeef) flt(3.14) flt(1.0e-5) int(5UL) <eof>"
  -- 5. multi-char punctuators
  assertEq "operators"
    (← go "a -> b ; x == y && z != 0;")
    "id(a) `->` id(b) `;` id(x) `==` id(y) `&&` id(z) `!=` int(0) `;` <eof>"
  -- 6. preprocessor passthrough
  assertEq "preprocessor"
    (← go "#define FOO 42\n#ifdef BAR\nint x;\n#endif")
    "#[define FOO 42] #[ifdef BAR] id(int) id(x) `;` #[endif] <eof>"
  -- 7. function-pointer typedef from clingo.h
  assertEq "clingo_logger_t"
    (← go "typedef void (*clingo_logger_t)(int code, char const *msg, void *data);")
    ("id(typedef) id(void) `(` `*` id(clingo_logger_t) `)` `(` " ++
     "id(int) id(code) `,` id(char) id(const) `*` id(msg) `,` " ++
     "id(void) `*` id(data) `)` `;` <eof>")
