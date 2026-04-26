import LeanBindgen.C.Token
import LeanBindgen.C.Parser
import LeanBindgen.C.Pretty

open LeanBindgen.C

private def parseAndPrint (src : String) : IO String := do
  match tokenize src with
  | .error e => return s!"TOKEN-ERR: {e}"
  | .ok toks =>
    match parseHeader toks with
    | .error e => return s!"PARSE-ERR: {e}"
    | .ok h    => return ((CHeader.toC h).trim)

private def assertEq (label got expected : String) : IO Unit := do
  if got = expected then IO.println s!"  ✓ {label}"
  else
    IO.eprintln s!"  ✗ {label}"
    IO.eprintln s!"      expected: {expected.replace "\n" "⏎"}"
    IO.eprintln s!"      got:      {got.replace "\n" "⏎"}"

def main : IO Unit := do
  IO.println "Parser → pretty-printer round-trip tests"

  -- 1. simple typedef
  assertEq "typedef int"
    (← parseAndPrint "typedef int my_int;")
    "typedef int my_int;"

  -- 2. typedef pointer
  assertEq "typedef pointer"
    (← parseAndPrint "typedef int *intptr_t;")
    "typedef int *intptr_t;"

  -- 3. function pointer typedef (the clingo_logger_t shape)
  assertEq "fn-ptr typedef (variadic-free)"
    (← parseAndPrint "typedef void (*clingo_logger_t)(int code, char const *msg, void *data);")
    "typedef void (*clingo_logger_t)(int code, char const *msg, void *data);"

  -- 4. function prototype
  assertEq "function prototype"
    (← parseAndPrint "void foo(int x, char *y);")
    "void foo(int x, char *y);"

  -- 5. struct definition (then a typedef to it would normally follow)
  assertEq "struct def"
    (← parseAndPrint "struct point { int x; int y; };")
    "struct point {\n  int x;\n  int y;\n};"

  -- 6. enum definition
  assertEq "enum def"
    (← parseAndPrint "enum color { RED, GREEN = 2, BLUE };")
    "enum color {\n  RED,\n  GREEN = 2,\n  BLUE\n};"

  -- 7. typedef chained after a defined struct → typedef-name resolves
  -- (the struct is only referenced, never defined, so no struct-def
  -- is emitted; the pretty-printer renders an empty param list as
  -- `(void)`.)
  assertEq "typedef of struct"
    (← parseAndPrint "typedef struct foo foo_t;\nfoo_t make_foo(void);")
    "typedef struct foo foo_t;\n\nfoo_t make_foo(void);"
