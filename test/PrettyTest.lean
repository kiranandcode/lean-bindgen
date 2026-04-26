import LeanBindgen.C.Ast
import LeanBindgen.C.Pretty

open LeanBindgen.C

/-! Hand-built AST values for representative declarations from clingo.h
    style headers; checks that the pretty-printer emits the canonical C
    syntax we'd want in generated shim/header output. -/

private def assertEq (label : String) (got expected : String) : IO Unit := do
  if got = expected then
    IO.println s!"  ✓ {label}"
  else
    IO.eprintln s!"  ✗ {label}"
    IO.eprintln s!"      expected: {expected}"
    IO.eprintln s!"      got:      {got}"

-- --- declarations ---

private def d_intVar : CDecl := .variable "x" (.scalar .int)

private def d_constCharStr : CDecl :=
  .variable "s" (.pointer (.const (.scalar .char)))

private def d_charConstPtr : CDecl :=
  .variable "p" (.const (.pointer (.scalar .char)))

private def d_arr : CDecl :=
  .variable "arr" (.array (.scalar .int) (some 10))

private def d_arrOfPtrs : CDecl :=
  .variable "arr" (.array (.pointer (.scalar .int)) (some 10))

private def d_funPtr : CDecl :=
  .typedef "fn_t"
    (.pointer (.function (.scalar .int) #[⟨none, .scalar .int⟩]))

private def d_clingoLogger : CDecl :=
  .typedef "clingo_logger_t"
    (.pointer (.function .void
      #[ ⟨some "code",    .typedef "clingo_warning_t"⟩,
         ⟨some "message", .pointer (.const (.scalar .char))⟩,
         ⟨some "data",    .pointer .void⟩ ]))

private def d_structFoo : CDecl :=
  .structDef (some "foo")
    #[ ⟨"x",    .scalar .int,                              none⟩,
       ⟨"name", .pointer (.const (.scalar .char)),         none⟩,
       ⟨"flags", .scalar .int .unsigned,                   some 8⟩ ]

private def d_enumColor : CDecl :=
  .enumDef (some "color")
    #[ ("RED",   none),
       ("GREEN", some 2),
       ("BLUE",  none) ]

private def d_proto : CDecl :=
  .function "clingo_signature_create"
    (.scalar .bool)
    #[ ⟨some "name",      .pointer (.const (.scalar .char))⟩,
       ⟨some "arity",     .typedef "uint32_t"⟩,
       ⟨some "positive",  .scalar .bool⟩,
       ⟨some "signature", .pointer (.typedef "clingo_signature_t")⟩ ]

private def d_variadic : CDecl :=
  .function "printf"
    (.scalar .int)
    #[ ⟨some "fmt", .pointer (.const (.scalar .char))⟩ ]
    (variadic := true)

def main : IO Unit := do
  IO.println "Pretty-printer tests"
  assertEq "int variable"        d_intVar.toC        "int x;"
  assertEq "const char *"        d_constCharStr.toC  "char const *s;"
  assertEq "char *const"         d_charConstPtr.toC  "char *const p;"
  assertEq "int[10]"             d_arr.toC           "int arr[10];"
  assertEq "int *arr[10]"        d_arrOfPtrs.toC     "int *arr[10];"
  assertEq "fn pointer typedef"  d_funPtr.toC
    "typedef int (*fn_t)(int);"
  assertEq "clingo_logger_t"     d_clingoLogger.toC
    "typedef void (*clingo_logger_t)(clingo_warning_t code, char const *message, void *data);"
  assertEq "struct foo"          d_structFoo.toC
    "struct foo {\n  int x;\n  char const *name;\n  unsigned int flags : 8;\n};"
  assertEq "enum color"          d_enumColor.toC
    "enum color {\n  RED,\n  GREEN = 2,\n  BLUE\n};"
  assertEq "function prototype"  d_proto.toC
    "_Bool clingo_signature_create(char const *name, uint32_t arity, _Bool positive, clingo_signature_t *signature);"
  assertEq "variadic"            d_variadic.toC
    "int printf(char const *fmt, ...);"
