import LeanBindgen.C.Ast

/-!
# Pretty-printing C AST → C source

The non-obvious part of C is that the *name* sits inside the type
(`int (*foo[3])(int)` parses as "foo is array-3 of pointer to function
returning int"). We render with the canonical K&R algorithm: walk the
type outward, splicing the name into the right slot, and parenthesising
when a pointer's target is an array or function.

Helpers:
- `Type.spec`: leading type-specifier text (`int`, `struct foo`, etc.).
  This is what stands alone for an abstract declarator with no name.
- `Type.declarator`: the type wrapped around a (possibly empty) name,
  emitting the right pointer/array/function syntax.

For full declarations we offer `Decl.toC` which terminates each form
appropriately (semicolons, braces).
-/

namespace LeanBindgen.C

/-- Strip outer qualifier wrappers and report what they were. -/
private def peelQualifiers : CType → (Bool × Bool × CType)
  | .const t   =>
    let (_, v, inner) := peelQualifiers t
    (true, v, inner)
  | .volatile t =>
    let (c, _, inner) := peelQualifiers t
    (c, true, inner)
  | t => (false, false, t)

private def ScalarKind.toC : ScalarKind → String
  | .char     => "char"
  | .short    => "short"
  | .int      => "int"
  | .long     => "long"
  | .longLong => "long long"
  | .float    => "float"
  | .double   => "double"
  | .bool     => "_Bool"

private def Signedness.prefix? : Signedness → Option String
  | .signed       => some "signed"
  | .unsigned     => some "unsigned"
  | .unspecified  => none

private def joinSpace (parts : List String) : String :=
  String.intercalate " " (parts.filter (· ≠ ""))

mutual

/-- Render the leading type-specifier (everything that appears *before*
the name in a declaration). -/
partial def CType.spec : CType → String
  | .void            => "void"
  | .scalar k s      =>
    match s.prefix? with
    | some sp => sp ++ " " ++ k.toC
    | none    => k.toC
  | .typedef n       => n
  | .structRef tag   => "struct " ++ tag
  | .unionRef tag    => "union " ++ tag
  | .enumRef tag     => "enum " ++ tag
  | .const t         => joinSpace [t.spec, "const"]
  | .volatile t      => joinSpace [t.spec, "volatile"]
  | .pointer t       => CType.declarator (.pointer t) ""
  | t@(.array ..)    => CType.declarator t ""
  | t@(.function ..) => CType.declarator t ""

/--
Render `ty` as a declarator wrapping `name`. `name` may be empty for an
abstract declarator (e.g. inside a parameter type list).

`.const (.pointer t)` is handled before the bare `.pointer` case so a
const-qualified *pointer* (as opposed to a pointer-to-const) glues the
qualifier onto the `*` correctly: `char *const p`. We achieve that by
prepending `const ` to the `name` *before* the pointer arm wraps it
with `*` — `*` then immediately precedes `const`.
-/
partial def CType.declarator : CType → String → String
  | .const (.pointer t), name    =>
    let nameQ := if name == "" then "const" else "const " ++ name
    CType.declarator (.pointer t) nameQ
  | .volatile (.pointer t), name =>
    let nameQ := if name == "" then "volatile" else "volatile " ++ name
    CType.declarator (.pointer t) nameQ
  | .pointer t, name =>
    -- Parenthesise when target is an array or function.
    let needsParens := match (peelQualifiers t).snd.snd with
      | .array .. | .function .. => true
      | _                        => false
    let inner := if needsParens then "(*" ++ name ++ ")" else "*" ++ name
    CType.declarator t inner
  | .array elt size, name =>
    let szStr := match size with | some n => toString n | none => ""
    CType.declarator elt (name ++ "[" ++ szStr ++ "]")
  | .function ret params variadic, name =>
    let paramStr :=
      if params.isEmpty && !variadic then "void"
      else
        let parts := params.toList.map fun p =>
          match p.name with
          | some n => p.type.declarator n
          | none   => p.type.spec
        let parts := if variadic then parts ++ ["..."] else parts
        ", ".intercalate parts
    CType.declarator ret (name ++ "(" ++ paramStr ++ ")")
  | .const t, name =>
    joinSpace [t.spec, "const"] ++ (if name == "" then "" else " " ++ name)
  | .volatile t, name =>
    joinSpace [t.spec, "volatile"] ++ (if name == "" then "" else " " ++ name)
  | t, name =>
    if name == "" then t.spec else t.spec ++ " " ++ name

end

/-- Render a single field (`int x;`, `char *name;`, `int bits : 5;`). -/
def CField.toC (f : CField) : String :=
  let body := f.type.declarator f.name
  match f.bitfield with
  | none   => body ++ ";"
  | some n => body ++ " : " ++ toString n ++ ";"

/-- Render a struct/union body (the brace-enclosed field list). -/
def renderFieldsBlock (fields : Array CField) : String :=
  if fields.isEmpty then "{ }"
  else
    let lines := fields.toList.map fun f => "  " ++ f.toC
    "{\n" ++ "\n".intercalate lines ++ "\n}"

private def renderEnumBody (variants : Array (String × Option Int)) : String :=
  if variants.isEmpty then "{ }"
  else
    let lines := variants.toList.map fun (name, val?) =>
      match val? with
      | some v => "  " ++ name ++ " = " ++ toString v
      | none   => "  " ++ name
    "{\n" ++ ",\n".intercalate lines ++ "\n}"

/-- Render a top-level declaration. -/
def CDecl.toC : CDecl → String
  | .typedef name ty =>
    "typedef " ++ ty.declarator name ++ ";"
  | .structDef tag? fields =>
    let head := match tag? with
      | some t => "struct " ++ t ++ " "
      | none   => "struct "
    head ++ renderFieldsBlock fields ++ ";"
  | .unionDef tag? fields =>
    let head := match tag? with
      | some t => "union " ++ t ++ " "
      | none   => "union "
    head ++ renderFieldsBlock fields ++ ";"
  | .enumDef tag? variants =>
    let head := match tag? with
      | some t => "enum " ++ t ++ " "
      | none   => "enum "
    head ++ renderEnumBody variants ++ ";"
  | .function name ret params variadic =>
    let fnTy : CType := .function ret params variadic
    fnTy.declarator name ++ ";"
  | .variable name ty =>
    ty.declarator name ++ ";"
  | .macroConst name val =>
    "#define " ++ name ++ " " ++ val

/-- Render an entire header — declarations separated by blank lines. -/
def CHeader.toC (h : CHeader) : String :=
  "\n\n".intercalate (h.decls.toList.map CDecl.toC)

instance : ToString CType  where toString t := t.spec
instance : ToString CDecl  where toString := CDecl.toC
instance : ToString CHeader where toString := CHeader.toC

end LeanBindgen.C
