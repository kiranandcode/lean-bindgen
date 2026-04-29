import LeanBindgen.Gen.LeanDecl

/-!
# Lean Declaration AST → String

Renders `LeanDecl` and `LeanModule` to Lean source text.
-/

namespace LeanBindgen.Gen

/-- Lean 4 keywords and command names that must be escaped with «» when
used as struct field names or constructor names. -/
private def leanKeywords : List String :=
  ["variable", "def", "theorem", "lemma", "axiom", "structure", "class",
   "instance", "inductive", "where", "let", "do", "if", "then", "else",
   "match", "return", "for", "while", "break", "continue", "import",
   "open", "end", "section", "namespace", "mutual", "opaque", "private",
   "protected", "noncomputable", "unsafe", "partial", "macro", "syntax",
   "elab", "set_option", "attribute", "deriving", "extends", "with",
   "fun", "have", "show", "suffices", "assume", "type", "sort", "Prop",
   "Type", "Sort"]

/-- Escape a name with «» if it is a Lean keyword. -/
private def escapeKeyword (s : String) : String :=
  if leanKeywords.contains s then "«" ++ s ++ "»" else s

private def renderDerivings (ds : Array String) : String :=
  if ds.isEmpty then ""
  else "\n  deriving " ++ ", ".intercalate ds.toList

partial def LeanDecl.render : LeanDecl → String
  | .defAlias name body derivings =>
    "def " ++ name ++ " := " ++ body ++ renderDerivings derivings
  | .opaque_ name type =>
    "opaque " ++ name ++ " : " ++ type
  | .inductive_ name ctors derivings =>
    let ctorLines := ctors.toList.map fun (c, payload?) =>
      match payload? with
      | none   => "  | " ++ c
      | some p => "  | " ++ c ++ " " ++ p
    "inductive " ++ name ++ " where\n" ++
      "\n".intercalate ctorLines ++
      renderDerivings derivings
  | .structure_ name fields derivings =>
    let fieldLines := fields.toList.map fun (f, ty) =>
      "  " ++ escapeKeyword f ++ " : " ++ ty
    "structure " ++ name ++ " where\n" ++
      "\n".intercalate fieldLines ++
      renderDerivings derivings
  | .externOpaque externSym name type =>
    "@[extern \"" ++ externSym ++ "\"]\nopaque " ++ name ++ " : " ++ type
  | .mutual_ decls =>
    "mutual\n\n" ++
      "\n\n".intercalate (decls.toList.map LeanDecl.render) ++
      "\n\nend"
  | .comment text => "-- " ++ text
  | .blank => ""

def LeanModule.render (m : LeanModule) : String :=
  let parts : Array String := #[]
  let parts := match m.headerComment with
    | some c => parts.push ("-- " ++ c)
    | none   => parts
  let parts := m.imports.foldl (fun acc i => acc.push ("import " ++ i)) parts
  let parts := match m.namespace_ with
    | some ns => parts.push ("\nnamespace " ++ ns)
    | none    => parts
  let parts := m.decls.foldl (fun acc d => acc.push (LeanDecl.render d)) parts
  let parts := match m.namespace_ with
    | some ns => parts.push ("end " ++ ns)
    | none    => parts
  "\n\n".intercalate parts.toList

end LeanBindgen.Gen
