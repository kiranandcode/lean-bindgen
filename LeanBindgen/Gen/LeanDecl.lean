/-!
# Lean Declaration Output AST

Describes the Lean declarations emitted by bindgen: type definitions,
extern opaques, and module structure.
-/

namespace LeanBindgen.Gen

/-- A single Lean declaration. -/
inductive LeanDecl where
  /-- `def Foo := Bar  deriving ...` -/
  | defAlias    (name : String) (body : String)
                (derivings : Array String := #[])
  /-- `opaque Foo : Type` -/
  | opaque_     (name : String) (type : String)
  /-- `inductive Foo where | ctor1 | ctor2 (x : T)  deriving ...` -/
  | inductive_  (name : String)
                (ctors : Array (String × Option String))  -- (ctor, payload?)
                (derivings : Array String := #[])
  /-- `structure Foo where  field1 : T1  field2 : T2  deriving ...` -/
  | structure_  (name : String)
                (fields : Array (String × String))        -- (field, type)
                (derivings : Array String := #[])
  /-- `@[extern "sym"] opaque name : type` -/
  | externOpaque (externSym : String) (name : String) (type : String)
  /-- `mutual ... end` wrapping multiple declarations -/
  | mutual_     (decls : Array LeanDecl)
  /-- `def name : type := value` — a constant definition -/
  | constDef    (name : String) (type : String) (value : String)
  /-- `-- comment` -/
  | comment     (text : String)
  /-- blank line -/
  | blank

instance : Inhabited LeanDecl := ⟨.blank⟩

/-- A complete Lean module. -/
structure LeanModule where
  headerComment : Option String := none
  imports       : Array String := #[]
  namespace_    : Option String := none
  decls         : Array LeanDecl := #[]
  deriving Inhabited

end LeanBindgen.Gen
