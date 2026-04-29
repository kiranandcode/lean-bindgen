/-!
# C Shim Output AST

Purpose-built C subset covering only the patterns the bindgen emits.
Types are `String` (not `CType`) since resolution already produces C
type spellings.
-/

namespace LeanBindgen.Gen

/-- A C expression (no side effects, evaluates to a value). -/
inductive CExpr where
  | var       (name : String)
  | intLit    (value : Int)
  | strLit    (value : String)
  | null
  | call      (fn : String) (args : Array CExpr)
  | field     (obj : CExpr) (name : String)          -- obj.name
  | arrow     (obj : CExpr) (name : String)          -- obj->name
  | subscript (arr : CExpr) (idx : CExpr)            -- arr[idx]
  | deref     (e : CExpr)                            -- *e
  | addrOf    (e : CExpr)                            -- &e
  | cast      (ty : String) (e : CExpr)              -- (ty)e
  | binop     (op : String) (lhs rhs : CExpr)        -- lhs op rhs
  | unop      (op : String) (e : CExpr)              -- op e  (prefix)
  | ternary   (cond thenE elseE : CExpr)             -- cond ? then : else
  | sizeof    (ty : String)                           -- sizeof(ty)
  | paren     (e : CExpr)                            -- (e)
  | raw       (s : String)                            -- escape hatch
  deriving Inhabited

/-- A C statement. Empty arrays mean "absent" for optional branches. -/
inductive CStmt where
  | varDecl   (ty : String) (name : String) (init : Option CExpr := none)
  | assign    (lhs : CExpr) (rhs : CExpr)
  | assignOp  (op : String) (lhs : CExpr) (rhs : CExpr)  -- lhs op= rhs
  | expr      (e : CExpr)                                  -- e;
  | ret       (e : Option CExpr := none)                   -- return e;
  /-- if/else. `elseBody = #[]` means no else branch. -/
  | ifElse    (cond : CExpr) (thenBody : Array CStmt)
              (elseBody : Array CStmt)
  /-- switch. `default = #[]` means no default case. -/
  | switch    (e : CExpr) (cases : Array (CExpr × Array CStmt))
              (default : Array CStmt)
  | forLoop   (init : String) (cond : String) (step : String)
              (body : Array CStmt)
  | block     (stmts : Array CStmt)
  | comment   (text : String)
  | blank
  | raw       (s : String)

instance : Inhabited CStmt := ⟨.blank⟩

/-- Linkage / visibility for a top-level function. -/
inductive CLinkage where
  | leanExport   -- LEAN_EXPORT
  | static_      -- static
  | plain        -- no qualifier
  deriving Inhabited, BEq

/-- A C function parameter. -/
structure CParam where
  ty   : String
  name : String
  deriving Inhabited

/-- A C top-level declaration. -/
inductive CTopLevel where
  | include     (path : String) (system : Bool := true)
  | forwardDecl (sig : String)
  | globalVar   (ty : String) (name : String) (init : Option String := none)
  | funcDef     (linkage : CLinkage) (retTy : String)
                (name : String) (params : Array CParam)
                (body : Array CStmt)
  | comment     (text : String)
  | blank
  | raw         (s : String)

instance : Inhabited CTopLevel := ⟨.blank⟩

/-- A complete C shim file. -/
def CShimFile := Array CTopLevel
  deriving Inhabited

end LeanBindgen.Gen
