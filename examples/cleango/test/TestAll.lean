import Clingo
import Clingo.Lang

open Clingo

/-! Full workflow test using both string API and DSL macro. -/

def collectModel (evt : SolveEvent) (ref : IO.Ref (Array String)) : IO Bool := do
  match evt with
  | .model (.some m) =>
    let .ok symbols ← Model.symbols m | return true
    let names ← symbols.mapM fun sym => pure s!"{Symbol.toRepr sym}"
    ref.modify (· ++ names)
    return true
  | _ => return true

def hasSubstr (s sub : String) : Bool :=
  (s.splitOn sub).length != 1

def main : IO Unit := do
  -- Test 1: String API (existing)
  IO.println "=== Test 1: String API ==="
  do
    let .ok control ← Control.mk (args := #[])
      | throw (IO.userError "failed to create control")
    let .ok () ← Control.add control "base" #[] "p(X) :- q(X). q(a). q(b)."
      | throw (IO.userError "failed to add program")
    let .ok () ← Control.ground control #[⟨"base", #[]⟩] (fun _ _ _ _ => pure true)
      | throw (IO.userError "failed to ground")
    let ref ← IO.mkRef #[]
    let .ok handle ← Control.solve control SolveMode.Neither #[] (collectModel · ref)
      | throw (IO.userError "failed to solve")
    let _ ← SolveHandle.wait handle (-1.0)
    let syms ← ref.get
    IO.println s!"  symbols: {syms}"
    assert! syms.any (hasSubstr · "q(a)")
    assert! syms.any (hasSubstr · "q(b)")
    assert! syms.any (hasSubstr · "p(a)")
    assert! syms.any (hasSubstr · "p(b)")
    IO.println "  PASS"

  -- Test 2: DSL macro (add_clingo_query!)
  IO.println "=== Test 2: DSL Macro ==="
  do
    let .ok control ← Control.mk (args := #[])
      | throw (IO.userError "failed to create control")
    -- Use the DSL macro to add: q(a). q(b). p(X) :- q(X).
    add_clingo_query! control :
      q(a).
      q(b).
      p(X) :- q(X).
    let .ok () ← Control.ground control #[⟨"base", #[]⟩] (fun _ _ _ _ => pure true)
      | throw (IO.userError "failed to ground")
    let ref ← IO.mkRef #[]
    let .ok handle ← Control.solve control SolveMode.Neither #[] (collectModel · ref)
      | throw (IO.userError "failed to solve")
    let _ ← SolveHandle.wait handle (-1.0)
    let syms ← ref.get
    IO.println s!"  symbols: {syms}"
    assert! syms.any (hasSubstr · "q(a)")
    assert! syms.any (hasSubstr · "q(b)")
    assert! syms.any (hasSubstr · "p(a)")
    assert! syms.any (hasSubstr · "p(b)")
    IO.println "  PASS"

  IO.println "All tests passed!"
