import Clingo

open Clingo

/-! Full workflow test using the generated AST types directly. -/

def handle_solve_event (evt : SolveEvent) : IO Bool := do
  match evt with
  | .model (.some m) =>
    IO.println "found model:"
    let .ok symbols ← Model.symbols m | IO.println "  (symbols failed)"; return true
    for sym in symbols do
      IO.println s!"  - {repr (Symbol.toRepr sym)}"
    return true
  | _ => return true

def main : IO Unit := do
  let .ok control ← Control.mk (args := #[])
    | throw (IO.userError "failed to create control")

  let .ok () ← Control.add control "base" #[] "p(X) :- q(X). q(a). q(b)."
    | throw (IO.userError "failed to add program")
  let .ok () ← Control.ground control #[⟨"base", #[]⟩] (fun _ _ _ _ => pure true)
    | throw (IO.userError "failed to ground")

  let .ok handle ← Control.solve control SolveMode.Neither #[] handle_solve_event
    | throw (IO.userError "failed to solve")
  let _ ← SolveHandle.wait handle (-1.0)

  IO.println "finished!"
