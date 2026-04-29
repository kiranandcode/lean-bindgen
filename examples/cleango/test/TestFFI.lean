import Clingo

open Clingo

def test_version : IO Unit := do
  let v := Clingo.version
  IO.println s!"clingo version {v}"

def test_signature : IO Unit := do
  let .ok test ← Clingo.Signature.mk "random" 0 | IO.println "signature creation failed"; return
  IO.println s!"signature name={Signature.name test}, arity={Signature.arity test}, positive={Signature.isPositive test}, eq={test == test}, hash={Signature.hash test}"

def test_symbol : IO Unit := do
  let sym := Symbol.mk_number 1000
  IO.println s!"made a number symbol, number={sym.number?}"
  IO.println s!"type is {repr (Symbol.type sym)}"

  let sym := Symbol.mk_infimum
  IO.println s!"made infimum, type={repr (Symbol.type sym)}"

  let sym := Symbol.mk_supremum
  IO.println s!"made supremum, type={repr (Symbol.type sym)}"

  let .ok sym ← Symbol.mk_string "hello" | IO.println "string creation failed"; return
  IO.println s!"made string, string={sym.string?}, type={repr (Symbol.type sym)}"

  let .ok _sym ← Symbol.mk_id "a" true | IO.println "id creation failed"; return
  IO.println s!"made id, hash={Symbol.hash sym}"

  let .ok funSym ← Symbol.mk_fun "hello" #[sym] true | IO.println "fun creation failed"; return
  IO.println s!"made fun, name={funSym.name?}, type={repr (Symbol.type funSym)}"

def test_symbol_repr : IO Unit := do
  let .ok sym ← Symbol.mk_fun "test" #[Symbol.mk_number 42] true
    | IO.println "fun creation failed"; return
  let r := Symbol.toRepr sym
  IO.println s!"repr: {repr r}"
  let sym2 ← Symbol.mk r
  IO.println s!"roundtrip eq: {Symbol.beq sym sym2}"

def test_control : IO Unit := do
  let modelCount : IO.Ref Nat ← IO.mkRef 0
  let my_callback (evt : SolveEvent) : IO Bool := do
    match evt with
    | .model (.some _m) =>
      modelCount.modify (· + 1)
    | .finish _res =>
      pure ()
    | _ => pure ()
    return true

  let .ok control ← Control.mk (args := #[])
    | throw (IO.userError "failed to create control")
  let .ok () ← Control.add control "base" #[] "p(a). p(b). q(X) :- p(X)."
    | throw (IO.userError "failed to add program")
  let .ok () ← Control.ground control #[⟨"base", #[]⟩] (fun _ _ _ _ => pure true)
    | throw (IO.userError "failed to ground")
  let .ok handle ← Control.solve control SolveMode.Neither #[] my_callback
    | throw (IO.userError "failed to solve")
  let _ ← SolveHandle.wait handle (-1.0)
  let n ← modelCount.get
  IO.println s!"control test done, found {n} models"

def main : IO Unit := do
  test_version
  test_signature
  test_symbol
  test_symbol_repr
  test_control
  IO.println "all FFI tests finished!"
