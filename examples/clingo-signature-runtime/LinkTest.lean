import Generated.Signature

open Generated.Signature

-- Re-export `Signature` at top level so the literal `Signature.mk` etc.
-- reads naturally below.
abbrev Signature := Generated.Signature.Signature

/-! End-to-end runtime test: drives the auto-generated bindings against
real libclingo (5.8.0 from Homebrew). Each assertion exercises a
different shim path. -/

private def assertEq [BEq α] [ToString α] (label : String) (got expected : α) : IO Unit := do
  if got == expected then
    IO.println s!"  ✓ {label} = {got}"
  else
    IO.eprintln s!"  ✗ {label}: expected {expected}, got {got}"

def main : IO Unit := do
  IO.println "lean-bindgen runtime check: clingo_signature_*"
  -- Constructor: bool-status with out-param. On success returns
  -- Except.ok Signature; on failure Except.err String.
  let made ← mk "edge" 2 true
  match made with
  | .ok sig =>
    IO.println s!"  · made signature: {repr sig}"
    assertEq "name(sig)"        (name sig)        "edge"
    assertEq "arity(sig)"       (arity sig)       (2 : UInt32)
    assertEq "isPositive(sig)"  (isPositive sig)  true
    assertEq "isNegative(sig)"  (isNegative sig)  false
    let made2 ← mk "edge" 2 true
    match made2 with
    | .ok sig2 =>
      assertEq "beq sig sig2"   (beq sig sig2)    true
      assertEq "blt sig sig2"   (blt sig sig2)    false
    | .error e => IO.eprintln s!"  ✗ second mk failed: {e}"
    let h := hash sig
    if h ≠ 0 then
      IO.println s!"  ✓ hash(sig) = {h} (nonzero)"
    else
      IO.eprintln "  ✗ hash returned 0"
    match (← mk "edge" 2 false) with
    | .ok neg =>
      assertEq "isNegative(neg)" (isNegative neg) true
      assertEq "isPositive(neg)" (isPositive neg) false
    | .error e => IO.eprintln s!"  ✗ negative mk failed: {e}"
  | .error e =>
    IO.eprintln s!"  ✗ mk failed: {e}"
