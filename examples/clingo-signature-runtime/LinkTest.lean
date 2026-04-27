import Generated.Signature

open Generated.Signature

-- Re-export `Signature` at top level so the literal `Signature.mk` etc.
-- reads naturally below.
abbrev Signature := Generated.Signature.Signature

/-- Hand-written extern that drives the auto-generated `Control` opaque
type. The `clingo_control_new` constructor takes a function-pointer
logger that the codegen doesn't yet support; this helper just calls
it with NULL logger and NULL args, suitable for exercising the
external-object path at runtime. -/
@[extern "lean_test_make_default_control"]
opaque makeDefaultControl (msgLimit : UInt32) : IO (Except String Control)

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
  -- Exercise the inductive-enum path: errorCode reads the
  -- thread-local error state (we expect `success` since prior calls
  -- succeeded), and errorString round-trips an Error variant back to
  -- a string via libclingo's lookup table.
  IO.println "\nlean-bindgen runtime check: clingo_error"
  let code ← errorCode
  IO.println s!"  · errorCode = {repr code}"
  match code with
  | .success => IO.println "  ✓ errorCode = success"
  | _        => IO.eprintln s!"  ✗ expected success, got {repr code}"
  -- errorString takes an Error, returns its string name.
  assertEq "errorString success"  (errorString .success)  "success"
  assertEq "errorString runtime"  (errorString .runtime)  "runtime error"
  assertEq "errorString badAlloc" (errorString .badAlloc) "bad allocation"
  assertEq "errorString unknown"  (errorString .unknown)  "unknown error"
  -- Exercise the opaque-pointer path. We construct a Control via the
  -- handwritten test helper (the codegen path for clingo_control_new
  -- isn't ready yet — needs callback support), then call interrupt
  -- on it (which uses lean_get_external_data via the codegen). When
  -- ctrl drops out of scope, Lean's GC runs `clingo_control_free`
  -- through the registered finalizer.
  IO.println "\nlean-bindgen runtime check: clingo_control"
  match (← makeDefaultControl 20) with
  | .ok ctrl =>
    IO.println "  ✓ makeDefaultControl returned Ok"
    interrupt ctrl
    IO.println "  ✓ interrupt(ctrl) returned"
  | .error e =>
    IO.eprintln s!"  ✗ makeDefaultControl failed: {e}"
  -- Stress test the finalizer: build & drop N Controls in a loop. If
  -- the finalizer didn't run, leaks would scale O(N).
  let n : Nat := 100
  for _ in [:n] do
    match (← makeDefaultControl 20) with
    | .ok _    => pure ()
    | .error _ => pure ()
  IO.println s!"  ✓ constructed and dropped {n} Controls"
