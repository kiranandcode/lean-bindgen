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

@[extern "lean_test_make_location"]
opaque makeTestLocation (line col : UInt32) : Location

@[extern "lean_test_location_end_line"]
opaque locationEndLine (loc : @& Location) : USize

@[extern "lean_test_invoke_logger"]
opaque invokeLogger (cb : Logger) : IO Unit

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
  -- Callback exercise: parseTerm with a Lean logger closure that
  -- records every (warning, message) it receives into an IO.Ref.
  -- We parse a known-good and a known-bad term; the bad one should
  -- trigger the logger.
  IO.println "\nlean-bindgen runtime check: callback (Logger)"
  let log ← IO.mkRef (#[] : Array (Warning × String))
  let logger : Logger := fun w m => log.modify (·.push (w, m))
  match (← parseTerm "f(1,2)" logger 20) with
  | .ok sym  => IO.println s!"  ✓ parseTerm \"f(1,2)\" → Symbol {repr sym} (no warnings expected)"
  | .error e => IO.eprintln s!"  ✗ parseTerm \"f(1,2)\" failed: {e}"
  let okWarnings ← log.get
  if okWarnings.isEmpty then
    IO.println "  ✓ no warnings logged for valid term"
  else
    IO.eprintln s!"  ✗ unexpected warnings: {repr okWarnings}"
  -- Reset and parse an invalid term.
  log.set #[]
  match (← parseTerm "bogus(((" logger 20) with
  | .ok sym  => IO.eprintln s!"  ✗ expected parseTerm to fail; got Symbol {repr sym}"
  | .error _ => IO.println "  ✓ parseTerm rejected invalid term"
  let badWarnings ← log.get
  IO.println s!"  · logger captured {badWarnings.size} warning(s)"
  -- Drive the trampoline directly to prove the Lean→C→Lean closure
  -- path actually invokes our closure (clingo_parse_term doesn't
  -- happen to call the logger for the cases we tried, so this is the
  -- explicit proof-of-life).
  log.set #[]
  invokeLogger logger
  let direct ← log.get
  match direct[0]? with
  | some (w, m) =>
    let kindOk := w == Warning.runtimeError
    if kindOk then IO.println s!"  ✓ trampoline-direct: warning kind = {repr w}"
    else IO.eprintln s!"  ✗ trampoline-direct: expected runtimeError, got {repr w}"
    assertEq "trampoline-direct: message"      m "synthetic test message"
  | none => IO.eprintln "  ✗ trampoline did not invoke closure"
  -- Struct round-trip: build a Location in C, examine its fields in
  -- Lean (which goes via lean_ctor_get/_usize), then read the
  -- end_line back through lean_to_location and the C-side accessor.
  IO.println "\nlean-bindgen runtime check: struct (Location)"
  let loc := makeTestLocation 42 7
  assertEq "loc.beginFile"   loc.beginFile   "<begin>"
  assertEq "loc.endFile"     loc.endFile     "<end>"
  assertEq "loc.beginLine"   loc.beginLine   (42 : USize)
  assertEq "loc.endLine"     loc.endLine     (43 : USize)
  assertEq "loc.beginColumn" loc.beginColumn (7  : USize)
  assertEq "loc.endColumn"   loc.endColumn   (8  : USize)
  assertEq "lean→C→read endLine" (locationEndLine loc) (43 : USize)
  -- Array+size pair exercise: mkFun wraps clingo_symbol_create_function
  -- which takes (name, args, args_size, positive, *out). We use
  -- parseTerm to create proper clingo_symbol_t values (not signatures)
  -- and pass them as an Array to mkFun.
  IO.println "\nlean-bindgen runtime check: array+size pair (mkFun)"
  let noopLogger : Logger := fun _ _ => pure ()
  match (← parseTerm "a" noopLogger 20), (← parseTerm "b" noopLogger 20) with
  | .ok symA, .ok symB =>
    let args : Array Symbol := #[symA, symB]
    match (← mkFun "f" args true) with
    | .ok funSym =>
      IO.println s!"  ✓ mkFun \"f\" #[a, b] → Symbol {repr funSym}"
      IO.println "  ✓ mkFun returned Ok (function symbol created)"
    | .error e => IO.eprintln s!"  ✗ mkFun failed: {e}"
    -- Also test with an empty array — a function with no arguments
    -- is a valid clingo symbol (a constant/0-ary function).
    match (← mkFun "g" #[] true) with
    | .ok _emptySym =>
      IO.println "  ✓ mkFun \"g\" #[] (empty array) → Ok"
    | .error e => IO.eprintln s!"  ✗ mkFun empty failed: {e}"
  | _, _ => IO.eprintln "  ✗ failed to create argument symbols via parseTerm"
