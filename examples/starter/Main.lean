import Generated.Counter

open Generated.Counter

def main : IO Unit := do
  -- Check version (pure — no IO needed)
  let (major, minor, patch) := version ()
  IO.println s!"counter library v{major}.{minor}.{patch}"

  -- Create a counter starting at 0
  let c ← create 0
  IO.println s!"Initial value: {← getValue c}"

  -- Increment a few times
  match ← increment c 10 with
  | .ok () => IO.println s!"After +10: {← getValue c}"
  | .error e => IO.println s!"Error: {e}"

  match ← increment c 32 with
  | .ok () => IO.println s!"After +32: {← getValue c}"
  | .error e => IO.println s!"Error: {e}"

  IO.println "Done!"
  -- `c` is freed automatically by GC (calls counter_free)
