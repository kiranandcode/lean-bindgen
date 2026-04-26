import LeanBindgen.C.Token
import LeanBindgen.C.Parser
import LeanBindgen.Codegen
import Examples.ClingoSignature

open LeanBindgen LeanBindgen.C LeanBindgen.Codegen

def main : IO Unit := do
  let path := clingoSignatureBindings.headerPath
  IO.println s!"Codegen test against {path}"
  let src ← IO.FS.readFile path
  let toks ← IO.ofExcept (tokenize src)
  let header ← IO.ofExcept (parseHeader toks)
  IO.println s!"  parsed {header.decls.size} decls"
  IO.println "\n=== Generated Lean module ==="
  match emitLeanModule clingoSignatureBindings header with
  | .ok s    => IO.println s
  | .error e => IO.eprintln s!"LEAN-EMIT ERROR: {e}"
  IO.println "\n=== Generated C shim ==="
  match emitShim clingoSignatureBindings header with
  | .ok s    => IO.println s
  | .error e => IO.eprintln s!"SHIM-EMIT ERROR: {e}"
  -- Write the shim to disk and try to compile it.
  let shimDir := ".lake/build/codegen-test"
  IO.FS.createDirAll shimDir
  let shimFile := s!"{shimDir}/signature-shim.c"
  let .ok shim := emitShim clingoSignatureBindings header | return
  IO.FS.writeFile shimFile shim
  IO.println s!"\nwrote {shimFile} ({shim.length} bytes)"
  -- Compile-only check (no link). Needs lean.h on the include path
  -- and the vendored clingo.h alongside. Locate the Lean toolchain via
  -- `lean --print-prefix`.
  let leanPrefix ← IO.Process.output { cmd := "lean", args := #["--print-prefix"] }
  let leanIncPath := s!"{leanPrefix.stdout.trim}/include"
  IO.println s!"compiling: cc -fsyntax-only -I{leanIncPath} -Ireference/cleango/bindings {shimFile}"
  let out ← IO.Process.output {
    cmd := "cc"
    args := #["-fsyntax-only",
              "-I", leanIncPath,
              "-I", "reference/cleango/bindings",
              shimFile]
  }
  if out.exitCode = 0 then
    IO.println "✓ shim compiles"
  else
    IO.eprintln s!"✗ shim compile failed (exit {out.exitCode}):"
    IO.eprintln out.stderr
