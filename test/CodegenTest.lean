import LeanBindgen.C.Token
import LeanBindgen.C.Parser
import LeanBindgen.Codegen
import Examples.ClingoSignature

open LeanBindgen LeanBindgen.C LeanBindgen.Codegen

/-- Resolve where to write generated artifacts. The lakefile under
`examples/clingo-signature-runtime/` expects `Generated/Signature.lean`
and `csrc/signature-shim.c` next to it. -/
private def runtimeRoot : System.FilePath := "examples/clingo-signature-runtime"

def main : IO Unit := do
  let path := clingoSignatureBindings.headerPath
  IO.println s!"Codegen test against {path}"
  let src ← IO.FS.readFile path
  let toks ← IO.ofExcept (tokenize src)
  let header ← IO.ofExcept (parseHeader toks)
  IO.println s!"  parsed {header.decls.size} decls"
  let leanText ← IO.ofExcept (emitLeanModule clingoSignatureBindings header)
  let shimText ← IO.ofExcept (emitShim clingoSignatureBindings header)
  IO.println "\n=== Generated Lean module ==="
  IO.println leanText
  IO.println "=== Generated C shim ==="
  IO.println shimText
  -- Write the artefacts into the runtime sub-package layout.
  let leanFile := runtimeRoot / "Generated" / "Signature.lean"
  let shimFile := runtimeRoot / "csrc" / "signature-shim.c"
  IO.FS.createDirAll (runtimeRoot / "Generated")
  IO.FS.createDirAll (runtimeRoot / "csrc")
  IO.FS.writeFile leanFile leanText
  IO.FS.writeFile shimFile shimText
  IO.println s!"\nwrote {leanFile} ({leanText.length} bytes)"
  IO.println s!"wrote {shimFile} ({shimText.length} bytes)"
  -- Quick syntax check on the shim: confirms it still typechecks
  -- against the system clingo.h and the Lean runtime headers.
  let leanPrefix ← IO.Process.output { cmd := "lean", args := #["--print-prefix"] }
  let leanIncPath := s!"{leanPrefix.stdout.trim}/include"
  let out ← IO.Process.output {
    cmd := "cc"
    args := #["-fsyntax-only", "-I", leanIncPath,
              "-I", "/opt/homebrew/include",
              shimFile.toString]
  }
  if out.exitCode = 0 then
    IO.println "✓ shim compiles against system clingo.h"
  else
    IO.eprintln s!"✗ shim compile failed (exit {out.exitCode}):"
    IO.eprintln out.stderr
