import LeanBindgen
import Bindings

open LeanBindgen LeanBindgen.C LeanBindgen.Codegen

/-! Codegen driver — run with `lake exe generate` to produce:
    - Generated/Counter.lean (Lean @[extern] declarations)
    - csrc/counter-shim.c    (C marshalling shim) -/

def main : IO Unit := do
  -- Parse the C header
  let b := counterBindings
  let src ← IO.FS.readFile b.headerPath
  let tokens ← IO.ofExcept (tokenize src)
  let hdr ← IO.ofExcept (parseHeader tokens)
  IO.println s!"Parsed {hdr.decls.size} declarations from {b.headerPath}"

  -- Generate Lean module
  let leanSrc ← IO.ofExcept (emitLeanModule b hdr)
  IO.FS.createDirAll b.outDir
  let leanPath := s!"{b.outDir}/Counter.lean"
  IO.FS.writeFile leanPath leanSrc
  IO.println s!"Wrote {leanPath} ({leanSrc.length} bytes)"

  -- Generate C shim
  let shimSrc ← IO.ofExcept (emitShim b hdr)
  let shimDir := (System.FilePath.mk b.shimPath).parent.getD "."
  IO.FS.createDirAll shimDir
  IO.FS.writeFile b.shimPath shimSrc
  IO.println s!"Wrote {b.shimPath} ({shimSrc.length} bytes)"

  IO.println "Done! Now run `lake build` to compile everything."
