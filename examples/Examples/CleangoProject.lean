import LeanBindgen.Annotation
import Examples.ClingoFull

open LeanBindgen

/-! # Cleango project bindings annotation

Reuses the full ClingoFull annotations with output paths redirected into
the `examples/cleango/` sub-package. Now parses the system header directly,
so no enum patching is needed.
-/

def cleangoBindings : Bindings := {
  headerPath := clingoFullBindings.headerPath
  leanModule := `Clingo.Generated.ClingoBindings
  outDir     := "examples/cleango/Clingo/Generated"
  shimPath   := "examples/cleango/csrc/clingo-bindings-shim.c"
  libPrefix  := "clingo"
  leanImports := #[]
  preprocessorArgs := clingoFullBindings.preprocessorArgs
  types      := clingoFullBindings.types
  functions  := clingoFullBindings.functions
}
