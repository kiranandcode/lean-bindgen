import LeanBindgen.Annotation
import Examples.ClingoFull

open LeanBindgen

/-! # Cleango project bindings annotation

Reuses the full ClingoFull annotations (88+ types, 66+ functions) with
output paths redirected into the `examples/cleango/` sub-package.
The generated Lean module and C shim form Layer 1 of the two-layer
cleango design; hand-written wrapper modules provide the user-facing API.
-/

/-- Override the solve event callback for system clingo 5.8+ which added
`clingo_solve_event_type_unsat = 1`, shifting statistics and finish by one.
The reference header has the old enum values (model=0, statistics=1, finish=2),
but the system library uses (model=0, unsat=1, statistics=2, finish=3). -/
private def patchSolveEventForSystemClingo (types : Array TypeAnno) : Array TypeAnno :=
  types.map fun ta =>
    if ta.cName == "clingo_solve_event_callback_t" then
      { ta with mapping := .eventCallback {
        eventTypeName := "SolveEvent"
        discriminantIdx := 0
        eventIdx := 1
        userDataIdx := 2
        outParams := #[3]
        leanReturnType := "Bool"
        variants := #[
          { cEnumValue := "0"  -- model (same in both versions)
            leanCtor := "model"
            interpretation := .opaquePtr "Model" true },
          { cEnumValue := "2"  -- statistics (was 1 in old, 2 in 5.8+)
            leanCtor := "statistics"
            interpretation := .ptrArray "Statistics" 2
            fieldNames := #["perStep", "accum"] },
          { cEnumValue := "3"  -- finish (was 2 in old, 3 in 5.8+)
            leanCtor := "finish"
            interpretation := .derefMapped "SolveResult" }
        ]
      } }
    else ta

def cleangoBindings : Bindings := {
  headerPath := clingoFullBindings.headerPath
  leanModule := `Clingo.Generated.ClingoBindings
  outDir     := "examples/cleango/Clingo/Generated"
  shimPath   := "examples/cleango/csrc/clingo-bindings-shim.c"
  libPrefix  := "clingo"
  leanImports := #[]
  types      := patchSolveEventForSystemClingo clingoFullBindings.types
  functions  := clingoFullBindings.functions
}
