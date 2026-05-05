import Lake
open Lake DSL System

package «counter-example»

/-! ## Dependencies -/

-- lean-bindgen provides the DSL macros and codegen library.
-- Only needed at codegen time — the generated output is pure Lean + C.
require «lean-bindgen» from git
  "https://github.com/kiranandcode/lean-bindgen" @ "main"

/-! ## Targets -/

-- The binding spec (uses lean-bindgen DSL)
lean_lib Bindings where

-- Codegen driver: `lake exe generate` produces Generated/Counter.lean + csrc/counter-shim.c
lean_exe generate where
  root := `Generate

-- The generated Lean module with @[extern] declarations
lean_lib Generated where

-- Demo executable
@[default_target]
lean_exe demo where
  root := `Main

/-! ## C compilation

Lake can't import library code into lakefiles, so we inline the
extern_lib stanza. This compiles:
  1. The generated shim (csrc/counter-shim.c)
  2. The vendor C library (vendor/counter.c)
into a single static archive that Lake links automatically. -/

extern_lib «counter-shim» pkg := do
  let leanIncDir := (← getLeanIncludeDir).toString
  let vendorDir := (pkg.dir / "vendor").toString
  let weakArgs := #["-I", leanIncDir, "-I", vendorDir]
  let traceArgs := #["-fPIC"]
  let sources : Array FilePath := #["csrc/counter-shim.c", "vendor/counter.c"]
  let oJobs ← sources.mapM fun src => do
    let oFile := pkg.buildDir / src.withExtension "o"
    let srcJob ← inputTextFile (pkg.dir / src)
    buildO oFile srcJob weakArgs traceArgs
  buildStaticLib (pkg.staticLibDir / nameToStaticLib "counter-shim") oJobs
