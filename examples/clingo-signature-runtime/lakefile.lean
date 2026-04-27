import Lake
open Lake DSL System

/-! Link-and-run validation for the lean-bindgen-generated bindings.
Builds the generated `Generated.Signature` module against an
`extern_lib` shim that calls real libclingo (installed via brew).

This sub-package uses the canonical inlined-stanza form of the
`buildCBinding` helper from `LeanBindgen/Lake.lean` because Lake does
not let downstream lakefiles import modules from required deps. -/

structure CBinding where
  shimSources : Array FilePath
  includeDirs : Array FilePath := #[]
  cFlags      : Array String   := #[]

def buildShimObject {n}
    (pkg : NPackage n) (cfg : CBinding) (src : FilePath)
    : FetchM (Job FilePath) := do
  let oFile  := pkg.buildDir / src.withExtension "o"
  let srcJob ← inputTextFile (pkg.dir / src)
  let leanInc := (← getLeanIncludeDir).toString
  let userIncs := cfg.includeDirs.foldl
    (init := #[]) (fun acc d => acc ++ #["-I", (pkg.dir / d).toString])
  buildO oFile srcJob
    (weakArgs  := #["-I", leanInc] ++ userIncs)
    (traceArgs := #["-fPIC"] ++ cfg.cFlags)

def buildCBinding {n}
    (pkg : NPackage n) (libName : String) (cfg : CBinding)
    : FetchM (Job FilePath) := do
  let oJobs ← cfg.shimSources.mapM (buildShimObject pkg cfg)
  buildStaticLib (pkg.staticLibDir / nameToStaticLib libName) oJobs

-- Where Homebrew installs clingo on macOS arm64.
def clingoIncludeDir : FilePath := "/opt/homebrew/include"
def clingoLibDir     : FilePath := "/opt/homebrew/lib"

package «clingo-signature-runtime» where
  moreLinkArgs := #[s!"-L{clingoLibDir}", "-lclingo"]

@[default_target]
lean_lib Generated where

@[default_target]
lean_exe «link-test» where
  root := `LinkTest

extern_lib «clingo-signature-shim» pkg :=
  buildCBinding pkg "clingo-signature-shim" {
    shimSources := #[
      "csrc/signature-shim.c",
      -- Hand-written extras for paths the codegen doesn't yet cover.
      "csrc/test-helpers.c"
    ]
    includeDirs := #[clingoIncludeDir]
  }
