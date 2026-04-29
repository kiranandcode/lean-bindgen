import Lake
open Lake DSL System

/-! Link-and-run validation for the lean-bindgen-generated zlib bindings.
Builds the generated `Generated.Zlib` module against an `extern_lib`
shim that calls real zlib (system-provided via -lz). -/

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

-- Link against system zlib. On macOS, Lean's bundled clang/lld needs
-- an explicit library search path since it uses a custom sysroot.
package «zlib-runtime» where
  moreLinkArgs := #[
    "-L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib",
    "-lz"
  ]

@[default_target]
lean_lib Generated where

@[default_target]
lean_exe «link-test» where
  root := `ZlibTest

extern_lib «zlib-shim» pkg :=
  buildCBinding pkg "zlib-shim" {
    shimSources := #[
      "csrc/zlib-shim.c",
      "csrc/zlib-wrapper.c"
    ]
    includeDirs := #["csrc"]
  }
