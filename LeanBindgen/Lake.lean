import Lake
open Lake DSL System

namespace LeanBindgen

/--
Configuration for a C-binding shim built into a single static archive.

A typical binding has three pieces:
  1. The Lean library that uses `@[extern]` to call C functions.
  2. A shim `.c` file that bridges Lean ↔ C ABI (allocates `lean_object`s,
     unboxes scalars, etc.).
  3. Optionally, additional C sources or a system library on the link line.

`CBinding` packages those into a single static lib that downstream Lean
libs link automatically when declared with `extern_lib`.
-/
structure CBinding where
  /--
  Shim source files, relative to the package root. Each is compiled with
  `cc -c -fPIC` and bundled into the archive.
  -/
  shimSources : Array FilePath
  /--
  Extra `-I` include directories, relative to the package root. The Lean
  include dir (so the shim can `#include "lean/lean.h"`) is added implicitly.
  -/
  includeDirs : Array FilePath := #[]
  /--
  Extra cc flags applied to every shim translation unit. `-fPIC` is added
  implicitly. Anything that affects code generation (e.g. `-O2`) belongs
  here so it participates in the build trace.
  -/
  cFlags : Array String := #[]

/--
Build one shim source file into an object file. Helper used by
`buildCBinding`; exposed in case callers want to drop down a level.
-/
def buildShimObject {n}
    (pkg : NPackage n) (cfg : CBinding) (src : FilePath)
    : FetchM (Job FilePath) := do
  let oFile  := pkg.buildDir / src.withExtension "o"
  let srcJob ← inputTextFile (pkg.dir / src)
  let leanInc := (← getLeanIncludeDir).toString
  let userIncs := cfg.includeDirs.foldl
    (init := #[]) (fun acc d => acc ++ #["-I", (pkg.dir / d).toString])
  let weak  := #["-I", leanInc] ++ userIncs
  let trace := #["-fPIC"] ++ cfg.cFlags
  buildO oFile srcJob (weakArgs := weak) (traceArgs := trace)

/--
Build all shim sources into a single static archive in `pkg.staticLibDir`.

Use as the body of an `extern_lib` declaration:
```
extern_lib my_shim pkg := LeanBindgen.buildCBinding pkg "my-shim" {
  shimSources := #["csrc/my-shim.c", "csrc/my-helper.c"]
  includeDirs := #["csrc"]
}
```

The archive name (`my-shim`) is converted to the platform-specific
filename (`libmy-shim.a` on Unix). Declaring it via `extern_lib` is
sufficient — no manual `moreLinkArgs` or `extraDepTargets` needed.

**Note on consumer use.** Lake (as of toolchain v4.29) does not support
importing helper modules from a required dep into a downstream
`lakefile.lean`; the lakefile elaborator runs with a fixed import set
that does not include depended-on libraries. Direct `import
LeanBindgen.Lake` from a downstream lakefile therefore fails. The
generator emits a *self-contained stanza* — including a copy of
`buildCBinding` and `buildShimObject` inlined — that users paste into
their own `lakefile.lean`. This module is the canonical source of that
stanza, used directly by the in-tree examples under `examples/`.
-/
def buildCBinding {n}
    (pkg : NPackage n) (libName : String) (cfg : CBinding)
    : FetchM (Job FilePath) := do
  let oJobs ← cfg.shimSources.mapM (buildShimObject pkg cfg)
  let staticName := nameToStaticLib libName
  buildStaticLib (pkg.staticLibDir / staticName) oJobs

end LeanBindgen
