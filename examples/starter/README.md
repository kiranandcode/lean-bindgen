# lean-bindgen starter example

A self-contained example showing how to use lean-bindgen to bind a C library.

## What's here

```
starter/
├── vendor/              The C library (header + implementation)
│   ├── counter.h
│   └── counter.c
├── Bindings.lean        Binding spec (lean-bindgen DSL)
├── Generate.lean        Codegen driver (produces Generated/ + csrc/)
├── Generated/
│   └── Counter.lean     Generated @[extern] declarations
├── csrc/
│   └── counter-shim.c  Generated C marshalling shim
├── Main.lean            Demo using the bindings
└── lakefile.lean        Build config
```

## Quick start

```sh
# Build and run (generated files are already committed)
lake build && .lake/build/bin/demo
```

Output:
```
counter library v1.0.0
Initial value: 0
After +10: 10
After +32: 42
Done!
```

## Regenerating bindings

If you modify `Bindings.lean` or the C header:

```sh
lake exe generate     # re-generates Generated/Counter.lean + csrc/counter-shim.c
lake build demo       # rebuild with new bindings
```

## How it works

1. **`Bindings.lean`** defines the mapping using lean-bindgen's DSL:

```lean
def counterBindings : Bindings := c_bindings {
  header "vendor/counter.h"
  module Generated.Counter
  out_dir "Generated"
  shim "csrc/counter-shim.c"
  lib "counter"

  opaque counter_t => Counter freed_by counter_free

  cfn counter_create => create +io
  cfn counter_increment => increment bool_status on_error string counter_error_message +io
  cfn counter_get_value => getValue +io
  cfn counter_version => version multi_out[0, 1, 2]
}
```

2. **`lake exe generate`** parses the header and emits the Lean module + C shim.

3. **`lakefile.lean`** compiles the shim + vendor library into a static archive:

```lean
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
```

## Adapting for a system library

If binding a system library (not vendored), replace the `extern_lib` with:

```lean
package «my-project» where
  moreLinkArgs := #["-L/usr/local/lib", "-lmylib"]  -- link the system library

extern_lib «mylib-shim» pkg := do
  let leanIncDir := (← getLeanIncludeDir).toString
  let weakArgs := #["-I", leanIncDir, "-I/usr/local/include"]
  let traceArgs := #["-fPIC"]
  let sources : Array FilePath := #["csrc/mylib-shim.c"]
  let oJobs ← sources.mapM fun src => do
    let oFile := pkg.buildDir / src.withExtension "o"
    let srcJob ← inputTextFile (pkg.dir / src)
    buildO oFile srcJob weakArgs traceArgs
  buildStaticLib (pkg.staticLibDir / nameToStaticLib "mylib-shim") oJobs
```
