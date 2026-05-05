# lean-bindgen

Generate Lean 4 FFI bindings from C headers. Write a concise DSL spec,
get `@[extern]` declarations + a C shim that handles all the marshalling.

## Getting started

### 1. Create your project

```sh
mkdir my-bindings && cd my-bindings
```

Set up the toolchain and add lean-bindgen as a dependency:

**`lean-toolchain`**
```
leanprover/lean4:v4.29.0-rc6
```

**`lakefile.lean`**
```lean
import Lake
open Lake DSL System

package «my-bindings»

require «lean-bindgen» from git
  "https://github.com/kiranandcode/lean-bindgen" @ "main"

lean_lib Bindings where
lean_exe generate where
  root := `Generate
lean_lib Generated where

@[default_target]
lean_exe demo where
  root := `Main

-- Compile the generated C shim (+ optionally the vendor library) into
-- a static archive. Lake links this into any exe that imports Generated.
extern_lib «mylib-shim» pkg := do
  let leanIncDir := (← getLeanIncludeDir).toString
  let vendorDir := (pkg.dir / "vendor").toString
  let weakArgs := #["-I", leanIncDir, "-I", vendorDir]
  let traceArgs := #["-fPIC"]
  let sources : Array FilePath := #["csrc/mylib-shim.c", "vendor/mylib.c"]
  let oJobs ← sources.mapM fun src => do
    let oFile := pkg.buildDir / src.withExtension "o"
    let srcJob ← inputTextFile (pkg.dir / src)
    buildO oFile srcJob weakArgs traceArgs
  buildStaticLib (pkg.staticLibDir / nameToStaticLib "mylib-shim") oJobs
```

> For system libraries (not vendored), replace `"vendor/mylib.c"` with
> just the shim, and add `moreLinkArgs := #["-L/usr/lib", "-lmylib"]` to
> the package declaration.

### 2. Write your binding spec

**`Bindings.lean`**
```lean
import LeanBindgen

open LeanBindgen LeanBindgen.DSL

def myBindings : Bindings := c_bindings {
  header "vendor/mylib.h"
  module Generated.MyLib
  out_dir "Generated"
  shim "csrc/mylib-shim.c"
  lib "mylib"

  opaque mylib_ctx_t => Context freed_by mylib_ctx_free

  cfn mylib_create => create +io
  cfn mylib_do_thing => doThing bool_status on_error string mylib_last_error +io
  cfn mylib_get_value => getValue +io
}
```

### 3. Write the codegen driver

**`Generate.lean`**
```lean
import LeanBindgen
import Bindings

open LeanBindgen LeanBindgen.C LeanBindgen.Codegen

def main : IO Unit := do
  let b := myBindings
  let src ← IO.FS.readFile b.headerPath
  let tokens ← IO.ofExcept (tokenize src)
  let hdr ← IO.ofExcept (parseHeader tokens)
  IO.println s!"Parsed {hdr.decls.size} declarations from {b.headerPath}"

  let leanSrc ← IO.ofExcept (emitLeanModule b hdr)
  IO.FS.createDirAll b.outDir
  IO.FS.writeFile s!"{b.outDir}/MyLib.lean" leanSrc
  IO.println s!"Wrote {b.outDir}/MyLib.lean ({leanSrc.length} bytes)"

  let shimSrc ← IO.ofExcept (emitShim b hdr)
  IO.FS.createDirAll "csrc"
  IO.FS.writeFile b.shimPath shimSrc
  IO.println s!"Wrote {b.shimPath} ({shimSrc.length} bytes)"
```

### 4. Generate and build

```sh
# Create placeholder files so Lake can resolve targets
mkdir -p csrc Generated
echo '' > csrc/mylib-shim.c
echo 'namespace Generated.MyLib end Generated.MyLib' > Generated/MyLib.lean

# Fetch dependencies
lake update

# Generate the Lean module + C shim from the header
lake exe generate

# Build everything (compiles shim, links, produces executable)
lake build
```

After the first `lake exe generate`, commit `Generated/` and `csrc/` —
then `lake build` works without regenerating. Re-run `generate` when the
C header changes.

### 5. Use the bindings

**`Main.lean`**
```lean
import Generated.MyLib

open Generated.MyLib

def main : IO Unit := do
  let ctx ← create 42
  match ← doThing ctx with
  | .ok () => IO.println s!"Value: {← getValue ctx}"
  | .error e => IO.println s!"Error: {e}"
```

### Complete working example

See [`examples/starter/`](./examples/starter/) for a self-contained
project that builds and runs without any external dependencies. Clone
and run:

```sh
cd examples/starter
lake update && lake build && .lake/build/bin/demo
```

```
counter library v1.0.0
Initial value: 0
After +10: 10
After +32: 42
Done!
```

---

## DSL reference

### Header fields

| Syntax | Purpose |
|--------|---------|
| `header "path"` | Path to C header file |
| `module My.Module` | Lean module name for generated code |
| `out_dir "path"` | Output directory for generated `.lean` |
| `shim "path"` | Output path for generated C shim |
| `lib "name"` | Library prefix (used in extern symbol names) |
| `preprocessor ["-DFOO", "-Ibar"]` | Preprocessor args (runs `cc -E -P`) |

### Type declarations

```lean
-- Scalar newtype (C typedef → Lean def)
scalar c_type_t => LeanName : UInt64    -- also UInt32, UInt16, UInt8, Int32, Int64

-- Inductive enum
enum c_enum_t => LeanEnum tag c_enum_tag_name
  | c_variant_1 => leanVariant1
  | c_variant_2 => leanVariant2

-- Opaque pointer with GC finalizer
opaque c_type_t => LeanType freed_by c_type_free

-- Opaque pointer (borrowed, no finalizer)
opaque c_type_t => LeanType borrowed

-- Struct record
struct c_struct_t => LeanStruct tag c_struct_tag
  | c_field_1 => leanField1
  | c_field_2 => leanField2

-- Callback typedef (Lean closure from C function pointer)
callback c_callback_t => LeanCallback

-- Bitfield struct (fields become Bool)
bitfield c_flags_t => LeanFlags tag c_flags_tag
  | c_flag_a => flagA
  | c_flag_b => flagB

-- Escape hatch: raw TypeAnno record
type_raw { cName := "...", lean := "...", mapping := ... }
```

### Function declarations

```lean
-- Direct call (pure or +io)
cfn c_function => leanName
cfn c_function => leanName +io

-- Out-param with bool status: bool fn(..., T *out) → IO (Except E T)
cfn c_function => leanName out[N] on_error string c_error_fn
cfn c_function => leanName out[N] on_error enum c_code LeanError c_msg_fn
cfn c_function => leanName out[N] on_error tuple c_code LeanError c_msg_fn

-- Void out-param: void fn(..., T *out) → T
cfn c_function => leanName void_out[N]

-- Option out-param: bool fn(..., T *out) → Option T
cfn c_function => leanName option_out[N]

-- Option out-array: bool fn(..., T **out, size_t *n) → Option (Array T)
cfn c_function => leanName option_out_array[N, M]

-- Multi out-param: void fn(T1 *a, T2 *b) → T1 × T2
cfn c_function => leanName multi_out[0, 1, 2]

-- Bool status (no out-param): bool fn(...) → Except E Unit
cfn c_function => leanName bool_status on_error string c_error_fn

-- Caller-allocates (two-step size+fill): fn_size + fn_fill → String or Array
cfn c_function => leanName caller_alloc "c_size_fn" [N, M] on_error string c_error_fn

-- Escape hatch
fn_raw { cName := "...", lean := "...", style := ... }
```

### Function modifiers

Append after the style:

| Modifier | Effect |
|----------|--------|
| `+io` | Wrap return in `IO` |
| `+nullable_return` | `char const *` → `Option String` |
| `+nullable_out` | Out-param may be NULL → `Option T` |
| `callback_data [N]` | Index N is user-data for preceding callback |
| `array_pairs [(N, M)]` | Params N,M are (ptr, size) → `Array T` |
| `byte_pairs [(N, M)]` | Like array_pairs but for `ByteArray` |
| `retained_params [N]` | Lean must inc-ref (C retains pointer) |
| `borrowed_params [N]` | Force `@&` on param N |
| `extern_sym "name"` | Override `@[extern "..."]` symbol |

### Constants

```lean
cconst MY_CONST : Int32 := "42"
cconst MY_FLAG : UInt32 := "1"
```

---

## What gets generated

For `cfn clingo_signature_create => mk out[3] on_error string clingo_error_message`:

**Lean side:**
```lean
@[extern "lean_clingo_signature_create"]
opaque mk : @& String → UInt32 → Bool → IO (Except String Signature)
```

**C side:**
```c
LEAN_EXPORT lean_obj_res lean_clingo_signature_create(
    b_lean_obj_arg name, uint32_t arity, uint8_t positive) {
  uint64_t signature;
  char const *name_c = lean_string_cstr(name);
  if (clingo_signature_create(name_c, arity, positive, &signature)) {
    lean_object* val = lean_box_uint64((uint64_t)signature);
    lean_object* ok  = lean_alloc_ctor(1, 1, 0);
    lean_ctor_set(ok, 0, val);
    return lean_io_result_mk_ok(ok);
  } else {
    char const *msg = clingo_error_message();
    if (msg == NULL) msg = "";
    lean_object* err = lean_alloc_ctor(0, 1, 0);
    lean_ctor_set(err, 0, lean_mk_string(msg));
    return lean_io_result_mk_ok(err);
  }
}
```

## How it works

```
       C header (e.g. clingo.h)
              │
              ▼
   ┌─────────────────────┐
   │ tokenize → parse    │   pure-Lean recursive-descent parser
   │ → flat semantic AST │   for C declarations
   └──────────┬──────────┘
              │
              │   + Bindings spec (DSL or records)
              ▼
   ┌─────────────────────┐
   │ emitLeanModule      │   Lean types + @[extern] opaques
   │ emitShim            │   C marshalling shim
   └──────────┬──────────┘
              │
              ▼
    Generated/<Module>.lean
    csrc/<lib>-shim.c
```

## Lake integration

Lake cannot import library modules into lakefile elaboration, so the
`extern_lib` stanza is inlined (~10 lines). This is the same pattern
used by [Raylib.lean](https://github.com/KislyjKisel/Raylib.lean) and
the Lean 4 upstream FFI tests.

The minimal stanza for a **system library**:

```lean
package «my-project» where
  moreLinkArgs := #["-L/opt/homebrew/lib", "-lmylib"]

extern_lib «mylib-shim» pkg := do
  let leanIncDir := (← getLeanIncludeDir).toString
  let weakArgs := #["-I", leanIncDir, "-I/opt/homebrew/include"]
  let traceArgs := #["-fPIC"]
  let sources : Array FilePath := #["csrc/mylib-shim.c"]
  let oJobs ← sources.mapM fun src => do
    let oFile := pkg.buildDir / src.withExtension "o"
    let srcJob ← inputTextFile (pkg.dir / src)
    buildO oFile srcJob weakArgs traceArgs
  buildStaticLib (pkg.staticLibDir / nameToStaticLib "mylib-shim") oJobs
```

For a **vendored library** (source included in your repo), add the `.c`
files to `sources` and the include path to `weakArgs` — no `moreLinkArgs`
needed since everything is in the static archive.

## Safety

The generated shim includes:

- **Thread-safe class registration** via `pthread_once`
- **Malloc overflow guards** (`SIZE_MAX / sizeof(T)` checks)
- **String field ownership** (`strdup` + matching `free_<type>` helpers)
- **Ctor limit validation** (tag, num_objs, scalar_sz bounds)
- **Callback arity check** (≤16 params for `lean_apply_N`)

## Coverage

### Type mappings

| DSL syntax | Generates |
|---|---|
| `scalar` | `def Foo := UIntN deriving Repr, Inhabited` |
| `enum` | Lean inductive + cidx↔C-int conversion |
| `opaque` | `opaque Foo : Type` + external class with GC finalizer |
| `struct` | Lean `structure` + toLean/toC field marshallers |
| `callback` | `def Foo := ... → IO R` + trampoline |
| `bitfield` | Lean `structure` of `Bool` + bitwise pack/unpack |
| `type_raw` | Tagged unions, event callbacks, mutable structs, variadic builders |

### Function styles

| DSL syntax | Lean signature |
|---|---|
| `cfn f => g` | `T₁ → ... → R` (pure) |
| `cfn f => g +io` | `T₁ → ... → IO R` |
| `out[N] on_error ...` | `IO (Except E T)` |
| `bool_status on_error ...` | `IO (Except E Unit)` |
| `void_out[N]` | `T` or `IO T` |
| `option_out[N]` | `IO (Option T)` |
| `option_out_array[N, M]` | `IO (Option (Array T))` |
| `multi_out[0, 1, 2]` | `T₁ × T₂ × T₃` |
| `caller_alloc "size_fn" [N, M]` | `IO (Except E String)` or `Array` |

## Running the tests

```sh
# All codegen tests (10 suites including DSL equivalence)
lake build test-codegen && ./.lake/build/bin/test-codegen

# Unit tests
lake build test-pretty && ./.lake/build/bin/test-pretty
lake build test-token  && ./.lake/build/bin/test-token
lake build test-parser && ./.lake/build/bin/test-parser

# Parser soak against clingo.h (414 decls)
lake build soak && ./.lake/build/bin/soak

# End-to-end runtime (needs libclingo)
brew install clingo
lake build test-codegen && ./.lake/build/bin/test-codegen
cd examples/clingo-signature-runtime
lake build && ./.lake/build/bin/link-test    # 32 assertions
```

## Project layout

```
lean-bindgen/
├── LeanBindgen/
│   ├── DSL.lean              the c_bindings macro
│   ├── Annotation.lean       Bindings / TypeAnno / FunctionAnno types
│   ├── Codegen.lean          emitter (~3600 lines)
│   ├── Lake.lean             reusable extern_lib helper
│   └── C/                    tokenizer + recursive-descent parser
├── examples/
│   ├── starter/              self-contained getting-started project
│   ├── Examples/
│   │   ├── ClingoSignatureDSL.lean   DSL example (clingo subset)
│   │   ├── ZlibDirectDSL.lean        DSL example (zlib)
│   │   ├── ClingoFull.lean           full clingo API (167+ types)
│   │   └── CleangoProject.lean       complete cleango binding
│   ├── clingo-signature-runtime/     link-and-run validation
│   └── cleango/                      full working clingo binding
├── test/
│   ├── CodegenTest.lean      10 codegen test suites
│   └── ...                   tokenizer, parser, pretty-printer tests
└── reference/                hand-written ground truth
```

## License

The reference material under `reference/cleango/` is from
[cleango](https://github.com/kiranandcode/cleango) (MIT). Everything
else in this project is unlicensed pending a decision — treat it as
private until that's resolved.
