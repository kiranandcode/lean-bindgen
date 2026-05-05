# lean-bindgen

Generate Lean 4 FFI bindings from C headers. Write a concise DSL spec,
get `@[extern]` declarations + a C shim that handles all the marshalling.

## Quick start

```lean
import LeanBindgen

open LeanBindgen LeanBindgen.DSL

def myBindings : Bindings := c_bindings {
  header "vendor/mylib.h"
  module Generated.MyLib
  out_dir "src/Generated"
  shim "csrc/mylib-shim.c"
  lib "mylib"

  -- Scalar newtype: wraps a C typedef as a Lean def
  scalar mylib_handle_t => Handle : UInt64

  -- Enum: maps a C enum to a Lean inductive
  enum mylib_status_t => Status tag mylib_status
    | mylib_status_ok => ok
    | mylib_status_fail => fail

  -- Opaque pointer: GC-released via finalizer
  opaque mylib_context_t => Context freed_by mylib_context_free

  -- Struct: auto-marshalled field-by-field
  struct mylib_config_t => Config tag mylib_config
    | name => name
    | value => value
    | flags => flags

  -- Functions
  cfn mylib_open => open out[1] on_error string mylib_last_error
  cfn mylib_close => close +io
  cfn mylib_status => status
  cfn mylib_version => version multi_out[0, 1, 2]

  -- Constants
  cconst MYLIB_DEFAULT_FLAGS : UInt32 := "0"
}
```

That's it. The `c_bindings { ... }` macro expands to the same `Bindings`
record the codegen consumes — no runtime cost, just less boilerplate.

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

## Real-world example

Here's a real binding to libclingo's signature API (~60 lines DSL vs ~120 lines records):

```lean
import LeanBindgen

open LeanBindgen LeanBindgen.DSL

def clingoBindings : Bindings := c_bindings {
  header "/opt/homebrew/include/clingo.h"
  module Generated.Clingo
  out_dir ".lake/build/generated"
  shim "csrc/clingo-shim.c"
  lib "clingo"
  preprocessor ["-DCLINGO_NO_VISIBILITY", "-I/opt/homebrew/include"]

  scalar clingo_signature_t => Signature : UInt64
  scalar clingo_symbol_t => Symbol : UInt64

  enum clingo_error_t => Error tag clingo_error
    | clingo_error_success => success
    | clingo_error_runtime => runtime
    | clingo_error_logic => logic
    | clingo_error_bad_alloc => badAlloc
    | clingo_error_unknown => unknown

  opaque clingo_control_t => Control freed_by clingo_control_free

  struct clingo_location_t => Location tag clingo_location
    | begin_file => beginFile
    | end_file => endFile
    | begin_line => beginLine
    | end_line => endLine
    | begin_column => beginColumn
    | end_column => endColumn

  enum clingo_warning_t => Warning tag clingo_warning
    | clingo_warning_operation_undefined => operationUndefined
    | clingo_warning_runtime_error => runtimeError
    | clingo_warning_atom_undefined => atomUndefined
    | clingo_warning_file_included => fileIncluded
    | clingo_warning_variable_unbounded => variableUnbounded
    | clingo_warning_global_variable => globalVariable
    | clingo_warning_other => other

  callback clingo_logger_t => Logger

  cfn clingo_signature_create => mk out[3] on_error string clingo_error_message
  cfn clingo_signature_name => name
  cfn clingo_signature_arity => arity
  cfn clingo_signature_is_positive => isPositive
  cfn clingo_signature_is_negative => isNegative
  cfn clingo_error_code => errorCode +io
  cfn clingo_error_string => errorString
  cfn clingo_control_interrupt => interrupt +io
  cfn clingo_parse_term => parseTerm out[4] on_error string clingo_error_message callback_data [2]
  cfn clingo_symbol_create_function => mkFun out[4] on_error string clingo_error_message array_pairs [(1, 2)]
}
```

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

## Integration with Lake

Link the generated shim in your `lakefile.lean`:

```lean
extern_lib mylib_shim pkg :=
  buildCBinding pkg "mylib-shim" {
    shimSources := #["csrc/mylib-shim.c"]
    includeDirs := #["/path/to/mylib/include"]
  }
```

See [`LeanBindgen/Lake.lean`](./LeanBindgen/Lake.lean) for the helper, or
[`examples/clingo-signature-runtime/lakefile.lean`](./examples/clingo-signature-runtime/lakefile.lean)
for a self-contained example.

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
│   ├── DSL.lean              ← the c_bindings macro
│   ├── Annotation.lean       Bindings / TypeAnno / FunctionAnno types
│   ├── Codegen.lean          emitter (~3600 lines)
│   ├── Lake.lean             reusable extern_lib helper
│   └── C/                    tokenizer + recursive-descent parser
├── examples/
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
