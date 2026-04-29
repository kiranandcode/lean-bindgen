# lean-bindgen

Generate Lean 4 bindings — `@[extern]` declarations on the Lean side
plus a C shim that bridges Lean's runtime ABI — from a C header and a
small annotation file.

Validated end-to-end against libclingo: the full clingo API (167 types,
66 functions, including recursive AST types, tagged unions, callbacks,
and event dispatchers) is auto-generated and structurally verified
against a hand-written reference binding. 32 runtime assertions, all
green, no leaks beyond Lean/clingo runtime baselines.

The codegen handles the mechanical work (extern decls, marshalling
glue, struct/enum/union conversion, callback trampolines, memory
management) and leaves the ergonomic API layer — renaming, default
args, type-class instances, custom `match` elaborators — to be
hand-written on top.

## How it works

```
       C header (e.g. clingo.h)
              │
              ▼
   ┌─────────────────────┐
   │ tokenize → parse    │   pure-Lean recursive-descent parser;
   │ → flat semantic AST │   handles the declaration subset of C
   │ (LeanBindgen.C.*)   │   (typedefs, structs, unions, enums,
   └──────────┬──────────┘   function prototypes, function-pointer
              │              typedefs, plus a small preprocessor
              ▼              for #define / extern "C" blocks)
        CHeader value
              │
              │   plus a Bindings record describing per-decl
              │   mappings (TypeAnno + FunctionAnno entries)
              ▼
   ┌─────────────────────┐
   │ emitLeanModule      │   Lean source: type definitions
   │ emitShim            │   (def/structure/inductive/opaque) +
   │ (LeanBindgen.       │   `@[extern]` opaques. C source: per-
   │  Codegen)           │   type marshalling helpers + per-function
   └──────────┬──────────┘   shim entry points.
              │
              ▼
    Generated.<module>.lean    (committed to your project)
    csrc/<lib>-shim.c          (built into an extern_lib via Lake)
```

Downstream consumers reference the generated module like any normal
Lean library and link the shim alongside the underlying C library —
the Lake helper at [`LeanBindgen/Lake.lean`](./LeanBindgen/Lake.lean)
provides a reusable `extern_lib` stanza for that.

## What gets generated

For an annotation entry like

```lean
{ cName := "clingo_signature_create"
  lean  := "mk"
  style := .outParamBoolStatus 3 (.string "clingo_error_message") }
```

paired with the C declaration

```c
bool clingo_signature_create(char const *name, uint32_t arity,
                             bool positive, clingo_signature_t *signature);
```

the codegen emits, on the Lean side:

```lean
@[extern "lean_clingo_signature_create"]
opaque mk : @& String → UInt32 → Bool → IO (Except String Signature)
```

and on the C side:

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

That's the boring half of binding work that the user no longer
writes by hand.

## Coverage

Eight `TypeMapping` kinds and eight `FunctionStyle` kinds, plus
per-function `borrowedParams` / `callbackUserDataParams` /
`retainedParams` / `arrayPairs` / `nullableOutParam` /
`nullableReturn` / `externSymbol` overrides.

### Type mappings

| Type mapping | Generates |
|---|---|
| `scalarNewtype` | `def Foo := UIntN deriving Repr, Inhabited` |
| `inductiveEnum` | Lean inductive + cidx ↔ C-int conversion helpers |
| `opaquePointer` | `opaque Foo : Type` + `lean_external_class` registration with GC finalizer |
| `structRecord` | Lean `structure` + field-by-field `toLean` / `toC` marshallers (handles nested structs, array fields, Lean ctor layout reordering) |
| `callback` | `def Foo := T1 → T2 → IO R` + trampoline that marshals C args → Lean closure invocation via `lean_apply_*` (supports nested callbacks, non-void returns, reverse trampolines) |
| `taggedUnion` | Lean `inductive` from C struct + tag enum + union; per-variant C ↔ Lean switch dispatch, deep free helpers |
| `bitfieldStruct` | Lean `structure` of `Bool` fields + bitwise pack/unpack helpers |
| `eventCallback` | Lean `inductive` for event variants + callback `def` alias + trampoline with switch dispatch on event discriminant (supports opaque ptrs, deref-mapped structs, ptr arrays) |

### Function styles

| Function style | Lean signature | C pattern |
|---|---|---|
| `direct` | `T₁ → ... → IO R` or pure | Plain call, marshal return |
| `outParamBoolStatus` | `IO (Except E T)` | `bool fn(..., T *out)` → `Except.ok`/`Except.error` |
| `boolStatus` | `IO (Except E Unit)` | `bool fn(...)` → `Except.ok ()`/`Except.error` |
| `optionOutParam` | `IO (Option T)` | `bool fn(..., T *out)` → `some`/`none` |
| `voidOutParam` | `IO T` or `T` | `void fn(..., T *out)` → unwrap |
| `optionOutArray` | `IO (Option (Array T))` | `bool fn(..., T const **out, size_t *n)` |
| `callerAllocates` | `IO (Except E (Array T))` or `String` | Two-step: query size, alloc, fill |
| `multiOutParam` | `IO (T₁ × T₂ × T₃)` or pure | `void fn(T₁ *a, T₂ *b, T₃ *c)` → right-nested Prod |

### Error return types

`outParamBoolStatus`, `boolStatus`, and `callerAllocates` accept an
`ErrorReturn` specifying how to surface the error:

| `ErrorReturn` | Lean type | Source |
|---|---|---|
| `.string "error_message_fn"` | `Except String T` | Calls `error_message_fn()` for message |
| `.enum "c_error_code" "LeanError"` | `Except LeanError T` | Maps C error enum to Lean inductive |
| `.tuple "c_error_code" "LeanError" "c_message_fn"` | `Except (LeanError × String) T` | Both enum + message |

### Additional per-function annotations

| Annotation | Purpose |
|---|---|
| `borrowedParams` | Indices of params passed as `@&` (borrowed reference) |
| `retainedParams` | Indices where Lean must `lean_inc` (C retains the pointer) |
| `callbackUserDataParams` | Indices of `(fn_ptr, void *user_data)` pairs → closure passing |
| `arrayPairs` | `(ptr_idx, size_idx)` pairs → `@& Array T` on Lean side |
| `nullableReturn` | Wrap nullable `char const *` return in `Option String` |
| `nullableOutParam` | Wrap nullable out-param pointer in `Option` |
| `externSymbol` | Override the generated `@[extern "..."]` symbol name |

## Full clingo API example

[`examples/Examples/ClingoFull.lean`](./examples/Examples/ClingoFull.lean)
annotates the complete libclingo API: 167 type annotations (7 scalar
newtypes, 21 enums, 7 opaque pointers, 3 bitfield structs, 4 callbacks,
1 event callback, 117+ struct records and tagged unions) and 66 function
annotations covering every function style above.

The generated output is ~170 KB of C shim and ~19 KB of Lean module.
A structural comparison (`test/compare_shims.py`) against the
hand-written [cleango](https://github.com/kiranandcode/cleango) binding
confirms functional equivalence: 134 matched function pairs (93
identical, 41 with expected design-level naming differences, 0 real
divergences).

## Project layout

```
lean-bindgen/
├── lakefile.lean
├── lean-toolchain                       v4.29.0-rc6
├── LeanBindgen.lean                     top-level umbrella
├── LeanBindgen/
│   ├── Lake.lean                        reusable extern_lib helper
│   ├── Annotation.lean                  Bindings / TypeAnno / FunctionAnno
│   ├── Codegen.lean                     the emitter (~2800 lines)
│   └── C/
│       ├── Ast.lean                     flat semantic AST
│       ├── Pretty.lean                  AST → C source (K&R declarators)
│       ├── Token.lean                   tokenizer
│       └── Parser.lean                  recursive-descent header parser
├── examples/
│   ├── Examples.lean                    umbrella for example annotations
│   ├── Examples/
│   │   ├── ClingoSignature.lean         small example bindings spec
│   │   └── ClingoFull.lean              full clingo API annotation
│   └── clingo-signature-runtime/        sub-package: link-and-run validation
│       ├── lakefile.lean                inlined buildCBinding stanza,
│       │                                links /opt/homebrew/lib/libclingo
│       ├── Generated/Signature.lean     ← generated by test-codegen
│       ├── csrc/signature-shim.c        ← generated by test-codegen
│       └── LinkTest.lean                runtime test exe (32 assertions)
├── test/
│   ├── PrettyTest.lean                  AST → C round-trip (12 cases)
│   ├── TokenTest.lean                   tokenizer (8 cases)
│   ├── ParserTest.lean                  parse → pretty round-trip (8 cases)
│   ├── Soak.lean                        full clingo.h soak (414 decls)
│   ├── CodegenTest.lean                 6 codegen test suites + cc -fsyntax-only
│   └── compare_shims.py                 structural comparison vs hand-written shim
├── reference/
│   ├── cleango/                         hand-written ground truth
│   └── c-parser-upstream/               opencompl C-parser (reference)
├── PLAN.md                              gap analysis & roadmap
└── README.md
```

## Running the tests

Lean 4.29.0-rc6 is required (auto-installed by elan from the
`lean-toolchain` file). Most tests are pure Lean; the runtime
validation needs libclingo and clang.

### Unit tests

```sh
# Pretty-printer round-trip (function-pointer typedefs, const placement,
# bitfield structs, variadic prototypes, …)
lake build test-pretty && ./.lake/build/bin/test-pretty

# Tokenizer (numeric forms, comments, preprocessor passthrough, …)
lake build test-token  && ./.lake/build/bin/test-token

# Parser → pretty-printer round-trip on representative declarations
lake build test-parser && ./.lake/build/bin/test-parser
```

### Soak the parser against `clingo.h`

```sh
lake build soak && ./.lake/build/bin/soak
# parses 414 decls; round-trips parse → print → re-parse with the
# same decl count.
```

### Drive codegen against `clingo.h`

```sh
lake build test-codegen && ./.lake/build/bin/test-codegen
# Runs 6 test suites:
#   1. Main codegen (signature subset, cc -fsyntax-only)
#   2. Tagged union codegen (AST term nodes)
#   3. Mixed-scalar struct layout (ctor field reordering)
#   4. Deep-struct codegen (malloc/deref/free helpers)
#   5. Recursive AST codegen (SCC ordering, forward decls)
#   6. Full cleango codegen (167 types, 66 functions, ~170KB shim)
```

### End-to-end link-and-run (needs libclingo)

```sh
brew install clingo                 # 5.8.0 at time of writing
lake build test-codegen && ./.lake/build/bin/test-codegen   # regenerate
cd examples/clingo-signature-runtime
lake build && ./.lake/build/bin/link-test
# 32 assertions, all green. Exercises signatures, enums, opaque
# controls with GC finalizers (100× create/drop), logger callback
# trampolines, location struct round-trips, and array+size pairs.
```

### Memory check

```sh
leaks --atExit -- ./.lake/build/bin/link-test | grep '^Process'
# Expect: 13 leaks for 288 total leaked bytes
# (Baseline 12/256 from Lean's runtime statics; the +1/+32 is
#  libclingo's thread-local error buffer. Constant whether we
#  allocate 0, 1, or 100 Controls.)
```

### Structural comparison against hand-written shim

```sh
python3 test/compare_shims.py
# Uses clang JSON AST to structurally compare the generated shim
# against the hand-written cleango shim. Reports matched pairs,
# expected design differences, and any real divergences.
```

## Defining a new binding

A `Bindings` record is just data:

```lean
import LeanBindgen.Annotation
open LeanBindgen

def myBindings : Bindings := {
  headerPath  := "vendor/mylib.h"
  leanModule  := `Generated.MyLib
  outDir      := "src/Generated"
  shimPath    := "csrc/mylib-shim.c"
  libPrefix   := "mylib"

  types := #[
    { cName := "mylib_handle_t", lean := "Handle",
      mapping := .opaquePointer "mylib_close" },
    { cName := "mylib_status_t", lean := "Status",
      mapping := .inductiveEnum {
        enumTag  := "mylib_status"
        variants := #[("MYLIB_OK", "ok"), ("MYLIB_FAIL", "fail")]
      } }
  ]

  functions := #[
    { cName := "mylib_open", lean := "open"
      style := .outParamBoolStatus 1 (.string "mylib_last_error") },
    { cName := "mylib_status", lean := "status" }
  ]
}
```

A small driver — see `test/CodegenTest.lean` — parses the header,
emits the Lean module + shim text, and writes them to disk. The
generated module gets compiled like any Lean lib; the shim gets
linked via [`LeanBindgen.Lake.buildCBinding`](./LeanBindgen/Lake.lean):

```lean
extern_lib mylib_shim pkg :=
  buildCBinding pkg "mylib-shim" {
    shimSources := #["csrc/mylib-shim.c"]
    includeDirs := #["/path/to/mylib/include"]
  }
```

Lake (as of v4.29) doesn't let downstream lakefiles import modules
from required deps, so the helper's body is also published as a
self-contained stanza you can paste directly into your own
`lakefile.lean` (see
[`examples/clingo-signature-runtime/lakefile.lean`](./examples/clingo-signature-runtime/lakefile.lean)).

## Status

| Component | State |
|---|---|
| Tokenizer | working; 8/8 tests |
| Parser | working; 8/8 tests; clears full clingo.h (414 decls) |
| Pretty-printer | working; 12/12 tests |
| Codegen — Lean | working; 8 type mappings, 8 function styles |
| Codegen — shim | same; passes `cc -fsyntax-only` against system clingo.h |
| Full clingo coverage | 167 types, 66 functions; ~170 KB shim compiles clean |
| Structural verification | 134/134 matched pairs equivalent to hand-written cleango |
| Runtime validation | 32/32 assertions against libclingo 5.8.0 |
| Memory | constant 13/288-byte baseline under `leaks` |

## License

The reference material under `reference/cleango/` is from
[cleango](https://github.com/kiranandcode/cleango) (MIT). The
opencompl C parser sources under `reference/c-parser-upstream/` are
their own repository (see its `LICENSE` if you redistribute).
Everything else in this project is unlicensed pending a decision —
treat it as private until that's resolved.
