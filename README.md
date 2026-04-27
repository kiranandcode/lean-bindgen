# lean-bindgen

Generate Lean 4 bindings — `@[extern]` declarations on the Lean side
plus a C shim that bridges Lean's runtime ABI — from a C header and a
small annotation file. Validated end-to-end against libclingo: 24
runtime assertions, all green, no leaks.

The codegen is *not* a complete C-to-Lean translator. It targets the
mechanical 70–80% (extern decls + marshalling glue) and leaves the
ergonomic API layer — renaming, default args, type-class instances,
custom `match` elaborators — to be hand-written on top. See
[`PLAN.md`](./PLAN.md) for what is and isn't covered, with an
explicit gap analysis vs. cleango's full surface.

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
  style := .outParamBoolStatus 3 "clingo_error_message" }
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

Five `TypeMapping` kinds, two `FunctionStyle` kinds, plus per-function
`borrowedParams` / `callbackUserDataParams` / `externSymbol` overrides.

| Type mapping | Generates |
|---|---|
| `scalarNewtype` | `def Foo := UIntN deriving Repr, Inhabited` |
| `inductiveEnum` | Lean inductive + per-enum cidx ↔ C-int conversion helpers |
| `opaquePointer (finalizer)` | `opaque Foo : Type` + `lean_external_class` registration with the named finalizer wired through Lean's GC |
| `structRecord` | Lean `structure` + `<lean>_to_lean` / `lean_to_<lean>` field-by-field marshallers |
| `callback` | `def Foo := T1 → T2 → IO Unit` (auto-derived from the C function-pointer signature) + per-typedef trampoline that marshals C args back into Lean and invokes the closure via `lean_apply_*` |

| Function style | Generates |
|---|---|
| `direct` (pure or `inIO`) | Plain extern + one-line shim |
| `outParamBoolStatus (idx, errFn)` | Drops the out-param from the Lean signature, wraps the result in `IO (Except String T)`, builds `Except.ok` on success and `Except.error` (from `errFn()`) on failure |

What's *not* yet supported — and why it matters for cleango's full
AST — is in [`PLAN.md`](./PLAN.md).

## Project layout

```
lean-bindgen/
├── lakefile.lean
├── lean-toolchain                       v4.29.0-rc6
├── LeanBindgen.lean                     top-level umbrella
├── LeanBindgen/
│   ├── Lake.lean                        reusable extern_lib helper
│   ├── Annotation.lean                  Bindings / TypeAnno / FunctionAnno
│   ├── Codegen.lean                     the emitter
│   └── C/
│       ├── Ast.lean                     flat semantic AST
│       ├── Pretty.lean                  AST → C source (K&R declarators)
│       ├── Token.lean                   tokenizer
│       └── Parser.lean                  recursive-descent header parser
├── examples/
│   ├── Examples.lean                    umbrella for example annotations
│   ├── Examples/
│   │   └── ClingoSignature.lean         the example bindings spec
│   └── clingo-signature-runtime/        sub-package: link-and-run validation
│       ├── lakefile.lean                inlined buildCBinding stanza,
│       │                                links /opt/homebrew/lib/libclingo
│       ├── Generated/Signature.lean     ← generated by `test-codegen`
│       ├── csrc/signature-shim.c        ← generated by `test-codegen`
│       ├── csrc/test-helpers.c          hand-written extras for paths
│       │                                the codegen doesn't yet cover
│       └── LinkTest.lean                runtime test exe
├── test/
│   ├── PrettyTest.lean                  AST → C round-trip (11 cases)
│   ├── TokenTest.lean                   tokenizer (7 cases)
│   ├── ParserTest.lean                  parse → pretty round-trip (7 cases)
│   ├── Soak.lean                        full clingo.h soak (414 decls)
│   └── CodegenTest.lean                 drives codegen + cc -fsyntax-only
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
# Produces examples/clingo-signature-runtime/Generated/Signature.lean
# and examples/clingo-signature-runtime/csrc/signature-shim.c, then
# runs `cc -fsyntax-only` on the shim against lean.h + clingo.h.
```

### End-to-end link-and-run (needs libclingo)

```sh
brew install clingo                 # 5.8.0 at time of writing
lake build test-codegen && ./.lake/build/bin/test-codegen   # regenerate
cd examples/clingo-signature-runtime
lake build && ./.lake/build/bin/link-test
# 24 assertions, all green. Construct/use/drop a libclingo Control
# 100 times to exercise the finalizer; invoke the codegen-emitted
# logger trampoline directly to confirm the closure-application path.
```

### Memory check

```sh
leaks --atExit -- ./.lake/build/bin/link-test | grep '^Process'
# Expect: 13 leaks for 288 total leaked bytes
# (Baseline 12/256 from Lean's runtime statics; the +1/+32 is
#  libclingo's thread-local error buffer. Constant whether we
#  allocate 0, 1, or 100 Controls.)
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
      style := .outParamBoolStatus 1 "mylib_last_error" },
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

14 commits on `main`; full development history.

| Component | State |
|---|---|
| Tokenizer | working; 7/7 tests |
| Parser | working; 7/7 tests; clears full clingo.h (414 decls) |
| Pretty-printer | working; 11/11 tests |
| Codegen — Lean | working for the 5 type mappings & 2 function styles above |
| Codegen — shim | same; passes `cc -fsyntax-only` against system libraries |
| Runtime validation | 24/24 assertions against libclingo 5.8.0 |
| Memory | constant 13/288-byte baseline under `leaks` |
| AST-level coverage (recursive types, tagged unions, arrays, …) | not yet — see [`PLAN.md`](./PLAN.md) |

## License

The reference material under `reference/cleango/` is from
[cleango](https://github.com/kiranandcode/cleango) (MIT). The
opencompl C parser sources under `reference/c-parser-upstream/` are
their own repository (see its `LICENSE` if you redistribute).
Everything else in this project is unlicensed pending a decision —
treat it as private until that's resolved.
