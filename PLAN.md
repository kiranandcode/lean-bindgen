# lean-bindgen — coverage and gaps

A snapshot of what the codegen produces today and the path to
auto-generating the full surface of cleango (including `Clingo/Ast.lean`
+ the corresponding 2 KLOC of shim glue).

## Status snapshot

### TypeMapping coverage

| Mapping | What it produces | Validated by |
|---|---|---|
| `scalarNewtype` | `def Foo := UIntN deriving Repr, Inhabited` | `Signature`, `Symbol` |
| `inductiveEnum` | `inductive Foo where \| v1 \| v2 …` + per-enum cidx ↔ C-int helpers | `Error` (5), `Warning` (7); 5 string round-trips against libclingo |
| `opaquePointer (finalizer)` | `opaque Foo : Type` + `lean_external_class` registration in shim, finalizer wrapper, lazy class getter | `Control` (with `clingo_control_free`); 100-Control finalizer stress under `leaks` |
| `structRecord` | `structure Foo where …` + per-struct `<lean>_to_lean` / `lean_to_<lean>` helpers | `Location` (2 String + 4 USize); 7-field round-trip |
| `callback` | `def Foo := T1 → T2 → IO Unit` (auto-derived) + per-typedef trampoline | `Logger : Warning → String → IO Unit`; trampoline-direct invocation captures `(.runtimeError, "synthetic test message")` |

### FunctionStyle coverage

| Style | What it produces |
|---|---|
| `direct` (pure or `inIO`) | `@[extern] opaque foo : T1 → T2 → ...` plus a one-line shim |
| `outParamBoolStatus (idx, errorFn)` | Drops the out-param from the Lean signature; wraps the result in `IO (Except String T)`; on success builds `Except.ok` via `lean_alloc_ctor`, on failure reads `errorFn()` for the message. Out-param can be a scalar typedef, an enum, an opaque, or a struct. |

### Validation

| Layer | Check |
|---|---|
| Parse | Full `clingo.h` (154 KB / 6095 tokens) → 414 decls; round-trip `parse → pretty-print → re-parse` is identity in decl count |
| Pretty | 11/11 unit tests including function-pointer typedef shape (`clingo_logger_t`-like) |
| Codegen → Lean | Generated `Generated/Signature.lean` builds clean against the system Lean toolchain |
| Codegen → C | Generated shim compiles under `cc -fsyntax-only` against `lean.h` + the system `clingo.h` |
| End-to-end runtime | 24 assertions pass against libclingo 5.8.0 (Homebrew); covers every codegen path |
| Memory | `leaks --atExit` shows constant `13 leaks / 288 bytes` regardless of how many Controls / closures we create-and-drop (baseline: 12/256 from Lean's own runtime statics, +1 leak / +32 bytes for libclingo's thread-local error buffer) |

## Gaps to full cleango AST coverage

cleango's `Clingo/Ast.lean` is ~280 lines / 38 type declarations, with
matching ~2080 lines of shim glue. To auto-generate that, four new
codegen capabilities are needed plus one quality-of-life concern. They
are listed in roughly the order they'd be implemented (each unlocks
something for the next).

### 1. Array+size pairs (item already on the original list)

C side:

```c
clingo_symbol_t const *args, size_t args_size
```

Lean side: `Array Symbol`. Per element marshal as if it were a
parameter; the whole array becomes a single Lean argument.

What it unlocks:
- `clingo_symbol_create_function`
- `clingo_control_ground` (parts list)
- `clingo_model_symbols`
- `Aggregate.elements`, `Term.Data.Function`'s `arguments`, etc.

What it takes:
- New `ParamMarshal.array (elementMarshal : ParamMarshal) (cElemTy : String)`.
- New `ReturnMarshal.array (elementMarshal : ReturnMarshal) (cElemTy : String)`.
- `FunctionAnno.arrayPairs : List (Nat × Nat)` — pairs (data-param-idx, size-param-idx) so the codegen knows which `size_t` slot belongs to which array param.
- Shim:
  - For Lean → C: walk the Lean `Array T`, malloc a buffer of N elements, marshal each, pass `(buffer, N)`, free after the call.
  - For C → Lean: walk N elements at the C buffer, marshal each, build a Lean `Array T`.
- For arrays-of-strings and arrays-of-opaques, watch lifetime (Lean closures own the strings during the call only).

Estimated size: ~half a session.

### 2. Nested struct-by-value fields

When a struct field has a type that's itself a `structRecord`-mapped
type, the shim's `lean_to_<outer>` and `<outer>_to_lean` helpers need
to delegate to the inner struct's helpers instead of reading/writing a
scalar.

What it unlocks: most of the leaf-level cleango AST records
(`Comparison`, `AggregateGuard`, `CSPProductTerm`, …) where one struct
contains another by value.

What it takes:
- In `emitStructHelpers`, the per-field emit currently does
  `lean_ctor_set_<suffix>(o, off, (T)v.field)`. Generalise to dispatch
  on the field's `ParamMarshal` / `ReturnMarshal`:
  - scalar → existing `lean_ctor_set_<suffix>`
  - boxed (string, opaque, nested struct) → `lean_ctor_set(o, idx, <marshal>)`
  - the offset/index threading already works; just the per-field expression changes.
- For nested *struct-by-value* fields specifically, we need to flatten
  the nested struct's payload into the outer struct's lean ctor —
  *or* box the inner struct as a separate `lean_object*` and store a
  pointer. Lean does the latter for its own struct fields; we should
  match that.

Estimated size: a few hours, mostly extending the existing
`emitStructHelpers` field loop.

### 3. Mutual recursion in the generated Lean

cleango's `Term` and `Term.Data` reference each other. Lean wraps such
groups in a `mutual ... end` block. Today the codegen emits each type
declaration independently in `b.types` order.

What it takes:
- Build a type-dependency graph: a node per `TypeAnno`, an edge
  `A → B` if `A`'s body mentions `B`.
- Tarjan SCC on the graph.
- For each SCC of size > 1, wrap the constituent declarations in
  `mutual ... end`.

Order-of-magnitude: bookkeeping; a few hundred lines.

### 4. Tagged unions → Lean inductive with payload ctors

This is the biggest piece. C side often looks like:

```c
typedef struct clingo_ast_term {
  clingo_location_t location;
  enum { ast_term_symbol, ast_term_variable, ast_term_function, ... } type;
  union {
    clingo_symbol_t symbol;
    char const *variable;
    struct { char const *name; clingo_ast_term_t const *args; size_t n_args; } function;
    /* … */
  } body;
} clingo_ast_term_t;
```

Lean side:

```lean
inductive Term.Data where
  | Symbol   (sym  : Symbol)
  | Variable (var  : String)
  | Function (name : String) (arguments : Array Term)
  | …
```

What it takes:
- New `TypeMapping.taggedUnion` carrying:
  - Tag field name and the discriminator C enum.
  - For each variant: (C tag value, Lean constructor name,
    payload field path, payload struct mapping).
- Lean-side emit: an `inductive` with one constructor per variant,
  payload arguments resolved from each variant's struct.
- Shim emit:
  - `<lean>_to_lean(<typedef> v)`: switch on the C tag; for each case,
    allocate a Lean ctor with the right `cidx` and per-variant payload
    fields.
  - `lean_to_<lean>(b_lean_obj_arg obj)`: read `lean_ptr_tag(obj)` for
    the cidx; switch over cidx, build the C struct (set `type` and the
    matching union field).
- This combines with #1 (arrays inside variants) and #3 (mutual
  recursion via `Term ↔ Term.Data`).

Estimated size: a full focused session.

### 5. Owned-by-C deep-structure lifetime (`malloc`/`free`)

For values built in Lean and handed to C as a deep struct (an AST
node containing pointers to nested AST nodes), the shim has to:
- Allocate (with `malloc`) the nested payloads on the heap, since they
  outlive the Lean-side scratch buffers.
- Provide a `lean_clingo_free_<type>` (recursive) that frees the
  payloads after the C library is done with them.

cleango's `clingo-shim.c` has six macros for this dance:

```c
CLINGO_CONV_OBJ(TY, RESULT, DEST, OBJ)
CLINGO_FREE_OBJ(TY, RESULT, DEST)
CLINGO_CONV_ARRAY(TY, RESULT, DEST, OBJ)
CLINGO_FREE_ARRAY(TY, RESULT, DEST)
CLINGO_CONV_ARRAY_SIZED(TY, RESULT, DEST, OBJ, SIZE)
CLINGO_FREE_ARRAY_SIZED(TY, RESULT, DEST, SIZE)
```

What it takes:
- For each `taggedUnion` and `structRecord` mapping, also emit a
  recursive `lean_clingo_free_<type>` that releases nested payloads.
- The shim's bool-status-with-out-param emitter needs to call free
  *after* the C library is done with the data — usually right after
  the `clingo_*` call returns, but for some APIs the C library
  retains the data and frees later.

This piece is *easy to get wrong*. cleango chose explicit malloc/free
to make the ownership model clear. Without it, deep-struct callers
would leak silently — the leaks would not show up in our existing
finalizer test because finalizers don't run until ref drops, not
during the C call.

Estimated size: depends on how many APIs in scope; for AST-only,
modest.

## Out of scope (deliberately)

- **`Nat` / `Int` (Lean bignums)**. Not encountered in clingo. If we
  ever target an API that returns arbitrary-precision integers,
  we'd add `lean_int_to_int` / `lean_int_to_nat` boxing helpers and
  a corresponding `ParamMarshal.bignum`.
- **C `Char` (4-byte unicode)**. clingo's char-typed APIs all use
  `char const *` (which we already map to `String`), so we map
  C `char` → `UInt8` and consider it a deliberate simplification.
- **Variadic functions**. clingo doesn't expose any. Lean's
  `lean_apply_*` doesn't generalise to varargs so binding them at
  all is impractical.
- **Persistent callbacks** (e.g., a `clingo_logger_t` stored inside
  a `clingo_control_t` for the Control's lifetime). Today's callback
  marshalling refs the closure across one C call only — fine for
  parser callbacks, ground callbacks, solve-event callbacks (all
  invoked only during the corresponding API call). If we ever need
  the persistent shape we'd attach the Lean closure to the
  external-class payload and dec it from the finalizer.
- **Wrapper layer** (cleango's `Symbol.cast`, custom `match` elab,
  default args, `mk_safe`/`mk_unsafe` pairs, ergonomic
  `with*` combinators). These are intentionally hand-written in
  cleango and should stay that way; the codegen is responsible for
  the *boring* extern + shim glue, not the API design.

## Suggested next sequence

1. Arrays (#1) — opens up the largest immediate surface.
2. Nested struct-by-value (#2) — small change, reuses #1 for arrays
   inside structs.
3. Mutual recursion (#3) — bookkeeping, minor.
4. Tagged unions (#4) — the big one.
5. Owned deep-structure lifetime (#5) — needed when emitted bindings
   pass deep AST structures *into* the C library (most clingo AST
   construction APIs).

After #1–#3, the codegen should be able to auto-generate the
shallow half of `Clingo/Ast.lean`. After #4–#5, the full AST plus the
recursive `Clingo/Lang.lean` material.
