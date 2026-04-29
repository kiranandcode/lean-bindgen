# Plan: AST-Based Codegen Refactor + Remaining Gaps

## Context

The codegen (`LeanBindgen/Codegen.lean`, ~2700 lines) builds C shim and Lean module source via direct string concatenation — ~63% of the file is `s!"..."` string building. This makes the code fragile (unbalanced braces, escape issues like the `\{` bug we just hit), hard to test, and hard to compose. The goal: generate a **C AST** and a **Lean declaration AST**, then pretty-print them to source strings.

Additionally, 2 API functions remain unimplemented: `clingo_control_solve` (event callback dispatch) and `clingo_version` (3 out-params → tuple).

---

## File Organization

```
LeanBindgen/
  Gen/
    CShim.lean          -- C shim output AST types
    CShimPretty.lean    -- C shim AST → String
    LeanDecl.lean       -- Lean declaration output AST types
    LeanDeclPretty.lean -- Lean declaration AST → String
  Codegen.lean          -- emit* functions produce AST instead of strings
  ...existing files unchanged...
```

Update `LeanBindgen.lean` with new imports.

---

## Part A: C Shim AST (`LeanBindgen/Gen/CShim.lean`)

Purpose-built C subset covering only the patterns we emit. Types are `String` (not `CType`) since resolution already produces C type spellings.

**Expressions** (~17 constructors): `var`, `intLit`, `strLit`, `null`, `call`, `field` (`.`), `arrow` (`->`), `subscript`, `deref` (`*`), `addrOf` (`&`), `cast`, `binop`, `unop`, `ternary`, `sizeof`, `paren`, `raw` (escape hatch).

**Statements** (~12 constructors): `varDecl`, `assign`, `assignOp` (`|=`), `expr`, `ret`, `ifElse`, `switch` (with cases array + default), `forLoop`, `block`, `comment`, `blank`, `raw`.

**Top-level** (~7 constructors): `include`, `forwardDecl`, `globalVar`, `funcDef` (with `CLinkage`: leanExport/static/plain), `comment`, `blank`, `raw`.

**File**: `CShimFile` = `Array CTopLevel`.

---

## Part B: Lean Declaration AST (`LeanBindgen/Gen/LeanDecl.lean`)

**Declarations** (~8 constructors): `defAlias`, `opaque_`, `inductive_` (ctors as `Array (name × Option payloadStr)`), `structure_` (fields as `Array (name × type)`), `externOpaque` (with extern symbol), `mutual_` (wraps other decls), `comment`, `blank`.

**Module**: `LeanModule` = headerComment + imports + namespace + `Array LeanDecl`.

---

## Part C: Pretty-Printers

### `CShimPretty.lean`
- `CExpr.render : CExpr → String` — inline, no newlines
- `CStmt.render (indent : Nat) : CStmt → String` — indented lines
- `CTopLevel.render : CTopLevel → String`
- `CShimFile.render : CShimFile → String`

No precedence tracking — use `CExpr.paren` explicitly. Follow the pattern established in `LeanBindgen/C/Pretty.lean`.

### `LeanDeclPretty.lean`
- `LeanDecl.render : LeanDecl → String`
- `LeanModule.render : LeanModule → String`

---

## Part D: Incremental Migration

Each phase is a standalone commit passing all tests. Strategy: convert one emitter at a time, render AST→String at the boundary so orchestrators don't change until the end.

### Phase 0: Infrastructure
- Create 4 new files (AST types + pretty-printers)
- Add imports to `LeanBindgen.lean` and `lakefile.lean` if needed
- Small unit test: construct a known `CTopLevel.funcDef`, render, check output

### Phase 1: Lean declarations
- `emitTypeDecl` → `LeanDecl`
- `emitFunctionDecl` → `LeanDecl.externOpaque`
- `emitLeanModule` → `LeanModule` then `.render`
- SCC ordering / field resolution logic unchanged

### Phase 2: Simple C helpers
- `emitEnumHelpers` → `Array CTopLevel` (two switch-based funcDefs)
- `emitOpaqueClass` → `Array CTopLevel` (finalizer + global + getter)
- `emitBitfieldHelpers` → `Array CTopLevel`

### Phase 3: Struct helpers
- `emitStructHelpers` + `emitFreeHelper` → `Array CTopLevel`
- Ctor layout logic (`reorderForCtorLayout`) unchanged; field-by-field emissions become AST node construction

### Phase 4: Tagged union helpers
- `emitTaggedUnionHelpers` + `emitTaggedUnionFreeHelper` → `Array CTopLevel`

### Phase 5: Callback trampolines
- `emitCallbackTrampoline` + `emitReverseTrampoline` → `CTopLevel`

### Phase 6: Shim function emitter
- Break `emitShimFunction` into per-style builders (7 variants), each → `Array CStmt`
- Convert `renderParamPass` → `(Array CStmt, Array CExpr, Array CStmt)` with AST constructors
- Convert `renderReturn` → `Array CStmt`
- Wrapper packages into `CTopLevel.funcDef`

### Phase 7: Orchestrator + cleanup
- `emitShim` → assemble `CShimFile` from `Array CTopLevel`, then `.render`
- Remove all `raw` escape hatches
- Delete dead string helpers

---

## Part E: Remaining 2 Functions

### E1. `clingo_version` (simple)
Add `FunctionStyle.multiOutParam (outParamIndices : Array Nat)`. The codegen declares one local per out-param, calls the void C function, boxes each result, builds a tuple via `lean_alloc_ctor(0, N, 0)`. Lean return type: nested `Prod`. Implement during Phase 6.

### E2. `clingo_control_solve` (complex, defer)
Requires custom event-dispatch trampoline: `switch (event_type) { case model: wrap Model; case stats: wrap Stats*; case finish: wrap SolveResult; }`. The current callback codegen assumes uniform params.

Approach: Add `TypeMapping.eventCallback` with per-variant cast/marshal info. Self-contained — implement after Phase 7 or defer.

---

## Verification (after each phase)

1. `lake build 'test-codegen' && .lake/build/bin/test-codegen` — all pattern checks + cc -fsyntax-only
2. `cd examples/clingo-signature-runtime && lake build && .lake/build/bin/link-test` — 27 runtime assertions
3. Diff generated output before/after to catch whitespace regressions

## Key Files
- `LeanBindgen/Codegen.lean` (2723 lines) — main refactoring target
- `LeanBindgen/C/Pretty.lean` — existing pretty-printer pattern to follow
- `test/CodegenTest.lean` — regression gate
- `examples/clingo-signature-runtime/LinkTest.lean` — runtime correctness gate
