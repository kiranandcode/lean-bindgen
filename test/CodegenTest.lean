import LeanBindgen.C.Token
import LeanBindgen.C.Parser
import LeanBindgen.Codegen
import Examples.ClingoSignature
import Examples.ClingoFull
import Examples.CleangoProject
import Examples.ZlibBindings

open LeanBindgen LeanBindgen.C LeanBindgen.Codegen

/-- Resolve where to write generated artifacts. The lakefile under
`examples/clingo-signature-runtime/` expects `Generated/Signature.lean`
and `csrc/signature-shim.c` next to it. -/
private def runtimeRoot : System.FilePath := "examples/clingo-signature-runtime"

/-- Bindings for a synthetic deep-struct test that exercises pointer-to-
struct fields (malloc in toC, dereference in toLean, free helpers). -/
private def deepStructBindings : Bindings := {
  headerPath := "test/deep_struct.h"
  leanModule := `Generated.DeepStruct
  outDir     := "/tmp"
  shimPath   := "/tmp/deep-struct-shim.c"
  libPrefix  := "test"
  types := #[
    { cName := "inner_t", lean := "Inner",
      mapping := .structRecord {
        cStructTag := "inner"
        fields := #[("name", "name"), ("value", "value")]
      } },
    { cName := "outer_t", lean := "Outer",
      mapping := .structRecord {
        cStructTag := "outer"
        fields := #[("label", "label"), ("child", "child"),
                    ("embedded", "embedded"), ("count", "count")]
      } }
  ]
}

/-- Bindings for a recursive AST test that exercises array-in-struct
fields and recursive C helper calls via forward declarations. -/
private def recursiveAstBindings : Bindings := {
  headerPath := "test/recursive_ast.h"
  leanModule := `Generated.RecursiveAst
  outDir     := "/tmp"
  shimPath   := "/tmp/recursive-ast-shim.c"
  libPrefix  := "test"
  types := #[
    { cName := "location_t", lean := "Loc",
      mapping := .structRecord {
        cStructTag := "location"
        fields := #[("file", "file"), ("line", "line")]
      } },
    { cName := "function_node_t", lean := "FunctionNode",
      mapping := .structRecord {
        cStructTag := "function_node"
        fields := #[("name", "name"), ("arguments", "arguments")]
        arrayFields := #[("arguments", "size")]
      } },
    { cName := "unary_op_t", lean := "UnaryOp",
      mapping := .structRecord {
        cStructTag := "unary_op"
        fields := #[("op", "op"), ("argument", "argument")]
      } },
    { cName := "term_t", lean := "Term",
      mapping := .taggedUnion {
        cStructTag := "term"
        tagField := "type"
        tagEnum  := "term_type"
        sharedFields := #[("location", "location")]
        variants := #[
          { cTag := "term_type_symbol", leanCtor := "symbol",
            unionField := "symbol" },
          { cTag := "term_type_function", leanCtor := "function",
            unionField := "function" },
          { cTag := "term_type_unary_op", leanCtor := "unaryOp",
            unionField := "unary_op" }
        ]
      } }
  ]
}

/-- Bindings for a synthetic mixed-scalar struct that exercises Lean's
ctor field reordering: pointer → USize → other scalars (descending size).

C declaration order: name(str), count(u32), offset(usize), flag(u8),
tag(str), length(usize), kind(u16).

Expected Lean runtime layout:
  boxed:  name(0), tag(1)           — pointer slots
  USize:  offset(2), length(3)      — slot indices
  scalar: count(@32), kind(@36), flag(@38)  — byte offsets
-/
private def mixedScalarBindings : Bindings := {
  headerPath := "test/mixed_scalars.h"
  leanModule := `Generated.MixedScalars
  outDir     := "/tmp"
  shimPath   := "/tmp/mixed-scalars-shim.c"
  libPrefix  := "test"
  types := #[
    { cName := "mixed_scalars_t", lean := "MixedScalars",
      mapping := .structRecord {
        cStructTag := "mixed_scalars"
        fields := #[
          ("name",   "name"),
          ("count",  "count"),
          ("offset", "offset"),
          ("flag",   "flag"),
          ("tag",    "tag"),
          ("length", "length"),
          ("kind",   "kind")
        ]
      } }
  ]
}

set_option maxRecDepth 1024

def main : IO Unit := do
  let path := clingoSignatureBindings.headerPath
  IO.println s!"Codegen test against {path}"
  let src ← IO.FS.readFile path
  let toks ← IO.ofExcept (tokenize src)
  let header ← IO.ofExcept (parseHeader toks)
  IO.println s!"  parsed {header.decls.size} decls"
  let leanText ← IO.ofExcept (emitLeanModule clingoSignatureBindings header)
  let shimText ← IO.ofExcept (emitShim clingoSignatureBindings header)
  IO.println "\n=== Generated Lean module ==="
  IO.println leanText
  IO.println "=== Generated C shim ==="
  IO.println shimText
  -- Write the artefacts into the runtime sub-package layout.
  let leanFile := runtimeRoot / "Generated" / "Signature.lean"
  let shimFile := runtimeRoot / "csrc" / "signature-shim.c"
  IO.FS.createDirAll (runtimeRoot / "Generated")
  IO.FS.createDirAll (runtimeRoot / "csrc")
  IO.FS.writeFile leanFile leanText
  IO.FS.writeFile shimFile shimText
  IO.println s!"\nwrote {leanFile} ({leanText.length} bytes)"
  IO.println s!"wrote {shimFile} ({shimText.length} bytes)"
  let leanPrefix ← IO.Process.output { cmd := "lean", args := #["--print-prefix"] }
  let leanIncPath := s!"{leanPrefix.stdout.trim}/include"
  -- Validate main shim against system clingo.h.
  let out ← IO.Process.output {
    cmd := "cc"
    args := #["-fsyntax-only", "-I", leanIncPath,
              "-I", "/opt/homebrew/include",
              shimFile.toString]
  }
  if out.exitCode = 0 then
    IO.println "✓ shim compiles against system clingo.h"
  else
    IO.eprintln s!"✗ shim compile failed (exit {out.exitCode}):"
    IO.eprintln out.stderr
  -- === Tagged union codegen test (reference header only) ===
  IO.println "\n=== Tagged union codegen test ==="
  let tuLean ← IO.ofExcept (emitLeanModule taggedUnionBindings header)
  let tuShim ← IO.ofExcept (emitShim taggedUnionBindings header)
  IO.println tuLean
  IO.println tuShim
  let refIncPath := "reference/cleango/bindings"
  let tuShimFile : System.FilePath := "/tmp/tagged-union-shim.c"
  IO.FS.writeFile tuShimFile tuShim
  let tuOut ← IO.Process.output {
    cmd := "cc"
    args := #["-fsyntax-only", "-I", leanIncPath,
              "-I", refIncPath,
              tuShimFile.toString]
  }
  if tuOut.exitCode = 0 then
    IO.println "✓ tagged-union shim compiles against reference clingo.h"
  else
    IO.eprintln s!"✗ tagged-union shim compile failed (exit {tuOut.exitCode}):"
    IO.eprintln tuOut.stderr
  -- === Mixed-scalar struct layout test ===
  IO.println "\n=== Mixed-scalar struct layout test ==="
  let msPath := mixedScalarBindings.headerPath
  let msSrc ← IO.FS.readFile msPath
  let msToks ← IO.ofExcept (tokenize msSrc)
  let msHeader ← IO.ofExcept (parseHeader msToks)
  let msShim ← IO.ofExcept (emitShim mixedScalarBindings msHeader)
  IO.println msShim
  -- Verify the generated shim has the correct Lean ctor layout.
  -- Expected layout (64-bit):
  --   boxed:  name→slot 0, tag→slot 1
  --   USize:  offset→slot 2, length→slot 3
  --   scalar: count(u32)→@32, kind(u16)→@36, flag(u8)→@38
  let mut ok := true
  let checks := #[
    -- lean_alloc_ctor(0, num_objs=2, scalar_sz=23)
    -- scalar_sz = 2*8 (usize) + 4 (u32) + 2 (u16) + 1 (u8) = 23
    ("lean_alloc_ctor(0, 2, 23)", "ctor alloc: 2 boxed, 23 scalar bytes"),
    -- Boxed fields in declaration order.
    ("lean_ctor_set(o, 0, lean_mk_string(v.name", "name → boxed slot 0"),
    ("lean_ctor_set(o, 1, lean_mk_string(v.tag",  "tag → boxed slot 1"),
    -- USize fields: slot indices = num_objs + j.
    ("lean_ctor_set_usize(o, 2, (size_t)v.offset)", "offset → USize slot 2"),
    ("lean_ctor_set_usize(o, 3, (size_t)v.length)", "length → USize slot 3"),
    -- Other scalars by descending size, byte offset starts at (2+2)*8=32.
    ("lean_ctor_set_uint32(o, 32,", "count(u32) → byte offset 32"),
    ("lean_ctor_set_uint16(o, 36,", "kind(u16) → byte offset 36"),
    ("lean_ctor_set_uint8(o, 38,",  "flag(u8) → byte offset 38")
  ]
  for (pattern, desc) in checks do
    if (msShim.splitOn pattern).length > 1 then
      IO.println s!"  ✓ {desc}"
    else
      IO.eprintln s!"  ✗ {desc} — pattern not found: {pattern}"
      ok := false
  if ok then
    IO.println "✓ all mixed-scalar layout checks passed"
  else
    IO.eprintln "✗ some mixed-scalar layout checks failed"
  -- === Deep-struct codegen test ===
  IO.println "\n=== Deep-struct codegen test ==="
  let dsPath := deepStructBindings.headerPath
  let dsSrc ← IO.FS.readFile dsPath
  let dsToks ← IO.ofExcept (tokenize dsSrc)
  let dsHeader ← IO.ofExcept (parseHeader dsToks)
  let dsLean ← IO.ofExcept (emitLeanModule deepStructBindings dsHeader)
  let dsShim ← IO.ofExcept (emitShim deepStructBindings dsHeader)
  IO.println dsLean
  IO.println dsShim
  -- Verify key patterns in the generated shim.
  let mut dsOk := true
  let dsChecks := #[
    -- malloc for pointer-to-struct child field in lean_to_outer
    ("malloc(sizeof(inner_t))", "malloc for child ptr-to-struct"),
    -- dereference in outer_to_lean
    ("*v.child", "dereference child in toLean"),
    -- free_inner called on child in free_outer
    ("free_inner", "free_inner called in free_outer"),
    -- free((void *)p->child) in free_outer
    ("free((void *)p->child)", "free child pointer in free_outer"),
    -- free_inner(&p->embedded) for by-value nested struct
    ("free_inner(&p->embedded)", "free_inner on embedded in free_outer"),
    -- #include <stdlib.h> present
    ("#include <stdlib.h>", "stdlib.h included for malloc/free")
  ]
  for (pattern, desc) in dsChecks do
    if (dsShim.splitOn pattern).length > 1 then
      IO.println s!"  ✓ {desc}"
    else
      IO.eprintln s!"  ✗ {desc} — pattern not found: {pattern}"
      dsOk := false
  -- Compile the shim with cc -fsyntax-only.
  let dsShimFile : System.FilePath := "/tmp/deep-struct-shim.c"
  IO.FS.writeFile dsShimFile dsShim
  let dsOut ← IO.Process.output {
    cmd := "cc"
    args := #["-fsyntax-only", "-I", leanIncPath,
              "-I", "test",
              dsShimFile.toString]
  }
  if dsOut.exitCode = 0 then
    IO.println "  ✓ deep-struct shim compiles (cc -fsyntax-only)"
  else
    IO.eprintln s!"  ✗ deep-struct shim compile failed (exit {dsOut.exitCode}):"
    IO.eprintln dsOut.stderr
    dsOk := false
  if dsOk then
    IO.println "✓ all deep-struct checks passed"
  else
    IO.eprintln "✗ some deep-struct checks failed"
  -- === Recursive AST codegen test ===
  IO.println "\n=== Recursive AST codegen test ==="
  let raPath := recursiveAstBindings.headerPath
  let raSrc ← IO.FS.readFile raPath
  let raToks ← IO.ofExcept (tokenize raSrc)
  let raHeader ← IO.ofExcept (parseHeader raToks)
  let raLean ← IO.ofExcept (emitLeanModule recursiveAstBindings raHeader)
  let raShim ← IO.ofExcept (emitShim recursiveAstBindings raHeader)
  IO.println raLean
  IO.println raShim
  let mut raOk := true
  let raChecks := #[
    -- Array-in-struct: lean_array_push loop in function_node_to_lean
    ("lean_array_push", "array push loop in toLean"),
    -- Array-in-struct: malloc buffer in lean_to_function_node
    ("lean_to_array(lean_ctor_get", "array extraction in toC"),
    -- Free helper: per-element free + free buffer
    ("free_term", "free_term called (recursive free)"),
    -- Forward declarations present
    ("Forward declarations", "forward declarations emitted"),
    -- Recursive: unary_op helper calls lean_to_term / term_to_lean
    ("lean_to_term(", "lean_to_term called (recursive toC)"),
    ("term_to_lean(", "term_to_lean called (recursive toLean)"),
    -- stdlib included
    ("#include <stdlib.h>", "stdlib.h included")
  ]
  for (pattern, desc) in raChecks do
    if (raShim.splitOn pattern).length > 1 then
      IO.println s!"  ✓ {desc}"
    else
      IO.eprintln s!"  ✗ {desc} — pattern not found: {pattern}"
      raOk := false
  -- Compile the shim with cc -fsyntax-only.
  let raShimFile : System.FilePath := "/tmp/recursive-ast-shim.c"
  IO.FS.writeFile raShimFile raShim
  let raOut ← IO.Process.output {
    cmd := "cc"
    args := #["-fsyntax-only", "-I", leanIncPath,
              "-I", "test",
              raShimFile.toString]
  }
  if raOut.exitCode = 0 then
    IO.println "  ✓ recursive AST shim compiles (cc -fsyntax-only)"
  else
    IO.eprintln s!"  ✗ recursive AST shim compile failed (exit {raOut.exitCode}):"
    IO.eprintln raOut.stderr
    raOk := false
  if raOk then
    IO.println "✓ all recursive AST checks passed"
  else
    IO.eprintln "✗ some recursive AST checks failed"
  -- === Full cleango codegen test ===
  IO.println "\n=== Full cleango codegen test ==="
  let fullPath := clingoFullBindings.headerPath
  let fullSrc ← IO.FS.readFile fullPath
  let fullToks ← IO.ofExcept (tokenize fullSrc)
  let fullHeader ← IO.ofExcept (parseHeader fullToks)
  IO.println s!"  parsed {fullHeader.decls.size} decls from reference header"
  let fullLean ← IO.ofExcept (emitLeanModule clingoFullBindings fullHeader)
  let fullShim ← IO.ofExcept (emitShim clingoFullBindings fullHeader)
  IO.println s!"  generated Lean: {fullLean.length} bytes"
  IO.println s!"  generated C shim: {fullShim.length} bytes"
  let fullShimFile : System.FilePath := "/tmp/clingo-full-shim.c"
  IO.FS.writeFile fullShimFile fullShim
  let fullLeanFile : System.FilePath := "/tmp/ClingoFull.lean"
  IO.FS.writeFile fullLeanFile fullLean
  let fullOut ← IO.Process.output {
    cmd := "cc"
    args := #["-fsyntax-only", "-I", leanIncPath,
              "-I", refIncPath,
              fullShimFile.toString]
  }
  if fullOut.exitCode = 0 then
    IO.println "✓ full cleango shim compiles against reference clingo.h"
  else
    IO.eprintln s!"✗ full cleango shim compile failed (exit {fullOut.exitCode}):"
    IO.eprintln fullOut.stderr
  -- Pattern checks for new features.
  let mut fullOk := true
  let fullChecks := #[
    -- multiOutParam: clingo_version should have &major etc.
    ("clingo_version(&", "multiOutParam: clingo_version calls with &out-params"),
    ("lean_alloc_ctor(0, 2, 0)", "multiOutParam: Prod.mk ctor in version shim"),
    -- eventCallback: trampoline with switch dispatch
    ("clingo_solve_event_callback_t_trampoline", "eventCallback: trampoline emitted"),
    ("switch (type)", "eventCallback: switch dispatch on event type"),
    -- eventCallback: event type inductive in Lean
    ("inductive SolveEvent", "eventCallback: SolveEvent inductive in Lean"),
    -- controlSolve function present
    ("lean_clingo_control_solve", "controlSolve: shim function emitted")
  ]
  for (pattern, desc) in fullChecks do
    if (fullShim.splitOn pattern).length > 1 ||
       (fullLean.splitOn pattern).length > 1 then
      IO.println s!"  ✓ {desc}"
    else
      IO.eprintln s!"  ✗ {desc} — pattern not found"
      fullOk := false
  if fullOk then
    IO.println "✓ all full cleango pattern checks passed"
  else
    IO.eprintln "✗ some full cleango pattern checks failed"
  -- === Cleango project generation ===
  IO.println "\n=== Cleango project generation ==="
  let cgLean ← IO.ofExcept (emitLeanModule cleangoBindings fullHeader)
  let cgShim ← IO.ofExcept (emitShim cleangoBindings fullHeader)
  IO.println s!"  generated Lean: {cgLean.length} bytes"
  IO.println s!"  generated C shim: {cgShim.length} bytes"
  let cgLeanDir : System.FilePath := "examples/cleango/Clingo/Generated"
  let cgCsrcDir : System.FilePath := "examples/cleango/csrc"
  IO.FS.createDirAll cgLeanDir
  IO.FS.createDirAll cgCsrcDir
  let cgLeanFile := cgLeanDir / "ClingoBindings.lean"
  let cgShimFile := cgCsrcDir / "clingo-bindings-shim.c"
  IO.FS.writeFile cgLeanFile cgLean
  IO.FS.writeFile cgShimFile cgShim
  IO.println s!"  wrote {cgLeanFile}"
  IO.println s!"  wrote {cgShimFile}"
  let cgOut ← IO.Process.output {
    cmd := "cc"
    args := #["-fsyntax-only", "-I", leanIncPath,
              "-I", refIncPath,
              cgShimFile.toString]
  }
  if cgOut.exitCode = 0 then
    IO.println "✓ cleango shim compiles against reference clingo.h"
  else
    IO.eprintln s!"✗ cleango shim compile failed (exit {cgOut.exitCode}):"
    IO.eprintln cgOut.stderr
  -- === Zlib ByteArray codegen test ===
  IO.println "\n=== Zlib ByteArray codegen test ==="
  let zlibPath := zlibBindings.headerPath
  let zlibSrc ← IO.FS.readFile zlibPath
  let zlibToks ← IO.ofExcept (tokenize zlibSrc)
  let zlibHeader ← IO.ofExcept (parseHeader zlibToks)
  IO.println s!"  parsed {zlibHeader.decls.size} decls from zlib wrapper header"
  let zlibLean ← IO.ofExcept (emitLeanModule zlibBindings zlibHeader)
  let zlibShim ← IO.ofExcept (emitShim zlibBindings zlibHeader)
  IO.println s!"  generated Lean: {zlibLean.length} bytes"
  IO.println s!"  generated C shim: {zlibShim.length} bytes"
  -- Write artifacts to zlib-runtime sub-package.
  let zlibRuntimeRoot : System.FilePath := "examples/zlib-runtime"
  let zlibLeanDir := zlibRuntimeRoot / "Generated"
  let zlibCsrcDir := zlibRuntimeRoot / "csrc"
  IO.FS.createDirAll zlibLeanDir
  IO.FS.createDirAll zlibCsrcDir
  let zlibLeanFile := zlibLeanDir / "Zlib.lean"
  let zlibShimFile := zlibCsrcDir / "zlib-shim.c"
  IO.FS.writeFile zlibLeanFile zlibLean
  IO.FS.writeFile zlibShimFile zlibShim
  IO.println s!"  wrote {zlibLeanFile}"
  IO.println s!"  wrote {zlibShimFile}"
  -- Compile the shim with cc -fsyntax-only.
  let zlibOut ← IO.Process.output {
    cmd := "cc"
    args := #["-fsyntax-only", "-I", leanIncPath,
              "-I", "examples/zlib",
              zlibShimFile.toString]
  }
  if zlibOut.exitCode = 0 then
    IO.println "  ✓ zlib shim compiles (cc -fsyntax-only)"
  else
    IO.eprintln s!"  ✗ zlib shim compile failed (exit {zlibOut.exitCode}):"
    IO.eprintln zlibOut.stderr
  -- Pattern checks for ByteArray-specific codegen.
  let mut zlibOk := true
  let zlibChecks := #[
    -- ByteArray input: lean_sarray_cptr for data extraction
    ("lean_sarray_cptr", "ByteArray input: lean_sarray_cptr present"),
    -- ByteArray input: lean_sarray_size for length extraction
    ("lean_sarray_size", "ByteArray input: lean_sarray_size present"),
    -- ByteArray output: lean_alloc_sarray for result construction
    ("lean_alloc_sarray(1,", "ByteArray output: lean_alloc_sarray present"),
    -- ByteArray output: memcpy to fill the sarray
    ("memcpy(lean_sarray_cptr(", "ByteArray output: memcpy into sarray present"),
    -- Except.ok ctor for success path
    ("lean_alloc_ctor(1, 1, 0)", "Except.ok ctor in success path"),
    -- Error path: zlib_last_error call
    ("zlib_last_error()", "error path calls zlib_last_error"),
    -- stdlib.h included (for free)
    ("#include <stdlib.h>", "stdlib.h included"),
    -- string.h included (for memcpy)
    ("#include <string.h>", "string.h included"),
    -- Opaque types: DeflateState and InflateState
    ("get_deflate_state_class", "DeflateState opaque class getter"),
    ("get_inflate_state_class", "InflateState opaque class getter")
  ]
  for (pattern, desc) in zlibChecks do
    if (zlibShim.splitOn pattern).length > 1 then
      IO.println s!"  ✓ {desc}"
    else
      IO.eprintln s!"  ✗ {desc} — pattern not found: {pattern}"
      zlibOk := false
  -- Also check the Lean module has ByteArray in signatures.
  let leanChecks := #[
    ("ByteArray", "Lean module mentions ByteArray"),
    ("Except String ByteArray", "Lean return type: Except String ByteArray"),
    ("opaque DeflateState", "opaque DeflateState declared"),
    ("opaque InflateState", "opaque InflateState declared")
  ]
  for (pattern, desc) in leanChecks do
    if (zlibLean.splitOn pattern).length > 1 then
      IO.println s!"  ✓ {desc}"
    else
      IO.eprintln s!"  ✗ {desc} — pattern not found: {pattern}"
      zlibOk := false
  if zlibOk then
    IO.println "✓ all zlib ByteArray codegen checks passed"
  else
    IO.eprintln "✗ some zlib ByteArray codegen checks failed"
