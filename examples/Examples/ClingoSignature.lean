import LeanBindgen.Annotation

open LeanBindgen

/-! Annotation file for a slice of clingo.h. Originally the
`clingo_signature_*` set; extended to include `clingo_error` to
exercise the inductive-enum codegen path. -/

def clingoSignatureBindings : Bindings := {
  headerPath := "reference/cleango/bindings/clingo.h"
  leanModule := `Generated.Signature
  outDir     := ".lake/build/generated"
  shimPath   := ".lake/build/generated/signature-shim.c"
  libPrefix  := "clingo"
  leanImports := #[]
  types := #[
    { cName := "clingo_signature_t", lean := "Signature",
      mapping := .scalarNewtype .uint64 },
    -- The C `enum clingo_error` plus its `int`-typedef pair becomes
    -- a Lean inductive. `cName` here is the typedef (`clingo_error_t`)
    -- — that's what appears in C function signatures.
    { cName := "clingo_error_t", lean := "Error",
      mapping := .inductiveEnum {
        enumTag  := "clingo_error"
        variants := #[
          ("clingo_error_success",   "success"),
          ("clingo_error_runtime",   "runtime"),
          ("clingo_error_logic",     "logic"),
          ("clingo_error_bad_alloc", "badAlloc"),
          ("clingo_error_unknown",   "unknown")
        ]
      } },
    -- Opaque external object: `clingo_control_t` is an incomplete C
    -- struct accessed only through `clingo_control_t *`. Lean wraps a
    -- pointer as an `opaque Control : Type`; the GC runs
    -- `clingo_control_free` when the last Lean reference drops.
    { cName := "clingo_control_t", lean := "Control",
      mapping := .opaquePointer "clingo_control_free" },
    -- Plain struct: `clingo_location_t` has 2 strings + 4 size_t.
    -- Lean's representation is 2 boxed (the Strings) followed by 4
    -- USize scalars. The shim emits per-field marshallers that
    -- mirror that layout.
    { cName := "clingo_location_t", lean := "Location",
      mapping := .structRecord {
        cStructTag := "clingo_location"
        fields := #[
          ("begin_file",   "beginFile"),
          ("end_file",     "endFile"),
          ("begin_line",   "beginLine"),
          ("end_line",     "endLine"),
          ("begin_column", "beginColumn"),
          ("end_column",   "endColumn")
        ]
      } }
  ]
  functions := #[
    -- The constructor: `bool clingo_signature_create(name, arity, positive, *out)`.
    { cName := "clingo_signature_create"
      lean  := "mk"
      style := .outParamBoolStatus 3 "clingo_error_message" },
    -- Direct-style accessors.
    { cName := "clingo_signature_name",         lean := "name" },
    { cName := "clingo_signature_arity",        lean := "arity" },
    { cName := "clingo_signature_is_positive",  lean := "isPositive" },
    { cName := "clingo_signature_is_negative",  lean := "isNegative" },
    { cName := "clingo_signature_is_equal_to",  lean := "beq" },
    { cName := "clingo_signature_is_less_than", lean := "blt" },
    { cName := "clingo_signature_hash",         lean := "hash" },
    -- Enum-typed: `errorCode` returns the thread-local error code
    -- (impure — `IO Error`), `errorString` is a pure mapping from a
    -- code to its string name.
    { cName := "clingo_error_code",   lean := "errorCode",   inIO := true },
    { cName := "clingo_error_string", lean := "errorString" },
    -- Opaque-receiving function: `clingo_control_interrupt` takes a
    -- `Control` and returns nothing. Exercises `lean_get_external_data`
    -- on the parameter side.
    { cName := "clingo_control_interrupt", lean := "interrupt", inIO := true }
  ]
}
