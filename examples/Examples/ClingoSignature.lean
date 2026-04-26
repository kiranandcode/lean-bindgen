import LeanBindgen.Annotation

open LeanBindgen

/-! Annotation file for the `clingo_signature_*` slice of clingo.h.
This is the smallest end-to-end target — one scalar typedef, eight
functions, two function styles. -/

def clingoSignatureBindings : Bindings := {
  headerPath := "reference/cleango/bindings/clingo.h"
  leanModule := `Generated.Signature
  outDir     := ".lake/build/generated"
  shimPath   := ".lake/build/generated/signature-shim.c"
  libPrefix  := "clingo"
  leanImports := #[]
  types := #[
    { cName := "clingo_signature_t", lean := "Signature",
      mapping := .scalarNewtype .uint64 }
  ]
  functions := #[
    -- The constructor: `bool clingo_signature_create(name, arity, positive, *out)`.
    { cName := "clingo_signature_create"
      lean  := "Signature.mk"
      style := .outParamBoolStatus 3 "clingo_error_message" },
    -- Direct-style accessors.
    { cName := "clingo_signature_name",          lean := "Signature.name" },
    { cName := "clingo_signature_arity",         lean := "Signature.arity" },
    { cName := "clingo_signature_is_positive",   lean := "Signature.isPositive" },
    { cName := "clingo_signature_is_negative",   lean := "Signature.isNegative" },
    { cName := "clingo_signature_is_equal_to",   lean := "Signature.beq" },
    { cName := "clingo_signature_is_less_than",  lean := "Signature.blt" },
    { cName := "clingo_signature_hash",          lean := "Signature.hash" }
  ]
}
