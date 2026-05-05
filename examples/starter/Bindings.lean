import LeanBindgen

open LeanBindgen LeanBindgen.DSL

/-! Binding specification for the counter library.
    This defines *what* to generate — run the codegen driver to produce
    the actual Lean module and C shim. -/

def counterBindings : Bindings := c_bindings {
  header "vendor/counter.h"
  module Generated.Counter
  out_dir "Generated"
  shim "csrc/counter-shim.c"
  lib "counter"

  -- Error enum: maps C enum counter_error_e → Lean inductive Error
  enum counter_error_t => Error tag counter_error_e
    | counter_error_none => none
    | counter_error_overflow => overflow
    | counter_error_null_pointer => nullPointer

  -- Opaque handle: GC calls counter_free when last reference drops
  opaque counter_t => Counter freed_by counter_free

  -- Functions
  cfn counter_create => create +io                                               -- direct: returns Counter
  cfn counter_increment => increment bool_status on_error string counter_error_message +io  -- bool → Except
  cfn counter_get_value => getValue +io                                          -- reads mutable state
  cfn counter_version => version multi_out[0, 1, 2]                             -- void + 3 out-params → tuple
}
