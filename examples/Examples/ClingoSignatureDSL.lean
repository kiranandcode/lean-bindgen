import LeanBindgen

open LeanBindgen LeanBindgen.DSL

/-! DSL version of ClingoSignature.lean — same bindings, ~55% fewer lines. -/

def clingoSignatureBindingsDSL : Bindings := c_bindings {
  header "reference/cleango/bindings/clingo.h"
  module Generated.SignatureDSL
  out_dir ".lake/build/generated"
  shim ".lake/build/generated/signature-dsl-shim.c"
  lib "clingo"

  scalar clingo_signature_t => Signature : UInt64

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

  scalar clingo_symbol_t => Symbol : UInt64

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
  cfn clingo_signature_is_equal_to => beq
  cfn clingo_signature_is_less_than => blt
  cfn clingo_signature_hash => hash
  cfn clingo_error_code => errorCode +io
  cfn clingo_error_string => errorString
  cfn clingo_control_interrupt => interrupt +io
  cfn clingo_parse_term => parseTerm out[4] on_error string clingo_error_message callback_data [2]
  cfn clingo_symbol_create_function => mkFun out[4] on_error string clingo_error_message array_pairs [(1, 2)]
}
