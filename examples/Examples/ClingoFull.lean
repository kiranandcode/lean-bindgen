import LeanBindgen.Annotation

open LeanBindgen

/-! # Full cleango bindings annotation

Comprehensive annotation file that covers all types and functions from
the clingo C API (reference/cleango/bindings/clingo.h). Generates the
equivalent of the hand-written clingo-shim.c.

Types and functions are organised in dependency order: scalars, enums,
opaques, bitfield structs, callbacks, struct records, tagged unions,
then functions.
-/

-- ============================================================
-- Core types + functions (bound against the reference header)
-- ============================================================

def clingoFullBindings : Bindings := {
  headerPath := "reference/cleango/bindings/clingo.h"
  leanModule := `Generated.ClingoFull
  outDir     := ".lake/build/generated"
  shimPath   := ".lake/build/generated/clingo-full-shim.c"
  libPrefix  := "clingo"
  leanImports := #[]
  types := #[
    -- ── Scalar newtypes ──────────────────────────────────────
    { cName := "clingo_literal_t",   lean := "Literal",
      mapping := .scalarNewtype .int32 },
    { cName := "clingo_atom_t",      lean := "Atom",
      mapping := .scalarNewtype .uint32 },
    { cName := "clingo_id_t",        lean := "ClingoId",
      mapping := .scalarNewtype .uint32 },
    { cName := "clingo_weight_t",    lean := "Weight",
      mapping := .scalarNewtype .int32 },
    { cName := "clingo_signature_t", lean := "Signature",
      mapping := .scalarNewtype .uint64 },
    { cName := "clingo_symbol_t",    lean := "Symbol",
      mapping := .scalarNewtype .uint64 },
    { cName := "clingo_symbolic_atom_iterator_t", lean := "SymbolicAtomIterator",
      mapping := .scalarNewtype .uint64 },

    -- ── Enumerations ─────────────────────────────────────────
    { cName := "clingo_error_t", lean := "Error",
      mapping := .inductiveEnum {
        enumTag := "clingo_error"
        variants := #[
          ("clingo_error_success",   "success"),
          ("clingo_error_runtime",   "runtime"),
          ("clingo_error_logic",     "logic"),
          ("clingo_error_bad_alloc", "badAlloc"),
          ("clingo_error_unknown",   "unknown")
        ]
      } },
    { cName := "clingo_warning_t", lean := "Warning",
      mapping := .inductiveEnum {
        enumTag := "clingo_warning"
        variants := #[
          ("clingo_warning_operation_undefined",  "operationUndefined"),
          ("clingo_warning_runtime_error",        "runtimeError"),
          ("clingo_warning_atom_undefined",       "atomUndefined"),
          ("clingo_warning_file_included",        "fileIncluded"),
          ("clingo_warning_variable_unbounded",   "variableUnbounded"),
          ("clingo_warning_global_variable",      "globalVariable"),
          ("clingo_warning_other",                "other")
        ]
      } },
    { cName := "clingo_truth_value_t", lean := "TruthValue",
      mapping := .inductiveEnum {
        enumTag := "clingo_truth_value"
        variants := #[
          ("clingo_truth_value_free",  "free"),
          ("clingo_truth_value_true",  "true"),
          ("clingo_truth_value_false", "false")
        ]
      } },
    { cName := "clingo_symbol_type_t", lean := "SymbolType",
      mapping := .inductiveEnum {
        enumTag := "clingo_symbol_type"
        variants := #[
          ("clingo_symbol_type_infimum",  "infimum"),
          ("clingo_symbol_type_number",   "number"),
          ("clingo_symbol_type_string",   "string"),
          ("clingo_symbol_type_function", "function"),
          ("clingo_symbol_type_supremum", "supremum")
        ]
      } },
    { cName := "clingo_model_type_t", lean := "ModelType",
      mapping := .inductiveEnum {
        enumTag := "clingo_model_type"
        variants := #[
          ("clingo_model_type_stable_model",          "stableModel"),
          ("clingo_model_type_brave_consequences",    "braveConsequences"),
          ("clingo_model_type_cautious_consequences", "cautiousConsequences")
        ]
      } },
    { cName := "clingo_statistics_type_t", lean := "StatisticsType",
      mapping := .inductiveEnum {
        enumTag := "clingo_statistics_type"
        variants := #[
          ("clingo_statistics_type_empty", "empty"),
          ("clingo_statistics_type_value", "value"),
          ("clingo_statistics_type_array", "array"),
          ("clingo_statistics_type_map",   "map")
        ]
      } },
    { cName := "clingo_solve_event_type_t", lean := "SolveEventType",
      mapping := .inductiveEnum {
        enumTag := "clingo_solve_event_type"
        variants := #[
          ("clingo_solve_event_type_model",      "model"),
          ("clingo_solve_event_type_statistics", "statistics"),
          ("clingo_solve_event_type_finish",     "finish")
        ]
      } },

    -- ── AST enumerations ─────────────────────────────────────
    { cName := "clingo_ast_sign_t", lean := "Sign",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_sign"
        variants := #[
          ("clingo_ast_sign_none",            "none"),
          ("clingo_ast_sign_negation",        "negation"),
          ("clingo_ast_sign_double_negation", "doubleNegation")
        ]
      } },
    { cName := "clingo_ast_comparison_operator_t", lean := "ComparisonOperator",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_comparison_operator"
        variants := #[
          ("clingo_ast_comparison_operator_greater_than",  "gt"),
          ("clingo_ast_comparison_operator_less_than",     "lt"),
          ("clingo_ast_comparison_operator_less_equal",    "leq"),
          ("clingo_ast_comparison_operator_greater_equal", "geq"),
          ("clingo_ast_comparison_operator_not_equal",     "neq"),
          ("clingo_ast_comparison_operator_equal",         "eq")
        ]
      } },
    { cName := "clingo_ast_term_type_t", lean := "AstTermType",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_term_type"
        variants := #[
          ("clingo_ast_term_type_symbol",            "symbol"),
          ("clingo_ast_term_type_variable",          "variable"),
          ("clingo_ast_term_type_unary_operation",   "unaryOperation"),
          ("clingo_ast_term_type_binary_operation",  "binaryOperation"),
          ("clingo_ast_term_type_interval",          "interval"),
          ("clingo_ast_term_type_function",          "function"),
          ("clingo_ast_term_type_external_function", "externalFunction"),
          ("clingo_ast_term_type_pool",              "pool")
        ]
      } },
    { cName := "clingo_ast_unary_operator_t", lean := "UnaryOperator",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_unary_operator"
        variants := #[
          ("clingo_ast_unary_operator_minus",    "minus"),
          ("clingo_ast_unary_operator_negation", "negation"),
          ("clingo_ast_unary_operator_absolute", "absolute")
        ]
      } },
    { cName := "clingo_ast_binary_operator_t", lean := "BinaryOperator",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_binary_operator"
        variants := #[
          ("clingo_ast_binary_operator_xor",            "xor"),
          ("clingo_ast_binary_operator_or",             "or"),
          ("clingo_ast_binary_operator_and",            "and"),
          ("clingo_ast_binary_operator_plus",           "plus"),
          ("clingo_ast_binary_operator_minus",          "minus"),
          ("clingo_ast_binary_operator_multiplication", "multiplication"),
          ("clingo_ast_binary_operator_division",       "division"),
          ("clingo_ast_binary_operator_modulo",         "modulo"),
          ("clingo_ast_binary_operator_power",          "power")
        ]
      } },
    { cName := "clingo_ast_literal_type_t", lean := "AstLiteralType",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_literal_type"
        variants := #[
          ("clingo_ast_literal_type_boolean",    "boolean"),
          ("clingo_ast_literal_type_symbolic",   "symbolic"),
          ("clingo_ast_literal_type_comparison", "comparison"),
          ("clingo_ast_literal_type_csp",        "csp")
        ]
      } },
    { cName := "clingo_ast_aggregate_function_t", lean := "AggregateFunction",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_aggregate_function"
        variants := #[
          ("clingo_ast_aggregate_function_count", "count"),
          ("clingo_ast_aggregate_function_sum",   "sum"),
          ("clingo_ast_aggregate_function_sump",  "sump"),
          ("clingo_ast_aggregate_function_min",   "min"),
          ("clingo_ast_aggregate_function_max",   "max")
        ]
      } },
    { cName := "clingo_ast_theory_term_type_t", lean := "TheoryTermType",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_theory_term_type"
        variants := #[
          ("clingo_ast_theory_term_type_symbol",        "symbol"),
          ("clingo_ast_theory_term_type_variable",      "variable"),
          ("clingo_ast_theory_term_type_tuple",         "tuple"),
          ("clingo_ast_theory_term_type_list",          "list"),
          ("clingo_ast_theory_term_type_set",           "set"),
          ("clingo_ast_theory_term_type_function",      "function"),
          ("clingo_ast_theory_term_type_unparsed_term", "unparsedTerm")
        ]
      } },
    { cName := "clingo_ast_head_literal_type_t", lean := "HeadLiteralType",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_head_literal_type"
        variants := #[
          ("clingo_ast_head_literal_type_literal",        "literal"),
          ("clingo_ast_head_literal_type_disjunction",    "disjunction"),
          ("clingo_ast_head_literal_type_aggregate",      "aggregate"),
          ("clingo_ast_head_literal_type_head_aggregate", "headAggregate"),
          ("clingo_ast_head_literal_type_theory_atom",    "theoryAtom")
        ]
      } },
    { cName := "clingo_ast_body_literal_type_t", lean := "BodyLiteralType",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_body_literal_type"
        variants := #[
          ("clingo_ast_body_literal_type_literal",        "literal"),
          ("clingo_ast_body_literal_type_conditional",    "conditional"),
          ("clingo_ast_body_literal_type_aggregate",      "aggregate"),
          ("clingo_ast_body_literal_type_body_aggregate", "bodyAggregate"),
          ("clingo_ast_body_literal_type_theory_atom",    "theoryAtom"),
          ("clingo_ast_body_literal_type_disjoint",       "disjoint")
        ]
      } },
    { cName := "clingo_ast_statement_type_t", lean := "StatementType",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_statement_type"
        variants := #[
          ("clingo_ast_statement_type_rule",                   "rule"),
          ("clingo_ast_statement_type_const",                  "const"),
          ("clingo_ast_statement_type_show_signature",         "showSignature"),
          ("clingo_ast_statement_type_show_term",              "showTerm"),
          ("clingo_ast_statement_type_minimize",               "minimize"),
          ("clingo_ast_statement_type_script",                 "script"),
          ("clingo_ast_statement_type_program",                "program"),
          ("clingo_ast_statement_type_external",               "external"),
          ("clingo_ast_statement_type_edge",                   "edge"),
          ("clingo_ast_statement_type_heuristic",              "heuristic"),
          ("clingo_ast_statement_type_project_atom",           "projectAtom"),
          ("clingo_ast_statement_type_project_atom_signature", "projectAtomSignature"),
          ("clingo_ast_statement_type_theory_definition",      "theoryDefinition"),
          ("clingo_ast_statement_type_defined",                "defined")
        ]
      } },
    { cName := "clingo_ast_script_type_t", lean := "ScriptType",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_script_type"
        variants := #[
          ("clingo_ast_script_type_lua",    "lua"),
          ("clingo_ast_script_type_python", "python")
        ]
      } },
    { cName := "clingo_ast_theory_operator_type_t", lean := "TheoryOperatorType",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_theory_operator_type"
        variants := #[
          ("clingo_ast_theory_operator_type_unary",        "unary"),
          ("clingo_ast_theory_operator_type_binary_left",  "binaryLeft"),
          ("clingo_ast_theory_operator_type_binary_right", "binaryRight")
        ]
      } },
    { cName := "clingo_ast_theory_atom_definition_type_t", lean := "TheoryAtomDefinitionType",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_theory_atom_definition_type"
        variants := #[
          ("clingo_ast_theory_atom_definition_type_head",      "head"),
          ("clingo_ast_theory_atom_definition_type_body",      "body"),
          ("clingo_ast_theory_atom_definition_type_any",       "any"),
          ("clingo_ast_theory_atom_definition_type_directive", "directive")
        ]
      } },

    -- ── Opaque pointer types ─────────────────────────────────
    { cName := "clingo_control_t", lean := "Control",
      mapping := .opaquePointer "clingo_control_free" },
    { cName := "clingo_model_t", lean := "Model",
      mapping := .opaquePointer "" },       -- borrowed from SolveHandle
    { cName := "clingo_solve_handle_t", lean := "SolveHandle",
      mapping := .opaquePointer "" },       -- explicitly closed
    { cName := "clingo_statistics_t", lean := "Statistics",
      mapping := .opaquePointer "" },       -- borrowed from Control
    { cName := "clingo_program_builder_t", lean := "ProgramBuilder",
      mapping := .opaquePointer "" },       -- borrowed from Control
    { cName := "clingo_backend_t", lean := "Backend",
      mapping := .opaquePointer "" },       -- borrowed from Control
    { cName := "clingo_symbolic_atoms_t", lean := "SymbolicAtoms",
      mapping := .opaquePointer "" },       -- borrowed
    { cName := "clingo_theory_atoms_t", lean := "TheoryAtoms",
      mapping := .opaquePointer "" },       -- borrowed
    { cName := "clingo_solve_control_t", lean := "SolveControl",
      mapping := .opaquePointer "" },       -- borrowed
    { cName := "clingo_configuration_t", lean := "Configuration",
      mapping := .opaquePointer "" },       -- borrowed
    { cName := "clingo_propagate_init_t", lean := "PropagateInit",
      mapping := .opaquePointer "" },
    { cName := "clingo_propagate_control_t", lean := "PropagateControl",
      mapping := .opaquePointer "" },
    { cName := "clingo_assignment_t", lean := "Assignment",
      mapping := .opaquePointer "" },

    -- ── Bitfield structs ─────────────────────────────────────
    { cName := "clingo_solve_result_bitset_t", lean := "SolveResult",
      mapping := .bitfieldStruct {
        enumTag := "clingo_solve_result"
        fields := #[
          ("clingo_solve_result_satisfiable",   "satisfiable"),
          ("clingo_solve_result_unsatisfiable", "unsatisfiable"),
          ("clingo_solve_result_exhausted",     "exhausted"),
          ("clingo_solve_result_interrupted",   "interrupted")
        ]
      } },
    { cName := "clingo_show_type_bitset_t", lean := "ShowType",
      mapping := .bitfieldStruct {
        enumTag := "clingo_show_type"
        fields := #[
          ("clingo_show_type_csp",        "csp"),
          ("clingo_show_type_shown",      "shown"),
          ("clingo_show_type_atoms",      "atoms"),
          ("clingo_show_type_terms",      "terms"),
          ("clingo_show_type_all",        "all"),
          ("clingo_show_type_complement", "complement")
        ]
      } },

    -- ── Callback typedefs ────────────────────────────────────
    { cName := "clingo_logger_t", lean := "Logger",
      mapping := .callback },
    { cName := "clingo_symbol_callback_t", lean := "SymbolCallback",
      mapping := .callback },
    { cName := "clingo_ground_callback_t", lean := "GroundCallback",
      mapping := .callback },
    -- clingo_solve_event_callback_t: custom handling needed (void *event
    -- carries different typed data per event type, bool *goon out-param)
    { cName := "clingo_ast_callback_t", lean := "AstCallback",
      mapping := .callback },

    -- ── Struct records (core) ────────────────────────────────
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
      } },
    { cName := "clingo_weighted_literal_t", lean := "WeightedLiteral",
      mapping := .structRecord {
        cStructTag := "clingo_weighted_literal"
        fields := #[
          ("literal", "literal"),
          ("weight",  "weight")
        ]
      } },
    { cName := "clingo_part_t", lean := "Part",
      mapping := .structRecord {
        cStructTag := "clingo_part"
        fields := #[
          ("name",   "name"),
          ("params", "params")
        ]
        arrayFields := #[("params", "size")]
      } },

    -- ── AST struct records ───────────────────────────────────
    -- Small leaf structs first, then larger ones that reference them.

    { cName := "clingo_ast_id_t", lean := "AstId",
      mapping := .structRecord {
        cStructTag := "clingo_ast_id"
        fields := #[
          ("location", "location"),
          ("id",       "id")
        ]
      } },
    { cName := "clingo_ast_comparison_t", lean := "AstComparison",
      mapping := .structRecord {
        cStructTag := "clingo_ast_comparison"
        fields := #[
          ("comparison", "comparison"),
          ("left",       "left"),
          ("right",      "right")
        ]
      } },

    -- Unary/Binary/Interval/Function/Pool are sub-structs of Term
    -- but they're only accessed through tagged-union pointers, so they
    -- need their own struct records for the codegen to resolve field types.
    { cName := "clingo_ast_unary_operation_t", lean := "AstUnaryOperation",
      mapping := .structRecord {
        cStructTag := "clingo_ast_unary_operation"
        fields := #[
          ("unary_operator", "unaryOperator"),
          ("argument",       "argument")
        ]
      } },
    { cName := "clingo_ast_binary_operation_t", lean := "AstBinaryOperation",
      mapping := .structRecord {
        cStructTag := "clingo_ast_binary_operation"
        fields := #[
          ("binary_operator", "binaryOperator"),
          ("left",            "left"),
          ("right",           "right")
        ]
      } },
    { cName := "clingo_ast_interval_t", lean := "AstInterval",
      mapping := .structRecord {
        cStructTag := "clingo_ast_interval"
        fields := #[
          ("left",  "left"),
          ("right", "right")
        ]
      } },
    { cName := "clingo_ast_function_t", lean := "AstFunction",
      mapping := .structRecord {
        cStructTag := "clingo_ast_function"
        fields := #[
          ("name",      "name"),
          ("arguments", "arguments")
        ]
        arrayFields := #[("arguments", "size")]
      } },
    { cName := "clingo_ast_pool_t", lean := "AstPool",
      mapping := .structRecord {
        cStructTag := "clingo_ast_pool"
        fields := #[
          ("arguments", "arguments")
        ]
        arrayFields := #[("arguments", "size")]
      } },

    -- CSP types
    { cName := "clingo_ast_csp_product_term_t", lean := "CspProductTerm",
      mapping := .structRecord {
        cStructTag := "clingo_ast_csp_product_term"
        fields := #[
          ("location",    "location"),
          ("coefficient", "coefficient"),
          ("variable",    "variable")
        ]
      } },
    { cName := "clingo_ast_csp_sum_term_t", lean := "CspSumTerm",
      mapping := .structRecord {
        cStructTag := "clingo_ast_csp_sum_term"
        fields := #[
          ("location", "location"),
          ("terms",    "terms")
        ]
        arrayFields := #[("terms", "size")]
      } },
    { cName := "clingo_ast_csp_guard_t", lean := "CspGuard",
      mapping := .structRecord {
        cStructTag := "clingo_ast_csp_guard"
        fields := #[
          ("comparison", "comparison"),
          ("term",       "term")
        ]
      } },
    { cName := "clingo_ast_csp_literal_t", lean := "CspLiteral",
      mapping := .structRecord {
        cStructTag := "clingo_ast_csp_literal"
        fields := #[
          ("term",   "term"),
          ("guards", "guards")
        ]
        arrayFields := #[("guards", "size")]
      } },

    -- Aggregate types
    { cName := "clingo_ast_aggregate_guard_t", lean := "AggregateGuard",
      mapping := .structRecord {
        cStructTag := "clingo_ast_aggregate_guard"
        fields := #[
          ("comparison", "comparison"),
          ("term",       "term")
        ]
      } },
    { cName := "clingo_ast_conditional_literal_t", lean := "ConditionalLiteral",
      mapping := .structRecord {
        cStructTag := "clingo_ast_conditional_literal"
        fields := #[
          ("literal",   "literal"),
          ("condition", "condition")
        ]
        arrayFields := #[("condition", "size")]
      } },
    { cName := "clingo_ast_aggregate_t", lean := "AstAggregate",
      mapping := .structRecord {
        cStructTag := "clingo_ast_aggregate"
        fields := #[
          ("elements",    "elements"),
          ("left_guard",  "leftGuard"),
          ("right_guard", "rightGuard")
        ]
        arrayFields := #[("elements", "size")]
      } },
    { cName := "clingo_ast_body_aggregate_element_t", lean := "BodyAggregateElement",
      mapping := .structRecord {
        cStructTag := "clingo_ast_body_aggregate_element"
        fields := #[
          ("tuple",     "tuple"),
          ("condition", "condition")
        ]
        arrayFields := #[
          ("tuple",     "tuple_size"),
          ("condition", "condition_size")
        ]
      } },
    { cName := "clingo_ast_body_aggregate_t", lean := "BodyAggregate",
      mapping := .structRecord {
        cStructTag := "clingo_ast_body_aggregate"
        fields := #[
          ("function",    "function"),
          ("elements",    "elements"),
          ("left_guard",  "leftGuard"),
          ("right_guard", "rightGuard")
        ]
        arrayFields := #[("elements", "size")]
      } },
    { cName := "clingo_ast_head_aggregate_element_t", lean := "HeadAggregateElement",
      mapping := .structRecord {
        cStructTag := "clingo_ast_head_aggregate_element"
        fields := #[
          ("tuple",               "tuple"),
          ("conditional_literal", "conditionalLiteral")
        ]
        arrayFields := #[("tuple", "tuple_size")]
      } },
    { cName := "clingo_ast_head_aggregate_t", lean := "HeadAggregate",
      mapping := .structRecord {
        cStructTag := "clingo_ast_head_aggregate"
        fields := #[
          ("function",    "function"),
          ("elements",    "elements"),
          ("left_guard",  "leftGuard"),
          ("right_guard", "rightGuard")
        ]
        arrayFields := #[("elements", "size")]
      } },
    { cName := "clingo_ast_disjunction_t", lean := "Disjunction",
      mapping := .structRecord {
        cStructTag := "clingo_ast_disjunction"
        fields := #[
          ("elements", "elements")
        ]
        arrayFields := #[("elements", "size")]
      } },
    { cName := "clingo_ast_disjoint_element_t", lean := "DisjointElement",
      mapping := .structRecord {
        cStructTag := "clingo_ast_disjoint_element"
        fields := #[
          ("location",  "location"),
          ("tuple",     "tuple"),
          ("term",      "term"),
          ("condition", "condition")
        ]
        arrayFields := #[
          ("tuple",     "tuple_size"),
          ("condition", "condition_size")
        ]
      } },
    { cName := "clingo_ast_disjoint_t", lean := "AstDisjoint",
      mapping := .structRecord {
        cStructTag := "clingo_ast_disjoint"
        fields := #[
          ("elements", "elements")
        ]
        arrayFields := #[("elements", "size")]
      } },

    -- Theory types
    { cName := "clingo_ast_theory_term_array_t", lean := "TheoryTermArray",
      mapping := .structRecord {
        cStructTag := "clingo_ast_theory_term_array"
        fields := #[
          ("terms", "terms")
        ]
        arrayFields := #[("terms", "size")]
      } },
    { cName := "clingo_ast_theory_function_t", lean := "TheoryFunction",
      mapping := .structRecord {
        cStructTag := "clingo_ast_theory_function"
        fields := #[
          ("name",      "name"),
          ("arguments", "arguments")
        ]
        arrayFields := #[("arguments", "size")]
      } },
    { cName := "clingo_ast_theory_unparsed_term_element_t", lean := "TheoryUnparsedTermElement",
      mapping := .structRecord {
        cStructTag := "clingo_ast_theory_unparsed_term_element"
        fields := #[
          -- NOTE: operators is `char const *const *` (array of strings) + size.
          -- We skip it for now; lean-bindgen doesn't yet handle arrays of strings.
          ("term", "term")
        ]
      } },
    { cName := "clingo_ast_theory_unparsed_term_t", lean := "TheoryUnparsedTerm",
      mapping := .structRecord {
        cStructTag := "clingo_ast_theory_unparsed_term"
        fields := #[
          ("elements", "elements")
        ]
        arrayFields := #[("elements", "size")]
      } },
    { cName := "clingo_ast_theory_atom_element_t", lean := "TheoryAtomElement",
      mapping := .structRecord {
        cStructTag := "clingo_ast_theory_atom_element"
        fields := #[
          ("tuple",     "tuple"),
          ("condition", "condition")
        ]
        arrayFields := #[
          ("tuple",     "tuple_size"),
          ("condition", "condition_size")
        ]
      } },
    { cName := "clingo_ast_theory_guard_t", lean := "TheoryGuard",
      mapping := .structRecord {
        cStructTag := "clingo_ast_theory_guard"
        fields := #[
          ("operator_name", "operatorName"),
          ("term",          "term")
        ]
      } },
    { cName := "clingo_ast_theory_atom_t", lean := "TheoryAtom",
      mapping := .structRecord {
        cStructTag := "clingo_ast_theory_atom"
        fields := #[
          ("term",     "term"),
          ("elements", "elements"),
          ("guard",    "guard")
        ]
        arrayFields := #[("elements", "size")]
      } },
    -- Theory definition types
    { cName := "clingo_ast_theory_operator_definition_t", lean := "TheoryOperatorDefinition",
      mapping := .structRecord {
        cStructTag := "clingo_ast_theory_operator_definition"
        fields := #[
          ("location", "location"),
          ("name",     "name"),
          ("priority", "priority"),
          ("type",     "type")
        ]
      } },
    { cName := "clingo_ast_theory_term_definition_t", lean := "TheoryTermDefinition",
      mapping := .structRecord {
        cStructTag := "clingo_ast_theory_term_definition"
        fields := #[
          ("location",  "location"),
          ("name",      "name"),
          ("operators", "operators")
        ]
        arrayFields := #[("operators", "size")]
      } },
    { cName := "clingo_ast_theory_guard_definition_t", lean := "TheoryGuardDefinition",
      mapping := .structRecord {
        cStructTag := "clingo_ast_theory_guard_definition"
        fields := #[
          ("term", "term")
          -- NOTE: operators is `char const *const *` (array of strings) + size.
          -- Skipped for now.
        ]
      } },
    { cName := "clingo_ast_theory_atom_definition_t", lean := "TheoryAtomDefinition",
      mapping := .structRecord {
        cStructTag := "clingo_ast_theory_atom_definition"
        fields := #[
          ("location", "location"),
          ("type",     "type"),
          ("name",     "name"),
          ("arity",    "arity"),
          ("elements", "elements"),
          ("guard",    "guard")
        ]
      } },
    { cName := "clingo_ast_theory_definition_t", lean := "TheoryDefinition",
      mapping := .structRecord {
        cStructTag := "clingo_ast_theory_definition"
        fields := #[
          ("name",  "name"),
          ("terms", "terms"),
          ("atoms", "atoms")
        ]
        arrayFields := #[
          ("terms", "terms_size"),
          ("atoms", "atoms_size")
        ]
      } },

    -- Statement variant payloads
    { cName := "clingo_ast_rule_t", lean := "AstRule",
      mapping := .structRecord {
        cStructTag := "clingo_ast_rule"
        fields := #[
          ("head", "head"),
          ("body", "body")
        ]
        arrayFields := #[("body", "size")]
      } },
    { cName := "clingo_ast_definition_t", lean := "AstDefinition",
      mapping := .structRecord {
        cStructTag := "clingo_ast_definition"
        fields := #[
          ("name",       "name"),
          ("value",      "value"),
          ("is_default", "isDefault")
        ]
      } },
    { cName := "clingo_ast_show_signature_t", lean := "AstShowSignature",
      mapping := .structRecord {
        cStructTag := "clingo_ast_show_signature"
        fields := #[
          ("signature", "signature"),
          ("csp",       "csp")
        ]
      } },
    { cName := "clingo_ast_show_term_t", lean := "AstShowTerm",
      mapping := .structRecord {
        cStructTag := "clingo_ast_show_term"
        fields := #[
          ("term", "term"),
          ("body", "body"),
          ("csp",  "csp")
        ]
        arrayFields := #[("body", "size")]
      } },
    { cName := "clingo_ast_defined_t", lean := "AstDefined",
      mapping := .structRecord {
        cStructTag := "clingo_ast_defined"
        fields := #[
          ("signature", "signature")
        ]
      } },
    { cName := "clingo_ast_minimize_t", lean := "AstMinimize",
      mapping := .structRecord {
        cStructTag := "clingo_ast_minimize"
        fields := #[
          ("weight",   "weight"),
          ("priority", "priority"),
          ("tuple",    "tuple"),
          ("body",     "body")
        ]
        arrayFields := #[
          ("tuple", "tuple_size"),
          ("body",  "body_size")
        ]
      } },
    { cName := "clingo_ast_script_t", lean := "AstScript",
      mapping := .structRecord {
        cStructTag := "clingo_ast_script"
        fields := #[
          ("type", "type"),
          ("code", "code")
        ]
      } },
    { cName := "clingo_ast_program_t", lean := "AstProgram",
      mapping := .structRecord {
        cStructTag := "clingo_ast_program"
        fields := #[
          ("name",       "name"),
          ("parameters", "parameters")
        ]
        arrayFields := #[("parameters", "size")]
      } },
    { cName := "clingo_ast_external_t", lean := "AstExternal",
      mapping := .structRecord {
        cStructTag := "clingo_ast_external"
        fields := #[
          ("atom", "atom"),
          ("body", "body"),
          ("type", "type")
        ]
        arrayFields := #[("body", "size")]
      } },
    { cName := "clingo_ast_edge_t", lean := "AstEdge",
      mapping := .structRecord {
        cStructTag := "clingo_ast_edge"
        fields := #[
          ("u",    "u"),
          ("v",    "v"),
          ("body", "body")
        ]
        arrayFields := #[("body", "size")]
      } },
    { cName := "clingo_ast_heuristic_t", lean := "AstHeuristic",
      mapping := .structRecord {
        cStructTag := "clingo_ast_heuristic"
        fields := #[
          ("atom",     "atom"),
          ("body",     "body"),
          ("bias",     "bias"),
          ("priority", "priority"),
          ("modifier", "modifier")
        ]
        arrayFields := #[("body", "size")]
      } },
    { cName := "clingo_ast_project_t", lean := "AstProject",
      mapping := .structRecord {
        cStructTag := "clingo_ast_project"
        fields := #[
          ("atom", "atom"),
          ("body", "body")
        ]
        arrayFields := #[("body", "size")]
      } },

    -- ── Tagged unions ────────────────────────────────────────
    { cName := "clingo_ast_term_t", lean := "AstTerm",
      mapping := .taggedUnion {
        cStructTag := "clingo_ast_term"
        tagField   := "type"
        tagEnum    := "clingo_ast_term_type"
        sharedFields := #[("location", "location")]
        variants := #[
          { cTag := "clingo_ast_term_type_symbol",
            leanCtor := "symbol", unionField := "symbol" },
          { cTag := "clingo_ast_term_type_variable",
            leanCtor := "variable", unionField := "variable" },
          { cTag := "clingo_ast_term_type_unary_operation",
            leanCtor := "unaryOperation", unionField := "unary_operation" },
          { cTag := "clingo_ast_term_type_binary_operation",
            leanCtor := "binaryOperation", unionField := "binary_operation" },
          { cTag := "clingo_ast_term_type_interval",
            leanCtor := "interval", unionField := "interval" },
          { cTag := "clingo_ast_term_type_function",
            leanCtor := "function", unionField := "function" },
          { cTag := "clingo_ast_term_type_external_function",
            leanCtor := "externalFunction", unionField := "external_function" },
          { cTag := "clingo_ast_term_type_pool",
            leanCtor := "pool", unionField := "pool" }
        ]
      } },
    { cName := "clingo_ast_literal_t", lean := "AstLiteral",
      mapping := .taggedUnion {
        cStructTag := "clingo_ast_literal"
        tagField   := "type"
        tagEnum    := "clingo_ast_literal_type"
        sharedFields := #[
          ("location", "location"),
          ("sign",     "sign")
        ]
        variants := #[
          { cTag := "clingo_ast_literal_type_boolean",
            leanCtor := "boolean", unionField := "boolean" },
          { cTag := "clingo_ast_literal_type_symbolic",
            leanCtor := "symbolic", unionField := "symbol" },
          { cTag := "clingo_ast_literal_type_comparison",
            leanCtor := "comparison", unionField := "comparison" },
          { cTag := "clingo_ast_literal_type_csp",
            leanCtor := "csp", unionField := "csp_literal" }
        ]
      } },
    { cName := "clingo_ast_head_literal_t", lean := "HeadLiteral",
      mapping := .taggedUnion {
        cStructTag := "clingo_ast_head_literal"
        tagField   := "type"
        tagEnum    := "clingo_ast_head_literal_type"
        sharedFields := #[("location", "location")]
        variants := #[
          { cTag := "clingo_ast_head_literal_type_literal",
            leanCtor := "literal", unionField := "literal" },
          { cTag := "clingo_ast_head_literal_type_disjunction",
            leanCtor := "disjunction", unionField := "disjunction" },
          { cTag := "clingo_ast_head_literal_type_aggregate",
            leanCtor := "aggregate", unionField := "aggregate" },
          { cTag := "clingo_ast_head_literal_type_head_aggregate",
            leanCtor := "headAggregate", unionField := "head_aggregate" },
          { cTag := "clingo_ast_head_literal_type_theory_atom",
            leanCtor := "theoryAtom", unionField := "theory_atom" }
        ]
      } },
    { cName := "clingo_ast_body_literal_t", lean := "BodyLiteral",
      mapping := .taggedUnion {
        cStructTag := "clingo_ast_body_literal"
        tagField   := "type"
        tagEnum    := "clingo_ast_body_literal_type"
        sharedFields := #[
          ("location", "location"),
          ("sign",     "sign")
        ]
        variants := #[
          { cTag := "clingo_ast_body_literal_type_literal",
            leanCtor := "literal", unionField := "literal" },
          { cTag := "clingo_ast_body_literal_type_conditional",
            leanCtor := "conditional", unionField := "conditional" },
          { cTag := "clingo_ast_body_literal_type_aggregate",
            leanCtor := "aggregate", unionField := "aggregate" },
          { cTag := "clingo_ast_body_literal_type_body_aggregate",
            leanCtor := "bodyAggregate", unionField := "body_aggregate" },
          { cTag := "clingo_ast_body_literal_type_theory_atom",
            leanCtor := "theoryAtom", unionField := "theory_atom" },
          { cTag := "clingo_ast_body_literal_type_disjoint",
            leanCtor := "disjoint", unionField := "disjoint" }
        ]
      } },
    { cName := "clingo_ast_theory_term_t", lean := "AstTheoryTerm",
      mapping := .taggedUnion {
        cStructTag := "clingo_ast_theory_term"
        tagField   := "type"
        tagEnum    := "clingo_ast_theory_term_type"
        sharedFields := #[("location", "location")]
        variants := #[
          { cTag := "clingo_ast_theory_term_type_symbol",
            leanCtor := "symbol", unionField := "symbol" },
          { cTag := "clingo_ast_theory_term_type_variable",
            leanCtor := "variable", unionField := "variable" },
          { cTag := "clingo_ast_theory_term_type_tuple",
            leanCtor := "tuple", unionField := "tuple" },
          { cTag := "clingo_ast_theory_term_type_list",
            leanCtor := "list", unionField := "list" },
          { cTag := "clingo_ast_theory_term_type_set",
            leanCtor := "set", unionField := "set" },
          { cTag := "clingo_ast_theory_term_type_function",
            leanCtor := "function", unionField := "function" },
          { cTag := "clingo_ast_theory_term_type_unparsed_term",
            leanCtor := "unparsedTerm", unionField := "unparsed_term" }
        ]
      } },
    { cName := "clingo_ast_statement_t", lean := "AstStatement",
      mapping := .taggedUnion {
        cStructTag := "clingo_ast_statement"
        tagField   := "type"
        tagEnum    := "clingo_ast_statement_type"
        sharedFields := #[("location", "location")]
        variants := #[
          { cTag := "clingo_ast_statement_type_rule",
            leanCtor := "rule", unionField := "rule" },
          { cTag := "clingo_ast_statement_type_const",
            leanCtor := "const", unionField := "definition" },
          { cTag := "clingo_ast_statement_type_show_signature",
            leanCtor := "showSignature", unionField := "show_signature" },
          { cTag := "clingo_ast_statement_type_show_term",
            leanCtor := "showTerm", unionField := "show_term" },
          { cTag := "clingo_ast_statement_type_minimize",
            leanCtor := "minimize", unionField := "minimize" },
          { cTag := "clingo_ast_statement_type_script",
            leanCtor := "script", unionField := "script" },
          { cTag := "clingo_ast_statement_type_program",
            leanCtor := "program", unionField := "program" },
          { cTag := "clingo_ast_statement_type_external",
            leanCtor := "external", unionField := "external" },
          { cTag := "clingo_ast_statement_type_edge",
            leanCtor := "edge", unionField := "edge" },
          { cTag := "clingo_ast_statement_type_heuristic",
            leanCtor := "heuristic", unionField := "heuristic" },
          { cTag := "clingo_ast_statement_type_project_atom",
            leanCtor := "projectAtom", unionField := "project_atom" },
          { cTag := "clingo_ast_statement_type_project_atom_signature",
            leanCtor := "projectAtomSignature", unionField := "project_signature" },
          { cTag := "clingo_ast_statement_type_theory_definition",
            leanCtor := "theoryDefinition", unionField := "theory_definition" },
          { cTag := "clingo_ast_statement_type_defined",
            leanCtor := "defined", unionField := "defined" }
        ]
      } }
  ]
  functions := #[
    -- ── Error / utility ──────────────────────────────────────
    { cName := "clingo_error_code",    lean := "errorCode",    inIO := true },
    { cName := "clingo_error_string",  lean := "errorString" },
    { cName := "clingo_error_message", lean := "errorMessage", inIO := true,
      nullableReturn := true },

    -- ── Signature ────────────────────────────────────────────
    { cName := "clingo_signature_create"
      lean  := "signatureCreate"
      style := .outParamBoolStatus 3 (.string "clingo_error_message") },
    { cName := "clingo_signature_name",         lean := "signatureName" },
    { cName := "clingo_signature_arity",        lean := "signatureArity" },
    { cName := "clingo_signature_is_positive",  lean := "signatureIsPositive" },
    { cName := "clingo_signature_is_negative",  lean := "signatureIsNegative" },
    { cName := "clingo_signature_is_equal_to",  lean := "signatureBeq" },
    { cName := "clingo_signature_is_less_than", lean := "signatureBlt" },
    { cName := "clingo_signature_hash",         lean := "signatureHash" },

    -- ── Symbol ───────────────────────────────────────────────
    { cName := "clingo_symbol_create_string"
      lean  := "symbolCreateString"
      style := .outParamBoolStatus 1 (.string "clingo_error_message") },
    { cName := "clingo_symbol_create_id"
      lean  := "symbolCreateId"
      style := .outParamBoolStatus 2 (.string "clingo_error_message") },
    { cName := "clingo_symbol_create_function"
      lean  := "symbolCreateFunction"
      style := .outParamBoolStatus 4 (.string "clingo_error_message")
      arrayPairs := [(1, 2)] },
    { cName := "clingo_symbol_type",         lean := "symbolType" },
    { cName := "clingo_symbol_is_equal_to",  lean := "symbolBeq" },
    { cName := "clingo_symbol_is_less_than", lean := "symbolBlt" },
    { cName := "clingo_symbol_hash",         lean := "symbolHash" },
    -- Query functions: return bool where false = "not applicable" (Option)
    { cName := "clingo_symbol_number"
      lean  := "symbolNumber"
      style := .optionOutParam 1 },
    { cName := "clingo_symbol_name"
      lean  := "symbolName"
      style := .optionOutParam 1 },
    { cName := "clingo_symbol_string"
      lean  := "symbolString"
      style := .optionOutParam 1 },
    { cName := "clingo_symbol_is_positive"
      lean  := "symbolIsPositive"
      style := .optionOutParam 1 },
    { cName := "clingo_symbol_is_negative"
      lean  := "symbolIsNegative"
      style := .optionOutParam 1 },
    -- bool clingo_symbol_arguments(clingo_symbol_t symbol,
    --   clingo_symbol_t const **arguments, size_t *arguments_size)
    -- Returns Option (Array Symbol): false = not applicable.
    { cName := "clingo_symbol_arguments"
      lean  := "symbolArguments"
      style := .optionOutArray 1 2 },
    -- Two-step: clingo_symbol_to_string_size + clingo_symbol_to_string
    -- bool clingo_symbol_to_string(clingo_symbol_t symbol, char *string, size_t size)
    { cName := "clingo_symbol_to_string"
      lean  := "symbolToString"
      style := .callerAllocates "clingo_symbol_to_string_size" 1 2
                 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },

    -- ── Parse ────────────────────────────────────────────────
    { cName := "clingo_parse_term"
      lean  := "parseTerm"
      style := .outParamBoolStatus 4 (.string "clingo_error_message")
      callbackUserDataParams := [2] },

    -- ── Symbol create (void return + out-param) ──────────────
    -- void clingo_symbol_create_number(int number, clingo_symbol_t *symbol)
    { cName := "clingo_symbol_create_number"
      lean  := "symbolCreateNumber"
      style := .voidOutParam 1 },
    -- void clingo_symbol_create_supremum(clingo_symbol_t *symbol)
    { cName := "clingo_symbol_create_supremum"
      lean  := "symbolCreateSupremum"
      style := .voidOutParam 0 },
    -- void clingo_symbol_create_infimum(clingo_symbol_t *symbol)
    { cName := "clingo_symbol_create_infimum"
      lean  := "symbolCreateInfimum"
      style := .voidOutParam 0 },
    -- clingo_symbol_to_string: bool + out-param → Except String String
    { cName := "clingo_symbol_to_string_size"
      lean  := "symbolToStringSize"
      style := .outParamBoolStatus 1 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },

    -- ── Control ──────────────────────────────────────────────
    { cName := "clingo_control_interrupt", lean := "controlInterrupt", inIO := true },
    -- bool clingo_control_new(char const *const *arguments, size_t arguments_size,
    --   clingo_logger_t logger, void *logger_data, unsigned message_limit,
    --   clingo_control_t **control)
    { cName := "clingo_control_new"
      lean  := "controlNew"
      style := .outParamBoolStatus 5 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true
      arrayPairs := [(0, 1)]
      callbackUserDataParams := [3] },
    -- bool clingo_control_load(clingo_control_t *control, char const *file)
    { cName := "clingo_control_load"
      lean  := "controlLoad"
      style := .boolStatus (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_control_add(clingo_control_t *control, char const *name,
    --   char const *const *parameters, size_t parameters_size, char const *program)
    { cName := "clingo_control_add"
      lean  := "controlAdd"
      style := .boolStatus (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true
      arrayPairs := [(2, 3)] },
    -- bool clingo_control_ground(clingo_control_t *control,
    --   clingo_part_t const *parts, size_t parts_size,
    --   clingo_ground_callback_t ground_callback, void *ground_callback_data)
    { cName := "clingo_control_ground"
      lean  := "controlGround"
      style := .boolStatus (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true
      arrayPairs := [(1, 2)]
      callbackUserDataParams := [4] },
    -- bool clingo_control_solve(clingo_control_t *control,
    --   clingo_solve_mode_bitset_t mode, clingo_literal_t const *assumptions,
    --   size_t assumptions_size, clingo_solve_event_callback_t notify,
    --   void *data, clingo_solve_handle_t **handle)
    -- NOTE: clingo_solve_event_callback_t is not mapped (custom event dispatch),
    -- so this function cannot be auto-generated yet. Skipped.

    -- bool clingo_control_statistics(clingo_control_t const *control,
    --   clingo_statistics_t const **statistics)
    { cName := "clingo_control_statistics"
      lean  := "controlStatistics"
      style := .outParamBoolStatus 1 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_control_program_builder(clingo_control_t *control,
    --   clingo_program_builder_t **builder)
    { cName := "clingo_control_program_builder"
      lean  := "controlProgramBuilder"
      style := .outParamBoolStatus 1 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },

    -- ── Model ────────────────────────────────────────────────
    -- bool clingo_model_type(clingo_model_t const *model, clingo_model_type_t *type)
    { cName := "clingo_model_type"
      lean  := "modelType"
      style := .outParamBoolStatus 1 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_model_number(clingo_model_t const *model, uint64_t *number)
    { cName := "clingo_model_number"
      lean  := "modelNumber"
      style := .outParamBoolStatus 1 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_model_symbols_size(clingo_model_t const *model,
    --   clingo_show_type_bitset_t show, size_t *size)
    { cName := "clingo_model_symbols_size"
      lean  := "modelSymbolsSize"
      style := .outParamBoolStatus 2 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_model_contains(clingo_model_t const *model,
    --   clingo_symbol_t atom, bool *contained)
    { cName := "clingo_model_contains"
      lean  := "modelContains"
      style := .outParamBoolStatus 2 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_model_is_true(clingo_model_t const *model,
    --   clingo_literal_t literal, bool *result)
    { cName := "clingo_model_is_true"
      lean  := "modelIsTrue"
      style := .outParamBoolStatus 2 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_model_cost_size(clingo_model_t const *model, size_t *size)
    { cName := "clingo_model_cost_size"
      lean  := "modelCostSize"
      style := .outParamBoolStatus 1 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_model_optimality_proven(clingo_model_t const *model, bool *proven)
    { cName := "clingo_model_optimality_proven"
      lean  := "modelOptimalityProven"
      style := .outParamBoolStatus 1 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- Two-step: clingo_model_symbols_size + clingo_model_symbols
    -- bool clingo_model_symbols(clingo_model_t const *model,
    --   clingo_show_type_bitset_t show, clingo_symbol_t *symbols, size_t size)
    { cName := "clingo_model_symbols"
      lean  := "modelSymbols"
      style := .callerAllocates "clingo_model_symbols_size" 2 3
                 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- Two-step: clingo_model_cost_size + clingo_model_cost
    -- bool clingo_model_cost(clingo_model_t const *model, int64_t *costs, size_t size)
    { cName := "clingo_model_cost"
      lean  := "modelCost"
      style := .callerAllocates "clingo_model_cost_size" 1 2
                 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },

    -- ── Solve handle ─────────────────────────────────────────
    -- bool clingo_solve_handle_get(clingo_solve_handle_t *handle,
    --   clingo_solve_result_bitset_t *result)
    { cName := "clingo_solve_handle_get"
      lean  := "solveHandleGet"
      style := .outParamBoolStatus 1 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- void clingo_solve_handle_wait(clingo_solve_handle_t *handle,
    --   double timeout, bool *result)
    { cName := "clingo_solve_handle_wait"
      lean  := "solveHandleWait"
      style := .voidOutParam 2
      inIO := true },
    -- bool clingo_solve_handle_resume(clingo_solve_handle_t *handle)
    { cName := "clingo_solve_handle_resume"
      lean  := "solveHandleResume"
      style := .boolStatus (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_solve_handle_cancel(clingo_solve_handle_t *handle)
    { cName := "clingo_solve_handle_cancel"
      lean  := "solveHandleCancel"
      style := .boolStatus (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_solve_handle_close(clingo_solve_handle_t *handle)
    { cName := "clingo_solve_handle_close"
      lean  := "solveHandleClose"
      style := .boolStatus (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_solve_handle_model(clingo_solve_handle_t *handle,
    --   clingo_model_t const **model)
    -- Returns Except Error (Option Model): NULL model = no model available.
    { cName := "clingo_solve_handle_model"
      lean  := "solveHandleModel"
      style := .outParamBoolStatus 1 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true
      nullableOutParam := true },

    -- ── Statistics ────────────────────────────────────────────
    -- bool clingo_statistics_root(clingo_statistics_t const *statistics, uint64_t *key)
    { cName := "clingo_statistics_root"
      lean  := "statisticsRoot"
      style := .outParamBoolStatus 1 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_statistics_type(clingo_statistics_t const *statistics,
    --   uint64_t key, clingo_statistics_type_t *type)
    { cName := "clingo_statistics_type"
      lean  := "statisticsType"
      style := .outParamBoolStatus 2 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_statistics_array_size(clingo_statistics_t const *statistics,
    --   uint64_t key, size_t *size)
    { cName := "clingo_statistics_array_size"
      lean  := "statisticsArraySize"
      style := .outParamBoolStatus 2 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_statistics_array_at(clingo_statistics_t const *statistics,
    --   uint64_t key, size_t offset, uint64_t *subkey)
    { cName := "clingo_statistics_array_at"
      lean  := "statisticsArrayAt"
      style := .outParamBoolStatus 3 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_statistics_map_size(clingo_statistics_t const *statistics,
    --   uint64_t key, size_t *size)
    { cName := "clingo_statistics_map_size"
      lean  := "statisticsMapSize"
      style := .outParamBoolStatus 2 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_statistics_map_has_subkey(clingo_statistics_t const *statistics,
    --   uint64_t key, char const *name, bool *result)
    { cName := "clingo_statistics_map_has_subkey"
      lean  := "statisticsMapHasSubkey"
      style := .outParamBoolStatus 3 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_statistics_map_subkey_name(clingo_statistics_t const *statistics,
    --   uint64_t key, size_t offset, char const **name)
    { cName := "clingo_statistics_map_subkey_name"
      lean  := "statisticsMapSubkeyName"
      style := .outParamBoolStatus 3 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_statistics_map_at(clingo_statistics_t const *statistics,
    --   uint64_t key, char const *name, uint64_t *subkey)
    { cName := "clingo_statistics_map_at"
      lean  := "statisticsMapAt"
      style := .outParamBoolStatus 3 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_statistics_value_get(clingo_statistics_t const *statistics,
    --   uint64_t key, double *value)
    { cName := "clingo_statistics_value_get"
      lean  := "statisticsValueGet"
      style := .outParamBoolStatus 2 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },

    -- ── Program builder ──────────────────────────────────────
    { cName := "clingo_program_builder_add"
      lean  := "programBuilderAdd"
      style := .boolStatus (.string "clingo_error_message")
      inIO := true },
    -- bool clingo_program_builder_begin(clingo_program_builder_t *builder)
    { cName := "clingo_program_builder_begin"
      lean  := "programBuilderBegin"
      style := .boolStatus (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_program_builder_end(clingo_program_builder_t *builder)
    { cName := "clingo_program_builder_end"
      lean  := "programBuilderEnd"
      style := .boolStatus (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true }
  ]
}
