import LeanBindgen.Annotation

open LeanBindgen

/-! # Full cleango bindings annotation

Comprehensive annotation file that covers all types and functions from
the clingo C API (system clingo 5.8+). Generates the equivalent of
the hand-written clingo-shim.c.

Types and functions are organised in dependency order: scalars, enums,
opaques, bitfield structs, callbacks, struct records, then functions.
The old struct-based AST API has been replaced by the opaque
`clingo_ast_t` attribute-based API.
-/

-- ============================================================
-- Core types + functions (bound against the system header)
-- ============================================================

def clingoFullBindings : Bindings := {
  headerPath := "/opt/homebrew/include/clingo.h"
  leanModule := `Generated.ClingoFull
  outDir     := ".lake/build/generated"
  shimPath   := ".lake/build/generated/clingo-full-shim.c"
  libPrefix  := "clingo"
  leanImports := #[]
  preprocessorArgs := #["-DCLINGO_NO_VISIBILITY"]
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
        enumTag := "clingo_error_e"
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
        enumTag := "clingo_warning_e"
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
        enumTag := "clingo_truth_value_e"
        variants := #[
          ("clingo_truth_value_free",  "free"),
          ("clingo_truth_value_true",  "true"),
          ("clingo_truth_value_false", "false")
        ]
      } },
    { cName := "clingo_symbol_type_t", lean := "SymbolType",
      mapping := .inductiveEnum {
        enumTag := "clingo_symbol_type_e"
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
        enumTag := "clingo_model_type_e"
        variants := #[
          ("clingo_model_type_stable_model",          "stableModel"),
          ("clingo_model_type_brave_consequences",    "braveConsequences"),
          ("clingo_model_type_cautious_consequences", "cautiousConsequences")
        ]
      } },
    { cName := "clingo_statistics_type_t", lean := "StatisticsType",
      mapping := .inductiveEnum {
        enumTag := "clingo_statistics_type_e"
        variants := #[
          ("clingo_statistics_type_empty", "empty"),
          ("clingo_statistics_type_value", "value"),
          ("clingo_statistics_type_array", "array"),
          ("clingo_statistics_type_map",   "map")
        ]
      } },
    { cName := "clingo_solve_event_type_t", lean := "SolveEventType",
      mapping := .inductiveEnum {
        enumTag := "clingo_solve_event_type_e"
        variants := #[
          ("clingo_solve_event_type_model",      "model"),
          ("clingo_solve_event_type_unsat",      "unsat"),
          ("clingo_solve_event_type_statistics", "statistics"),
          ("clingo_solve_event_type_finish",     "finish")
        ]
      } },

    -- ── AST enumerations ─────────────────────────────────────
    { cName := "clingo_ast_sign_t", lean := "Sign",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_sign_e"
        variants := #[
          ("clingo_ast_sign_no_sign",         "none"),
          ("clingo_ast_sign_negation",        "negation"),
          ("clingo_ast_sign_double_negation", "doubleNegation")
        ]
      } },
    { cName := "clingo_ast_comparison_operator_t", lean := "ComparisonOperator",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_comparison_operator_e"
        variants := #[
          ("clingo_ast_comparison_operator_greater_than",  "gt"),
          ("clingo_ast_comparison_operator_less_than",     "lt"),
          ("clingo_ast_comparison_operator_less_equal",    "leq"),
          ("clingo_ast_comparison_operator_greater_equal", "geq"),
          ("clingo_ast_comparison_operator_not_equal",     "neq"),
          ("clingo_ast_comparison_operator_equal",         "eq")
        ]
      } },
    { cName := "clingo_ast_unary_operator_t", lean := "UnaryOperator",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_unary_operator_e"
        variants := #[
          ("clingo_ast_unary_operator_minus",    "minus"),
          ("clingo_ast_unary_operator_negation", "negation"),
          ("clingo_ast_unary_operator_absolute", "absolute")
        ]
      } },
    { cName := "clingo_ast_binary_operator_t", lean := "BinaryOperator",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_binary_operator_e"
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
    { cName := "clingo_ast_aggregate_function_t", lean := "AggregateFunction",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_aggregate_function_e"
        variants := #[
          ("clingo_ast_aggregate_function_count", "count"),
          ("clingo_ast_aggregate_function_sum",   "sum"),
          ("clingo_ast_aggregate_function_sump",  "sump"),
          ("clingo_ast_aggregate_function_min",   "min"),
          ("clingo_ast_aggregate_function_max",   "max")
        ]
      } },
    { cName := "clingo_ast_theory_sequence_type_t", lean := "TheorySequenceType",
      mapping := .inductiveEnum {
        enumTag := "clingo_theory_sequence_type_e"
        variants := #[
          ("clingo_theory_sequence_type_tuple", "tuple"),
          ("clingo_theory_sequence_type_set",   "set"),
          ("clingo_theory_sequence_type_list",  "list")
        ]
      } },
    { cName := "clingo_ast_theory_operator_type_t", lean := "TheoryOperatorType",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_theory_operator_type_e"
        variants := #[
          ("clingo_ast_theory_operator_type_unary",        "unary"),
          ("clingo_ast_theory_operator_type_binary_left",  "binaryLeft"),
          ("clingo_ast_theory_operator_type_binary_right", "binaryRight")
        ]
      } },
    { cName := "clingo_ast_theory_atom_definition_type_t", lean := "TheoryAtomDefinitionType",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_theory_atom_definition_type_e"
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
        enumTag := "clingo_solve_result_e"
        fields := #[
          ("clingo_solve_result_satisfiable",   "satisfiable"),
          ("clingo_solve_result_unsatisfiable", "unsatisfiable"),
          ("clingo_solve_result_exhausted",     "exhausted"),
          ("clingo_solve_result_interrupted",   "interrupted")
        ]
      } },
    { cName := "clingo_show_type_bitset_t", lean := "ShowType",
      mapping := .bitfieldStruct {
        enumTag := "clingo_show_type_e"
        fields := #[
          ("clingo_show_type_shown",      "shown"),
          ("clingo_show_type_atoms",      "atoms"),
          ("clingo_show_type_terms",      "terms"),
          ("clingo_show_type_theory",     "theory"),
          ("clingo_show_type_all",        "all"),
          ("clingo_show_type_complement", "complement")
        ]
      } },

    { cName := "clingo_solve_mode_bitset_t", lean := "SolveMode",
      mapping := .bitfieldStruct {
        enumTag := "clingo_solve_mode_e"
        fields := #[
          ("clingo_solve_mode_async", "async"),
          ("clingo_solve_mode_yield", "yield")
        ]
      } },

    -- ── Callback typedefs ────────────────────────────────────
    { cName := "clingo_logger_t", lean := "Logger",
      mapping := .callback },
    { cName := "clingo_symbol_callback_t", lean := "SymbolCallback",
      mapping := .callback },
    { cName := "clingo_ground_callback_t", lean := "GroundCallback",
      mapping := .callback },
    { cName := "clingo_solve_event_callback_t", lean := "SolveEventCallback",
      mapping := .eventCallback {
        eventTypeName := "SolveEvent"
        discriminantIdx := 0  -- clingo_solve_event_type_t type
        eventIdx := 1         -- void *event
        userDataIdx := 2      -- void *data
        outParams := #[3]     -- bool *goon
        leanReturnType := "Bool"
        variants := #[
          { cEnumValue := "clingo_solve_event_type_model"
            leanCtor := "model"
            interpretation := .opaquePtr "Model" true },
          { cEnumValue := "clingo_solve_event_type_statistics"
            leanCtor := "statistics"
            interpretation := .ptrArray "Statistics" 2
            fieldNames := #["perStep", "accum"] },
          { cEnumValue := "clingo_solve_event_type_finish"
            leanCtor := "finish"
            interpretation := .derefMapped "SolveResult" }
        ]
      } },
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

    -- ── New opaque AST node type (clingo 5.8+ attribute API) ─
    { cName := "clingo_ast_t", lean := "AstNode",
      mapping := .opaquePointer "clingo_ast_release" },

    -- ── New AST enumerations (5.8+ attribute API) ─────────────
    { cName := "clingo_ast_type_t", lean := "AstType",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_type_e"
        variants := #[
          ("clingo_ast_type_id",                           "id"),
          ("clingo_ast_type_variable",                     "variable"),
          ("clingo_ast_type_symbolic_term",                "symbolicTerm"),
          ("clingo_ast_type_unary_operation",              "unaryOperation"),
          ("clingo_ast_type_binary_operation",             "binaryOperation"),
          ("clingo_ast_type_interval",                     "interval"),
          ("clingo_ast_type_function",                     "function"),
          ("clingo_ast_type_pool",                         "pool"),
          ("clingo_ast_type_boolean_constant",             "booleanConstant"),
          ("clingo_ast_type_symbolic_atom",                "symbolicAtom"),
          ("clingo_ast_type_comparison",                   "comparison"),
          ("clingo_ast_type_guard",                        "guard"),
          ("clingo_ast_type_conditional_literal",          "conditionalLiteral"),
          ("clingo_ast_type_aggregate",                    "aggregate"),
          ("clingo_ast_type_body_aggregate_element",       "bodyAggregateElement"),
          ("clingo_ast_type_body_aggregate",               "bodyAggregate"),
          ("clingo_ast_type_head_aggregate_element",       "headAggregateElement"),
          ("clingo_ast_type_head_aggregate",               "headAggregate"),
          ("clingo_ast_type_disjunction",                  "disjunction"),
          ("clingo_ast_type_theory_sequence",              "theorySequence"),
          ("clingo_ast_type_theory_function",              "theoryFunction"),
          ("clingo_ast_type_theory_unparsed_term_element", "theoryUnparsedTermElement"),
          ("clingo_ast_type_theory_unparsed_term",         "theoryUnparsedTerm"),
          ("clingo_ast_type_theory_guard",                 "theoryGuard"),
          ("clingo_ast_type_theory_atom_element",          "theoryAtomElement"),
          ("clingo_ast_type_theory_atom",                  "theoryAtom"),
          ("clingo_ast_type_literal",                      "literal"),
          ("clingo_ast_type_theory_operator_definition",   "theoryOperatorDefinition"),
          ("clingo_ast_type_theory_term_definition",       "theoryTermDefinition"),
          ("clingo_ast_type_theory_guard_definition",      "theoryGuardDefinition"),
          ("clingo_ast_type_theory_atom_definition",       "theoryAtomDefinition"),
          ("clingo_ast_type_rule",                         "rule"),
          ("clingo_ast_type_definition",                   "definition"),
          ("clingo_ast_type_show_signature",               "showSignature"),
          ("clingo_ast_type_show_term",                    "showTerm"),
          ("clingo_ast_type_minimize",                     "minimize"),
          ("clingo_ast_type_script",                       "script"),
          ("clingo_ast_type_program",                      "program"),
          ("clingo_ast_type_external",                     "external"),
          ("clingo_ast_type_edge",                         "edge"),
          ("clingo_ast_type_heuristic",                    "heuristic"),
          ("clingo_ast_type_project_atom",                 "projectAtom"),
          ("clingo_ast_type_project_signature",            "projectSignature"),
          ("clingo_ast_type_defined",                      "defined"),
          ("clingo_ast_type_theory_definition",            "theoryDefinition"),
          ("clingo_ast_type_comment",                      "comment")
        ]
      } },
    { cName := "clingo_ast_attribute_type_t", lean := "AstAttributeType",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_attribute_type_e"
        variants := #[
          ("clingo_ast_attribute_type_number",       "number"),
          ("clingo_ast_attribute_type_symbol",       "symbol"),
          ("clingo_ast_attribute_type_location",     "location"),
          ("clingo_ast_attribute_type_string",       "string"),
          ("clingo_ast_attribute_type_ast",          "ast"),
          ("clingo_ast_attribute_type_optional_ast", "optionalAst"),
          ("clingo_ast_attribute_type_string_array", "stringArray"),
          ("clingo_ast_attribute_type_ast_array",    "astArray")
        ]
      } },
    { cName := "clingo_ast_attribute_t", lean := "AstAttribute",
      mapping := .inductiveEnum {
        enumTag := "clingo_ast_attribute_e"
        variants := #[
          ("clingo_ast_attribute_argument",      "argument"),
          ("clingo_ast_attribute_arguments",     "arguments"),
          ("clingo_ast_attribute_arity",         "arity"),
          ("clingo_ast_attribute_atom",          "atom"),
          ("clingo_ast_attribute_atoms",         "atoms"),
          ("clingo_ast_attribute_atom_type",     "atomType"),
          ("clingo_ast_attribute_bias",          "bias"),
          ("clingo_ast_attribute_body",          "body"),
          ("clingo_ast_attribute_code",          "code"),
          ("clingo_ast_attribute_coefficient",   "coefficient"),
          ("clingo_ast_attribute_comparison",    "comparison"),
          ("clingo_ast_attribute_condition",     "condition"),
          ("clingo_ast_attribute_elements",      "elements"),
          ("clingo_ast_attribute_external",      "external"),
          ("clingo_ast_attribute_external_type", "externalType"),
          ("clingo_ast_attribute_function",      "function"),
          ("clingo_ast_attribute_guard",         "guard"),
          ("clingo_ast_attribute_guards",        "guards"),
          ("clingo_ast_attribute_head",          "head"),
          ("clingo_ast_attribute_is_default",    "isDefault"),
          ("clingo_ast_attribute_left",          "left"),
          ("clingo_ast_attribute_left_guard",    "leftGuard"),
          ("clingo_ast_attribute_literal",       "literal"),
          ("clingo_ast_attribute_location",      "location"),
          ("clingo_ast_attribute_modifier",      "modifier"),
          ("clingo_ast_attribute_name",          "name"),
          ("clingo_ast_attribute_node_u",        "nodeU"),
          ("clingo_ast_attribute_node_v",        "nodeV"),
          ("clingo_ast_attribute_operator_name", "operatorName"),
          ("clingo_ast_attribute_operator_type", "operatorType"),
          ("clingo_ast_attribute_operators",     "operators"),
          ("clingo_ast_attribute_parameters",    "parameters"),
          ("clingo_ast_attribute_positive",      "positive"),
          ("clingo_ast_attribute_priority",      "priority"),
          ("clingo_ast_attribute_right",         "right"),
          ("clingo_ast_attribute_right_guard",   "rightGuard"),
          ("clingo_ast_attribute_sequence_type", "sequenceType"),
          ("clingo_ast_attribute_sign",          "sign"),
          ("clingo_ast_attribute_symbol",        "symbol"),
          ("clingo_ast_attribute_term",          "term"),
          ("clingo_ast_attribute_terms",         "terms"),
          ("clingo_ast_attribute_value",         "value"),
          ("clingo_ast_attribute_variable",      "variable"),
          ("clingo_ast_attribute_weight",        "weight"),
          ("clingo_ast_attribute_comment_type",  "commentType")
        ]
      } },

    -- ── Variadic AST builder (clingo_ast_build) ─────────────
    { cName := "clingo_ast_build", lean := "AstBuilder",
      mapping := .variadicBuilder {
        variadicFn := "clingo_ast_build"
        resultType := "AstNode"
        resultCType := "clingo_ast"
        locationCType := "clingo_location_t"
        locationLean := "Location"
        symbolCType := "clingo_symbol_t"
        symbolLean := "Symbol"
        errorReturn := .tuple "clingo_error_code" "Error" "clingo_error_message"
        constructors := #[
          -- Terms
          { enumValue := "clingo_ast_type_id", leanName := "buildId",
            args := #[⟨.location, "location", none⟩, ⟨.string, "name", none⟩] },
          { enumValue := "clingo_ast_type_variable", leanName := "buildVariable",
            args := #[⟨.location, "location", none⟩, ⟨.string, "name", none⟩] },
          { enumValue := "clingo_ast_type_symbolic_term", leanName := "buildSymbolicTerm",
            args := #[⟨.location, "location", none⟩, ⟨.symbol, "symbol", none⟩] },
          { enumValue := "clingo_ast_type_unary_operation", leanName := "buildUnaryOperation",
            args := #[⟨.location, "location", none⟩, ⟨.number, "op", some "UnaryOperator"⟩, ⟨.ast, "argument", none⟩] },
          { enumValue := "clingo_ast_type_binary_operation", leanName := "buildBinaryOperation",
            args := #[⟨.location, "location", none⟩, ⟨.number, "op", some "BinaryOperator"⟩, ⟨.ast, "left", none⟩, ⟨.ast, "right", none⟩] },
          { enumValue := "clingo_ast_type_interval", leanName := "buildInterval",
            args := #[⟨.location, "location", none⟩, ⟨.ast, "left", none⟩, ⟨.ast, "right", none⟩] },
          { enumValue := "clingo_ast_type_function", leanName := "buildFunction",
            args := #[⟨.location, "location", none⟩, ⟨.string, "name", none⟩, ⟨.astArray, "arguments", none⟩, ⟨.number, "external", none⟩] },
          { enumValue := "clingo_ast_type_pool", leanName := "buildPool",
            args := #[⟨.location, "location", none⟩, ⟨.astArray, "arguments", none⟩] },
          -- Atoms
          { enumValue := "clingo_ast_type_boolean_constant", leanName := "buildBooleanConstant",
            args := #[⟨.number, "value", none⟩] },
          { enumValue := "clingo_ast_type_symbolic_atom", leanName := "buildSymbolicAtom",
            args := #[⟨.ast, "symbol", none⟩] },
          { enumValue := "clingo_ast_type_comparison", leanName := "buildComparison",
            args := #[⟨.ast, "term", none⟩, ⟨.astArray, "guards", none⟩] },
          { enumValue := "clingo_ast_type_guard", leanName := "buildGuard",
            args := #[⟨.number, "comparison", some "ComparisonOperator"⟩, ⟨.ast, "term", none⟩] },
          -- Literals
          { enumValue := "clingo_ast_type_literal", leanName := "buildLiteral",
            args := #[⟨.location, "location", none⟩, ⟨.number, "sign", some "Sign"⟩, ⟨.ast, "atom", none⟩] },
          -- Aggregates
          { enumValue := "clingo_ast_type_conditional_literal", leanName := "buildConditionalLiteral",
            args := #[⟨.location, "location", none⟩, ⟨.ast, "literal", none⟩, ⟨.astArray, "condition", none⟩] },
          { enumValue := "clingo_ast_type_aggregate", leanName := "buildAggregate",
            args := #[⟨.location, "location", none⟩, ⟨.optionalAst, "leftGuard", none⟩, ⟨.astArray, "elements", none⟩, ⟨.optionalAst, "rightGuard", none⟩] },
          { enumValue := "clingo_ast_type_body_aggregate_element", leanName := "buildBodyAggregateElement",
            args := #[⟨.astArray, "terms", none⟩, ⟨.astArray, "condition", none⟩] },
          { enumValue := "clingo_ast_type_body_aggregate", leanName := "buildBodyAggregate",
            args := #[⟨.location, "location", none⟩, ⟨.optionalAst, "leftGuard", none⟩, ⟨.number, "function", some "AggregateFunction"⟩, ⟨.astArray, "elements", none⟩, ⟨.optionalAst, "rightGuard", none⟩] },
          { enumValue := "clingo_ast_type_head_aggregate_element", leanName := "buildHeadAggregateElement",
            args := #[⟨.astArray, "terms", none⟩, ⟨.ast, "condition", none⟩] },
          { enumValue := "clingo_ast_type_head_aggregate", leanName := "buildHeadAggregate",
            args := #[⟨.location, "location", none⟩, ⟨.optionalAst, "leftGuard", none⟩, ⟨.number, "function", some "AggregateFunction"⟩, ⟨.astArray, "elements", none⟩, ⟨.optionalAst, "rightGuard", none⟩] },
          { enumValue := "clingo_ast_type_disjunction", leanName := "buildDisjunction",
            args := #[⟨.location, "location", none⟩, ⟨.astArray, "elements", none⟩] },
          -- Theory terms
          { enumValue := "clingo_ast_type_theory_sequence", leanName := "buildTheorySequence",
            args := #[⟨.location, "location", none⟩, ⟨.number, "sequenceType", some "TheorySequenceType"⟩, ⟨.astArray, "terms", none⟩] },
          { enumValue := "clingo_ast_type_theory_function", leanName := "buildTheoryFunction",
            args := #[⟨.location, "location", none⟩, ⟨.string, "name", none⟩, ⟨.astArray, "arguments", none⟩] },
          { enumValue := "clingo_ast_type_theory_unparsed_term_element", leanName := "buildTheoryUnparsedTermElement",
            args := #[⟨.stringArray, "operators", none⟩, ⟨.ast, "term", none⟩] },
          { enumValue := "clingo_ast_type_theory_unparsed_term", leanName := "buildTheoryUnparsedTerm",
            args := #[⟨.location, "location", none⟩, ⟨.astArray, "elements", none⟩] },
          { enumValue := "clingo_ast_type_theory_guard", leanName := "buildTheoryGuard",
            args := #[⟨.string, "operatorName", none⟩, ⟨.ast, "term", none⟩] },
          { enumValue := "clingo_ast_type_theory_atom_element", leanName := "buildTheoryAtomElement",
            args := #[⟨.astArray, "terms", none⟩, ⟨.astArray, "condition", none⟩] },
          { enumValue := "clingo_ast_type_theory_atom", leanName := "buildTheoryAtom",
            args := #[⟨.location, "location", none⟩, ⟨.ast, "term", none⟩, ⟨.astArray, "elements", none⟩, ⟨.optionalAst, "guard", none⟩] },
          -- Statements
          { enumValue := "clingo_ast_type_rule", leanName := "buildRule",
            args := #[⟨.location, "location", none⟩, ⟨.ast, "head", none⟩, ⟨.astArray, "body", none⟩] },
          { enumValue := "clingo_ast_type_definition", leanName := "buildDefinition",
            args := #[⟨.location, "location", none⟩, ⟨.string, "name", none⟩, ⟨.ast, "value", none⟩, ⟨.number, "isDefault", none⟩] },
          { enumValue := "clingo_ast_type_show_signature", leanName := "buildShowSignature",
            args := #[⟨.location, "location", none⟩, ⟨.string, "name", none⟩, ⟨.number, "arity", none⟩, ⟨.number, "positive", none⟩] },
          { enumValue := "clingo_ast_type_show_term", leanName := "buildShowTerm",
            args := #[⟨.location, "location", none⟩, ⟨.ast, "term", none⟩, ⟨.astArray, "body", none⟩] },
          { enumValue := "clingo_ast_type_minimize", leanName := "buildMinimize",
            args := #[⟨.location, "location", none⟩, ⟨.ast, "weight", none⟩, ⟨.ast, "priority", none⟩, ⟨.astArray, "terms", none⟩, ⟨.astArray, "body", none⟩] },
          { enumValue := "clingo_ast_type_script", leanName := "buildScript",
            args := #[⟨.location, "location", none⟩, ⟨.string, "name", none⟩, ⟨.string, "code", none⟩] },
          { enumValue := "clingo_ast_type_program", leanName := "buildProgram",
            args := #[⟨.location, "location", none⟩, ⟨.string, "name", none⟩, ⟨.astArray, "parameters", none⟩] },
          { enumValue := "clingo_ast_type_external", leanName := "buildExternal",
            args := #[⟨.location, "location", none⟩, ⟨.ast, "atom", none⟩, ⟨.astArray, "body", none⟩, ⟨.ast, "externalType", none⟩] },
          { enumValue := "clingo_ast_type_edge", leanName := "buildEdge",
            args := #[⟨.location, "location", none⟩, ⟨.ast, "nodeU", none⟩, ⟨.ast, "nodeV", none⟩, ⟨.astArray, "body", none⟩] },
          { enumValue := "clingo_ast_type_heuristic", leanName := "buildHeuristic",
            args := #[⟨.location, "location", none⟩, ⟨.ast, "atom", none⟩, ⟨.astArray, "body", none⟩, ⟨.ast, "bias", none⟩, ⟨.ast, "priority", none⟩, ⟨.ast, "modifier", none⟩] },
          { enumValue := "clingo_ast_type_project_atom", leanName := "buildProjectAtom",
            args := #[⟨.location, "location", none⟩, ⟨.ast, "atom", none⟩, ⟨.astArray, "body", none⟩] },
          { enumValue := "clingo_ast_type_project_signature", leanName := "buildProjectSignature",
            args := #[⟨.location, "location", none⟩, ⟨.string, "name", none⟩, ⟨.number, "arity", none⟩, ⟨.number, "positive", none⟩] },
          { enumValue := "clingo_ast_type_defined", leanName := "buildDefined",
            args := #[⟨.location, "location", none⟩, ⟨.string, "name", none⟩, ⟨.number, "arity", none⟩, ⟨.number, "positive", none⟩] },
          -- Theory definitions
          { enumValue := "clingo_ast_type_theory_operator_definition", leanName := "buildTheoryOperatorDefinition",
            args := #[⟨.location, "location", none⟩, ⟨.string, "name", none⟩, ⟨.number, "priority", none⟩, ⟨.number, "operatorType", some "TheoryOperatorType"⟩] },
          { enumValue := "clingo_ast_type_theory_term_definition", leanName := "buildTheoryTermDefinition",
            args := #[⟨.location, "location", none⟩, ⟨.string, "name", none⟩, ⟨.astArray, "operators", none⟩] },
          { enumValue := "clingo_ast_type_theory_guard_definition", leanName := "buildTheoryGuardDefinition",
            args := #[⟨.stringArray, "operators", none⟩, ⟨.string, "term", none⟩] },
          { enumValue := "clingo_ast_type_theory_atom_definition", leanName := "buildTheoryAtomDefinition",
            args := #[⟨.location, "location", none⟩, ⟨.number, "atomType", some "TheoryAtomDefinitionType"⟩, ⟨.string, "name", none⟩, ⟨.number, "arity", none⟩, ⟨.string, "term", none⟩, ⟨.optionalAst, "guard", none⟩] },
          { enumValue := "clingo_ast_type_theory_definition", leanName := "buildTheoryDefinition",
            args := #[⟨.location, "location", none⟩, ⟨.string, "name", none⟩, ⟨.astArray, "terms", none⟩, ⟨.astArray, "atoms", none⟩] },
          -- Comment
          { enumValue := "clingo_ast_type_comment", leanName := "buildComment",
            args := #[⟨.location, "location", none⟩, ⟨.string, "value", none⟩, ⟨.number, "commentType", none⟩] }
        ]
      } },
  ]
  functions := #[
    -- ── Version ──────────────────────────────────────────────
    { cName := "clingo_version"
      lean  := "version"
      style := .multiOutParam #[0, 1, 2]
      inIO := false },

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
    { cName := "clingo_control_solve"
      lean  := "controlSolve"
      style := .outParamBoolStatus 6 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true
      arrayPairs := [(2, 3)]
      callbackUserDataParams := [5] },

    -- bool clingo_control_statistics(clingo_control_t const *control,
    --   clingo_statistics_t const **statistics)
    { cName := "clingo_control_statistics"
      lean  := "controlStatistics"
      style := .outParamBoolStatus 1 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_program_builder_init(clingo_control_t *control,
    --   clingo_program_builder_t **builder)
    { cName := "clingo_program_builder_init"
      lean  := "programBuilderInit"
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
      inIO := true },

    -- ── AST introspection (5.8+ opaque API) ──────────────────
    -- bool clingo_ast_get_type(clingo_ast_t *ast, clingo_ast_type_t *type)
    { cName := "clingo_ast_get_type"
      lean  := "astGetType"
      style := .outParamBoolStatus 1 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_ast_to_string_size(clingo_ast_t *ast, size_t *size)
    { cName := "clingo_ast_to_string_size"
      lean  := "astToStringSize"
      style := .outParamBoolStatus 1 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true },
    -- bool clingo_ast_to_string(clingo_ast_t *ast, char *string, size_t size)
    { cName := "clingo_ast_to_string"
      lean  := "astToString"
      style := .callerAllocates "clingo_ast_to_string_size" 1 2
                 (.tuple "clingo_error_code" "Error" "clingo_error_message")
      inIO := true }
  ]
}
