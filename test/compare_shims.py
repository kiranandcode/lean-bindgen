#!/usr/bin/env python3
"""
Structural comparison between the hand-written cleango shim and the
auto-generated lean-bindgen shim, using clang's JSON AST dump.

Usage:
    python3 test/compare_shims.py [--dump]

Requires clang on PATH and lean --print-prefix to find lean/lean.h.
"""

import json
import subprocess
import sys
import re
from collections import defaultdict
from pathlib import Path

# ── Paths ────────────────────────────────────────────────────────────

ROOT = Path(__file__).resolve().parent.parent
HAND_WRITTEN = ROOT / "reference" / "cleango" / "bindings" / "clingo-shim.c"
GENERATED    = Path("/tmp/clingo-full-shim.c")
HEADER_DIR   = ROOT / "reference" / "cleango" / "bindings"

# ── Name mapping rules ───────────────────────────────────────────────
# The hand-written shim uses `lean_clingo_<X>_to_<X>` naming while
# the generated shim uses `lean_to_<X>` / `<X>_to_lean`.  The function
# shims use `lean_clingo_<api_fn>` vs `lean_<api_fn>`.

# Category 1: Lean→C struct conversion helpers
#   hand: lean_clingo_term_to_term  →  gen: lean_to_ast_term
# Category 2: C→Lean struct conversion helpers
#   hand: lean_clingo_location_to_location  →  gen: location_to_lean
#   (hand-written only has C→Lean for location and solve_result)
# Category 3: Free helpers
#   hand: lean_clingo_free_term  →  gen: free_ast_term
# Category 4: Function shims (extern symbols)
#   hand: lean_clingo_version  →  gen: lean_clingo_version
# Category 5: Callback trampolines
#   hand: lean_clingo_solve_event_callback_wrapper  →  gen: clingo_solve_event_callback_t_trampoline
# Category 6: Enum helpers
#   hand: (inline in functions)  →  gen: lean_to_error / error_to_lean

# ── Known structural differences (rules) ─────────────────────────────
# These are expected differences between hand-written and generated code.

KNOWN_DIFFERENCES = {
    "naming": "Generated uses `lean_to_X`/`X_to_lean`/`free_X` pattern; "
              "hand-written uses `lean_clingo_X_to_X`/`lean_clingo_free_X`",

    "direction": "Hand-written only has Lean→C converters + free helpers for AST types. "
                 "Generated also emits C→Lean (`X_to_lean`) for every type.",

    "param_style": "Hand-written passes out-pointer: `void f(lean_object*, T *result)`. "
                   "Generated returns by value: `T f(b_lean_obj_arg obj)`.",

    "utility_helpers": "Hand-written has lean_mk_tuple/except_ok/err/option helpers; "
                       "generated inlines ctor allocation directly.",

    "symbol_constructors": "Hand-written has lean_clingo_symbol_mk_* (7 fns) + lean_clingo_mk "
                           "dispatch. Generated uses extern shims matching the C API directly.",

    "repr_function": "Hand-written has lean_clingo_repr (recursive symbol→string). "
                     "Not generated (application-level, not part of C API).",

    "error_handling": "Hand-written uses lean_mk_clingo_error/lean_clingo_mk_io_error helpers. "
                      "Generated inlines the error code+message pattern per function.",

    "location_direction": "Hand-written has both directions (lean_clingo_location_to_location "
                          "and clingo_lean_location_to_location). Generated has both as "
                          "location_to_lean / lean_to_location.",

    "solve_event_trampoline": "Hand-written has lean_clingo_solve_event_callback_wrapper + "
                              "3 mk_solve_event_* helpers. Generated has single "
                              "clingo_solve_event_callback_t_trampoline with inline dispatch.",

    "statistics_api": "Hand-written has 8 custom statistics functions extracting external data. "
                      "Generated uses outParamBoolStatus for each.",

    "model_api": "Hand-written has lean_clingo_calculate_flags + custom model accessors. "
                 "Generated uses bitfieldStruct toC for ShowType + standard function styles.",
}


def get_lean_include():
    """Get the lean include path."""
    r = subprocess.run(["lean", "--print-prefix"], capture_output=True, text=True)
    return r.stdout.strip() + "/include"


def parse_ast(filepath, extra_includes=None):
    """Run clang -ast-dump=json and return the parsed JSON."""
    lean_inc = get_lean_include()
    args = [
        "clang", "-Xclang", "-ast-dump=json", "-fsyntax-only",
        f"-I{lean_inc}", f"-I{HEADER_DIR}",
    ]
    if extra_includes:
        for inc in extra_includes:
            args.append(f"-I{inc}")
    args.append(str(filepath))
    r = subprocess.run(args, capture_output=True, text=True)
    if r.returncode != 0:
        print(f"clang failed for {filepath}:", r.stderr[:500], file=sys.stderr)
        sys.exit(1)
    return json.loads(r.stdout)


def extract_functions(ast, filepath):
    """Extract function definitions from clang AST JSON.

    Returns dict: name → {params, return_type, body_summary}
    where body_summary captures structural elements useful for comparison.
    """
    funcs = {}
    fpath = str(filepath)
    fname = Path(filepath).name

    # Track the "current file" as clang only emits file on transitions.
    current_file = [None]

    def resolve_file(node):
        """Resolve the source file from a node, tracking file changes."""
        loc = node.get("loc", {})
        range_info = node.get("range", {})
        # Check all places clang might put the file path
        for source in [
            loc.get("file"),
            loc.get("expansionLoc", {}).get("file"),
            range_info.get("begin", {}).get("file"),
            range_info.get("begin", {}).get("expansionLoc", {}).get("file"),
        ]:
            if source:
                current_file[0] = source
                return source
        return current_file[0]

    def visit(node):
        if not isinstance(node, dict):
            return
        if node.get("kind") == "FunctionDecl":
            loc_file = resolve_file(node)
            is_ours = (loc_file and fname in loc_file) if loc_file else False
            # Check if it has a body (CompoundStmt inner)
            inner = node.get("inner", [])
            has_body = any(n.get("kind") == "CompoundStmt" for n in inner if isinstance(n, dict))
            if is_ours and has_body:
                name = node.get("name", "")
                rtype = node.get("type", {}).get("qualType", "")
                params = []
                body_node = None
                for child in inner:
                    if isinstance(child, dict):
                        if child.get("kind") == "ParmVarDecl":
                            params.append({
                                "name": child.get("name", ""),
                                "type": child.get("type", {}).get("qualType", ""),
                            })
                        elif child.get("kind") == "CompoundStmt":
                            body_node = child
                summary = summarize_body(body_node) if body_node else {}
                funcs[name] = {
                    "return_type": rtype,
                    "params": params,
                    "body": summary,
                }
        for child in node.get("inner", []):
            if isinstance(child, dict):
                visit(child)

    visit(ast)
    return funcs


def summarize_body(node):
    """Extract a structural summary of a function body for comparison.

    Captures: switch cases, field accesses, malloc calls, function calls,
    lean_ctor_set/get patterns, etc.
    """
    summary = {
        "switch_cases": [],        # list of case label values
        "function_calls": [],      # list of called function names
        "malloc_count": 0,
        "free_count": 0,
        "field_accesses": [],      # member expressions
        "ctor_set_count": 0,
        "ctor_get_count": 0,
        "lean_box_calls": 0,
        "lean_unbox_calls": 0,
        "string_ops": 0,           # lean_mk_string / lean_string_cstr
        "has_switch": False,
        "depth": 0,
    }

    def walk(n, depth=0):
        if not isinstance(n, dict):
            return
        kind = n.get("kind", "")
        summary["depth"] = max(summary["depth"], depth)

        if kind == "SwitchStmt":
            summary["has_switch"] = True
        elif kind == "CaseStmt":
            # Try to get the case value
            inner = n.get("inner", [])
            if inner and isinstance(inner[0], dict):
                val = inner[0]
                if val.get("kind") == "DeclRefExpr":
                    ref = val.get("referencedDecl", {}).get("name", "")
                    if ref:
                        summary["switch_cases"].append(ref)
                elif val.get("kind") == "IntegerLiteral":
                    summary["switch_cases"].append(
                        f"int:{val.get('value', '?')}")
                elif val.get("kind") == "ConstantExpr":
                    cval = val.get("value", "?")
                    summary["switch_cases"].append(f"const:{cval}")
        elif kind == "CallExpr":
            inner = n.get("inner", [])
            if inner and isinstance(inner[0], dict):
                callee = inner[0]
                if callee.get("kind") == "ImplicitCastExpr":
                    callee_inner = callee.get("inner", [{}])[0]
                    if isinstance(callee_inner, dict) and callee_inner.get("kind") == "DeclRefExpr":
                        fname = callee_inner.get("referencedDecl", {}).get("name", "")
                        summary["function_calls"].append(fname)
                        if fname == "malloc":
                            summary["malloc_count"] += 1
                        elif fname == "free":
                            summary["free_count"] += 1
                        elif fname.startswith("lean_ctor_set"):
                            summary["ctor_set_count"] += 1
                        elif fname.startswith("lean_ctor_get"):
                            summary["ctor_get_count"] += 1
                        elif "lean_box" in fname or "lean_alloc_ctor" in fname:
                            summary["lean_box_calls"] += 1
                        elif "lean_unbox" in fname:
                            summary["lean_unbox_calls"] += 1
                        elif fname in ("lean_mk_string", "lean_string_cstr"):
                            summary["string_ops"] += 1
                elif callee.get("kind") == "DeclRefExpr":
                    fname = callee.get("referencedDecl", {}).get("name", "")
                    summary["function_calls"].append(fname)

        for child in n.get("inner", []):
            if isinstance(child, dict):
                walk(child, depth + 1)

    walk(node)

    # Deduplicate function calls but keep count
    call_counts = defaultdict(int)
    for c in summary["function_calls"]:
        call_counts[c] += 1
    summary["function_calls"] = dict(call_counts)

    return summary


# ── Name mapping ─────────────────────────────────────────────────────

def build_name_map(hand_funcs, gen_funcs):
    """Build a mapping from hand-written function names to generated ones.

    Returns: list of (hand_name, gen_name, category) tuples
    """
    mappings = []
    used_gen = set()

    # Rule 1: Lean→C conversion helpers
    # lean_clingo_X_to_X → lean_to_Y (where Y is the generated type name)
    hw_converters = {}
    for name in hand_funcs:
        m = re.match(r'lean_clingo_(\w+)_to_\1$', name)
        if m:
            hw_converters[name] = m.group(1)

    # Build reverse map for generated: lean_to_Y → Y
    gen_lean_to = {}
    for name in gen_funcs:
        m = re.match(r'lean_to_(\w+)$', name)
        if m:
            gen_lean_to[m.group(1)] = name

    # Type name normalization: hand-written drops "ast_" prefix sometimes
    # and uses shorter names. Build fuzzy matching.
    type_name_map = {
        # hand → gen (the C type name component)
        "term": "ast_term",
        "comparison": "ast_comparison",
        "literal": "ast_literal",
        "head_literal": "head_literal",
        "body_literal": "body_literal",
        "conditional_literal": "conditional_literal",
        "aggregate": "ast_aggregate",
        "head_aggregate": "head_aggregate",
        "body_aggregate": "body_aggregate",
        "head_aggregate_element": "head_aggregate_element",
        "body_aggregate_element": "body_aggregate_element",
        "aggregate_guard": "aggregate_guard",
        "disjunction": "disjunction",
        "disjoint": "ast_disjoint",
        "disjoint_element": "disjoint_element",
        "csp_product_term": "csp_product_term",
        "csp_sum_term": "csp_sum_term",
        "csp_guard": "csp_guard",
        "csp_literal": "csp_literal",
        "theory_term": "ast_theory_term",
        "theory_function": "theory_function",
        "theory_term_array": "theory_term_array",
        "theory_unparsed_term_element": "theory_unparsed_term_element",
        "theory_unparsed_term": "theory_unparsed_term",
        "theory_atom_element": "theory_atom_element",
        "theory_guard": "theory_guard",
        "theory_atom": "theory_atom",
        "theory_operator_definition": "theory_operator_definition",
        "theory_term_definition": "theory_term_definition",
        "theory_guard_definition": "theory_guard_definition",
        "theory_atom_definition": "theory_atom_definition",
        "theory_definition": "theory_definition",
        "statement": "ast_statement",
        "rule": "ast_rule",
        "definition": "ast_definition",
        "show_signature": "ast_show_signature",
        "show_term": "ast_show_term",
        "minimize": "ast_minimize",
        "script": "ast_script",
        "program": "ast_program",
        "external": "ast_external",
        "edge": "ast_edge",
        "heuristic": "ast_heuristic",
        "project": "ast_project",
        "defined": "ast_defined",
        "id": "ast_id",
        "string": None,  # no generated equivalent
    }

    for hw_name, hw_type in hw_converters.items():
        gen_type = type_name_map.get(hw_type)
        if gen_type and gen_type in gen_lean_to:
            gen_name = gen_lean_to[gen_type]
            mappings.append((hw_name, gen_name, "lean_to_c"))
            used_gen.add(gen_name)
        else:
            mappings.append((hw_name, None, "lean_to_c_unmatched"))

    # Rule 2: Free helpers
    # lean_clingo_free_X → free_Y
    for name in hand_funcs:
        m = re.match(r'lean_clingo_free_(\w+)$', name)
        if m:
            hw_type = m.group(1)
            gen_type = type_name_map.get(hw_type)
            gen_name = f"free_{gen_type}" if gen_type else None
            if gen_name and gen_name in gen_funcs:
                mappings.append((name, gen_name, "free_helper"))
                used_gen.add(gen_name)
            else:
                mappings.append((name, None, "free_unmatched"))

    # Rule 3: Function shims (extern symbols)
    # Both use lean_clingo_X naming for the extern symbols.
    for name in hand_funcs:
        if name in used_gen or any(name == m[0] for m in mappings):
            continue
        # Check if same name exists in generated
        if name in gen_funcs:
            mappings.append((name, name, "function_shim"))
            used_gen.add(name)
        else:
            # Try lean_clingo_ prefix match
            # These are hand-written-only functions (custom implementations)
            mappings.append((name, None, "hand_only"))

    # Rule 4: C→Lean direction (generated has these, hand-written mostly doesn't)
    for name in gen_funcs:
        if name not in used_gen:
            m = re.match(r'(\w+)_to_lean$', name)
            if m:
                mappings.append((None, name, "gen_only_to_lean"))
                used_gen.add(name)

    return mappings, used_gen


def compare_conversion_pair(hw_name, gen_name, hw_func, gen_func, category):
    """Compare a matched pair of functions structurally.

    Returns (issues, expected_diffs, notes) where:
    - issues: real structural problems
    - expected_diffs: differences explained by known design choices
    - notes: informational matches
    """
    issues = []
    expected = []
    notes = []

    hw_body = hw_func.get("body", {})
    gen_body = gen_func.get("body", {})

    # Compare param style
    hw_params = hw_func["params"]
    gen_params = gen_func["params"]
    hw_has_outptr = any("*" in p["type"] and p["name"] == "result"
                       for p in hw_params)
    gen_returns_val = "*" not in gen_func["return_type"] or "lean" in gen_func["return_type"]
    if hw_has_outptr and gen_returns_val and category == "lean_to_c":
        notes.append("param_style: hand=out-pointer, gen=return-by-value")

    # Compare switch cases
    hw_cases = sorted(hw_body.get("switch_cases", []))
    gen_cases = sorted(gen_body.get("switch_cases", []))
    hw_has_sw = hw_body.get("has_switch", False)
    gen_has_sw = gen_body.get("has_switch", False)

    if hw_has_sw and gen_has_sw:
        if len(hw_cases) != len(gen_cases):
            # Tagged union Lean→C: hand-written uses enum constant names in
            # switch but generated uses lean_ptr_tag integer indices. The
            # generated TU lean_to_X doesn't have a switch — it dispatches
            # via case 0: / case 1: etc. in a different code path.
            # Free helpers: hand-written sometimes frees sub-variants that
            # generated tracks differently (e.g. free_string is a no-op).
            if category in ("lean_to_c", "free_helper"):
                expected.append(
                    f"switch_cases: hand={len(hw_cases)}, gen={len(gen_cases)} "
                    f"[variant count diff — gen uses cidx dispatch / different free granularity]")
            else:
                issues.append(f"switch_cases: hand={len(hw_cases)}, gen={len(gen_cases)}")
        else:
            notes.append(f"switch_cases: both have {len(hw_cases)} cases")
    elif hw_has_sw and not gen_has_sw:
        if category == "lean_to_c":
            # Generated tagged-union lean_to_X uses cidx dispatch via
            # lean_ptr_tag() which clang may represent differently than
            # a C switch statement (it's inside the TU helper, not the
            # outer function).
            expected.append(
                f"switch: hand has switch, gen delegates to lean_to_X TU helper "
                f"(switch is inside the helper, not the outer function)")
        else:
            issues.append("hand has switch, gen doesn't")
    elif not hw_has_sw and gen_has_sw:
        issues.append("gen has switch, hand doesn't")

    # Compare malloc count
    hw_malloc = hw_body.get("malloc_count", 0)
    gen_malloc = gen_body.get("malloc_count", 0)
    if hw_malloc != gen_malloc:
        if category == "lean_to_c":
            # Generated lean_to_X for tagged unions may have different
            # malloc counts because:
            # 1. Hand-written sometimes does extra malloc for the result
            #    struct itself (out-pointer style), gen returns by value
            # 2. Some pointer-to-struct sub-variants that hand-written
            #    malloc-then-converts are done inline in generated
            expected.append(
                f"malloc: hand={hw_malloc}, gen={gen_malloc} "
                f"[return-by-value / different sub-alloc strategy]")
        elif category == "function_shim":
            # Hand-written function shims sometimes malloc the statement
            # struct, generated returns by value from lean_to_X
            expected.append(
                f"malloc: hand={hw_malloc}, gen={gen_malloc} "
                f"[hand mallocs struct, gen uses stack return-by-value]")
        else:
            issues.append(f"malloc: hand={hw_malloc}, gen={gen_malloc}")
    elif hw_malloc > 0:
        notes.append(f"malloc: both have {hw_malloc}")

    # Compare free count
    hw_free = hw_body.get("free_count", 0)
    gen_free = gen_body.get("free_count", 0)
    if hw_free != gen_free:
        if category == "function_shim":
            # Generated function shims inline the Except ctor construction
            # which involves more explicit lean_alloc_ctor + lean_ctor_set.
            # Free differences come from:
            # - Generated calls free_X helper vs hand-written inline free
            # - Generated frees array buffers on both success and error paths
            expected.append(
                f"free: hand={hw_free}, gen={gen_free} "
                f"[gen uses free helpers / different error-path cleanup]")
        elif category == "free_helper":
            # Generated free helpers may have different granularity for
            # sub-variant cleanup (e.g. not freeing string fields).
            expected.append(
                f"free: hand={hw_free}, gen={gen_free} "
                f"[different sub-field free granularity]")
        elif category == "lean_to_c":
            expected.append(
                f"free: hand={hw_free}, gen={gen_free} "
                f"[return-by-value eliminates some frees]")
        else:
            issues.append(f"free: hand={hw_free}, gen={gen_free}")

    # Compare ctor operations
    hw_ctor_sets = hw_body.get("ctor_set_count", 0)
    gen_ctor_sets = gen_body.get("ctor_set_count", 0)
    if abs(hw_ctor_sets - gen_ctor_sets) > 2:
        if category == "function_shim":
            # Generated function shims inline Except/IO result construction
            # using lean_alloc_ctor + lean_ctor_set, while hand-written uses
            # helper functions (lean_mk_except_ok, lean_mk_io_error, etc.)
            expected.append(
                f"ctor_set: hand={hw_ctor_sets}, gen={gen_ctor_sets} "
                f"[gen inlines Except/IO ctor; hand uses helper fns]")
        else:
            issues.append(f"ctor_set: hand={hw_ctor_sets}, gen={gen_ctor_sets}")

    # Deep comparison: compare the called sub-functions (conversion helpers).
    # Map hand-written call names to generated names and compare which helpers
    # each function calls.
    hw_calls_set = set(hw_body.get("function_calls", {}).keys())
    gen_calls_set = set(gen_body.get("function_calls", {}).keys())

    # Filter to conversion-related calls only (skip lean runtime fns)
    def is_project_call(name):
        return (("to_lean" in name or "lean_to" in name or
                 "free_" in name) and
                "lean_ctor" not in name and
                "lean_alloc" not in name and
                "lean_box" not in name and
                "lean_unbox" not in name and
                "lean_mk_string" not in name and
                "lean_string_cstr" not in name and
                "lean_dec" not in name and
                "lean_inc" not in name and
                "lean_io" not in name and
                "lean_get_external" not in name and
                "lean_array" not in name)

    hw_proj = sorted(c for c in hw_calls_set if is_project_call(c))
    gen_proj = sorted(c for c in gen_calls_set if is_project_call(c))

    if hw_proj or gen_proj:
        # Try to match each hw call to a gen call via the type name map
        hw_unmatched_calls = []
        gen_unmatched_calls = list(gen_proj)
        matched_calls = []

        for hw_call in hw_proj:
            # Try to find matching gen call
            found = False
            for gen_call in gen_unmatched_calls:
                # Fuzzy match: strip prefixes and compare type stems
                hw_stem = (hw_call.replace("lean_clingo_", "")
                           .replace("_to_", " ").split()[0] if "_to_" in hw_call
                           else hw_call.replace("lean_clingo_free_", ""))
                gen_stem = (gen_call.replace("lean_to_", "")
                            .replace("_to_lean", "")
                            .replace("free_", ""))
                # Normalize: remove "ast_" prefix for comparison
                hw_norm = hw_stem.replace("ast_", "")
                gen_norm = gen_stem.replace("ast_", "")
                if hw_norm == gen_norm or hw_stem == gen_stem:
                    matched_calls.append((hw_call, gen_call))
                    gen_unmatched_calls.remove(gen_call)
                    found = True
                    break
            if not found:
                hw_unmatched_calls.append(hw_call)

        if matched_calls:
            notes.append(f"sub-calls matched: {len(matched_calls)} "
                        f"({', '.join(f'{h}→{g}' for h,g in matched_calls[:3])}"
                        f"{'...' if len(matched_calls) > 3 else ''})")
        if hw_unmatched_calls:
            expected.append(f"hand-only sub-calls: {hw_unmatched_calls} "
                          f"[may be inlined or use different helpers in gen]")
        if gen_unmatched_calls:
            # Generated may call extra helpers (e.g. C→Lean direction helpers
            # that the hand-written Lean→C-only converter doesn't need)
            notes.append(f"gen-extra sub-calls: {gen_unmatched_calls}")

    return issues, expected, notes


def main():
    dump_mode = "--dump" in sys.argv

    print("=" * 70)
    print("Structural Comparison: Hand-Written vs Auto-Generated Clingo Shim")
    print("=" * 70)

    if not GENERATED.exists():
        print(f"\nGenerated shim not found at {GENERATED}")
        print("Run `lake exe test-codegen` first to produce it.")
        sys.exit(1)

    print(f"\nHand-written: {HAND_WRITTEN}")
    print(f"Generated:    {GENERATED}")

    print("\nParsing with clang -ast-dump=json ...")
    hw_ast = parse_ast(HAND_WRITTEN)
    gen_ast = parse_ast(GENERATED)

    print("Extracting function definitions ...")
    hw_funcs = extract_functions(hw_ast, HAND_WRITTEN)
    gen_funcs = extract_functions(gen_ast, GENERATED)
    print(f"  Hand-written: {len(hw_funcs)} functions")
    print(f"  Generated:    {len(gen_funcs)} functions")

    if dump_mode:
        print("\n--- Hand-written functions ---")
        for n in sorted(hw_funcs):
            p = ", ".join(f"{p['type']} {p['name']}" for p in hw_funcs[n]["params"])
            print(f"  {hw_funcs[n]['return_type']} {n}({p})")
        print("\n--- Generated functions ---")
        for n in sorted(gen_funcs):
            p = ", ".join(f"{p['type']} {p['name']}" for p in gen_funcs[n]["params"])
            print(f"  {gen_funcs[n]['return_type']} {n}({p})")

    print("\nBuilding name mapping ...")
    mappings, used_gen = build_name_map(hw_funcs, gen_funcs)

    # ── Report ────────────────────────────────────────────────────────

    print("\n" + "=" * 70)
    print("KNOWN STRUCTURAL DIFFERENCES")
    print("=" * 70)
    for key, desc in KNOWN_DIFFERENCES.items():
        print(f"\n  [{key}]")
        print(f"    {desc}")

    # Categorize mappings
    matched = [(h, g, c) for h, g, c in mappings if h and g]
    hand_only = [(h, g, c) for h, g, c in mappings if h and not g]
    gen_only = [(h, g, c) for h, g, c in mappings if not h and g]

    print("\n" + "=" * 70)
    print(f"MATCHED FUNCTIONS ({len(matched)} pairs)")
    print("=" * 70)

    total_issues = 0
    total_expected = 0
    for hw_name, gen_name, category in sorted(matched, key=lambda x: x[0]):
        issues, expected, notes = compare_conversion_pair(
            hw_name, gen_name, hw_funcs[hw_name], gen_funcs[gen_name], category)
        if issues:
            total_issues += 1
            icon = "✗"
        elif expected:
            total_expected += 1
            icon = "≈"
        else:
            icon = "✓"
        label = f"  [{category}] {hw_name}"
        if hw_name != gen_name:
            label += f"  →  {gen_name}"
        print(f"\n  {icon} {label}")
        for note in notes:
            print(f"      {note}")
        for exp in expected:
            print(f"      ~ {exp}")
        for issue in issues:
            print(f"      ⚠ {issue}")

    print("\n" + "=" * 70)
    print(f"HAND-WRITTEN ONLY ({len(hand_only)} functions)")
    print("=" * 70)
    print("  These exist only in the hand-written shim (not auto-generated).")
    print("  Most are utility helpers or have a different design in generated code.\n")
    for hw_name, _, category in sorted(hand_only, key=lambda x: x[0]):
        reason = ""
        if category == "lean_to_c_unmatched":
            reason = " — no generated Lean→C converter (may be inlined or different naming)"
        elif category == "free_unmatched":
            reason = " — string free (no-op) or different naming"
        elif category == "hand_only":
            # Categorize known hand-only functions
            if "mk_tuple" in hw_name or "mk_except" in hw_name or "mk_option" in hw_name:
                reason = " — utility helper (generated inlines ctor alloc)"
            elif "mk_clingo_error" in hw_name or "mk_io_error" in hw_name:
                reason = " — error helper (generated inlines per-function)"
            elif "repr" in hw_name:
                reason = " — application-level repr (not part of C API)"
            elif "symbol_mk" in hw_name or "clingo_mk" == hw_name.split("lean_")[-1]:
                reason = " — custom symbol constructor (generated uses C API directly)"
            elif "callback_wrapper" in hw_name or "event_callback" in hw_name:
                reason = " — callback trampoline (generated with different name)"
            elif "calculate_flags" in hw_name:
                reason = " — bitfield helper (generated uses bitfieldStruct toC)"
            elif "solve_result" in hw_name:
                reason = " — bitfield convert (generated uses bitfieldStruct helpers)"
            elif "model_size" in hw_name:
                reason = " — convenience wrapper (split into separate API calls)"
            elif "model_costs" in hw_name:
                reason = " — callerAllocates pattern in generated"
            elif "solve_handle" in hw_name:
                reason = " — generated uses outParamBoolStatus style"
            elif "mk_solve_event" in hw_name:
                reason = " — inlined into event callback trampoline"
            else:
                reason = ""
        print(f"  • {hw_name}{reason}")

    # Count unmatched generated functions (excluding enum/opaque helpers)
    gen_unmatched = [name for name in gen_funcs
                     if name not in used_gen
                     and not name.startswith("finalize_")
                     and not name.startswith("get_")
                     and name != "noop_foreach"
                     and name != "noop_finalize"]

    if gen_unmatched:
        print("\n" + "=" * 70)
        print(f"GENERATED ONLY ({len(gen_unmatched)} functions)")
        print("=" * 70)
        print("  Additional functions in the auto-generated shim.\n")
        for name in sorted(gen_unmatched)[:40]:
            print(f"  • {name}")
        if len(gen_unmatched) > 40:
            print(f"  ... and {len(gen_unmatched) - 40} more")

    # ── Summary ───────────────────────────────────────────────────────

    print("\n" + "=" * 70)
    print("SUMMARY")
    print("=" * 70)
    print(f"  Matched pairs:        {len(matched)}")
    print(f"    Identical:          {len(matched) - total_issues - total_expected}")
    print(f"    Expected diffs (≈): {total_expected}")
    print(f"    Real diffs (✗):     {total_issues}")
    print(f"  Hand-written only:    {len(hand_only)}")
    print(f"  Generated only:       {len(gen_unmatched)}")
    print(f"  Generated C→Lean:     {sum(1 for _,_,c in mappings if c == 'gen_only_to_lean')}"
          f" (hand-written has none for AST types)")
    print()

    if total_issues == 0:
        print("  ✓ All matched functions are structurally equivalent")
        if total_expected > 0:
            print(f"    ({total_expected} have expected design-level differences)")
    else:
        print(f"  ✗ {total_issues} matched functions have UNEXPECTED structural differences")
        print("    (review the ⚠ markers above)")

    return 0 if total_issues == 0 else 1


if __name__ == "__main__":
    sys.exit(main())
