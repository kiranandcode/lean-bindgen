#ifndef RECURSIVE_AST_H
#define RECURSIVE_AST_H

#include <stdint.h>
#include <stddef.h>

/* Minimal AST mimicking clingo_ast_term_t pattern:
   - term_t is a tagged union (location + type + union)
   - function_t has an array-of-terms field
   - unary_op_t has a pointer-to-term field (recursive)
   This tests array-in-struct fields + recursive helper calls. */

typedef struct location {
    const char *file;
    size_t line;
} location_t;

typedef enum term_type {
    term_type_symbol   = 0,
    term_type_function = 1,
    term_type_unary_op = 2
} term_type_t;

/* Forward declarations for recursive types. */
typedef struct term term_t;

typedef struct function_node {
    const char *name;
    term_t const *arguments;   /* array of terms */
    size_t size;
} function_node_t;

typedef struct unary_op {
    uint32_t op;
    term_t const *argument;    /* pointer to single term (recursive) */
} unary_op_t;

struct term {
    location_t location;
    term_type_t type;
    union {
        uint64_t symbol;
        function_node_t const *function;
        unary_op_t const *unary_op;
    };
};

#endif /* RECURSIVE_AST_H */
