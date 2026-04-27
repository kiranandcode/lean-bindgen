#ifndef DEEP_STRUCT_H
#define DEEP_STRUCT_H

#include <stdint.h>
#include <stddef.h>

typedef struct inner {
    const char *name;
    uint32_t value;
} inner_t;

typedef struct outer {
    const char *label;
    inner_t const *child;    /* pointer-to-struct (needs malloc) */
    inner_t  embedded;       /* by-value nested struct (no malloc for root) */
    size_t count;
} outer_t;

#endif /* DEEP_STRUCT_H */
