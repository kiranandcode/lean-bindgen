/* Synthetic header for testing mixed-scalar struct layout.
   This exercises Lean's ctor field reordering: pointer fields first,
   then USize, then other scalars by descending byte size. */

#include <stdint.h>
#include <stddef.h>

typedef struct mixed_scalars {
    const char *name;       /* boxed (String)   — ctor slot 0       */
    uint32_t    count;      /* 4-byte scalar    — after USize area  */
    size_t      offset;     /* USize            — ctor slot 1       */
    uint8_t     flag;       /* 1-byte scalar    — after uint32      */
    const char *tag;        /* boxed (String)   — ctor slot 1... wait, slot numbering */
    size_t      length;     /* USize            — ctor slot ...     */
    uint16_t    kind;       /* 2-byte scalar    — between u32 and u8*/
} mixed_scalars_t;

/* Round-trip helpers for the runtime test. */
mixed_scalars_t make_mixed(const char *name, uint32_t count, size_t offset,
                           uint8_t flag, const char *tag, size_t length,
                           uint16_t kind);
const char* mixed_get_name(mixed_scalars_t *v);
const char* mixed_get_tag(mixed_scalars_t *v);
uint32_t    mixed_get_count(mixed_scalars_t *v);
size_t      mixed_get_offset(mixed_scalars_t *v);
uint8_t     mixed_get_flag(mixed_scalars_t *v);
size_t      mixed_get_length(mixed_scalars_t *v);
uint16_t    mixed_get_kind(mixed_scalars_t *v);
