#ifndef COUNTER_H
#define COUNTER_H

#include <stdint.h>
#include <stdbool.h>

typedef struct counter_t counter_t;

typedef enum counter_error_e {
  counter_error_none = 0,
  counter_error_overflow = 1,
  counter_error_null_pointer = 2
} counter_error_t;

// Create a new counter with an initial value. Returns NULL on allocation failure.
counter_t *counter_create(int32_t initial_value);

// Free a counter.
void counter_free(counter_t *c);

// Increment by `amount`. Returns false on overflow.
bool counter_increment(counter_t *c, int32_t amount);

// Get the current value.
int32_t counter_get_value(counter_t *c);

// Get the last error message (thread-local).
char const *counter_error_message(void);

// Get major/minor/patch version.
void counter_version(int32_t *major, int32_t *minor, int32_t *patch);

#endif
