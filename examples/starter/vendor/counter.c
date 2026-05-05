#include "counter.h"
#include <stdlib.h>
#include <limits.h>

struct counter_t {
  int32_t value;
};

static _Thread_local char const *last_error = "";

counter_t *counter_create(int32_t initial_value) {
  counter_t *c = (counter_t *)malloc(sizeof(counter_t));
  if (!c) return NULL;
  c->value = initial_value;
  return c;
}

void counter_free(counter_t *c) {
  free(c);
}

bool counter_increment(counter_t *c, int32_t amount) {
  if (!c) {
    last_error = "null pointer";
    return false;
  }
  int64_t result = (int64_t)c->value + (int64_t)amount;
  if (result > INT32_MAX || result < INT32_MIN) {
    last_error = "integer overflow";
    return false;
  }
  c->value = (int32_t)result;
  return true;
}

int32_t counter_get_value(counter_t *c) {
  return c ? c->value : 0;
}

char const *counter_error_message(void) {
  return last_error;
}

void counter_version(int32_t *major, int32_t *minor, int32_t *patch) {
  *major = 1;
  *minor = 0;
  *patch = 0;
}
