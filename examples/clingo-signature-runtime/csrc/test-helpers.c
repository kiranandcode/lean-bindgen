/* Hand-written test helpers that the lean-bindgen codegen doesn't
yet cover. We use these to *exercise* the opaque-pointer path at
runtime even though the function-pointer-laden constructor
`clingo_control_new` itself isn't directly bindable yet. */

#include "lean/lean.h"
#include "clingo.h"

/* Public-linkage class getter emitted by the codegen for the
`clingo_control_t` ↔ Lean `Control` mapping. */
extern lean_external_class *get_control_class(void);

/* Make a default-configured Control. Equivalent to:
     bool clingo_control_new(NULL, 0, NULL, NULL, msg_limit, &out)
   wrapped in our standard Except String result shape. */
LEAN_EXPORT lean_obj_res lean_test_make_default_control(uint32_t msg_limit, lean_object* /* IO state */ _w) {
  (void)_w;
  clingo_control_t *ctrl = NULL;
  if (clingo_control_new(NULL, 0, NULL, NULL, msg_limit, &ctrl)) {
    lean_object *val = lean_alloc_external(get_control_class(), ctrl);
    lean_object *ok  = lean_alloc_ctor(1, 1, 0); /* Except.ok */
    lean_ctor_set(ok, 0, val);
    return lean_io_result_mk_ok(ok);
  } else {
    char const *msg = clingo_error_message();
    if (msg == NULL) msg = "";
    lean_object *err = lean_alloc_ctor(0, 1, 0); /* Except.error */
    lean_ctor_set(err, 0, lean_mk_string(msg));
    return lean_io_result_mk_ok(err);
  }
}
