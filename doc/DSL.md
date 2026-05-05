# lean-bindgen DSL Guide

The `c_bindings { ... }` macro provides concise syntax for declaring C-to-Lean
bindings. It expands at compile time into a `Bindings` record — no runtime cost.

```lean
import LeanBindgen
open LeanBindgen LeanBindgen.DSL

def myBindings : Bindings := c_bindings {
  -- header fields, type declarations, function declarations, constants
}
```

---

## Header fields

These configure where the C header is, where generated files go, and what
library prefix to use for extern symbol names.

```lean
c_bindings {
  header "vendor/counter.h"                  -- path to C header (required)
  module Generated.Counter                   -- Lean module name for output (required)
  out_dir "Generated"                        -- directory for generated .lean file
  shim "csrc/counter-shim.c"                -- path for generated C shim
  lib "counter"                              -- prefix: extern symbols become lean_counter_*
  preprocessor ["-DFOO", "-I/usr/include"]   -- optional: runs cc -E -P before parsing
}
```

| Field | Type | Notes |
|-------|------|-------|
| `header` | String | Path to the C header file, relative to project root |
| `module` | Name | Fully qualified Lean module name (e.g. `Generated.MyLib`) |
| `out_dir` | String | Output directory for the generated `.lean` file |
| `shim` | String | Output path for the generated C shim `.c` file |
| `lib` | String | Library prefix — `@[extern]` symbols become `lean_<lib>_<cFnName>` |
| `preprocessor` | Array String | If non-empty, runs `cc -E -P <args> <header>` before parsing |

---

## Type declarations

### `scalar` — Scalar newtypes

Maps a C `typedef` of an integer type to a Lean `def`.

```lean
scalar clingo_signature_t => Signature : UInt64
```

**C declaration it matches:**
```c
typedef uint64_t clingo_signature_t;
```

**Generated Lean:**
```lean
def Signature := UInt64
  deriving Repr, Inhabited
```

**Supported scalar types:** `UInt8`, `UInt16`, `UInt32`, `UInt64`, `Int32`, `Int64`

---

### `enum` — Inductive enums

Maps a C enum to a Lean inductive type with conversion helpers in the shim.

```lean
enum counter_error_t => Error tag counter_error_e
  | counter_error_none => none
  | counter_error_overflow => overflow
  | counter_error_null_pointer => nullPointer
```

**C declarations it matches:**
```c
typedef enum counter_error_e {
  counter_error_none = 0,
  counter_error_overflow = 1,
  counter_error_null_pointer = 2
} counter_error_t;
```

**Syntax breakdown:**
- `counter_error_t` — the C typedef name (what appears in function signatures)
- `Error` — the Lean type name
- `tag counter_error_e` — the C enum tag name (used to look up constants in the header)
- Each `| c_name => lean_name` maps a C enumerator to a Lean constructor

**Generated Lean:**
```lean
inductive Error where
  | none
  | overflow
  | nullPointer
  deriving Repr, Inhabited, BEq
```

**Generated C shim** (conversion helpers):
```c
static counter_error_t lean_to_error(uint8_t cidx) {
  switch (cidx) {
    case 0: return counter_error_none;
    case 1: return counter_error_overflow;
    case 2: return counter_error_null_pointer;
    default: return (counter_error_t)0;
  }
}

static uint8_t error_to_lean(counter_error_t v) {
  switch (v) {
    case counter_error_none: return 0;
    case counter_error_overflow: return 1;
    case counter_error_null_pointer: return 2;
    default: return 0;
  }
}
```

---

### `opaque` — Opaque pointers

Maps an opaque C struct pointer to a Lean `opaque` type backed by a
`lean_external_class` with a GC finalizer.

```lean
opaque counter_t => Counter freed_by counter_free
```

**C declarations it matches:**
```c
typedef struct counter_t counter_t;
void counter_free(counter_t *c);
```

**Syntax breakdown:**
- `counter_t` — the C type name
- `Counter` — the Lean type name
- `freed_by counter_free` — the C function called when GC collects the object

**Generated Lean:**
```lean
opaque Counter : Type
```

**Generated C shim:**
```c
static void finalize_counter(void *ptr) {
  counter_free((counter_t *)ptr);
}

static lean_external_class *g_counter_class = NULL;
static pthread_once_t g_counter_once = PTHREAD_ONCE_INIT;

static void init_counter_class(void) {
  g_counter_class = lean_register_external_class(&finalize_counter, &noop_foreach);
}

lean_external_class *get_counter_class(void) {
  pthread_once(&g_counter_once, init_counter_class);
  return g_counter_class;
}
```

**Variant — borrowed (no finalizer):**
```lean
opaque clingo_model_t => Model borrowed
```
Use `borrowed` when Lean does not own the pointer (e.g. it's temporarily
lent by the C library and must not be freed).

---

### `struct` — Struct records

Maps a C struct to a Lean `structure` with field-by-field marshalling.

```lean
struct clingo_location_t => Location tag clingo_location
  | begin_file => beginFile
  | end_file => endFile
  | begin_line => beginLine
  | end_line => endLine
```

**C declaration it matches:**
```c
typedef struct clingo_location {
  char const *begin_file;
  char const *end_file;
  size_t begin_line;
  size_t end_line;
} clingo_location_t;
```

**Syntax breakdown:**
- `clingo_location_t` — the C typedef name
- `Location` — the Lean structure name
- `tag clingo_location` — the C struct tag (used to look up fields in the header)
- Each `| c_field => lean_field` maps a C field name to a Lean field name

**Generated Lean:**
```lean
structure Location where
  beginFile : String
  endFile : String
  beginLine : USize
  endLine : USize
  deriving Repr, Inhabited
```

**Generated C shim** includes `location_to_lean` and `lean_to_location`
helpers that marshal each field according to its resolved type.

---

### `callback` — Callback typedefs

Maps a C function-pointer typedef to a Lean closure type with a trampoline.

```lean
callback clingo_logger_t => Logger
```

**C declaration it matches:**
```c
typedef void (*clingo_logger_t)(clingo_warning_t code, char const *message, void *data);
```

**Generated Lean:**
```lean
def Logger := Warning → String → IO Unit
```

The `void *data` parameter is automatically recognized as user-data and
dropped from the Lean signature. The shim generates a trampoline that
unpacks the Lean closure from the user-data pointer and calls it via
`lean_apply_N`.

---

### `bitfield` — Bitfield structs

Maps a C struct with single-bit flags to a Lean structure of `Bool` fields.

```lean
bitfield clingo_solve_result_bitset_t => SolveResult tag clingo_solve_result_e
  | clingo_solve_result_satisfiable => satisfiable
  | clingo_solve_result_unsatisfiable => unsatisfiable
  | clingo_solve_result_exhausted => exhausted
  | clingo_solve_result_interrupted => interrupted
```

**Generated Lean:**
```lean
structure SolveResult where
  satisfiable : Bool
  unsatisfiable : Bool
  exhausted : Bool
  interrupted : Bool
  deriving Repr, Inhabited
```

The shim packs/unpacks using bitwise operations on the underlying integer.

---

### `type_raw` — Escape hatch

For complex types not covered by the DSL (tagged unions, event callbacks,
mutable structs, variadic builders), pass a raw `TypeAnno` record:

```lean
type_raw { cName := "z_stream", lean := "ZStream",
           mapping := .mutableStruct { ... } }
```

---

## Function declarations

### `cfn` — Direct style

The simplest form: a direct C function call mapped 1:1.

```lean
cfn counter_get_value => getValue +io
```

**C declaration:**
```c
int32_t counter_get_value(counter_t *c);
```

**Generated Lean:**
```lean
@[extern "lean_counter_get_value"]
opaque getValue : @& Counter → IO (Int32)
```

Without `+io`, the function is declared pure:
```lean
cfn clingo_signature_name => name
-- generates: opaque name : Signature → String
```

---

### `out[N] on_error` — Out-param with bool status

The most common C pattern: returns `bool`, writes result to an out-pointer,
and exposes errors via a separate function.

```lean
cfn clingo_signature_create => mk out[3] on_error string clingo_error_message
```

**Syntax breakdown:**
- `out[3]` — parameter at index 3 (0-based) is the output pointer
- `on_error string clingo_error_message` — on failure, call `clingo_error_message()` for the error string

**C declaration:**
```c
bool clingo_signature_create(char const *name, uint32_t arity,
                             bool positive, clingo_signature_t *signature);
```

**Generated Lean:**
```lean
@[extern "lean_clingo_signature_create"]
opaque mk : @& String → UInt32 → Bool → IO (Except String Signature)
```

The out-param (index 3) is removed from the Lean signature and becomes
the success value in `Except.ok`.

**Error return variants:**
```lean
on_error string c_error_fn            -- Except String T (calls c_error_fn() for message)
on_error enum c_code LeanError c_msg  -- Except LeanError T (maps C error code to enum)
on_error tuple c_code LeanError c_msg -- Except (LeanError × String) T (both)
```

---

### `bool_status on_error` — Bool status without out-param

For functions that return `bool` for success/failure but have no output value.

```lean
cfn counter_increment => increment bool_status on_error string counter_error_message +io
```

**C declaration:**
```c
bool counter_increment(counter_t *c, int32_t amount);
```

**Generated Lean:**
```lean
@[extern "lean_counter_increment"]
opaque increment : @& Counter → Int32 → IO (Except String Unit)
```

---

### `void_out[N]` — Void return with out-param

For `void`-returning functions that write their result to a pointer.

```lean
cfn clingo_symbol_create_number => symbolCreateNumber void_out[1]
```

**C declaration:**
```c
void clingo_symbol_create_number(int number, clingo_symbol_t *symbol);
```

**Generated Lean:**
```lean
opaque symbolCreateNumber : Int32 → Symbol
```

---

### `option_out[N]` — Optional out-param

For `bool`-returning functions where `false` means "no result" (not error).

```lean
cfn clingo_symbol_number => symbolNumber option_out[1]
```

**Generated Lean:**
```lean
opaque symbolNumber : Symbol → Option Int32
```

---

### `option_out_array[N, M]` — Optional array out-param

For bool + pointer + size out-params returning an optional array.

```lean
cfn clingo_symbol_arguments => symbolArguments option_out_array[1, 2]
```

**C declaration:**
```c
bool clingo_symbol_arguments(clingo_symbol_t symbol,
                             clingo_symbol_t const **arguments, size_t *arguments_size);
```

**Generated Lean:**
```lean
opaque symbolArguments : Symbol → Option (Array Symbol)
```

---

### `multi_out[0, 1, 2]` — Multiple out-params as tuple

For `void`-returning functions with multiple out-pointers.

```lean
cfn counter_version => version multi_out[0, 1, 2]
```

**C declaration:**
```c
void counter_version(int32_t *major, int32_t *minor, int32_t *patch);
```

**Generated Lean:**
```lean
@[extern "lean_counter_version"]
opaque version : Unit → (Int32 × (Int32 × Int32))
```

Note: when all params are out-params, the Lean function takes `Unit →` to
avoid being a thunk.

---

### `caller_alloc` — Caller-allocates pattern

For two-step APIs where you first query the size, then fill a buffer.

```lean
cfn clingo_symbol_to_string => symbolToString
  caller_alloc "clingo_symbol_to_string_size" [1, 2]
  on_error tuple clingo_error_code Error clingo_error_message +io
```

**C declarations:**
```c
bool clingo_symbol_to_string_size(clingo_symbol_t symbol, size_t *size);
bool clingo_symbol_to_string(clingo_symbol_t symbol, char *string, size_t size);
```

**Generated Lean:**
```lean
opaque symbolToString : Symbol → IO (Except (Error × String) String)
```

---

## Function modifiers

Modifiers appear after the function style and customize behavior:

### `+io`

Wraps the return type in `IO`. Use for any function with side effects.

```lean
cfn counter_create => create +io
-- IO (Counter) instead of Counter
```

### `+nullable_return`

For `char const *` returns that may be NULL. Wraps in `Option String`.

```lean
cfn clingo_error_message => errorMessage +io +nullable_return
-- IO (Option String)
```

### `+nullable_out`

For out-params that may be NULL on success. Wraps in `Option`.

```lean
cfn clingo_solve_handle_model => solveHandleModel +io +nullable_out
  out[1] on_error tuple clingo_error_code Error clingo_error_message
-- IO (Except (Error × String) (Option Model))
```

### `callback_data [N]`

Declares that parameter N is a `void *` user-data slot paired with the
preceding callback parameter. It's dropped from the Lean signature and
the shim passes the boxed closure pointer automatically.

```lean
cfn clingo_parse_term => parseTerm out[4] on_error string clingo_error_message callback_data [2]
```

Here param 1 is a logger callback and param 2 is its user-data.
The Lean signature only shows the callback as a closure argument.

### `array_pairs [(N, M)]`

Declares that params N (pointer) and M (size) form an array pair.
Both are collapsed into a single `@& Array T` argument on the Lean side.

```lean
cfn clingo_symbol_create_function => mkFun
  out[4] on_error string clingo_error_message array_pairs [(1, 2)]
```

**C signature:** `bool fn(name, Symbol *args, size_t args_size, bool pos, Symbol *out)`
**Lean signature:** `@& String → @& Array Symbol → Bool → IO (Except String Symbol)`

### `byte_pairs [(N, M)]`

Like `array_pairs` but for raw byte data. Maps to `ByteArray` using
`lean_sarray_*` (compact scalar array).

### `retained_params [N]`

Indices of parameters whose data is retained by C beyond the call.
The shim deep-copies (malloc) nested payloads and does NOT free them.

### `borrowed_params [N]`

Force `@&` (borrowed reference) on parameter N.

### `extern_sym "name"`

Override the generated `@[extern "..."]` symbol name.

```lean
cfn my_function => myFn extern_sym "custom_lean_fn_name"
-- @[extern "custom_lean_fn_name"]
```

---

## Constants

Define Lean constants with explicit values.

```lean
cconst Z_OK : Int32 := "0"
cconst Z_STREAM_END : Int32 := "1"
cconst MAX_WBITS : Int32 := "15"
cconst Z_DEFAULT_COMPRESSION : Int32 := "-1"
```

**Generated Lean:**
```lean
def Z_OK : Int32 := 0
def Z_STREAM_END : Int32 := 1
def MAX_WBITS : Int32 := 15
def Z_DEFAULT_COMPRESSION : Int32 := -1
```

Supported types: `Int32`, `Int64`, `UInt8`, `UInt16`, `UInt32`, `UInt64`

---

## Complete example

Here's the full binding spec for the starter example (`examples/starter/Bindings.lean`):

```lean
import LeanBindgen

open LeanBindgen LeanBindgen.DSL

def counterBindings : Bindings := c_bindings {
  header "vendor/counter.h"
  module Generated.Counter
  out_dir "Generated"
  shim "csrc/counter-shim.c"
  lib "counter"

  -- Maps: typedef enum counter_error_e { ... } counter_error_t;
  -- To:   inductive Error where | none | overflow | nullPointer
  enum counter_error_t => Error tag counter_error_e
    | counter_error_none => none
    | counter_error_overflow => overflow
    | counter_error_null_pointer => nullPointer

  -- Maps: typedef struct counter_t counter_t; + void counter_free(counter_t *);
  -- To:   opaque Counter : Type  (GC calls counter_free)
  opaque counter_t => Counter freed_by counter_free

  -- Maps: counter_t *counter_create(int32_t initial_value);
  -- To:   opaque create : Int32 → IO (Counter)
  cfn counter_create => create +io

  -- Maps: bool counter_increment(counter_t *c, int32_t amount);
  -- To:   opaque increment : @& Counter → Int32 → IO (Except String Unit)
  cfn counter_increment => increment bool_status on_error string counter_error_message +io

  -- Maps: int32_t counter_get_value(counter_t *c);
  -- To:   opaque getValue : @& Counter → IO (Int32)
  cfn counter_get_value => getValue +io

  -- Maps: void counter_version(int32_t *major, int32_t *minor, int32_t *patch);
  -- To:   opaque version : Unit → (Int32 × (Int32 × Int32))
  cfn counter_version => version multi_out[0, 1, 2]
}
```
