import LeanBindgen.Annotation

open LeanBindgen

/-! Annotation file for binding zlib.h directly (via preprocessor).
Exercises: preprocessorArgs, mutableStruct, and constants. -/

def zlibDirectBindings : Bindings := {
  headerPath := "/opt/homebrew/opt/zlib/include/zlib.h"
  leanModule := `Generated.ZlibDirect
  outDir     := "/tmp"
  shimPath   := "/tmp/zlib-direct-shim.c"
  libPrefix  := "zlib"
  preprocessorArgs := #["-I/opt/homebrew/opt/zlib/include"]
  types := #[
    -- z_stream as a mutable struct
    { cName := "z_stream", lean := "ZStream",
      mapping := .mutableStruct {
        cStructTag := "z_stream_s"
        cTypedef   := "z_stream"
        fields := #[
          { cName := "next_in",  leanName := "nextIn",
            kind := .byteArrayInput "avail_in" },
          { cName := "next_out", leanName := "nextOut",
            kind := .byteArrayOutput "avail_out" },
          { cName := "total_in",  leanName := "totalIn",  readOnly := true },
          { cName := "total_out", leanName := "totalOut", readOnly := true },
          { cName := "msg",       leanName := "msg",
            kind := .stringReadOnly }
        ]
      } }
  ]
  functions := #[
    -- deflateInit2_ (the real init function behind the macro)
    { cName := "deflateInit2_", lean := "Zlib.deflateInit2",
      style := .direct, inIO := true },
    -- deflate
    { cName := "deflate", lean := "Zlib.deflate",
      style := .direct, inIO := true },
    -- deflateEnd
    { cName := "deflateEnd", lean := "Zlib.deflateEnd",
      style := .direct, inIO := true },
    -- inflateInit2_
    { cName := "inflateInit2_", lean := "Zlib.inflateInit2",
      style := .direct, inIO := true },
    -- inflate
    { cName := "inflate", lean := "Zlib.inflate",
      style := .direct, inIO := true },
    -- inflateEnd
    { cName := "inflateEnd", lean := "Zlib.inflateEnd",
      style := .direct, inIO := true }
  ]
  constants := #[
    { cName := "Z_OK",            lean := "Z_OK",           type := "Int32", value := "0" },
    { cName := "Z_STREAM_END",    lean := "Z_STREAM_END",   type := "Int32", value := "1" },
    { cName := "Z_NEED_DICT",     lean := "Z_NEED_DICT",    type := "Int32", value := "2" },
    { cName := "Z_FINISH",        lean := "Z_FINISH",       type := "Int32", value := "4" },
    { cName := "Z_NO_FLUSH",      lean := "Z_NO_FLUSH",     type := "Int32", value := "0" },
    { cName := "Z_DEFLATED",      lean := "Z_DEFLATED",     type := "Int32", value := "8" },
    { cName := "MAX_WBITS",       lean := "MAX_WBITS",      type := "Int32", value := "15" },
    { cName := "Z_DEFAULT_COMPRESSION", lean := "Z_DEFAULT_COMPRESSION",
      type := "Int32", value := "-1" }
  ]
}
