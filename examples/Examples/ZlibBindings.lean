import LeanBindgen.Annotation

open LeanBindgen

/-! Annotation file for zlib_lean_api.h — exercises the new
`byteArrayPairs` and `byteArrayOutBoolStatus` codegen paths. -/

def zlibBindings : Bindings := {
  headerPath := "examples/zlib/zlib_lean_api.h"
  leanModule := `Generated.Zlib
  outDir     := "examples/zlib-runtime"
  shimPath   := "examples/zlib-runtime/csrc/zlib-shim.c"
  libPrefix  := "zlib"
  leanImports := #[]
  types := #[
    { cName := "zlib_deflate_state", lean := "DeflateState",
      mapping := .opaquePointer "zlib_deflate_free" },
    { cName := "zlib_inflate_state", lean := "InflateState",
      mapping := .opaquePointer "zlib_inflate_free" }
  ]
  functions := #[
    -- ── Checksums ──
    { cName := "zlib_crc32", lean := "Zlib.crc32",
      byteArrayPairs := [(1, 2)] },
    { cName := "zlib_adler32", lean := "Zlib.adler32",
      byteArrayPairs := [(1, 2)] },

    -- ── Whole-buffer compress (with level) ──
    { cName := "zlib_compress", lean := "Zlib.compress",
      style := .byteArrayOutBoolStatus 3 4 (.string "zlib_last_error"),
      byteArrayPairs := [(0, 1)] },
    { cName := "gzip_compress", lean := "Zlib.gzipCompress",
      style := .byteArrayOutBoolStatus 3 4 (.string "zlib_last_error"),
      byteArrayPairs := [(0, 1)] },
    { cName := "raw_deflate_compress", lean := "Zlib.rawDeflateCompress",
      style := .byteArrayOutBoolStatus 3 4 (.string "zlib_last_error"),
      byteArrayPairs := [(0, 1)] },

    -- ── Whole-buffer decompress (no level) ──
    { cName := "zlib_decompress", lean := "Zlib.decompress",
      style := .byteArrayOutBoolStatus 2 3 (.string "zlib_last_error"),
      byteArrayPairs := [(0, 1)] },
    { cName := "gzip_decompress", lean := "Zlib.gzipDecompress",
      style := .byteArrayOutBoolStatus 2 3 (.string "zlib_last_error"),
      byteArrayPairs := [(0, 1)] },
    { cName := "raw_deflate_decompress", lean := "Zlib.rawDeflateDecompress",
      style := .byteArrayOutBoolStatus 2 3 (.string "zlib_last_error"),
      byteArrayPairs := [(0, 1)] },

    -- ── Streaming: gzip deflate ──
    { cName := "gzip_deflate_new", lean := "Zlib.gzipDeflateNew",
      style := .outParamBoolStatus 1 (.string "zlib_last_error"),
      inIO := true },
    { cName := "gzip_deflate_push", lean := "Zlib.gzipDeflatePush",
      style := .byteArrayOutBoolStatus 3 4 (.string "zlib_last_error"),
      byteArrayPairs := [(1, 2)],
      inIO := true },
    { cName := "gzip_deflate_finish", lean := "Zlib.gzipDeflateFinish",
      style := .byteArrayOutBoolStatus 1 2 (.string "zlib_last_error"),
      inIO := true },

    -- ── Streaming: gzip inflate ──
    { cName := "gzip_inflate_new", lean := "Zlib.gzipInflateNew",
      style := .outParamBoolStatus 0 (.string "zlib_last_error"),
      inIO := true },
    { cName := "gzip_inflate_push", lean := "Zlib.gzipInflatePush",
      style := .byteArrayOutBoolStatus 3 4 (.string "zlib_last_error"),
      byteArrayPairs := [(1, 2)],
      inIO := true },
    { cName := "gzip_inflate_finish", lean := "Zlib.gzipInflateFinish",
      style := .byteArrayOutBoolStatus 1 2 (.string "zlib_last_error"),
      inIO := true },

    -- ── Error reporting ──
    { cName := "zlib_last_error", lean := "Zlib.lastError",
      nullableReturn := true, inIO := true }
  ]
}
