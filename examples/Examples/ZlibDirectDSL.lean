import LeanBindgen

open LeanBindgen LeanBindgen.DSL

/-! DSL version of ZlibDirect.lean — same bindings, concise syntax. -/

def zlibDirectBindingsDSL : Bindings := c_bindings {
  header "/opt/homebrew/opt/zlib/include/zlib.h"
  module Generated.ZlibDirectDSL
  out_dir "/tmp"
  shim "/tmp/zlib-direct-dsl-shim.c"
  lib "zlib"
  preprocessor ["-I/opt/homebrew/opt/zlib/include"]

  type_raw { cName := "z_stream", lean := "ZStream",
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

  cfn deflateInit2_ => Zlib.deflateInit2 +io
  cfn deflate => Zlib.deflate +io
  cfn deflateEnd => Zlib.deflateEnd +io
  cfn inflateInit2_ => Zlib.inflateInit2 +io
  cfn inflate => Zlib.inflate +io
  cfn inflateEnd => Zlib.inflateEnd +io

  cconst Z_OK : Int32 := "0"
  cconst Z_STREAM_END : Int32 := "1"
  cconst Z_NEED_DICT : Int32 := "2"
  cconst Z_FINISH : Int32 := "4"
  cconst Z_NO_FLUSH : Int32 := "0"
  cconst Z_DEFLATED : Int32 := "8"
  cconst MAX_WBITS : Int32 := "15"
  cconst Z_DEFAULT_COMPRESSION : Int32 := "-1"
}
