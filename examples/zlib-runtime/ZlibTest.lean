import Generated.Zlib

open Generated

/-- Assert a condition, panicking with a message on failure. -/
def assert (cond : Bool) (msg : String) : IO Unit :=
  if !cond then throw (IO.Error.userError s!"assertion failed: {msg}")
  else pure ()

/-- Convert a String to a ByteArray (UTF-8 encoding). -/
def stringToByteArray (s : String) : ByteArray :=
  s.toUTF8

/-- Convert a ByteArray to a String (UTF-8 decoding). -/
def byteArrayToString (ba : ByteArray) : String :=
  String.fromUTF8! ba

def main : IO Unit := do
  IO.println "=== zlib runtime tests ==="

  -- ── CRC32 ──
  let data := stringToByteArray "Hello, World!"
  let crc := Zlib.crc32 0 data
  IO.println s!"  CRC32 = 0x{String.mk (Nat.toDigits 16 crc.toNat)}"
  assert (crc == 3964322768) "CRC32 of 'Hello, World!'"

  -- ── Adler32 ──
  let adler := Zlib.adler32 1 data
  IO.println s!"  Adler32 = 0x{String.mk (Nat.toDigits 16 adler.toNat)}"
  assert (adler == 530449514) "Adler32 of 'Hello, World!'"

  -- ── zlib compress/decompress round-trip ──
  let testData := stringToByteArray "The quick brown fox jumps over the lazy dog"
  match ← Zlib.compress testData 6 with
  | .error e => throw (IO.Error.userError s!"zlib compress failed: {e}")
  | .ok compressed =>
    IO.println s!"  zlib compressed: {testData.size} → {compressed.size} bytes"
    match ← Zlib.decompress compressed with
    | .error e => throw (IO.Error.userError s!"zlib decompress failed: {e}")
    | .ok decompressed =>
      assert (decompressed == testData) "zlib round-trip"
      IO.println "  zlib round-trip OK"

  -- ── gzip compress/decompress round-trip ──
  match ← Zlib.gzipCompress testData 6 with
  | .error e => throw (IO.Error.userError s!"gzip compress failed: {e}")
  | .ok compressed =>
    IO.println s!"  gzip compressed: {testData.size} → {compressed.size} bytes"
    match ← Zlib.gzipDecompress compressed with
    | .error e => throw (IO.Error.userError s!"gzip decompress failed: {e}")
    | .ok decompressed =>
      assert (decompressed == testData) "gzip round-trip"
      IO.println "  gzip round-trip OK"

  -- ── raw deflate compress/decompress round-trip ──
  match ← Zlib.rawDeflateCompress testData 6 with
  | .error e => throw (IO.Error.userError s!"raw deflate compress failed: {e}")
  | .ok compressed =>
    IO.println s!"  raw deflate compressed: {testData.size} → {compressed.size} bytes"
    match ← Zlib.rawDeflateDecompress compressed with
    | .error e => throw (IO.Error.userError s!"raw deflate decompress failed: {e}")
    | .ok decompressed =>
      assert (decompressed == testData) "raw deflate round-trip"
      IO.println "  raw deflate round-trip OK"

  -- ── Streaming gzip deflate → inflate ──
  match ← Zlib.gzipDeflateNew 6 with
  | .error e => throw (IO.Error.userError s!"gzip_deflate_new failed: {e}")
  | .ok deflateState =>
    let chunk1 := stringToByteArray "Hello, "
    let chunk2 := stringToByteArray "World!"
    match ← Zlib.gzipDeflatePush deflateState chunk1 with
    | .error e => throw (IO.Error.userError s!"gzip_deflate_push failed: {e}")
    | .ok part1 =>
      match ← Zlib.gzipDeflatePush deflateState chunk2 with
      | .error e => throw (IO.Error.userError s!"gzip_deflate_push failed: {e}")
      | .ok part2 =>
        match ← Zlib.gzipDeflateFinish deflateState with
        | .error e => throw (IO.Error.userError s!"gzip_deflate_finish failed: {e}")
        | .ok part3 =>
          -- Concatenate compressed parts
          let compressed := part1 ++ part2 ++ part3
          IO.println s!"  streaming gzip deflate: 13 → {compressed.size} bytes"

          -- Decompress with whole-buffer API for verification
          match ← Zlib.gzipDecompress compressed with
          | .error e => throw (IO.Error.userError s!"gzip_decompress of streamed data failed: {e}")
          | .ok decompressed =>
            let expected := stringToByteArray "Hello, World!"
            assert (decompressed == expected) "streaming gzip deflate round-trip"
            IO.println "  streaming gzip deflate → decompress OK"

  -- ── Streaming gzip inflate ──
  -- First compress whole buffer, then inflate in streaming mode
  match ← Zlib.gzipCompress (stringToByteArray "ABCDEFGHIJ") 6 with
  | .error e => throw (IO.Error.userError s!"gzip compress for inflate test failed: {e}")
  | .ok compressed =>
    match ← Zlib.gzipInflateNew with
    | .error e => throw (IO.Error.userError s!"gzip_inflate_new failed: {e}")
    | .ok inflateState =>
      match ← Zlib.gzipInflatePush inflateState compressed with
      | .error e => throw (IO.Error.userError s!"gzip_inflate_push failed: {e}")
      | .ok inflated =>
        match ← Zlib.gzipInflateFinish inflateState with
        | .error e => throw (IO.Error.userError s!"gzip_inflate_finish failed: {e}")
        | .ok rest =>
          let result := inflated ++ rest
          let expected := stringToByteArray "ABCDEFGHIJ"
          assert (result == expected) "streaming gzip inflate"
          IO.println "  streaming gzip inflate OK"

  -- ── Error handling: decompress invalid data ──
  let garbage := ByteArray.mk #[0xFF, 0xFE, 0xFD, 0xFC]
  let garbageResult ← Zlib.decompress garbage
  match garbageResult with
  | Except.error e =>
    IO.println s!"  error on invalid data: \"{e}\" (expected)"
  | Except.ok _ =>
    throw (IO.Error.userError "expected error on invalid data, got ok")

  IO.println "=== all zlib runtime tests passed ==="
