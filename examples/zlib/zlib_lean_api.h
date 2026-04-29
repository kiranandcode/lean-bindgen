/*
 * zlib_lean_api.h — lean-bindgen-friendly wrapper around zlib.
 *
 * This header declares a thin C API that wraps zlib operations in a
 * shape lean-bindgen can parse (no macros, no varargs, no inline bodies).
 * The implementations live in zlib-wrapper.c.
 */
#ifndef ZLIB_LEAN_API_H
#define ZLIB_LEAN_API_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/* ── Checksums ──────────────────────────────────────────────────── */

uint32_t zlib_crc32(uint32_t init, const uint8_t *data, size_t len);
uint32_t zlib_adler32(uint32_t init, const uint8_t *data, size_t len);

/* ── Whole-buffer compress / decompress ────────────────────────── */

/* zlib format (RFC 1950) */
bool zlib_compress(const uint8_t *src, size_t src_len, uint8_t level,
                   uint8_t **out, size_t *out_len);
bool zlib_decompress(const uint8_t *src, size_t src_len,
                     uint8_t **out, size_t *out_len);

/* gzip format (RFC 1952) */
bool gzip_compress(const uint8_t *src, size_t src_len, uint8_t level,
                   uint8_t **out, size_t *out_len);
bool gzip_decompress(const uint8_t *src, size_t src_len,
                     uint8_t **out, size_t *out_len);

/* raw deflate (no header/trailer) */
bool raw_deflate_compress(const uint8_t *src, size_t src_len, uint8_t level,
                          uint8_t **out, size_t *out_len);
bool raw_deflate_decompress(const uint8_t *src, size_t src_len,
                            uint8_t **out, size_t *out_len);

/* ── Streaming state ───────────────────────────────────────────── */

typedef struct zlib_deflate_state zlib_deflate_state;
typedef struct zlib_inflate_state zlib_inflate_state;

void zlib_deflate_free(zlib_deflate_state *state);
void zlib_inflate_free(zlib_inflate_state *state);

/* ── Streaming: gzip deflate ───────────────────────────────────── */

bool gzip_deflate_new(uint8_t level, zlib_deflate_state **out);
bool gzip_deflate_push(zlib_deflate_state *state,
                       const uint8_t *chunk, size_t chunk_len,
                       uint8_t **out, size_t *out_len);
bool gzip_deflate_finish(zlib_deflate_state *state,
                         uint8_t **out, size_t *out_len);

/* ── Streaming: gzip inflate ───────────────────────────────────── */

bool gzip_inflate_new(zlib_inflate_state **out);
bool gzip_inflate_push(zlib_inflate_state *state,
                       const uint8_t *chunk, size_t chunk_len,
                       uint8_t **out, size_t *out_len);
bool gzip_inflate_finish(zlib_inflate_state *state,
                         uint8_t **out, size_t *out_len);

/* ── Error reporting ───────────────────────────────────────────── */

const char *zlib_last_error(void);

#endif /* ZLIB_LEAN_API_H */
