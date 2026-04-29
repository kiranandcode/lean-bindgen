/*
 * zlib-wrapper.c — Implementation of zlib_lean_api.h using zlib.
 *
 * This file is hand-written (not generated). It implements the thin
 * wrapper functions declared in zlib_lean_api.h.
 */

#include <zlib.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "zlib_lean_api.h"

/* ── Thread-local error message ────────────────────────────────── */

static _Thread_local char g_last_error[256] = "";

static void set_error(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(g_last_error, sizeof(g_last_error), fmt, ap);
    va_end(ap);
}

const char *zlib_last_error(void) {
    return g_last_error[0] ? g_last_error : NULL;
}

/* ── Checksums ─────────────────────────────────────────────────── */

uint32_t zlib_crc32(uint32_t init, const uint8_t *data, size_t len) {
    return (uint32_t)crc32((uLong)init, data, (uInt)len);
}

uint32_t zlib_adler32(uint32_t init, const uint8_t *data, size_t len) {
    return (uint32_t)adler32((uLong)init, data, (uInt)len);
}

/* ── Generic compress/decompress with window bits ──────────────── */

static bool do_compress(const uint8_t *src, size_t src_len, uint8_t level,
                        int windowBits, uint8_t **out, size_t *out_len) {
    z_stream strm;
    memset(&strm, 0, sizeof(strm));
    int ret = deflateInit2(&strm, level, Z_DEFLATED, windowBits, 8,
                           Z_DEFAULT_STRATEGY);
    if (ret != Z_OK) {
        set_error("deflateInit2 failed: %s", zError(ret));
        return false;
    }

    size_t bound = deflateBound(&strm, (uLong)src_len);
    uint8_t *buf = (uint8_t *)malloc(bound);
    if (!buf) {
        deflateEnd(&strm);
        set_error("malloc(%zu) failed", bound);
        return false;
    }

    strm.next_in  = (Bytef *)src;
    strm.avail_in = (uInt)src_len;
    strm.next_out  = buf;
    strm.avail_out = (uInt)bound;

    ret = deflate(&strm, Z_FINISH);
    if (ret != Z_STREAM_END) {
        free(buf);
        deflateEnd(&strm);
        set_error("deflate failed: %s", zError(ret));
        return false;
    }

    *out_len = strm.total_out;
    *out = buf;
    deflateEnd(&strm);
    return true;
}

static bool do_decompress(const uint8_t *src, size_t src_len,
                           int windowBits, uint8_t **out, size_t *out_len) {
    z_stream strm;
    memset(&strm, 0, sizeof(strm));
    int ret = inflateInit2(&strm, windowBits);
    if (ret != Z_OK) {
        set_error("inflateInit2 failed: %s", zError(ret));
        return false;
    }

    strm.next_in  = (Bytef *)src;
    strm.avail_in = (uInt)src_len;

    size_t cap = src_len * 4;
    if (cap < 256) cap = 256;
    uint8_t *buf = (uint8_t *)malloc(cap);
    if (!buf) {
        inflateEnd(&strm);
        set_error("malloc(%zu) failed", cap);
        return false;
    }
    size_t total = 0;

    do {
        if (total >= cap) {
            cap *= 2;
            uint8_t *newbuf = (uint8_t *)realloc(buf, cap);
            if (!newbuf) {
                free(buf);
                inflateEnd(&strm);
                set_error("realloc(%zu) failed", cap);
                return false;
            }
            buf = newbuf;
        }
        strm.next_out  = buf + total;
        strm.avail_out = (uInt)(cap - total);
        ret = inflate(&strm, Z_NO_FLUSH);
        if (ret == Z_STREAM_ERROR || ret == Z_DATA_ERROR ||
            ret == Z_MEM_ERROR || ret == Z_NEED_DICT) {
            free(buf);
            inflateEnd(&strm);
            set_error("inflate failed: %s", zError(ret));
            return false;
        }
        total = strm.total_out;
    } while (ret != Z_STREAM_END);

    *out = buf;
    *out_len = total;
    inflateEnd(&strm);
    return true;
}

/* ── Public whole-buffer functions ─────────────────────────────── */

bool zlib_compress(const uint8_t *src, size_t src_len, uint8_t level,
                   uint8_t **out, size_t *out_len) {
    return do_compress(src, src_len, level, 15, out, out_len);
}

bool zlib_decompress(const uint8_t *src, size_t src_len,
                     uint8_t **out, size_t *out_len) {
    return do_decompress(src, src_len, 15, out, out_len);
}

bool gzip_compress(const uint8_t *src, size_t src_len, uint8_t level,
                   uint8_t **out, size_t *out_len) {
    return do_compress(src, src_len, level, 15 + 16, out, out_len);
}

bool gzip_decompress(const uint8_t *src, size_t src_len,
                     uint8_t **out, size_t *out_len) {
    return do_decompress(src, src_len, 15 + 16, out, out_len);
}

bool raw_deflate_compress(const uint8_t *src, size_t src_len, uint8_t level,
                          uint8_t **out, size_t *out_len) {
    return do_compress(src, src_len, level, -15, out, out_len);
}

bool raw_deflate_decompress(const uint8_t *src, size_t src_len,
                            uint8_t **out, size_t *out_len) {
    return do_decompress(src, src_len, -15, out, out_len);
}

/* ── Streaming state ───────────────────────────────────────────── */

struct zlib_deflate_state {
    z_stream strm;
    int       windowBits;
};

struct zlib_inflate_state {
    z_stream strm;
    int       windowBits;
};

void zlib_deflate_free(zlib_deflate_state *state) {
    if (state) {
        deflateEnd(&state->strm);
        free(state);
    }
}

void zlib_inflate_free(zlib_inflate_state *state) {
    if (state) {
        inflateEnd(&state->strm);
        free(state);
    }
}

/* ── Streaming: gzip deflate ───────────────────────────────────── */

bool gzip_deflate_new(uint8_t level, zlib_deflate_state **out) {
    zlib_deflate_state *s = (zlib_deflate_state *)calloc(1, sizeof(*s));
    if (!s) { set_error("calloc failed"); return false; }
    s->windowBits = 15 + 16;
    int ret = deflateInit2(&s->strm, level, Z_DEFLATED, s->windowBits, 8,
                           Z_DEFAULT_STRATEGY);
    if (ret != Z_OK) {
        set_error("deflateInit2 failed: %s", zError(ret));
        free(s);
        return false;
    }
    *out = s;
    return true;
}

bool gzip_deflate_push(zlib_deflate_state *state,
                       const uint8_t *chunk, size_t chunk_len,
                       uint8_t **out, size_t *out_len) {
    size_t bound = deflateBound(&state->strm, (uLong)chunk_len);
    uint8_t *buf = (uint8_t *)malloc(bound > 0 ? bound : 1);
    if (!buf) { set_error("malloc failed"); return false; }

    state->strm.next_in  = (Bytef *)chunk;
    state->strm.avail_in = (uInt)chunk_len;
    state->strm.next_out  = buf;
    state->strm.avail_out = (uInt)bound;

    int ret = deflate(&state->strm, Z_NO_FLUSH);
    if (ret != Z_OK) {
        free(buf);
        set_error("deflate (push) failed: %s", zError(ret));
        return false;
    }

    *out_len = bound - state->strm.avail_out;
    *out = buf;
    return true;
}

bool gzip_deflate_finish(zlib_deflate_state *state,
                         uint8_t **out, size_t *out_len) {
    size_t cap = 256;
    uint8_t *buf = (uint8_t *)malloc(cap);
    if (!buf) { set_error("malloc failed"); return false; }

    size_t base_total = state->strm.total_out;
    size_t written = 0;

    state->strm.avail_in = 0;
    state->strm.next_in  = NULL;

    int ret;
    do {
        if (written >= cap) {
            cap *= 2;
            uint8_t *newbuf = (uint8_t *)realloc(buf, cap);
            if (!newbuf) { free(buf); set_error("realloc failed"); return false; }
            buf = newbuf;
        }
        state->strm.next_out  = buf + written;
        state->strm.avail_out = (uInt)(cap - written);
        ret = deflate(&state->strm, Z_FINISH);
        written = state->strm.total_out - base_total;
    } while (ret == Z_OK);

    if (ret != Z_STREAM_END) {
        free(buf);
        set_error("deflate (finish) failed: %s", zError(ret));
        return false;
    }

    *out     = buf;
    *out_len = written;
    return true;
}

/* ── Streaming: gzip inflate ───────────────────────────────────── */

bool gzip_inflate_new(zlib_inflate_state **out) {
    zlib_inflate_state *s = (zlib_inflate_state *)calloc(1, sizeof(*s));
    if (!s) { set_error("calloc failed"); return false; }
    s->windowBits = 15 + 16;
    int ret = inflateInit2(&s->strm, s->windowBits);
    if (ret != Z_OK) {
        set_error("inflateInit2 failed: %s", zError(ret));
        free(s);
        return false;
    }
    *out = s;
    return true;
}

bool gzip_inflate_push(zlib_inflate_state *state,
                       const uint8_t *chunk, size_t chunk_len,
                       uint8_t **out, size_t *out_len) {
    size_t cap = chunk_len * 4;
    if (cap < 256) cap = 256;
    uint8_t *buf = (uint8_t *)malloc(cap);
    if (!buf) { set_error("malloc failed"); return false; }

    state->strm.next_in  = (Bytef *)chunk;
    state->strm.avail_in = (uInt)chunk_len;

    size_t base_total = state->strm.total_out;
    size_t written = 0;
    int ret;
    do {
        if (written >= cap) {
            cap *= 2;
            uint8_t *newbuf = (uint8_t *)realloc(buf, cap);
            if (!newbuf) { free(buf); set_error("realloc failed"); return false; }
            buf = newbuf;
        }
        state->strm.next_out  = buf + written;
        state->strm.avail_out = (uInt)(cap - written);
        ret = inflate(&state->strm, Z_NO_FLUSH);
        if (ret == Z_DATA_ERROR || ret == Z_MEM_ERROR || ret == Z_NEED_DICT) {
            free(buf);
            set_error("inflate (push) failed: %s", zError(ret));
            return false;
        }
        written = state->strm.total_out - base_total;
    } while (ret != Z_STREAM_END && state->strm.avail_in > 0);

    *out     = buf;
    *out_len = written;
    return true;
}

bool gzip_inflate_finish(zlib_inflate_state *state,
                         uint8_t **out, size_t *out_len) {
    /* For inflate, "finish" just flushes any remaining buffered data. */
    size_t cap = 256;
    uint8_t *buf = (uint8_t *)malloc(cap);
    if (!buf) { set_error("malloc failed"); return false; }

    size_t base_total = state->strm.total_out;
    size_t written = 0;

    state->strm.avail_in = 0;
    state->strm.next_in  = NULL;

    int ret;
    do {
        if (written >= cap) {
            cap *= 2;
            uint8_t *newbuf = (uint8_t *)realloc(buf, cap);
            if (!newbuf) { free(buf); set_error("realloc failed"); return false; }
            buf = newbuf;
        }
        state->strm.next_out  = buf + written;
        state->strm.avail_out = (uInt)(cap - written);
        ret = inflate(&state->strm, Z_FINISH);
        if (ret == Z_DATA_ERROR || ret == Z_MEM_ERROR) {
            free(buf);
            set_error("inflate (finish) failed: %s", zError(ret));
            return false;
        }
        written = state->strm.total_out - base_total;
    } while (ret != Z_STREAM_END && state->strm.avail_out == 0);

    *out     = buf;
    *out_len = written;
    return true;
}
