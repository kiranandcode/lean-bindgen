#ifndef MUTABLE_STRUCT_H
#define MUTABLE_STRUCT_H

#include <stddef.h>
#include <stdint.h>

typedef struct my_stream {
    uint8_t *next_in;
    size_t   avail_in;
    uint8_t *next_out;
    size_t   avail_out;
    uint32_t total_in;
    uint32_t total_out;
    const char *msg;
    int      level;
} my_stream_t;

/* A function that takes a pointer to my_stream_t */
int my_stream_init(my_stream_t *strm, int level);
int my_stream_process(my_stream_t *strm, int flush);
void my_stream_end(my_stream_t *strm);

#endif
