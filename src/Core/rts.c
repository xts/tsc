#include <stdio.h>
#include <stdlib.h>

#define CHAR_MASK 0xf
#define FIXNUM_MASK 0x3
#define BOOLEAN_TRUE 0x2f
#define BOOLEAN_FALSE 0x6f
#define NIL 0x3f

int is_fixnum(void *value) {
    return !((int)value & FIXNUM_MASK);
}

int is_true(void *value) {
    return (int)value == BOOLEAN_TRUE;
}

int is_false(void *value) {
    return (int)value == BOOLEAN_FALSE;
}

int is_boolean(void *value) {
    return is_true(value) || is_false(value);
}

int is_nil(void *value) {
    return (int)value == NIL;
}

int is_char(void *value) {
    return ((int)value & 0xff) == CHAR_MASK;
}

int is_string(void *value) {
    return ((int)value & 0x7) == 0x3;
}

int from_fixnum(void *value) {
    return (int)(value) >> 2;
}

char from_char(void *value) {
    return ((int)(value) >> 8) & 0xff;
}

const char *from_string(void *value) {
    return (const char *)((uintptr_t)(value) & ~7);
}

void print(void* value) {
    if (is_fixnum(value)) {
        printf("%d", from_fixnum(value));
    } else if (is_true(value)) {
        printf("#t");
    } else if (is_false(value)) {
        printf("#f");
    } else if (is_char(value)) {
        printf("%c", from_char(value));
    } else if (is_nil(value)) {
        printf("()");
    } else if (is_string(value)) {
        printf("%s", from_string(value));
    } else {
        printf("Invalid value: %p", value);
    }
    printf("\n");
}

void scheme_entry(void *);

int main() {
    void *heap = malloc(1024768);
    scheme_entry(heap);
}
