#include <stdio.h>

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

int is_pair(void *value) {
    return ((int)value & 0x7) == 0x1;
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

void print(void *value);

void print_pair(void *value, int in_pair) {
    uintptr_t *p = (uintptr_t*)value;
    p = (uintptr_t *)((long unsigned)p & ~1);
    void *car = (void *)p[0];
    void *cdr = (void *)p[1];

    if (!in_pair) {
        printf("(");
    }
    print(car);
    if (is_pair(cdr)) {
        printf(" ");
        print_pair(cdr, 1);
    }
    else if (is_nil(cdr)) {
        printf(")");
    }
    else {
        printf(" . ");
        print(cdr);
        printf(")");
    }
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
    } else if (is_pair(value)) {
        print_pair(value, 0);
    } else {
        printf("Invalid value: %p", value);
    }
}

char read_char() {
    return getc(stdin);
}
