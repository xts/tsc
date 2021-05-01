#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "memory.h"

#define TAG_MASK 0x7
#define TAG_ALLOC 0x2

/* The scheme process decrements this value once per allocation. If it is
 * negative, gc_collect() will be called. */
extern uint64_t scheme_gc_countdown;

extern void print(uint64_t *value);

static void reset_countdown() {
    scheme_gc_countdown = 0;
}

static uint64_t get_tag(uint64_t *p) {
    return (uintptr_t)p & TAG_MASK;
}

static uint64_t *set_tag(uint64_t *p, uint64_t tag) {
    return (uint64_t *)((uintptr_t)p | tag);
}

static uint64_t *untag(uint64_t *p) {
    return (uint64_t *)((uintptr_t)p & ~TAG_MASK);
}

/*
 * Find the size of an allocation.
 *
 * @param header Pointer to the allocation header.
 * @return Size of allocation in words, not including the header.
 */
static uint64_t allocation_size(uint64_t *header) {
    return *header >> 3;
}

/*
 * Find the header of the allocation that includes `ptr`.
 *
 * @param ptr Pointer to find the allocation of.
 * @return Address of header, or 0 if the pointer was not
 *         pointing to allocated memory.
 */
static uint64_t *allocation_header(uint64_t *ptr) {
    uint64_t *p = untag(ptr);

    while (p >= memory_heap()) {
        if (get_tag(*(uint64_t **)p) == TAG_ALLOC) {
            if (p + allocation_size(p) >= untag(ptr)) {
                return p;
            }
            break;
        }
        --p;
    }

    return 0;
}

static int is_heap_pointer(uint64_t *p) {
    return untag(p) >= memory_heap() && untag(p) <= memory_heap() + memory_heap_size();
}

static int is_other_heap_pointer(uint64_t *p) {
    return untag(p) >= memory_other_heap() && untag(p) <= memory_other_heap() + memory_heap_size();
}

static uint64_t *get_forwarding_pointer(uint64_t *header) {
    uint64_t *ptr = *(uint64_t **)(header + 1);
    if (is_other_heap_pointer(ptr)) {
        return ptr;
    }
    return 0;
}

static void set_forwarding_pointer(uint64_t *header, uint64_t *ptr) {
    *(uint64_t **)(header + 1) = ptr;
}

static uint64_t *move_allocation(uint64_t *ptr, uint64_t **alloc_ptr) {
    /* Find the allocation header (and verify that it's a valid pointer.) */
    uint64_t *header = allocation_header(untag(ptr));
    if (!header) {
        return 0;
    }

    /* Remember the tag and offset of the pointer we received. */
    uint64_t tag = get_tag(ptr);
    uint64_t offset = untag(ptr) - header;

    /* If this allocation has already been moved, it will have a forwarding pointer
     * and we can return it directly (with tag and offset applied.) */
    uint64_t *address = get_forwarding_pointer(header);

    if (!address) {
        /* Allocate space for the copy in the other heap. */
        uint64_t size = allocation_size(header);
        address = *alloc_ptr;
        *alloc_ptr += size + 1;

        /* Copy the header. */
        *(uint64_t **)address = *(uint64_t **)header;

        /* Recursively copy the contents. */
        for (size_t i = 1; i <= size; ++i) {
            uint64_t *value = *(uint64_t **)(header + i);

            /* Set the forwarding pointer once we've read the first word. */
            if (i == 1) {
                set_forwarding_pointer(header, address);
            }

            if (is_heap_pointer(value)) {
                value = move_allocation(value, alloc_ptr);
            }

            *(uint64_t **)(address + i) = value;
        }
    }

    /* Return the new address with the appropriate offset and tag. */
    return set_tag(address + offset, tag);
}

/*
 * Scan stack for heap-allocated values and copy them.
 *
 * @param sp A lower bound on the stack space to search.
 * @param hp The current heap pointer in the scheme process.
 * @param alloc_ptr The allocation pointer in the new heap.
 */
static void scan_stack(uint64_t *sp, uint64_t *hp, uint64_t **alloc_ptr) {
    while (++sp < memory_stack_top()) {
        /* Read value from stack. */
        uint64_t *value = *(uint64_t **)sp;

        /* If it's a heap pointer, try to move the object it's pointing to and
         * update the stack value to point to the new location. The move will
         * fail if it's a pointer to unallocated space. */
        if (is_heap_pointer(value) && value < hp) {
            uint64_t *ptr = move_allocation(value, alloc_ptr);
            if (ptr) {
                *(uint64_t **)sp = ptr;
            }
        }
    }

    // Taint the old memory for debugging purposes.
    //memset(memory_heap(), 0xff, memory_heap_size() * sizeof(uint64_t));
}

/*
 * Garbage collector entry point.
 *
 * @param heap_ptr The current heap pointer.
 * @return A new heap pointer for the scheme process to use.
 */
uint64_t *gc_collect(uint64_t *hp) {
    /* We're on the scheme process' stack. Capture the frame pointer to set a
     * lower limit on the stack space to scan. */
    uint64_t *sp = __builtin_frame_address(0);

    /* Reset the GC countdown so it won't immediately trigger again. */
    reset_countdown();

    /* Scan the stack for pointers into memory, copying reachable objects to the new heap. */
    uint64_t *alloc_ptr = memory_other_heap();
    scan_stack(sp, hp, &alloc_ptr);

    /* Switch to the new heap and return the new heap pointer. */
    memory_switch_heap();
    return alloc_ptr;
}

void gc_init() {
    reset_countdown();
}
