#include <stdio.h>
#include <string.h>

#include "gc.h"
#include "memory.h"

int scheme_entry(void *heap, void *stack);

int main(int argc, char *argv[]) {
    /* Initialise heaps, stack, and page fault handlers. */
    memory_init();
    gc_init();

    /* Start program. */
    int ret = scheme_entry(memory_heap(), memory_stack_top());

    /* Optionally print statistics. */
    if (argc > 1 && !strcmp(argv[1], "--stats")) {
        memory_print_stats();
    }

    return ret;
}
