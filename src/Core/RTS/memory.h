#ifndef MEMORY_H_
#define MEMORY_H_

#include <sys/types.h>
#include <stdint.h>

/* Process stack. */
uint64_t *memory_stack();
size_t memory_stack_size();

/* Process heap. */
uint64_t *memory_heap();
uint64_t *memory_other_heap();
size_t memory_heap_size();
void memory_switch_heaps();

/* Initialise the memory system. */
void memory_init();

/* Print stack/heap statistics. */
void memory_print_stats();

#endif // MEMORY_H_
