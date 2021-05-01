#ifndef MEMORY_H_
#define MEMORY_H_

#include <sys/types.h>
#include <stdint.h>

/* Process stack. */
uint64_t *memory_stack();
size_t memory_stack_size();
uint64_t *memory_stack_top();

/* Process heap. */
uint64_t *memory_heap();
uint64_t *memory_other_heap();
size_t memory_heap_size();
uint64_t *memory_switch_heap();

/* Initialise the memory system. */
void memory_init();

/* Print stack/heap statistics. */
void memory_print_stats();

#endif // MEMORY_H_
