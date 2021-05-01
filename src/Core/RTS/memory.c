#include "memory.h"

#include <sys/mman.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Constants. */
#define PAGE_SIZE 4096
#define PAGE_WORDS PAGE_SIZE / 8
#define HEAP_SIZE 64  /* Heap pages to allocate. */
#define STACK_SIZE 4  /* Stack pages to allocate. */

/* Memory locations. */
static uint64_t *g_stack = 0;
static uint64_t *g_moat1 = 0;
static uint64_t *g_heap1 = 0;
static uint64_t *g_moat2 = 0;
static uint64_t *g_heap2 = 0;
static uint64_t *g_active_heap = 0;

static void install_page_fault_handler(void (*handler)(int,siginfo_t *,void *)) {
    /* Set up an alternative stack for signal handlers. */
    struct __darwin_sigaltstack altstack;
    altstack.ss_sp = malloc(SIGSTKSZ);
    altstack.ss_size = SIGSTKSZ;
    altstack.ss_flags = 0;

    if (sigaltstack(&altstack, 0) != 0) {
        perror("sigaltstack");
        exit(1);
    }

    /* Install SIGBUS and SIGSEGV handlers to catch page faults. */
    struct sigaction action;
    action.sa_flags = SA_SIGINFO | SA_ONSTACK;
    action.sa_sigaction = handler;

    if (sigaction(SIGBUS, &action, NULL) == -1) {
        perror("sigfpe: sigaction");
        exit(1);
    }

    if (sigaction(SIGSEGV, &action, NULL) == -1) {
        perror("sigfpe: sigaction");
        exit(1);
    }
}

void page_fault_handler(int signo, siginfo_t *info, void *context) {
    (void)signo;
    (void)context;

    /* If the page fault occurred in the lower half of our first moat, or in our second moat, it
     * means our heap allocations (which grow upwards) failed. */
    if ((info->si_addr >= (void *)g_moat1 && info->si_addr < (void *)g_moat1 + PAGE_SIZE / 2) ||
        (info->si_addr >= (void *)g_moat2 && info->si_addr < (void *)g_moat2 + PAGE_SIZE)) {
        fprintf(stderr, "panic: out of memory\n");
    }
    /* If the page fault occurred in the upper half of our first moat, it
     * means our stack allocations (which grow downwards) failed. */
    else if (info->si_addr >= (void *)g_moat1 + PAGE_SIZE / 2 && info->si_addr < (void *)g_moat1 + PAGE_SIZE) {
        fprintf(stderr, "panic: stack overflow\n");
    }
    /* Otherwise we have no idea. */
    else {
        fprintf(stderr, "page fault at %p\n", info->si_addr);
    }

    exit(11);
}

uint64_t *memory_stack() {
    return g_stack;
}

size_t memory_stack_size() {
    return STACK_SIZE * PAGE_WORDS;
}

uint64_t *memory_stack_top() {
    return memory_stack() + memory_stack_size();
}

uint64_t *memory_heap() {
    return g_active_heap;
}

size_t memory_heap_size() {
    return HEAP_SIZE * PAGE_WORDS;
}

uint64_t *memory_other_heap() {
    return g_active_heap == g_heap1 ? g_heap2 : g_heap1;
}

uint64_t *memory_switch_heap() {
    return g_active_heap = memory_other_heap();
}

void memory_init() {
    /* Allocate heap and stack, with an overflow trap in between. */
    const int size = (HEAP_SIZE * 2 + STACK_SIZE + 2) * PAGE_SIZE;
    uint64_t *mem = (uint64_t *)aligned_alloc(PAGE_SIZE, size);
    memset((void *)mem, 0, sizeof(mem));
    g_heap2 = mem;
    g_moat2 = g_heap2 + PAGE_WORDS * HEAP_SIZE;
    g_heap1 = g_moat2 + PAGE_WORDS;
    g_moat1 = g_heap1 + PAGE_WORDS * HEAP_SIZE;
    g_stack = g_moat1 + PAGE_WORDS;

    g_active_heap = g_heap1;

    /* Protect the space between the heap and the stack
     * to detect OOM and stack overflow. */
    if (mprotect((void *)g_moat1, PAGE_SIZE, PROT_READ) == -1 ||
        mprotect((void *)g_moat2, PAGE_SIZE, PROT_READ) == -1) {
        perror("mprotect");
        exit(1);
    }

    /* Install a page fault handler to report OOM/stack overflow. */
    install_page_fault_handler(page_fault_handler);
}

void memory_print_stats() {
    /* Peak stack usage is guessed at by finding the shallowest non-zero value. */
    uint64_t * p = memory_stack();
    void *stack_top = memory_stack() + memory_stack_size();
    while (!*p++ && (void *)p < stack_top);
    unsigned long bytes_used = stack_top - (void *)p;
    printf("Stack memory used: %8ld bytes (%7ld words)\n", bytes_used, bytes_used / 8);
}
