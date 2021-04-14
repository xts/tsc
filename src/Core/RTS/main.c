#include <sys/mman.h>
#include <sys/types.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

#define PAGE_SIZE 4096
#define HEAP_PAGES 4   /* Heap memory to allocate, in pages. */
#define STACK_PAGES 2  /* Stack memory to allocate, in pages. */

/* The address of our page fault moat, residing between the heap and the stack. */
void *g_moat = NULL;

void *align(void *ptr, int to) {
    uintptr_t start = (uintptr_t)ptr;
    if (start & (to - 1)) {
        start += to - (start & (to - 1));
    }
    return (void *)start;
}

void install_page_fault_handler(void (*handler)(int,siginfo_t *,void *)) {
    /* Set up an alternative stack for signal handlers. */
    struct __darwin_sigaltstack altstack;
    altstack.ss_sp = malloc(SIGSTKSZ);
    altstack.ss_size = SIGSTKSZ;
    altstack.ss_flags = 0;

    if (sigaltstack(&altstack, 0) != 0) {
        perror("sigaltstack");
        exit(1);
    }

    /* Install SIGBUS handler to catch (macos) page traps. */
    struct sigaction action;
    action.sa_flags = SA_SIGINFO | SA_ONSTACK;
    action.sa_sigaction = handler;

    if (sigaction(SIGBUS, &action, NULL) == -1) {
        perror("sigfpe: sigaction");
        exit(1);
    }
}

void page_fault_handler(int signo, siginfo_t *info, void *context) {
    (void)signo;
    (void)context;

    /* If the page fault occurred in the lower half of our moat, it
     * means our heap allocations (which grow upwards) failed. */
    if (info->si_addr >= g_moat && info->si_addr < g_moat + PAGE_SIZE / 2) {
        fprintf(stderr, "panic: out of memory\n");
    }
    /* If the page fault occurred in the upper half of our moat, it
     * means our stack allocations (which grow downwards) failed. */
    else if (info->si_addr >= g_moat + PAGE_SIZE / 2 && info->si_addr < g_moat + PAGE_SIZE) {
        fprintf(stderr, "panic: stack overflow\n");
    }
    /* Otherwise we have no idea. */
    else {
        fprintf(stderr, "page fault at %p\n", info->si_addr);
    }

    exit(11);
}

int scheme_entry(void *heap, void *stack);

int main() {
    /* Allocate heap and stack, with an overflow trap in between. */
    void *mem = malloc(PAGE_SIZE * (HEAP_PAGES + STACK_PAGES + 1));
    void *heap = align(mem, PAGE_SIZE);
    g_moat = heap + PAGE_SIZE * HEAP_PAGES;
    void *stack = g_moat + PAGE_SIZE;

    /* Protect the space between the heap and the stack
     * to detect OOM and stack overflow. */
    if (mprotect(g_moat, PAGE_SIZE, PROT_READ) == -1) {
        perror("mprotect");
        exit(1);
    }

    /* Install a page fault handler to report OOM/stack overflow. */
    install_page_fault_handler(page_fault_handler);

    /* Start program. */
    return scheme_entry(heap, stack + STACK_PAGES * PAGE_SIZE - 8); //
}
