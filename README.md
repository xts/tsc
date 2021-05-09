# Toy compiler

Compiles a statically typed scheme-like language to x86_64 assembly and links it with clang.

- :heavy_check_mark: Closures
- :heavy_check_mark: Proper tail recursion
- :heavy_check_mark: Copying garbage collector
- :heavy_check_mark: Global type inferencing
- :x: Decent error messages

&nbsp;

## Building

Run `cabal build` to build.

Run `make test` to run the test suite with parallelisation enabled.

## Implementation details

See [`src/Core/Compiler.hs`](../main/src/Core/Compiler.hs) for the pipeline.

See [`src/Core/Prelude/prelude.scm`](../main/src/Core/Prelude/prelude.scm) for non-primitive functions available to programs.

The heap, stack, and code all use units of 64-bit words.

### Memory allocations

Memory allocations are performed by bumping a pointer. Every `N`th allocation will
trigger a GC, for some `N`.

For simplicity, all allocations are 16-byte aligned and the immediately preceding word
holds a tagged allocation header containing the size in words (excluding the header.)

The GC performs a depth-first search on the stack, moving reachable allocations to another
heap space and compacting the heap in the process.

### Type tags.

Values are typed based on the tag in their bottom three bits.

| Tag   | Type               |
| ----- | -------------------|
| `001` | Pointer to pair    |
| `010` | Allocation header  | 
| `011` | Pointer to string  |
| `110` | Pointer to closure |
| `?00` | Fixed-size number  |
| `111` | Character          |

Some of these are now obsoleted by the type checker and may go away.

### Stack frame layout

Function arguments are placed in the callee's stack space, and space is reserved
for all let bindings. A function taking `k` arguments and having up to `n` let
bindings in scope will see the following stack values relative to RBP.

| Offset  | Contents                  |
| ------- | ------------------------- |
| ...     | Free stack space          |
| k + n   | Let binding n             |
| ...     | ...                       |
| k + 1   | Let binding 1             |
| k       | Function argument k       |
| ...     | ...                       |
| 1       | Function argument 1       |
| 0       | Address of previous frame |
| -1      | Return address            |


### Closure layout

Closures are allocated as vectors on the heap, with the following layout.

| Index | Content                    |
| ----- | -------------------------- |
| 0     | Pointer to function code   |
| 1     | Pointer to free variable 1 |
| ...   | ...                        |
| k     | Pointer to free variable k |

### Register usage

There is no register allocator yet, so the rules are simple.

| Register | Value |
|----------|-------|
| rsp      | Pointer to latest value pushed to the stack |
| rbp      | Pointer to the start of the stack frame |
| rsi      | Pointer to next heap allocation |
| rdi      | Pointer to current closure |
| rax      | Input and output for operators |

Other registers may be used but are not expected to be preserved between operations.

### Source material

- R. Milner, "A Theory of Type Polymorphism in Programming," 1977.
- A. Ghuloum, "An Incremental Approach to Compiler Construction," 2006.
- M. Grabmüller, "Algorithm W Step by Step," 2006.
