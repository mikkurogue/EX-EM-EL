# EX EM EL

The XML parser for my needs.

### Recommendations

If you decide to use this library (once its in a releasable state as a standalone library). There
are some recommendations for use. For practical examples, see the `src/lib/xml.zig` tests.

Allocation, the `Parser` and `Tokenizer` have multiple points of allocation. As we do follow
the same structure to provide your own allocator to structs or methods that require allocation.

The recommendation is to use the `std.heap.ArenaAllocator`, as this library has to allocate nested lists.

As we parse the input in to a parent-child system, or more accurate a tree, we must allocate each level if there are necessary allocations needed. 

As mentioned, see `src/lib/xml.zig` test cases for "practical" implementations using the arena allocator.

### Contributions

Feel free to contribute and improve the library, as it will only embolden the ecosystem.
