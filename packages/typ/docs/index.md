# Typ Manual

This directory is the manual for `typ`.

Right now the main manual is:

- [checker/index.md](./checker/index.md)
  the checker, solver, lowering, engine, and feature-spec stack

The point of this top-level index is to keep the docs hierarchical instead of
letting them drift back into one flat directory of unrelated files.

As `typ` grows, new manuals can sit beside `checker/` instead of getting mixed
into it.

Repository-specific checker parity is tracked in [corpus_coverage.md](./corpus_coverage.md).

The supported expression rules and soundness boundary are specified in
[bidirectional_typing.md](./bidirectional_typing.md).
