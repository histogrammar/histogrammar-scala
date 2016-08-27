Scala implementation of Histogrammar
====================================

See [histogrammar.org](http://histogrammar.org) for a complete introduction to Histogrammar.

This is a Scala implementation for Scala version 2.10.

**FIXME:** add tests (and check implementation) for Scala 2.11. It should work, but still.

Installation
============

Histogrammar has a standard Maven POM. With Maven 3+, run

```bash
mvn install
```

in the base directory (to compile everything) or one of the subdirectories. All subdirectories depend on `core`, so this must be installed first.

Status
======

![Build status](https://travis-ci.org/histogrammar/histogrammar-scala.svg)

The Scala implementation is verified against the Python implementation by running exactly the same tests on both. They agree numerically to one part in a trillion and exchange the same JSON.

All primitives except `UntypedLabel` preserve type information in the Scala REPL, so you can extract values or tab-complete without casting.

In the future, JIT-compilation will be available, similar to the ROOT/Cling interface in Python, but compiling to Java bytecode with [Janino](http://janino-compiler.github.io/janino/) instead of native bytecode with LLVM.

| Primitive         | Scala | JVM JIT |
|:------------------|:------|:--------|
| Count             | done  |         |
| Sum               | done  |         |
| Average           | done  |         |
| Deviate           | done  |         |
| Minimize          | done  |         |
| Maximize          | done  |         |
| Bag               | done  |         |
| Bin               | done  |         |
| SparselyBin       | done  |         |
| CentrallyBin      | done  |         |
| IrregularlyBin    | done  |         |
| Categorize        | done  |         |
| Fraction          | done  |         |
| Stack             | done  |         |
| Select            | done  |         |
| Label             | done  |         |
| UntypedLabel      | done  |         |
| Index             | done  |         |
| Branch            | done  |         |
