Scala implementation of Histogrammar
====================================

[![DOI](https://zenodo.org/badge/doi/10.5281/zenodo.61344.svg)](http://dx.doi.org/10.5281/zenodo.61344)

See [https://histogrammar.github.io/histogrammar-docs](https://histogrammar.github.io/histogrammar-docs) for a complete introduction to Histogrammar.

This is a Scala implementation for Scala versions 2.10, 2.11, 2.12 and 2.13.

Latest Scala release: v1.0.30 (Dec 2023).

Announcements
=============

Spark 3.X
---------

With Spark 3.X, based on Scala 2.12 or 2.13, make sure to pick up the correct histogrammar jar file:

```
spark = SparkSession.builder.config("spark.jars.packages", "io.github.histogrammar:histogrammar_2.12:1.0.30,io.github.histogrammar:histogrammar-sparksql_2.12:1.0.30").getOrCreate()
```

For Scala 2.13, in the string above simply replace "2.12" with "2.13".

December, 2023


Installation
============

Histogrammar has a standard Maven POM. With Maven 3+, run

```bash
mvn install -P scala-2.XX
```

in the base directory (to compile everything) or one of the subdirectories, where XX selects the scala version (2.10, 2.11, 2.12, 2.13). 
All subdirectories depend on `core`, so this must be installed first.

Status
======

![Build status](https://travis-ci.org/histogrammar/histogrammar-scala.svg)

The Scala implementation is verified against the Python implementation by running exactly the same tests on both. They agree numerically to one part in a trillion, with the same NaN/infinity handling, and exchange the same JSON.

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
