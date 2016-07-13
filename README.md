Scala implementation of Histogrammar
====================================

See [histogrammar.org](http://histogrammar.org) for a complete introduction to Histogrammar.

This is a Scala implementation for Scala version 2.10.

**FIXME:** add tests (and check implementation) for Scala 2.11.

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

The tests are pretty good, though not as thorough as [histogrammar-python](https://github.com/histogrammar/histogrammar-python).

Primitive implementation is mature. All primitives except `UntypedLabel` preserve type information in the Scala REPL, so you can extract values or tab-complete without casting.

| Primitive         | Scala |
|:------------------|:------|
| Count             | done  |
| Sum               | done  |
| Average           | done  |
| Deviate           | done  |
| AbsoluteErr       | done  |
| Minimize          | done  |
| Maximize          | done  |
| Quantile          | done  |
| Bin               | done  |
| SparselyBin       | done  |
| CentrallyBin      | done  |
| AdaptivelyBin     | done  |
| Categorize        | done  |
| Fraction          | done  |
| Stack             | done  |
| Partition         | done  |
| Select            | done  |
| Limit             | done  |
| Label             | done  |
| UntypedLabel      | done  |
| Index             | done  |
| Branch            | done  |
| Bag               | done  |
| Sample            | done  |
