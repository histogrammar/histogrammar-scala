// Copyright 2016 DIANA-HEP
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.dianahep

import scala.collection.immutable.ListMap
import scala.collection.immutable.SortedMap
import scala.collection.mutable
import scala.language.existentials
import scala.language.implicitConversions

import org.dianahep.histogrammar.json._

package histogrammar {
  /** Exception type for improperly configured containers. */
  class ContainerException(message: String, cause: Exception = null) extends Exception(message, cause)

  //////////////////////////////////////////////////////////////// general definition of an container, its factory, and mix-in

  /** Interface for a container factory, always named as imperative verbs, such as "Count" and "Bin".
    * 
    * Each factory has:
    * 
    *    - a custom `apply` method to create an active container than can aggregate data (and is therefore mutable). This active container is named by the gerund form of the verb, such as "Counting" and "Binning".
    *    - a custom `ed` method to create a fixed container that cannot aggregate data (immutable), only merge with the `+` operator. This fixed container is named by the past tense form of the verb, such as "Counted" and "Binned".
    *    - a uniform `fromJsonFragment` method that can reconstruct a fixed (immutable) container from its JSON representation. This is used by the `Factory` object's `fromJson` entry point. (Click on the "t" in a circle in the upper-left to see the `Factory` object's documentation, rather than the `Factory` trait.
    *    - `unapply` methods to unpack active and fixed containers in Scala pattern matching.
    * 
    */
  trait Factory {
    /** Name of the concrete `Factory` as a string; used to label the container type in JSON. */
    def name: String
    /** Help text that can be queried interactively: a one-liner that can be included in a menu. */
    def help: String
    /** Help text that can be queried interactively: more detail than `help`. ('''FIXME:''' currently only contains the `apply` signature.) */
    def detailedHelp: String
    /** Reconstructs a container of known type from JSON. General users should call the `Factory` object's `fromJson`, which uses header data to identify the container type. (This is called by `fromJson`.) */
    def fromJsonFragment(json: Json, nameFromParent: Option[String]): Container[_] with NoAggregation
  }

  /** Entry point for constructing containers from JSON and centralized registry of container types.
    * 
    * Containers filled in Python or on remote sites are serialized as JSON that the `Factory` object can reconstruct. Reconstructed containers are fixed (immutable, cannot aggregate), but can be merged with the `+` operator. (Click on the "o" in a circle in the upper-left to see the `Factory` trait's documentation, which explains the difference.)
    * 
    * To do this, the `Factory` object must dispatch JSON to the appropriate container for interpretation. It therefore manages a global registry of container types (concrete instances of the `Factory` trait). General users are not expected to add to this registry, but they could if they want to.
    */
  object Factory {
    private var known = ListMap[String, Factory]()

    /** Get a list of registered containers as a `Map` from factory name to `Factory` object. */
    def registered = known

    /** Add a new `Factory` to the registry, introducing a new container type on the fly. General users usually wouldn't do this, but they could. This method is used internally to define the standard container types. */
    def register(factory: Factory) {
      known = known.updated(factory.name, factory)
    }

    register(Count)
    register(Sum)
    register(Average)
    register(Deviate)
    register(Minimize)
    register(Maximize)

    register(Bin)
    register(SparselyBin)
    register(CentrallyBin)
    register(IrregularlyBin)
    register(Categorize)

    register(Fraction)
    register(Stack)

    register(Select)
    register(Limit)
    register(Label)
    register(UntypedLabel)
    register(Index)
    register(Branch)

    register(Bag)

    /** Get a registered container by its name. */
    def apply(name: String) = known.get(name) match {
      case Some(x) => x
      case None => throw new ContainerException(s"unrecognized container (is it a custom container that hasn't been registered?): $name")
    }

    /** User's entry point for reconstructing a container from JSON text.
      * 
      * The container's type is not known at compile-time, so it must be cast (with the container's `as` method) or pattern-matched (with the corresponding `Factory`).
      */
    def fromJson(str: String): Container[_] with NoAggregation = Json.parse(str) match {
      case Some(json) => fromJson(json)
      case None => throw new InvalidJsonException(str)
    }

    /** User's entry point for reconstructing a container from a JSON object.
      * 
      * The container's type is not known at compile-time, so it must be cast (with the container's `as` method) or pattern-matched (with the corresponding `Factory`).
      */
    def fromJson(json: Json): Container[_] with NoAggregation = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("type", "data")) =>
        val get = pairs.toMap

        val name = get("type") match {
          case JsonString(x) => x
          case x => throw new JsonFormatException(x, "Factory.type")
        }

        Factory(name).fromJsonFragment(get("data"), None)

      case _ => throw new JsonFormatException(json, "Factory")
    }

    /** User's entry point for reading a container as JSON from a UTF-8 encoded file.
      * 
      * The container's type is not known at compile-time, so it must be cast (with the container's `as` method) or pattern-matched (with the corresponding `Factory`).
      */
    def fromJsonFile(fileName: String): Container[_] with NoAggregation = fromJsonFile(new java.io.File(fileName))

    /** User's entry point for reading a container as JSON from a UTF-8 encoded file.
      * 
      * The container's type is not known at compile-time, so it must be cast (with the container's `as` method) or pattern-matched (with the corresponding `Factory`).
      */
    def fromJsonFile(file: java.io.File): Container[_] with NoAggregation = fromJsonStream(new java.io.FileInputStream(file))

    /** User's entry point for reading a container as JSON from a UTF-8 encoded file.
      * 
      * Note: fully consumes the `inputStream`.
      * 
      * The container's type is not known at compile-time, so it must be cast (with the container's `as` method) or pattern-matched (with the corresponding `Factory`).
      */
    def fromJsonStream(inputStream: java.io.InputStream): Container[_] with NoAggregation =
      fromJson(new java.util.Scanner(inputStream).useDelimiter("\\A").next)
  }

  /** Interface for classes that contain aggregated data, such as "Counted" or "Binned" (immutable) or "Counting" or "Binning" (mutable).
    * 
    * There are two "tenses" of containers: present tense ("Counting", "Binning", etc.) that additionally mix-in [[org.dianahep.histogrammar.Aggregation]] and have a `fill` method for accumulating data, and past tense ("Counted", "Binned", etc.) that can only be merged with the `+` operator.
    * 
    * Containers are monoids: they have a neutral element (`zero`) and an associative operator (`+`). Thus, partial sums aggregated in parallel can be combined arbitrarily.
    * 
    * The `Container` is parameterized by itself (an example of the [[https://en.wikipedia.org/wiki/Curiously_recurring_template_pattern curiously recurring template pattern]]) to pass type information at compile-time.
    */
  trait Container[CONTAINER <: Container[CONTAINER]] extends Serializable {
    /** Intended for the general user to copy a complex container's type into the `as` method of a container whose type is not known at compile-time.
      * 
      * Typical use: `filledHistogram.as[initialHistogram.Type]`
      */
    type Type
    /** The type of the immutable version of this container. */
    type EdType <: Container[EdType] with NoAggregation
    /** Reference to the container's factory for runtime reflection. */
    def factory: Factory

    /** Every `Container` accumulates a sum of weights of observed data.
      * 
      * The [[org.dianahep.histogrammar.Counting]]/[[org.dianahep.histogrammar.Counted]] container ''only'' accumulates a sum of weights.
      * 
      * Its data type is `Double` because in principal, it can be any non-negative real number.
      */
    def entries: Double
    /** Create an empty container with the same parameters as this one.
      * 
      * If this container is mutable (with [[org.dianahep.histogrammar.Aggregation]]), the new one will be, too.
      * 
      * The original is unaffected.
      */
    def zero: CONTAINER
    /** Add two containers of the same type.
      * 
      * If these containers are mutable (with [[org.dianahep.histogrammar.Aggregation]]), the new one will be, too.
      * 
      * The originals are unaffected.
      */
    def +(that: CONTAINER): CONTAINER
    /** Copy this container, making a clone with no reference to the original.
      * 
      * If these containers are mutable (with [[org.dianahep.histogrammar.Aggregation]]), the new one will be, too.
      */
    def copy = this + zero

    /** Convert this container to JSON (dropping its `fill` method, making it immutable).
      * 
      * Note that the [[org.dianahep.histogrammar.json.Json]] object has a `stringify` method to serialize.
      */
    def toJson: Json = JsonObject("type" -> JsonString(factory.name), "data" -> toJsonFragment(false))
    /** Used internally to convert the container to JSON without its `"type"` header. */
    def toJsonFragment(suppressName: Boolean): Json
    /** Convert any Container into a NoAggregation Container. */
    def ed: EdType = Factory.fromJson(toJson).asInstanceOf[EdType]
    /** Cast the container to a given type. Especially useful for containers reconstructed from JSON or stored in [[org.dianahep.histogrammar.UntypedLabeling]]/[[org.dianahep.histogrammar.UntypedLabeled]]. */
    def as[OTHER <: Container[OTHER]] = this.asInstanceOf[OTHER]

    /** List of sub-aggregators, to make it possible to walk the tree. */
    def children: Seq[Container[_]]
  }

  /** Mix-in to provide a quantity name for immutable Containers (analogous to [[org.dianahep.histogrammar.AnyQuantity]] for mutable Containers). */
  trait QuantityName {
    def quantityName: Option[String]
  }

  /** Mix-in to declare that the [[org.dianahep.histogrammar.Container]] is immutable (opposite of [[org.dianahep.histogrammar.Aggregation]]). */
  trait NoAggregation

  /** Mix-in to add mutability to a [[org.dianahep.histogrammar.Container]].
    * 
    * Containers without `Aggregation` can only be merged with the `+` operator, but containers with `Aggregation` can additionally be accumulated with `fill`.
    * 
    * Containers without `Aggregation` are named as past-tense verbs, such as "Counted" and "Binned", which containers with `Aggregation` are named with the gerund form, such as "Counting" and "Binning".
    * 
    * `Aggregation` is parameterized by the fill data type `Datum`, which is an abstract type member rather than a type parameter (square brackets) for better type inference.
    * 
    * This data type is implemented as contravariant: a container that expects to be filled with a given data type can accept that data type's subclass.
    */
  trait Aggregation {
    /** Type of data expected by `fill`. */
    type Datum

    /** The `entries` member of mutable containers is a `var`, rather than `val`. */
    def entries_=(x: Double)
    /** Entry point for the general user to pass data into the container for aggregation.
      * 
      * Usually all containers in a collection of histograms take the same input data by passing it recursively through the tree. Quantities to plot are specified by the individual container's lambda functions.
      * 
      * The container is changed in-place.
      */
    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0)

    /** List of sub-aggregators, to make it possible to walk the tree. */
    protected var checkedForCrossReferences = false
    protected def checkForCrossReferences(memo: mutable.Set[Aggregation] = mutable.Set[Aggregation]()) {
      if (!checkedForCrossReferences) {
        if (memo.exists(_ eq this))
          throw new ContainerException(s"cannot fill a tree that contains the same aggregator twice: $this")
        memo += this
        this.asInstanceOf[Container[_]].children.foreach(_.asInstanceOf[Aggregation].checkForCrossReferences(memo))
        checkedForCrossReferences = true
      }
    }
  }

  /** Sub-trait of [[org.dianahep.histogrammar.Aggregation]] for all containers except [[org.dianahep.histogrammar.Counting]].
    * 
    * `AggregationOnData` containers actually depend on their `Datum` type; `Counting` is the only one that ignores it.
    */
  trait AggregationOnData extends Aggregation

  /** Trait for aggregations that use a fill rule of any type. */
  trait AnyQuantity[DATUM, RANGE] {
    def quantity: UserFcn[DATUM, RANGE]
  }

  /** Trait for aggregations that use a numerical (double-valued) fill rule. */
  trait NumericalQuantity[DATUM] extends AnyQuantity[DATUM, Double]

  /** Trait for aggregations that use a categorical (string-valued) fill rule. */
  trait CategoricalQuantity[DATUM] extends AnyQuantity[DATUM, String]

  /** Increment function for Apache Spark's `aggregate` method.
    * 
    * Typical use: `filledHistogram = datasetRDD.aggregate(initialHistogram)(new Increment, new Combine)` where `datasetRDD` is a collection of `initialHistogram`'s `Datum` type.
    */
  class Increment[DATUM, CONTAINER <: Container[CONTAINER] with AggregationOnData {type Datum >: DATUM}] extends Function2[CONTAINER, DATUM, CONTAINER] with Serializable {
    def apply(h: CONTAINER, x: DATUM): CONTAINER = {h.fill(x); h}
  }

  /** Combine function for Apache Spark's `aggregate` method.
    * 
    * Typical use: `filledHistogram = datasetRDD.aggregate(initialHistogram)(new Increment, new Combine)` where `datasetRDD` is a collection of `initialHistogram`'s `Datum` type.
    */
  class Combine[CONTAINER <: Container[CONTAINER]] extends Function2[CONTAINER, CONTAINER, CONTAINER] with Serializable {
    def apply(h1: CONTAINER, h2: CONTAINER): CONTAINER = h1 + h2
  }

  /** Persistent JSON output file or named pipe (for use with Histogrammar Watcher (hgwatch)). */
  class JsonDump(file: java.io.File) {
    def this(fileName: String) = this(new java.io.File(fileName))
    private val fileOutputStream = new java.io.FileOutputStream(file, true)
    def append[C <: Container[C]](container: C) {
      fileOutputStream.write(container.toJson.stringify.getBytes("UTF-8"))
      fileOutputStream.write("\n".getBytes("UTF-8"))
      fileOutputStream.flush()
    }
    def append(json: Json) {
      fileOutputStream.write(json.stringify.getBytes("UTF-8"))
      fileOutputStream.write("\n".getBytes("UTF-8"))
      fileOutputStream.flush()
    }
    def close() {
      fileOutputStream.close()
    }
  }
}

/** Main library for Histogrammar.
  * 
  * Defines all types for the general user, including implicits to construct Histogrammar-specific types from Scala basic types.
  * 
  * A general user is expected to `import org.dianahep.histogrammar._` to bring all of these implicits into scope.
  */
package object histogrammar {
  /** Help function for interactive use. Used to discover container types. */
  def help = Factory.registered map {case (name, factory) => f"${name}%-15s ${factory.help}"} mkString("\n")

  //////////////////////////////////////////////////////////////// define implicits

  /** Base trait for user functions. */
  trait UserFcn[-DOMAIN, +RANGE] extends Serializable {
    /** Optional name for the function; added to JSON for bookkeeping if present. */
    def name: Option[String]
    /** Tracks whether this function has a cache to ensure that a function doesn't get double-cached. */
    def hasCache: Boolean
    /** Call the function. */
    def apply[SUB <: DOMAIN](x: SUB): RANGE

    /** Tracks whether this function has a name to raise an error if it gets named again. */
    def hasName = !name.isEmpty

    /** Create a named version of this function.
      * 
      * Note that the `{x: Datum => f(x)} named "something"` syntax is more human-readable.
      * 
      * Note that this function commutes with `cached` (they can be applied in either order).
      */
    def named(n: String) =
      if (hasName)
        throw new IllegalArgumentException(s"two names applied to the same function: ${name.get} and $n")
      else {
        val f = this
        new UserFcn[DOMAIN, RANGE] {
          def name = Some(n)
          def hasCache = f.hasCache
          def apply[SUB <: DOMAIN](x: SUB): RANGE = f(x)
        }
      }

    /** Create a cached version of this function.
      * 
      * Note that the `{x: Datum => f(x)} cached` syntax is more human-readable.
      * 
      * Note that this function commutes with `named` (they can be applied in either order).
      * 
      * '''Example:'''
      * 
      * {{{
      * val f = cache {x: Double => complexFunction(x)}
      * f(3.14)   // computes the function
      * f(3.14)   // re-uses the old value
      * f(4.56)   // computes the function again at a new point
      * }}}
      */
    def cached =
      if (hasCache)
        this
      else {
        val f = this
        new UserFcn[DOMAIN, RANGE] {
          private var last: Option[(DOMAIN, RANGE)] = None
          def name = f.name
          def hasCache = true
          def apply[SUB <: DOMAIN](x: SUB): RANGE = (x, last) match {
            case (xref: AnyRef, Some((oldx: AnyRef, oldy))) if (xref eq oldx) => oldy
            case (_,            Some((oldx, oldy)))         if (x == oldx)    => oldy
            case _ =>
              val y = f(x)
              last = Some(x -> y)
              y
          }
        }
      }
  }

  implicit class NumericalFcnFromBoolean[-DATUM](val original: DATUM => Boolean) extends UserFcn[DATUM, Double] {
    def name = None
    def hasCache = false
    def apply[SUB <: DATUM](x: SUB): Double = if (original(x)) 1.0 else 0.0
  }
  implicit class NumericalFcnFromByte[-DATUM](val original: DATUM => Byte) extends UserFcn[DATUM, Double] {
    def name = None
    def hasCache = false
    def apply[SUB <: DATUM](x: SUB): Double = original(x).toDouble
  }
  implicit class NumericalFcnFromShort[-DATUM](val original: DATUM => Short) extends UserFcn[DATUM, Double] {
    def name = None
    def hasCache = false
    def apply[SUB <: DATUM](x: SUB): Double = original(x).toDouble
  }
  implicit class NumericalFcnFromInt[-DATUM](val original: DATUM => Int) extends UserFcn[DATUM, Double] {
    def name = None
    def hasCache = false
    def apply[SUB <: DATUM](x: SUB): Double = original(x).toDouble
  }
  implicit class NumericalFcnFromLong[-DATUM](val original: DATUM => Long) extends UserFcn[DATUM, Double] {
    def name = None
    def hasCache = false
    def apply[SUB <: DATUM](x: SUB): Double = original(x).toDouble
  }
  implicit class NumericalFcnFromFloat[-DATUM](val original: DATUM => Float) extends UserFcn[DATUM, Double] {
    def name = None
    def hasCache = false
    def apply[SUB <: DATUM](x: SUB): Double = original(x).toDouble
  }
  implicit class NumericalFcnFromDouble[-DATUM](val original: DATUM => Double) extends UserFcn[DATUM, Double] {
    def name = None
    def hasCache = false
    def apply[SUB <: DATUM](x: SUB): Double = original(x)
  }

  /** Wraps a user's function for extracting strings (categories) from the input data type. */
  implicit class CategoricalFcn[-DATUM](val original: DATUM => String) extends UserFcn[DATUM, String] {
    def name = None
    def hasCache = false
    def apply[SUB <: DATUM](x: SUB): String = original(x)
  }

  /** Wraps a user's function for extracting multidimensional numeric data from the input data type. */
  implicit class MultivariateFcn[-DATUM](val original: DATUM => Iterable[Double]) extends UserFcn[DATUM, Vector[Double]] {
    def name = None
    def hasCache = false
    def apply[SUB <: DATUM](x: SUB): Vector[Double] = original(x).toVector
  }

  /** Default weighting function that always returns 1.0. */
  class Unweighted[DATUM] extends UserFcn[DATUM, Double] {
    def name = Some("unweighted")
    def hasCache = true
    def apply[SUB <: DATUM](x: SUB) = 1.0
  }

  /** Default weighting function that always returns 1.0. */
  def unweighted[DATUM] = new Unweighted[DATUM]

  /** Introduces a `===` operator for all `Double` tolerance comparisons.
    * 
    * Custom equality rules:
    * 
    *   - NaN == NaN (NaNs are used by some primitives to indicate missing data).
    *   - Infinity == Infinity and -Infinity == -Infinity (naturally, but has to be explicit given the following).
    *   - if [[org.dianahep.histogrammar.util.relativeTolerance]] is greater than zero, numbers may differ by this small ratio.
    *   - if [[org.dianahep.histogrammar.util.absoluteTolerance]] is greater than zero, numbers may differ by this small difference.
    * 
    * Python's math.isclose algorithm is applied for non-NaNs:
    * 
    *     `abs(x - y) <= max(relativeTolerance * max(abs(x), abs(y)), absoluteTolerance)`
    */
  implicit class Numeq(val x: Double) extends AnyVal {
    def ===(that: Double) =
      if (this.x.isNaN  &&  that.isNaN)
        true
      else if (this.x.isInfinite  &&  that.isInfinite)
        (this.x > 0.0) == (that > 0.0)
      else if (util.relativeTolerance > 0.0  &&  util.absoluteTolerance > 0.0)
        Math.abs(this.x - that) <= Math.max(util.relativeTolerance * Math.max(Math.abs(this.x), Math.abs(that)), util.absoluteTolerance)
      else if (util.relativeTolerance > 0.0)
        Math.abs(this.x - that) <= util.relativeTolerance * Math.max(Math.abs(this.x), Math.abs(that))
      else if (util.absoluteTolerance > 0.0)
        Math.abs(this.x - that) <= util.absoluteTolerance
      else
        this.x == that
  }

  //////////////////////////////////////////////////////////////// indexes for nested collections

  sealed trait CollectionIndex

  implicit class IntegerIndex(val int: Int) extends CollectionIndex {
    override def toString() = int.toString
  }
  object IntegerIndex {
    def unapply(x: IntegerIndex) = Some(x.int)
  }

  implicit class StringIndex(val string: String) extends CollectionIndex {
    override def toString() = "\"" + scala.util.parsing.json.JSONFormat.quoteString(string) + "\""
  }
  object StringIndex {
    def unapply(x: StringIndex) = Some(x.string)
  }

  implicit class SymbolIndex(val symbol: Symbol) extends CollectionIndex {
    override def toString() = symbol.toString
  }
  object SymbolIndex {
    def unapply(x: SymbolIndex) = Some(x.symbol)
  }

  trait Collection {
    def apply(indexes: CollectionIndex*): Container[_]

    def walk[X](op: Seq[CollectionIndex] => X): Seq[X] = walk(op, Seq[CollectionIndex]())

    private def walk[X](op: Seq[CollectionIndex] => X, base: Seq[CollectionIndex]): Seq[X] = this match {
      case labeled: Labeled[_] => labeled.pairs.flatMap {
        case (k, vs: Collection) => vs.walk(op, base :+ StringIndex(k))
        case (k, v) => Seq(op(base :+ StringIndex(k)))
      }

      case labeling: Labeling[_] => labeling.pairs.flatMap {
        case (k, vs: Collection) => vs.walk(op, base :+ StringIndex(k))
        case (k, v) => Seq(op(base :+ StringIndex(k)))
      }

      case untypedLabeled: UntypedLabeled => untypedLabeled.pairs.flatMap {
        case (k, vs: Collection) => vs.walk(op, base :+ StringIndex(k))
        case (k, v) => Seq(op(base :+ StringIndex(k)))
      }

      case untypedLabeling: UntypedLabeling[_] => untypedLabeling.pairs.flatMap {
        case (k, vs: Collection) => vs.walk(op, base :+ StringIndex(k))
        case (k, v) => Seq(op(base :+ StringIndex(k)))
      }

      case indexed: Indexed[_] => indexed.values.zipWithIndex.flatMap {
        case (vs: Collection, i) => vs.walk(op, base :+ IntegerIndex(i))
        case (v, i) => Seq(op(base :+ IntegerIndex(i)))
      }

      case indexing: Indexing[_] => indexing.values.zipWithIndex.flatMap {
        case (vs: Collection, i) => vs.walk(op, base :+ IntegerIndex(i))
        case (v, i) => Seq(op(base :+ IntegerIndex(i)))
      }

      case branched: Branched[_, _] => branched.values.zipWithIndex.flatMap {
        case (vs: Collection, 0) => vs.walk(op, base :+ SymbolIndex('i0))
        case (vs: Collection, 1) => vs.walk(op, base :+ SymbolIndex('i1))
        case (vs: Collection, 2) => vs.walk(op, base :+ SymbolIndex('i2))
        case (vs: Collection, 3) => vs.walk(op, base :+ SymbolIndex('i3))
        case (vs: Collection, 4) => vs.walk(op, base :+ SymbolIndex('i4))
        case (vs: Collection, 5) => vs.walk(op, base :+ SymbolIndex('i5))
        case (vs: Collection, 6) => vs.walk(op, base :+ SymbolIndex('i6))
        case (vs: Collection, 7) => vs.walk(op, base :+ SymbolIndex('i7))
        case (vs: Collection, 8) => vs.walk(op, base :+ SymbolIndex('i8))
        case (vs: Collection, 9) => vs.walk(op, base :+ SymbolIndex('i9))
        case (vs: Collection, i) => vs.walk(op, base :+ IntegerIndex(i))
        case (v, 0) => Seq(op(base :+ SymbolIndex('i0)))
        case (v, 1) => Seq(op(base :+ SymbolIndex('i1)))
        case (v, 2) => Seq(op(base :+ SymbolIndex('i2)))
        case (v, 3) => Seq(op(base :+ SymbolIndex('i3)))
        case (v, 4) => Seq(op(base :+ SymbolIndex('i4)))
        case (v, 5) => Seq(op(base :+ SymbolIndex('i5)))
        case (v, 6) => Seq(op(base :+ SymbolIndex('i6)))
        case (v, 7) => Seq(op(base :+ SymbolIndex('i7)))
        case (v, 8) => Seq(op(base :+ SymbolIndex('i8)))
        case (v, 9) => Seq(op(base :+ SymbolIndex('i9)))
        case (v, i) => Seq(op(base :+ IntegerIndex(i)))
      }

      case branching: Branching[_, _] => branching.values.zipWithIndex.flatMap {
        case (vs: Collection, 0) => vs.walk(op, base :+ SymbolIndex('i0))
        case (vs: Collection, 1) => vs.walk(op, base :+ SymbolIndex('i1))
        case (vs: Collection, 2) => vs.walk(op, base :+ SymbolIndex('i2))
        case (vs: Collection, 3) => vs.walk(op, base :+ SymbolIndex('i3))
        case (vs: Collection, 4) => vs.walk(op, base :+ SymbolIndex('i4))
        case (vs: Collection, 5) => vs.walk(op, base :+ SymbolIndex('i5))
        case (vs: Collection, 6) => vs.walk(op, base :+ SymbolIndex('i6))
        case (vs: Collection, 7) => vs.walk(op, base :+ SymbolIndex('i7))
        case (vs: Collection, 8) => vs.walk(op, base :+ SymbolIndex('i8))
        case (vs: Collection, 9) => vs.walk(op, base :+ SymbolIndex('i9))
        case (vs: Collection, i) => vs.walk(op, base :+ IntegerIndex(i))
        case (v, 0) => Seq(op(base :+ SymbolIndex('i0)))
        case (v, 1) => Seq(op(base :+ SymbolIndex('i1)))
        case (v, 2) => Seq(op(base :+ SymbolIndex('i2)))
        case (v, 3) => Seq(op(base :+ SymbolIndex('i3)))
        case (v, 4) => Seq(op(base :+ SymbolIndex('i4)))
        case (v, 5) => Seq(op(base :+ SymbolIndex('i5)))
        case (v, 6) => Seq(op(base :+ SymbolIndex('i6)))
        case (v, 7) => Seq(op(base :+ SymbolIndex('i7)))
        case (v, 8) => Seq(op(base :+ SymbolIndex('i8)))
        case (v, 9) => Seq(op(base :+ SymbolIndex('i9)))
        case (v, i) => Seq(op(base :+ IntegerIndex(i)))
      }
    }

    def indexes = walk((x: Seq[CollectionIndex]) => x).toSeq
  }

  //////////////////////////////////////////////////////////////// typesafe extractors for Branched/Branching

  /** Add `i0`, `i1`, etc. methods to `Branched` containers. */
  implicit class Branched0[C0 <: Container[C0] with NoAggregation, TAIL <: BranchedList](x: Branched[C0, TAIL]) {
    def i0 = x.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branched` containers. */
  implicit class Branched1[C0 <: Container[C0] with NoAggregation, C1 <: Container[C1] with NoAggregation, TAIL <: BranchedList](x: Branched[C0, Branched[C1, TAIL]]) {
    def i1 = x.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branched` containers. */
  implicit class Branched2[C0 <: Container[C0] with NoAggregation, C1 <: Container[C1] with NoAggregation, C2 <: Container[C2] with NoAggregation, TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, TAIL]]]) {
    def i2 = x.tail.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branched` containers. */
  implicit class Branched3[C0 <: Container[C0] with NoAggregation, C1 <: Container[C1] with NoAggregation, C2 <: Container[C2] with NoAggregation, C3 <: Container[C3] with NoAggregation, TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, Branched[C3, TAIL]]]]) {
    def i3 = x.tail.tail.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branched` containers. */
  implicit class Branched4[C0 <: Container[C0] with NoAggregation, C1 <: Container[C1] with NoAggregation, C2 <: Container[C2] with NoAggregation, C3 <: Container[C3] with NoAggregation, C4 <: Container[C4] with NoAggregation, TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, Branched[C3, Branched[C4, TAIL]]]]]) {
    def i4 = x.tail.tail.tail.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branched` containers. */
  implicit class Branched5[C0 <: Container[C0] with NoAggregation, C1 <: Container[C1] with NoAggregation, C2 <: Container[C2] with NoAggregation, C3 <: Container[C3] with NoAggregation, C4 <: Container[C4] with NoAggregation, C5 <: Container[C5] with NoAggregation, TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, Branched[C3, Branched[C4, Branched[C5, TAIL]]]]]]) {
    def i5 = x.tail.tail.tail.tail.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branched` containers. */
  implicit class Branched6[C0 <: Container[C0] with NoAggregation, C1 <: Container[C1] with NoAggregation, C2 <: Container[C2] with NoAggregation, C3 <: Container[C3] with NoAggregation, C4 <: Container[C4] with NoAggregation, C5 <: Container[C5] with NoAggregation, C6 <: Container[C6] with NoAggregation, TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, Branched[C3, Branched[C4, Branched[C5, Branched[C6, TAIL]]]]]]]) {
    def i6 = x.tail.tail.tail.tail.tail.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branched` containers. */
  implicit class Branched7[C0 <: Container[C0] with NoAggregation, C1 <: Container[C1] with NoAggregation, C2 <: Container[C2] with NoAggregation, C3 <: Container[C3] with NoAggregation, C4 <: Container[C4] with NoAggregation, C5 <: Container[C5] with NoAggregation, C6 <: Container[C6] with NoAggregation, C7 <: Container[C7] with NoAggregation, TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, Branched[C3, Branched[C4, Branched[C5, Branched[C6, Branched[C7, TAIL]]]]]]]]) {
    def i7 = x.tail.tail.tail.tail.tail.tail.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branched` containers. */
  implicit class Branched8[C0 <: Container[C0] with NoAggregation, C1 <: Container[C1] with NoAggregation, C2 <: Container[C2] with NoAggregation, C3 <: Container[C3] with NoAggregation, C4 <: Container[C4] with NoAggregation, C5 <: Container[C5] with NoAggregation, C6 <: Container[C6] with NoAggregation, C7 <: Container[C7] with NoAggregation, C8 <: Container[C8] with NoAggregation, TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, Branched[C3, Branched[C4, Branched[C5, Branched[C6, Branched[C7, Branched[C8, TAIL]]]]]]]]]) {
    def i8 = x.tail.tail.tail.tail.tail.tail.tail.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branched` containers. */
  implicit class Branched9[C0 <: Container[C0] with NoAggregation, C1 <: Container[C1] with NoAggregation, C2 <: Container[C2] with NoAggregation, C3 <: Container[C3] with NoAggregation, C4 <: Container[C4] with NoAggregation, C5 <: Container[C5] with NoAggregation, C6 <: Container[C6] with NoAggregation, C7 <: Container[C7] with NoAggregation, C8 <: Container[C8] with NoAggregation, C9 <: Container[C9] with NoAggregation, TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, Branched[C3, Branched[C4, Branched[C5, Branched[C6, Branched[C7, Branched[C8, Branched[C9, TAIL]]]]]]]]]]) {
    def i9 = x.tail.tail.tail.tail.tail.tail.tail.tail.tail.head
  }

  /** Add `i0`, `i1`, etc. methods to `Branching` containers. */
  implicit class Branching0[C0 <: Container[C0] with Aggregation, TAIL <: BranchingList](x: Branching[C0, TAIL]) {
    def i0 = x.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branching` containers. */
  implicit class Branching1[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, TAIL <: BranchingList](x: Branching[C0, Branching[C1, TAIL]]) {
    def i1 = x.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branching` containers. */
  implicit class Branching2[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, TAIL <: BranchingList](x: Branching[C0, Branching[C1, Branching[C2, TAIL]]]) {
    def i2 = x.tail.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branching` containers. */
  implicit class Branching3[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, TAIL <: BranchingList](x: Branching[C0, Branching[C1, Branching[C2, Branching[C3, TAIL]]]]) {
    def i3 = x.tail.tail.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branching` containers. */
  implicit class Branching4[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, TAIL <: BranchingList](x: Branching[C0, Branching[C1, Branching[C2, Branching[C3, Branching[C4, TAIL]]]]]) {
    def i4 = x.tail.tail.tail.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branching` containers. */
  implicit class Branching5[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, TAIL <: BranchingList](x: Branching[C0, Branching[C1, Branching[C2, Branching[C3, Branching[C4, Branching[C5, TAIL]]]]]]) {
    def i5 = x.tail.tail.tail.tail.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branching` containers. */
  implicit class Branching6[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation, TAIL <: BranchingList](x: Branching[C0, Branching[C1, Branching[C2, Branching[C3, Branching[C4, Branching[C5, Branching[C6, TAIL]]]]]]]) {
    def i6 = x.tail.tail.tail.tail.tail.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branching` containers. */
  implicit class Branching7[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation, C7 <: Container[C7] with Aggregation, TAIL <: BranchingList](x: Branching[C0, Branching[C1, Branching[C2, Branching[C3, Branching[C4, Branching[C5, Branching[C6, Branching[C7, TAIL]]]]]]]]) {
    def i7 = x.tail.tail.tail.tail.tail.tail.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branching` containers. */
  implicit class Branching8[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation, C7 <: Container[C7] with Aggregation, C8 <: Container[C8] with Aggregation, TAIL <: BranchingList](x: Branching[C0, Branching[C1, Branching[C2, Branching[C3, Branching[C4, Branching[C5, Branching[C6, Branching[C7, Branching[C8, TAIL]]]]]]]]]) {
    def i8 = x.tail.tail.tail.tail.tail.tail.tail.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branching` containers. */
  implicit class Branching9[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation, C7 <: Container[C7] with Aggregation, C8 <: Container[C8] with Aggregation, C9 <: Container[C9] with Aggregation, TAIL <: BranchingList](x: Branching[C0, Branching[C1, Branching[C2, Branching[C3, Branching[C4, Branching[C5, Branching[C6, Branching[C7, Branching[C8, Branching[C9, TAIL]]]]]]]]]]) {
    def i9 = x.tail.tail.tail.tail.tail.tail.tail.tail.tail.head
  }

  //////////////////////////////////////////////////////////////// type alias for Histogram

  /** Type alias for conventional histograms (filled). */
  type Histogrammed = Selected[Binned[Counted, Counted, Counted, Counted]]
  /** Type alias for conventional histograms (filling). */
  type Histogramming[DATUM] = Selecting[DATUM, Binning[DATUM, Counting, Counting, Counting, Counting]]
  /** Convenience function for creating a conventional histogram. */
  def Histogram[DATUM]
    (num: Int,
    low: Double,
    high: Double,
    quantity: UserFcn[DATUM, Double],
    selection: UserFcn[DATUM, Double] = unweighted[DATUM]) =
    Select(selection, Bin(num, low, high, quantity))

  //////////////////////////////////////////////////////////////// type alias for SparselyHistogram

  /** Type alias for sparsely binned histograms (filled). */
  type SparselyHistogrammed = Selected[SparselyBinned[Counted, Counted]]
  /** Type alias for sparsely binned histograms (filling). */
  type SparselyHistogramming[DATUM] = Selecting[DATUM, SparselyBinning[DATUM, Counting, Counting]]
  /** Convenience function for creating a sparsely binned histogram. */
  def SparselyHistogram[DATUM]
    (binWidth: Double,
    quantity: UserFcn[DATUM, Double],
    selection: UserFcn[DATUM, Double] = unweighted[DATUM],
    origin: Double = 0.0) =
    Select(selection, SparselyBin(binWidth, quantity, origin = origin))

  //////////////////////////////////////////////////////////////// methods for Histogram and SparselyHistogram

  implicit def anyBinnedToHistogramMethods[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Binned[Counted, U, O, N]): HistogramMethods =
    new HistogramMethods(new Selected(hist.entries, unweighted.name, new Binned(hist.low, hist.high, hist.entries, hist.quantityName, hist.values, new Counted(hist.underflow.entries), new Counted(hist.overflow.entries), new Counted(hist.nanflow.entries))))

  implicit def anyBinningToHistogramMethods[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Binning[DATUM, Counting, U, O, N]): HistogramMethods =
    anyBinnedToHistogramMethods(hist.ed)

  implicit def anySelectedBinnedToHistogramMethods[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Selected[Binned[Counted, U, O, N]]): HistogramMethods =
    new HistogramMethods(new Selected(hist.entries, hist.quantityName, anyBinnedToHistogramMethods(hist.cut).selected.cut))

  implicit def anySelectingBinningToHistogramMethods[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, Binning[DATUM, Counting, U, O, N]]): HistogramMethods =
    anySelectedBinnedToHistogramMethods(hist.ed)

  implicit def anySparselyBinnedToHistogramMethods[N <: Container[N] with NoAggregation](hist: SparselyBinned[Counted, N]): HistogramMethods =
    anySelectedSparselyBinnedToHistogramMethods(new Selected(hist.entries, unweighted.name, hist))
  
  implicit def anySparselyBinningToHistogramMethods[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: SparselyBinning[DATUM, Counting, N]): HistogramMethods =
    anySparselyBinnedToHistogramMethods(hist.ed)

  implicit def anySelectedSparselyBinnedToHistogramMethods[N <: Container[N] with NoAggregation](hist: Selected[SparselyBinned[Counted, N]]): HistogramMethods =
    if (hist.cut.numFilled > 0)
      new HistogramMethods(
        new Selected(hist.entries, hist.quantityName, new Binned(hist.cut.low.get, hist.cut.high.get, hist.cut.entries, hist.cut.quantityName, hist.cut.minBin.get to hist.cut.maxBin.get map {i => new Counted(hist.cut.at(i).flatMap(x => Some(x.entries)).getOrElse(0L))}, new Counted(0.0), new Counted(0.0), new Counted(hist.cut.nanflow.entries)))
      )
    else
      new HistogramMethods(
        new Selected(hist.entries, hist.quantityName, new Binned(hist.cut.origin, hist.cut.origin + 1.0, hist.cut.entries, hist.cut.quantityName, Seq(new Counted(0.0)), new Counted(0.0), new Counted(0.0), new Counted(hist.cut.nanflow.entries)))
      )

  implicit def anySelectingSparselyBinningToHistogramMethods[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, SparselyBinning[DATUM, Counting, N]]): HistogramMethods =
    anySelectedSparselyBinnedToHistogramMethods(hist.ed)

  /** Methods that are implicitly added to container combinations that look like histograms. */
  class HistogramMethods(val selected: Selected[Binned[Counted, Counted, Counted, Counted]]) {
    /** Access the [[org.dianahep.histogrammar.Binned]] object, rather than the [[org.dianahep.histogrammar.Selected]] wrapper. */
    def binned = selected.cut

    /** Bin values as numbers, rather than [[org.dianahep.histogrammar.Counted]]/[[org.dianahep.histogrammar.Counting]]. */
    def numericalValues: Seq[Double] = binned.values.map(_.entries)
    /** Overflow as a number, rather than [[org.dianahep.histogrammar.Counted]]/[[org.dianahep.histogrammar.Counting]]. */
    def numericalOverflow: Double = binned.overflow.entries
    /** Underflow as a number, rather than [[org.dianahep.histogrammar.Counted]]/[[org.dianahep.histogrammar.Counting]]. */
    def numericalUnderflow: Double = binned.underflow.entries
    /** Nanflow as a number, rather than [[org.dianahep.histogrammar.Counted]]/[[org.dianahep.histogrammar.Counting]]. */
    def numericalNanflow: Double = binned.nanflow.entries

    /** Low-central-high confidence interval width for all bins,
      * takes `z` as argument, where `z` is the "number of sigmas:"
        with `z = 1` is the 68% confidence level above the central value.
      * @param absz absolute value of `z` to evaluate.
      */    
    def confidenceIntervalValues(absz: Double = 1.0): Seq[Double] = { 
       numericalValues map {v => absz*Math.sqrt(v)}
    }
  }

  //////////////////////////////////////////////////////////////// type alias for Profile

  /** Type alias for binwise averages (filled). */
  type Profiled = Selected[Binned[Averaged, Counted, Counted, Counted]]
  /** Type alias for binwise averages (filling). */
  type Profiling[DATUM] = Selecting[DATUM, Binning[DATUM, Averaging[DATUM], Counting, Counting, Counting]]
  /** Convenience function for creating binwise averages. */
  def Profile[DATUM]
    (num: Int,
    low: Double,
    high: Double,
    binnedQuantity: UserFcn[DATUM, Double],
    averagedQuantity: UserFcn[DATUM, Double],
    selection: UserFcn[DATUM, Double] = unweighted[DATUM]) =
    Select(selection, Bin(num, low, high, binnedQuantity, Average(averagedQuantity)))

  //////////////////////////////////////////////////////////////// type alias for Profile

  /** Type alias for sparsely binned binwise averages. (filled). */
  type SparselyProfiled = Selected[SparselyBinned[Averaged, Counted]]
  /** Type alias for sparsely binned binwise averages (filling). */
  type SparselyProfiling[DATUM] = Selecting[DATUM, SparselyBinning[DATUM, Averaging[DATUM], Counting]]
  /** Convenience function for creating sparsely binned binwise averages. */
  def SparselyProfile[DATUM]
    (binWidth: Double,
    binnedQuantity: UserFcn[DATUM, Double],
    averagedQuantity: UserFcn[DATUM, Double],
    selection: UserFcn[DATUM, Double] = unweighted[DATUM],
    origin: Double = 0.0) =
    Select(selection, SparselyBin(binWidth, binnedQuantity, Average(averagedQuantity), origin = origin))

  //////////////////////////////////////////////////////////////// methods for Profile and SparselyProfile

  implicit def anyBinnedToProfileMethods[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Binned[Averaged, U, O, N]): ProfileMethods =
    new ProfileMethods(new Selected(hist.entries, unweighted.name, new Binned(hist.low, hist.high, hist.entries, hist.quantityName, hist.values.map(v => new Averaged(v.entries, v.quantityName, v.mean)), new Counted(hist.underflow.entries), new Counted(hist.overflow.entries), new Counted(hist.nanflow.entries))))

  implicit def anyBinningToProfileMethods[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Binning[DATUM, Averaging[DATUM], U, O, N]): ProfileMethods =
    anyBinnedToProfileMethods(hist.ed)

  implicit def anySelectedBinnedToProfileMethods[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Selected[Binned[Averaged, U, O, N]]): ProfileMethods =
    new ProfileMethods(new Selected(hist.entries, hist.quantityName, anyBinnedToProfileMethods(hist.cut).selected.cut))

  implicit def anySelectingBinningToProfileMethods[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, Binning[DATUM, Averaging[DATUM], U, O, N]]): ProfileMethods =
    anySelectedBinnedToProfileMethods(hist.ed)

  implicit def anySparselyBinnedToProfileMethods[N <: Container[N] with NoAggregation](hist: SparselyBinned[Averaged, N]): ProfileMethods =
    anySelectedSparselyBinnedToProfileMethods(new Selected(hist.entries, unweighted.name, hist))
  
  implicit def anySparselyBinningToProfileMethods[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: SparselyBinning[DATUM, Averaging[DATUM], N]): ProfileMethods =
    anySparselyBinnedToProfileMethods(hist.ed)

  implicit def anySelectedSparselyBinnedToProfileMethods[N <: Container[N] with NoAggregation](hist: Selected[SparselyBinned[Averaged, N]]): ProfileMethods =
    if (hist.cut.numFilled > 0)
      new ProfileMethods(
        new Selected(hist.entries, hist.quantityName, new Binned(hist.cut.low.get, hist.cut.high.get, hist.cut.entries, hist.cut.quantityName, hist.cut.minBin.get to hist.cut.maxBin.get map {i => hist.cut.at(i).flatMap(x => Some(new Averaged(x.entries, x.quantityName, x.mean))).getOrElse(new Averaged(0.0, None, 0.0))}, new Counted(0.0), new Counted(0.0), new Counted(hist.cut.nanflow.entries)))
      )
    else
      new ProfileMethods(
        new Selected(hist.entries, hist.quantityName, new Binned(hist.cut.origin, hist.cut.origin + 1.0, hist.cut.entries, hist.cut.quantityName, Seq(new Averaged(0.0, None, 0.0)), new Counted(0.0), new Counted(0.0), new Counted(hist.cut.nanflow.entries)))
      )

  implicit def anySelectingSparselyBinningToProfileMethods[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, SparselyBinning[DATUM, Averaging[DATUM], N]]): ProfileMethods =
    anySelectedSparselyBinnedToProfileMethods(hist.ed)

  /** Methods that are implicitly added to container combinations that look like a physicist's "profile plot." */
  class ProfileMethods(val selected: Selected[Binned[Averaged, Counted, Counted, Counted]]) {
    def binned = selected.cut

    /** Bin means as numbers, rather than [[org.dianahep.histogrammar.Averaged]]/[[org.dianahep.histogrammar.Averaging]]. */
    def meanValues: Seq[Double] = binned.values.map(_.mean)

    /** Overflow as a number, rather than [[org.dianahep.histogrammar.Counted]]/[[org.dianahep.histogrammar.Counting]]. */
    def numericalOverflow: Double = binned.overflow.entries
    /** Underflow as a number, rather than [[org.dianahep.histogrammar.Counted]]/[[org.dianahep.histogrammar.Counting]]. */
    def numericalUnderflow: Double = binned.underflow.entries
    /** Nanflow as a number, rather than [[org.dianahep.histogrammar.Counted]]/[[org.dianahep.histogrammar.Counting]]. */
    def numericalNanflow: Double = binned.nanflow.entries
  }

  //////////////////////////////////////////////////////////////// type alias for ProfileErr

  /** Type alias for a physicist's "profile plot," which is a Profile with variances (filled). */
  type ProfileErred = Selected[Binned[Deviated, Counted, Counted, Counted]]
  /** Type alias for a physicist's "profile plot," which is a Profile with variances (filling). */
  type ProfileErring[DATUM] = Selecting[DATUM, Binning[DATUM, Deviating[DATUM], Counting, Counting, Counting]]
  /** Convenience function for creating a physicist's "profile plot," which is a Profile with variances. */
  def ProfileErr[DATUM]
    (num: Int,
    low: Double,
    high: Double,
    binnedQuantity: UserFcn[DATUM, Double],
    averagedQuantity: UserFcn[DATUM, Double],
    selection: UserFcn[DATUM, Double] = unweighted[DATUM]) =
    Select(selection, Bin(num, low, high, binnedQuantity, Deviate(averagedQuantity)))

  //////////////////////////////////////////////////////////////// type alias for SparselyProfileErr

  /** Type alias for a physicist's sparsely binned "profile plot," which is a Profile with variances (filled). */
  type SparselyProfileErred = Selected[SparselyBinned[Deviated, Counted]]
  /** Type alias for a physicist's sparsely binned "profile plot," which is a Profile with variances (filling). */
  type SparselyProfileErring[DATUM] = Selecting[DATUM, SparselyBinning[DATUM, Deviating[DATUM], Counting]]
  /** Convenience function for creating a physicist's sparsely binned "profile plot," which is a Profile with variances. */
  def SparselyProfileErr[DATUM]
    (binWidth: Double,
    binnedQuantity: UserFcn[DATUM, Double],
    averagedQuantity: UserFcn[DATUM, Double],
    selection: UserFcn[DATUM, Double] = unweighted[DATUM],
    origin: Double = 0.0) =
    Select(selection, SparselyBin(binWidth, binnedQuantity, Deviate(averagedQuantity), origin = origin))

  //////////////////////////////////////////////////////////////// methods for ProfileErr and SparselyProfileErr
  
  implicit def anyBinnedToProfileErrMethods[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Binned[Deviated, U, O, N]): ProfileErrMethods =
    new ProfileErrMethods(new Selected(hist.entries, unweighted.name, new Binned(hist.low, hist.high, hist.entries, hist.quantityName, hist.values.map(v => new Deviated(v.entries, v.quantityName, v.mean, v.variance)), new Counted(hist.underflow.entries), new Counted(hist.overflow.entries), new Counted(hist.nanflow.entries))))

  implicit def anyBinningToProfileErrMethods[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Binning[DATUM, Deviating[DATUM], U, O, N]): ProfileErrMethods =
    anyBinnedToProfileErrMethods(hist.ed)

  implicit def anySelectedBinnedToProfileErrMethods[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Selected[Binned[Deviated, U, O, N]]): ProfileErrMethods =
    new ProfileErrMethods(new Selected(hist.entries, hist.quantityName, anyBinnedToProfileErrMethods(hist.cut).selected.cut))

  implicit def anySelectingBinningToProfileErrMethods[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, Binning[DATUM, Deviating[DATUM], U, O, N]]): ProfileErrMethods =
    anySelectedBinnedToProfileErrMethods(hist.ed)

  implicit def anySparselyBinnedToProfileErrMethods[N <: Container[N] with NoAggregation](hist: SparselyBinned[Deviated, N]): ProfileErrMethods =
    anySelectedSparselyBinnedToProfileErrMethods(new Selected(hist.entries, unweighted.name, hist))
  
  implicit def anySparselyBinningToProfileErrMethods[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: SparselyBinning[DATUM, Deviating[DATUM], N]): ProfileErrMethods =
    anySparselyBinnedToProfileErrMethods(hist.ed)

  implicit def anySelectedSparselyBinnedToProfileErrMethods[N <: Container[N] with NoAggregation](hist: Selected[SparselyBinned[Deviated, N]]): ProfileErrMethods =
    if (hist.cut.numFilled > 0)
      new ProfileErrMethods(
        new Selected(hist.entries, hist.quantityName, new Binned(hist.cut.low.get, hist.cut.high.get, hist.cut.entries, hist.cut.quantityName, hist.cut.minBin.get to hist.cut.maxBin.get map {i => hist.cut.at(i).flatMap(x => Some(new Deviated(x.entries, x.quantityName, x.mean, x.variance))).getOrElse(new Deviated(0.0, None, 0.0, 0.0))}, new Counted(0.0), new Counted(0.0), new Counted(hist.cut.nanflow.entries)))
      )
    else
      new ProfileErrMethods(
        new Selected(hist.entries, hist.quantityName, new Binned(hist.cut.origin, hist.cut.origin + 1.0, hist.cut.entries, hist.cut.quantityName, Seq(new Deviated(0.0, None, 0.0, 0.0)), new Counted(0.0), new Counted(0.0), new Counted(hist.cut.nanflow.entries)))
      )

  implicit def anySelectingSparselyBinningToProfileErrMethods[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, SparselyBinning[DATUM, Deviating[DATUM], N]]): ProfileErrMethods =
    anySelectedSparselyBinnedToProfileErrMethods(hist.ed)

  /** Methods that are implicitly added to container combinations that look like a physicist's "profile plot." */
  class ProfileErrMethods(val selected: Selected[Binned[Deviated, Counted, Counted, Counted]]) {
    def binned = selected.cut

    /** Bin means as numbers, rather than [[org.dianahep.histogrammar.Deviated]]/[[org.dianahep.histogrammar.Deviating]]. */
    def meanValues: Seq[Double] = binned.values.map(_.mean)
    /** Bin variances as numbers, rather than [[org.dianahep.histogrammar.Deviated]]/[[org.dianahep.histogrammar.Deviating]]. */
    def varianceValues: Seq[Double] = binned.values.map(_.variance)

    /** Overflow as a number, rather than [[org.dianahep.histogrammar.Counted]]/[[org.dianahep.histogrammar.Counting]]. */
    def numericalOverflow: Double = binned.overflow.entries
    /** Underflow as a number, rather than [[org.dianahep.histogrammar.Counted]]/[[org.dianahep.histogrammar.Counting]]. */
    def numericalUnderflow: Double = binned.underflow.entries
    /** Nanflow as a number, rather than [[org.dianahep.histogrammar.Counted]]/[[org.dianahep.histogrammar.Counting]]. */
    def numericalNanflow: Double = binned.nanflow.entries
  }

  //////////////////////////////////////////////////////////////// methods for StackedHistogram, including cases for mixed tenses

  implicit def anyBinnedToStackedHistogramMethods[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: Stacked[Binned[Counted, U, O, N], SN]): StackedHistogramMethods =
    new StackedHistogramMethods(new Stacked(hist.entries, hist.quantityName, hist.bins.map({case (x, v) => (x, anyBinnedToHistogramMethods(v).selected)}), new Counted(hist.nanflow.entries)))

  implicit def anyBinningToStackedHistogramMethods[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: Stacking[DATUM, Binning[DATUM, Counting, U, O, N], SN]): StackedHistogramMethods =
    anyBinnedToStackedHistogramMethods(hist.ed)

  implicit def anySelectedBinnedToStackedHistogramMethods[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: Stacked[Selected[Binned[Counted, U, O, N]], SN]): StackedHistogramMethods =
    new StackedHistogramMethods(new Stacked(hist.entries, hist.quantityName, hist.bins.map({case (x, v) => (x, anySelectedBinnedToHistogramMethods(v).selected)}), new Counted(hist.nanflow.entries)))

  implicit def anySelectingBinningToStackedHistogramMethods[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: Stacking[DATUM, Selecting[DATUM, Binning[DATUM, Counting, U, O, N]], SN]): StackedHistogramMethods =
    anySelectedBinnedToStackedHistogramMethods(hist.ed)

  implicit def anySparselyBinnedToStackedHistogramMethods[N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: Stacked[SparselyBinned[Counted, N], SN]): StackedHistogramMethods =
    new StackedHistogramMethods(new Stacked(hist.entries, hist.quantityName, hist.bins.map({case (x, v) => (x, anySparselyBinnedToHistogramMethods(v).selected)}), new Counted(hist.nanflow.entries)))
  
  implicit def anySparselyBinningToStackedHistogramMethods[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: Stacking[DATUM, SparselyBinning[DATUM, Counting, N], SN]): StackedHistogramMethods =
    anySparselyBinnedToStackedHistogramMethods(hist.ed)

  implicit def anySelectedSparselyBinnedToStackedHistogramMethods[N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: Stacked[Selected[SparselyBinned[Counted, N]], SN]): StackedHistogramMethods =
    new StackedHistogramMethods(new Stacked(hist.entries, hist.quantityName, hist.bins.map({case (x, v) => (x, anySelectedSparselyBinnedToHistogramMethods(v).selected)}), new Counted(hist.nanflow.entries)))

  implicit def anySelectingSparselyBinningToStackedHistogramMethods[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: Stacking[DATUM, Selecting[DATUM, SparselyBinning[DATUM, Counting, N]], SN]): StackedHistogramMethods =
    anySelectedSparselyBinnedToStackedHistogramMethods(hist.ed)

  implicit def anyBinnedMixedToStackedHistogramMethods[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with NoAggregation](hist: Stacked[Binning[DATUM, Counting, U, O, N], SN]): StackedHistogramMethods =
    new StackedHistogramMethods(new Stacked(hist.entries, hist.quantityName, hist.bins.map({case (x, v) => (x, new Selected(v.entries, unweighted.name, new Binned(v.low, v.high, v.entries, v.quantity.name, v.values map {vi: Counting => new Counted(vi.entries)}, new Counted(v.underflow.entries), new Counted(v.overflow.entries), new Counted(v.nanflow.entries))))}), new Counted(hist.nanflow.entries)))

  implicit def anySelectedBinnedMixedToStackedHistogramMethods[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with NoAggregation](hist: Stacked[Selecting[DATUM, Binning[DATUM, Counting, U, O, N]], SN]): StackedHistogramMethods =
    new StackedHistogramMethods(new Stacked(hist.entries, hist.quantityName, hist.bins.map({case (x, v) => (x, new Selected(v.entries, unweighted.name, new Binned(v.cut.low, v.cut.high, v.cut.entries, v.cut.quantity.name, v.cut.values map {vi: Counting => new Counted(vi.entries)}, new Counted(v.cut.underflow.entries), new Counted(v.cut.overflow.entries), new Counted(v.cut.nanflow.entries))))}), new Counted(hist.nanflow.entries)))

  implicit def anySparselyBinnedMixedToStackedHistogramMethods[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with NoAggregation](hist: Stacked[SparselyBinning[DATUM, Counting, N], SN]): StackedHistogramMethods =
    new StackedHistogramMethods(new Stacked(hist.entries, hist.quantityName, hist.bins.map({case (x, v) => (x, anySparselyBinnedToHistogramMethods(new SparselyBinned(v.binWidth, v.entries, v.quantity.name, "", SortedMap(v.bins.toSeq.map({case (i, vi: Counting) => (i, new Counted(vi.entries))}): _*), new Counted(v.nanflow.entries), v.origin)).selected)}), new Counted(hist.nanflow.entries)))

  implicit def anySelectedSparselyBinnedMixedToStackedHistogramMethods[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with NoAggregation](hist: Stacked[Selecting[DATUM, SparselyBinning[DATUM, Counting, N]], SN]): StackedHistogramMethods =
    new StackedHistogramMethods(new Stacked(hist.entries, hist.quantityName, hist.bins.map({case (x, v) => (x, anySelectedSparselyBinnedToHistogramMethods(new Selected(v.entries, v.quantity.name, new SparselyBinned(v.cut.binWidth, v.cut.entries, v.cut.quantity.name, "", SortedMap(v.cut.bins.toSeq.map({case (i, vi: Counting) => (i, new Counted(vi.entries))}): _*), new Counted(v.cut.nanflow.entries), v.cut.origin))).selected)}), new Counted(hist.nanflow.entries)))

  /** Methods that are implicitly added to container combinations that look like stacked histograms. */
  class StackedHistogramMethods(val stacked: Stacked[Selected[Binned[Counted, Counted, Counted, Counted]], Counted]) {
    def low = stacked.bins.head._2.cut.low
    def high = stacked.bins.head._2.cut.high
    def num = stacked.bins.head._2.cut.num

    stacked.values foreach {selected =>
      if (selected.cut.low != low)
        throw new ContainerException(s"Stack invalid because low values differ (${low} vs ${selected.cut.low})")
    }
    stacked.values foreach {selected =>
      if (selected.cut.high != high)
        throw new ContainerException(s"Stack invalid because high values differ (${high} vs ${selected.cut.high})")
    }
    stacked.values foreach {selected =>
      if (selected.cut.num != num)
        throw new ContainerException(s"Stack invalid because num values differ (${num} vs ${selected.cut.num})")
    }
  }

  //////////////////////////////////////////////////////////////// methods for PartitionedHistogram

  implicit def anyBinnedToPartitionedHistogramMethods[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: IrregularlyBinned[Binned[Counted, U, O, N], SN]): PartitionedHistogramMethods =
    new PartitionedHistogramMethods(new IrregularlyBinned(hist.entries, hist.quantityName, hist.bins.map({case (x, v) => (x, anyBinnedToHistogramMethods(v).selected)}), new Counted(hist.nanflow.entries)))

  implicit def anyBinningToPartitionedHistogramMethods[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: IrregularlyBinning[DATUM, Binning[DATUM, Counting, U, O, N], SN]): PartitionedHistogramMethods =
    anyBinnedToPartitionedHistogramMethods(hist.ed)

  implicit def anySelectedBinnedToPartitionedHistogramMethods[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: IrregularlyBinned[Selected[Binned[Counted, U, O, N]], SN]): PartitionedHistogramMethods =
    new PartitionedHistogramMethods(new IrregularlyBinned(hist.entries, hist.quantityName, hist.bins.map({case (x, v) => (x, anySelectedBinnedToHistogramMethods(v).selected)}), new Counted(hist.nanflow.entries)))

  implicit def anySelectingBinningToPartitionedHistogramMethods[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: IrregularlyBinning[DATUM, Selecting[DATUM, Binning[DATUM, Counting, U, O, N]], SN]): PartitionedHistogramMethods =
    anySelectedBinnedToPartitionedHistogramMethods(hist.ed)

  implicit def anySparselyBinnedToPartitionedHistogramMethods[N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: IrregularlyBinned[SparselyBinned[Counted, N], SN]): PartitionedHistogramMethods =
    new PartitionedHistogramMethods(new IrregularlyBinned(hist.entries, hist.quantityName, hist.bins.map({case (x, v) => (x, anySparselyBinnedToHistogramMethods(v).selected)}), new Counted(hist.nanflow.entries)))
  
  implicit def anySparselyBinningToPartitionedHistogramMethods[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: IrregularlyBinning[DATUM, SparselyBinning[DATUM, Counting, N], SN]): PartitionedHistogramMethods =
    anySparselyBinnedToPartitionedHistogramMethods(hist.ed)

  implicit def anySelectedSparselyBinnedToPartitionedHistogramMethods[N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: IrregularlyBinned[Selected[SparselyBinned[Counted, N]], SN]): PartitionedHistogramMethods =
    new PartitionedHistogramMethods(new IrregularlyBinned(hist.entries, hist.quantityName, hist.bins.map({case (x, v) => (x, anySelectedSparselyBinnedToHistogramMethods(v).selected)}), new Counted(hist.nanflow.entries)))

  implicit def anySelectingSparselyBinningToPartitionedHistogramMethods[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: IrregularlyBinning[DATUM, Selecting[DATUM, SparselyBinning[DATUM, Counting, N]], SN]): PartitionedHistogramMethods =
    anySelectedSparselyBinnedToPartitionedHistogramMethods(hist.ed)

  /** Methods that are implicitly added to container combinations that look like partitioned histograms. */
  class PartitionedHistogramMethods(val partitioned: IrregularlyBinned[Selected[Binned[Counted, Counted, Counted, Counted]], Counted]) {
    def low = partitioned.bins.head._2.cut.low
    def high = partitioned.bins.head._2.cut.high
    def num = partitioned.bins.head._2.cut.num

    partitioned.values foreach {selected =>
      if (selected.cut.low != low)
        throw new ContainerException(s"Partition invalid because low values differ (${low} vs ${selected.cut.low})")
    }
    partitioned.values foreach {selected =>
      if (selected.cut.high != high)
        throw new ContainerException(s"Partition invalid because high values differ (${high} vs ${selected.cut.high})")
    }
    partitioned.values foreach {selected =>
      if (selected.cut.num != num)
        throw new ContainerException(s"Partition invalid because num values differ (${num} vs ${selected.cut.num})")
    }
  }

  //////////////////////////////////////////////////////////////// methods for FractionedHistogram

  implicit def anyBinnedToFractionedHistogramMethods[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Fractioned[Binned[Counted, U, O, N]]): FractionedHistogramMethods =
    new FractionedHistogramMethods(new Fractioned(hist.entries, hist.quantityName, anyBinnedToHistogramMethods(hist.numerator).selected, anyBinnedToHistogramMethods(hist.denominator).selected))

  implicit def anyBinningToFractionedHistogramMethods[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Fractioning[DATUM, Binning[DATUM, Counting, U, O, N]]): FractionedHistogramMethods =
    anyBinnedToFractionedHistogramMethods(hist.ed)

  implicit def anySelectedBinnedToFractionedHistogramMethods[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Fractioned[Selected[Binned[Counted, U, O, N]]]): FractionedHistogramMethods =
    new FractionedHistogramMethods(new Fractioned(hist.entries, hist.quantityName, anySelectedBinnedToHistogramMethods(hist.numerator).selected, anySelectedBinnedToHistogramMethods(hist.denominator).selected))

  implicit def anySelectingBinningToFractionedHistogramMethods[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Fractioning[DATUM, Selecting[DATUM, Binning[DATUM, Counting, U, O, N]]]): FractionedHistogramMethods =
    anySelectedBinnedToFractionedHistogramMethods(hist.ed)

  implicit def anySparselyBinnedToFractionedHistogramMethods[N <: Container[N] with NoAggregation](hist: Fractioned[SparselyBinned[Counted, N]]): FractionedHistogramMethods =
    new FractionedHistogramMethods(new Fractioned(hist.entries, hist.quantityName, anySparselyBinnedToHistogramMethods(hist.numerator).selected, anySparselyBinnedToHistogramMethods(hist.denominator).selected))
  
  implicit def anySparselyBinningToFractionedHistogramMethods[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Fractioning[DATUM, SparselyBinning[DATUM, Counting, N]]): FractionedHistogramMethods =
    anySparselyBinnedToFractionedHistogramMethods(hist.ed)

  implicit def anySelectedSparselyBinnedToFractionedHistogramMethods[N <: Container[N] with NoAggregation](hist: Fractioned[Selected[SparselyBinned[Counted, N]]]): FractionedHistogramMethods =
    new FractionedHistogramMethods(new Fractioned(hist.entries, hist.quantityName, anySelectedSparselyBinnedToHistogramMethods(hist.numerator).selected, anySelectedSparselyBinnedToHistogramMethods(hist.denominator).selected))

  implicit def anySelectingSparselyBinningToFractionedHistogramMethods[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Fractioning[DATUM, Selecting[DATUM, SparselyBinning[DATUM, Counting, N]]]): FractionedHistogramMethods =
    anySelectedSparselyBinnedToFractionedHistogramMethods(hist.ed)

  /** Methods that are implicitly added to container combinations that look like fractioned histograms. */
  class FractionedHistogramMethods(val fractioned: Fractioned[Selected[Binned[Counted, Counted, Counted, Counted]]]) {
    def numeratorBinned = fractioned.numerator.cut
    def denominatorBinned = fractioned.denominator.cut

    if (numeratorBinned.low != denominatorBinned.low)
      throw new ContainerException(s"Fraction invalid because low differs between numerator and denominator (${numeratorBinned.low} vs ${denominatorBinned.low})")
    if (numeratorBinned.high != denominatorBinned.high)
      throw new ContainerException(s"Fraction invalid because high differs between numerator and denominator (${numeratorBinned.high} vs ${denominatorBinned.high})")
    if (numeratorBinned.values.size != denominatorBinned.values.size)
      throw new ContainerException(s"Fraction invalid because number of values differs between numerator and denominator (${numeratorBinned.values.size} vs ${denominatorBinned.values.size})")

    def low = numeratorBinned.low
    def high = numeratorBinned.high
    def num = numeratorBinned.num

    /** Bin fractions as numbers. */
    def numericalValues: Seq[Double] = numeratorBinned.values zip denominatorBinned.values map {case (n, d) => n.entries / d.entries}
    /** Low-central-high confidence interval triplet for all bins, given a confidence interval function.
      * 
      * @param confidenceInterval confidence interval function, which takes (numerator entries, denominator entries, `z`) as arguments, where `z` is the "number of sigmas:" `z = 0` is the central value, `z = -1` is the 68% confidence level below the central value, and `z = 1` is the 68% confidence level above the central value.
      * @param absz absolute value of `z` to evaluate.
      * @return confidence interval evaluated at `(-absz, 0, absz)`.
      */
    def confidenceIntervalValues(confidenceInterval: (Double, Double, Double) => Double, absz: Double = 1.0): Seq[(Double, Double, Double)] = numeratorBinned.values zip denominatorBinned.values map {case (n, d) =>
      (confidenceInterval(n.entries, d.entries, -absz), confidenceInterval(n.entries, d.entries, 0.0), confidenceInterval(n.entries, d.entries, absz))
    }

    /** Overflow fraction as a number. */
    def numericalOverflow: Double = numeratorBinned.overflow.entries / denominatorBinned.overflow.entries
    /** Low-central-high confidence interval triplet for the overflow bin, given a confidence interval function.
      * 
      * @param confidenceInterval confidence interval function, which takes (numerator entries, denominator entries, `z`) as arguments, where `z` is the "number of sigmas:" `z = 0` is the central value, `z = -1` is the 68% confidence level below the central value, and `z = 1` is the 68% confidence level above the central value.
      * @param absz absolute value of `z` to evaluate.
      * @return confidence interval evaluated at `(-absz, 0, absz)`.
      */
    def confidenceIntervalOverflow(confidenceInterval: (Double, Double, Double) => Double, absz: Double = 1.0): (Double, Double, Double) = {
      val (n, d) = (numeratorBinned.overflow, denominatorBinned.overflow)
      (confidenceInterval(n.entries, d.entries, -absz), confidenceInterval(n.entries, d.entries, 0.0), confidenceInterval(n.entries, d.entries, absz))
    }

    /** Underflow fraction as a number. */
    def numericalUnderflow: Double = numeratorBinned.underflow.entries / denominatorBinned.underflow.entries
    /** Low-central-high confidence interval triplet for the overflow bin, given a confidence interval function.
      * 
      * @param confidenceInterval confidence interval function, which takes (numerator entries, denominator entries, `z`) as arguments, where `z` is the "number of sigmas:" `z = 0` is the central value, `z = -1` is the 68% confidence level below the central value, and `z = 1` is the 68% confidence level above the central value.
      * @param absz absolute value of `z` to evaluate.
      * @return confidence interval evaluated at `(-absz, 0, absz)`.
      */
    def confidenceIntervalUnderflow(confidenceInterval: (Double, Double, Double) => Double, absz: Double = 1.0): (Double, Double, Double) = {
      val (n, d) = (numeratorBinned.underflow, denominatorBinned.underflow)
      (confidenceInterval(n.entries, d.entries, -absz), confidenceInterval(n.entries, d.entries, 0.0), confidenceInterval(n.entries, d.entries, absz))
    }

    /** Nanflow fraction as a number. */
    def numericalNanflow: Double = numeratorBinned.nanflow.entries / denominatorBinned.nanflow.entries
    /** Low-central-high confidence interval triplet for the nanflow bin, given a confidence interval function.
      * 
      * @param confidenceInterval confidence interval function, which takes (numerator entries, denominator entries, `z`) as arguments, where `z` is the "number of sigmas:" `z = 0` is the central value, `z = -1` is the 68% confidence level below the central value, and `z = 1` is the 68% confidence level above the central value.
      * @param absz absolute value of `z` to evaluate.
      * @return confidence interval evaluated at `(-absz, 0, absz)`.
      */
    def confidenceIntervalNanflow(confidenceInterval: (Double, Double, Double) => Double, absz: Double = 1.0): (Double, Double, Double) = {
      val (n, d) = (numeratorBinned.nanflow, denominatorBinned.nanflow)
      (confidenceInterval(n.entries, d.entries, -absz), confidenceInterval(n.entries, d.entries, 0.0), confidenceInterval(n.entries, d.entries, absz))
    }
  }

  //////////////////////////////////////////////////////////////// type alias for TwoDimensionallyHistogram

  /** Type alias for conventional, two-dimensional histograms (filled). */
  type TwoDimensionallyHistogrammed = Selected[Binned[Binned[Counted, Counted, Counted, Counted], Counted, Counted, Counted]]
  /** Type alias for conventional, two-dimensional histograms (filling). */
  type TwoDimensionallyHistogramming[DATUM] = Selecting[DATUM, Binning[DATUM, Binning[DATUM, Counting, Counting, Counting, Counting], Counting, Counting, Counting]]
  /** Convenience function for creating a conventional, two-dimensional histogram. */
  def TwoDimensionallyHistogram[DATUM]
    (xnum: Int,
     xlow: Double,
     xhigh: Double,
     xquantity: UserFcn[DATUM, Double],
     ynum: Int,
     ylow: Double,
     yhigh: Double,
     yquantity: UserFcn[DATUM, Double],
     selection: UserFcn[DATUM, Double] = unweighted[DATUM]) =
    Select(selection, Bin(xnum, xlow, xhigh, xquantity, Bin(ynum, ylow, yhigh, yquantity)))

  //////////////////////////////////////////////////////////////// type alias for TwoDimensionallySparselyHistogram

  /** Type alias for sparsely binned, two-dimensional histograms (filled). */
  type TwoDimensionallySparselyHistogrammed = Selected[SparselyBinned[Counted, Counted]]
  /** Type alias for sparsely binned, two-dimensional histograms (filling). */
  type TwoDimensionallySparselyHistogramming[DATUM] = Selecting[DATUM, SparselyBinning[DATUM, Counting, Counting]]
  /** Convenience function for creating a sparsely binned, two-dimensional histogram. */
  def TwoDimensionallySparselyHistogram[DATUM]
    (xbinWidth: Double,
     xquantity: UserFcn[DATUM, Double],
     ybinWidth: Double,
     yquantity: UserFcn[DATUM, Double],
     selection: UserFcn[DATUM, Double] = unweighted[DATUM],
     xorigin: Double = 0.0,
     yorigin: Double = 0.0) =
    Select(selection, SparselyBin(xbinWidth, xquantity, SparselyBin(ybinWidth, yquantity, origin = yorigin), origin = xorigin))

  //////////////////////////////////////////////////////////////// methods for TwoDimensionallyHistogram and TwoDimensionallySparselyHistogram

  implicit def binnedToTwoDimensionallyHistogramMethods[UX <: Container[UX] with NoAggregation, OX <: Container[OX] with NoAggregation, NX <: Container[NX] with NoAggregation, UY <: Container[UY] with NoAggregation, OY <: Container[OY] with NoAggregation, NY <: Container[NY] with NoAggregation](hist: Binned[Binned[Counted, UY, OY, NY], UX, OX, NX]): TwoDimensionallyHistogramMethods =
    new TwoDimensionallyHistogramMethods(
      new Selected(hist.entries, unweighted.name,
        new Binned(hist.low, hist.high, hist.entries, hist.quantityName, hist.values map {v =>
          new Binned(v.low, v.high, v.entries, v.quantityName, v.values, new Counted(v.underflow.entries), new Counted(v.overflow.entries), new Counted(v.nanflow.entries))},
          new Counted(hist.underflow.entries), new Counted(hist.overflow.entries), new Counted(hist.nanflow.entries))))

  implicit def binningToTwoDimensionallyHistogramMethods[DATUM, UX <: Container[UX] with Aggregation{type Datum >: DATUM}, OX <: Container[OX] with Aggregation{type Datum >: DATUM}, NX <: Container[NX] with Aggregation{type Datum >: DATUM}, UY <: Container[UY] with Aggregation{type Datum >: DATUM}, OY <: Container[OY] with Aggregation{type Datum >: DATUM}, NY <: Container[NY] with Aggregation{type Datum >: DATUM}](hist: Binning[DATUM, Binning[DATUM, Counting, UY, OY, NY], UX, OX, NX]): TwoDimensionallyHistogramMethods =
    binnedToTwoDimensionallyHistogramMethods(hist.ed)

  implicit def selectedBinnedToTwoDimensionallyHistogramMethods[UX <: Container[UX] with NoAggregation, OX <: Container[OX] with NoAggregation, NX <: Container[NX] with NoAggregation, UY <: Container[UY] with NoAggregation, OY <: Container[OY] with NoAggregation, NY <: Container[NY] with NoAggregation](hist: Selected[Binned[Binned[Counted, UY, OY, NY], UX, OX, NX]]): TwoDimensionallyHistogramMethods =
    new TwoDimensionallyHistogramMethods(
      new Selected(hist.entries, hist.quantityName,
        new Binned(hist.cut.low, hist.cut.high, hist.cut.entries, hist.cut.quantityName, hist.cut.values map {v =>
          new Binned(v.low, v.high, v.entries, v.quantityName, v.values, new Counted(v.underflow.entries), new Counted(v.overflow.entries), new Counted(v.nanflow.entries))},
          new Counted(hist.cut.underflow.entries), new Counted(hist.cut.overflow.entries), new Counted(hist.cut.nanflow.entries))))

  implicit def selectingBinningToTwoDimensionallyHistogramMethods[DATUM, UX <: Container[UX] with Aggregation{type Datum >: DATUM}, OX <: Container[OX] with Aggregation{type Datum >: DATUM}, NX <: Container[NX] with Aggregation{type Datum >: DATUM}, UY <: Container[UY] with Aggregation{type Datum >: DATUM}, OY <: Container[OY] with Aggregation{type Datum >: DATUM}, NY <: Container[NY] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, Binning[DATUM, Binning[DATUM, Counting, UY, OY, NY], UX, OX, NX]]): TwoDimensionallyHistogramMethods =
    selectedBinnedToTwoDimensionallyHistogramMethods(hist.ed)

  implicit def sparselyBinnedToTwoDimensionallyHistogramMethods[NX <: Container[NX] with NoAggregation, NY <: Container[NY] with NoAggregation](hist: SparselyBinned[SparselyBinned[Counted, NY], NX]): TwoDimensionallyHistogramMethods =
    selectedSparselyBinnedToTwoDimensionallyHistogramMethods(new Selected(hist.entries, unweighted.name, hist))

  implicit def sparselyBinningToTwoDimensionallyHistogramMethods[DATUM, NX <: Container[NX] with Aggregation{type Datum >: DATUM}, NY <: Container[NY] with Aggregation{type Datum >: DATUM}](hist: SparselyBinning[DATUM, SparselyBinning[DATUM, Counting, NY], NX]): TwoDimensionallyHistogramMethods =
    sparselyBinnedToTwoDimensionallyHistogramMethods(hist.ed)

  implicit def selectedSparselyBinnedToTwoDimensionallyHistogramMethods[NX <: Container[NX] with NoAggregation, NY <: Container[NY] with NoAggregation](hist: Selected[SparselyBinned[SparselyBinned[Counted, NY], NX]]): TwoDimensionallyHistogramMethods = {
    val (xlow, xhigh, xminBin, xmaxBin) = (hist.cut.low, hist.cut.high, hist.cut.minBin, hist.cut.maxBin) match {
      case (Some(low), Some(high), Some(minBin), Some(maxBin)) =>
        (low, high, minBin, maxBin)
      case _ => 
        (hist.cut.origin, hist.cut.origin + 1.0, 0L, 1L)
    }
    val sample = hist.cut.values.head
    val (ylow, yhigh, yminBin, ymaxBin) = (hist.cut.values.flatMap(_.minBin), hist.cut.values.flatMap(_.maxBin)) match {
      case (mins, maxes) if (!mins.isEmpty  &&  !maxes.isEmpty) =>
        val minBin = mins.min
        val maxBin = maxes.max
        val low = minBin * sample.binWidth + sample.origin
        val high = (maxBin + 1L) * sample.binWidth + sample.origin
        (low, high, minBin, maxBin)
      case _ =>
        (sample.origin, sample.origin + 1.0, 0L, 1L)
    }
    new TwoDimensionallyHistogramMethods(
      new Selected(hist.entries, hist.quantityName,
        new Binned(xlow, xhigh, hist.cut.entries, hist.cut.quantityName, xminBin to xmaxBin map {i => hist.cut.at(i) match {
            case Some(sparse) => new Binned(ylow, yhigh, sparse.entries, sample.quantityName, yminBin to ymaxBin map {j => new Counted(sparse.at(j).flatMap(x => Some(x.entries)).getOrElse(0L))}, new Counted(0.0), new Counted(0.0), new Counted(sparse.nanflow.entries))
            case None => new Binned(ylow, yhigh, 0.0, sample.quantityName, yminBin to ymaxBin map {j => new Counted(0.0)}, new Counted(0.0), new Counted(0.0), new Counted(0.0))
          }}, new Counted(0.0), new Counted(0.0), new Counted(hist.cut.nanflow.entries))))
  }

  implicit def selectingSparselyBinningToTwoDimensionallyHistogramMethods[DATUM, NX <: Container[NX] with Aggregation{type Datum >: DATUM}, NY <: Container[NY] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, SparselyBinning[DATUM, SparselyBinning[DATUM, Counting, NY], NX]]): TwoDimensionallyHistogramMethods =
    selectedSparselyBinnedToTwoDimensionallyHistogramMethods(hist.ed)

  /** Methods that are implicitly added to container combinations that look like two-dimensional histograms. */
  class TwoDimensionallyHistogramMethods(val selected: Selected[Binned[Binned[Counted, Counted, Counted, Counted], Counted, Counted, Counted]]) {
    /** Bin values as numbers, rather than [[org.dianahep.histogrammar.Counted]]/[[org.dianahep.histogrammar.Counting]]. */
    def numericalValues: Seq[Seq[Double]] = selected.cut.values map {ybinned => ybinned.values.map(_.entries)}

    /** Number of entries that missed the two-dimensional region in any direction. */
    def numericalOverflow: Double = selected.cut.underflow.entries + selected.cut.overflow.entries + selected.cut.values.map(_.underflow.entries).sum + selected.cut.values.map(_.overflow.entries).sum

    /** Number of entries that yielded NaN in either the xquantity or the yquantity. */
    def numericalNanflow: Double = selected.cut.nanflow.entries + selected.cut.values.map(_.nanflow.entries).sum
  }

  //////////////////////////////////////////////////////////////// methods for (nested) collections

  implicit def labeledToCollectionMethods(labeled: Labeled[_]) = new CollectionMethods(labeled)
  implicit def labelingToCollectionMethods(labeling: Labeling[_]) = new CollectionMethods(labeling)
  implicit def untypedLabeledToCollectionMethods(untypedLabeled: UntypedLabeled) = new CollectionMethods(untypedLabeled)
  implicit def untypedLabelingToCollectionMethods(untypedLabeling: UntypedLabeling[_]) = new CollectionMethods(untypedLabeling)
  implicit def indexedToCollectionMethods(indexed: Indexed[_]) = new CollectionMethods(indexed)
  implicit def indexingToCollectionMethods(indexing: Indexing[_]) = new CollectionMethods(indexing)
  implicit def branchedToCollectionMethods(branched: Branched[_, _]) = new CollectionMethods(branched)
  implicit def branchingToCollectionMethods(branching: Branching[_, _]) = new CollectionMethods(branching)

  /** Methods that are implicitly added to container combinations that look like (nested) collections. */
  class CollectionMethods(collection: Collection)

}
