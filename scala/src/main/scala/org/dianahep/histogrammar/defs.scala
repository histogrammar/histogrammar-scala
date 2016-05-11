// Copyright 2016 Jim Pivarski
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
    def fromJsonFragment(json: Json): Container[_]
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
    register(AbsoluteErr)
    register(Minimize)
    register(Maximize)
    register(Quantile)

    register(Bin)
    register(SparselyBin)
    register(CentrallyBin)
    register(AdaptivelyBin)
    register(Categorize)

    register(Fraction)
    register(Stack)
    register(Partition)

    register(Limit)
    register(Label)
    register(UntypedLabel)
    register(Index)
    register(Branch)

    register(Bag)
    register(Sample)

    /** Get a registered container by its name. */
    def apply(name: String) = known.get(name) match {
      case Some(x) => x
      case None => throw new ContainerException(s"unrecognized container (is it a custom container that hasn't been registered?): $name")
    }

    /** User's entry point for reconstructing a container from JSON text. The container's type is not known at compile-time, so it must be cast (with the container's `as` method) or pattern-matched (with the corresponding `Factory`). */
    def fromJson(str: String): Container[_] = Json.parse(str) match {
      case Some(json) => fromJson(json)
      case None => throw new InvalidJsonException(str)
    }

    /** User's entry point for reconstructing a container from a JSON object. The container's type is not known at compile-time, so it must be cast (with the container's `as` method) or pattern-matched (with the corresponding `Factory`). */
    def fromJson(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("type", "data")) =>
        val get = pairs.toMap

        val name = get("type") match {
          case JsonString(x) => x
          case x => throw new JsonFormatException(x, "Factory.type")
        }

        Factory(name).fromJsonFragment(get("data"))

      case _ => throw new JsonFormatException(json, "Factory")
    }
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
    def toJson: Json = JsonObject("type" -> JsonString(factory.name), "data" -> toJsonFragment)
    /** Used internally to convert the container to JSON without its `"type"` header. */
    def toJsonFragment: Json
    /** Cast the container to a given type. Especially useful for containers reconstructed from JSON or stored in [[org.dianahep.histogrammar.UntypedLabeling]]/[[org.dianahep.histogrammar.UntypedLabeled]]. */
    def as[OTHER <: Container[OTHER]] = this.asInstanceOf[OTHER]
  }

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
  }

  /** Sub-trait of [[org.dianahep.histogrammar.Aggregation]] for all containers except [[org.dianahep.histogrammar.Counting]].
    * 
    * `AggregationOnData` containers actually depend on their `Datum` type; `Counting` is the only one that ignores it.
    */
  trait AggregationOnData extends Aggregation

  /** Increment function for Apache Spark's `aggregate` method.
    * 
    * Typical use: `filledHistogram = datasetRDD.aggregate(initialHistogram)(increment[initialHistogram.Type], combine[initialHistogram.Type])` where `datasetRDD` is a collection on `initialHistogram`'s `Datum` type.
    */
  class Increment[DATUM, CONTAINER <: Container[CONTAINER] with AggregationOnData {type Datum >: DATUM}] extends Function2[CONTAINER, DATUM, CONTAINER] with Serializable {
    def apply(h: CONTAINER, x: DATUM): CONTAINER = {h.fill(x); h}
  }

  /** Combine function for Apache Spark's `aggregate` method.
    * 
    * Typical use: `filledHistogram = datasetRDD.aggregate(initialHistogram)(increment[initialHistogram.Type], combine[initialHistogram.Type])` where `datasetRDD` is a collection on `initialHistogram`'s `Datum` type.
    */
  class Combine[CONTAINER <: Container[CONTAINER]] extends Function2[CONTAINER, CONTAINER, CONTAINER] with Serializable {
    def apply(h1: CONTAINER, h2: CONTAINER): CONTAINER = h1 + h2
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

  /** Wraps a user's function for weighting data.
    * 
    * If a boolean-valued function is supplied, it will automatically be converted into a function that returns 0.0 and 1.0.
    * 
    * A value of 1.0 corresponds to normal weight, and a value of 0.0 ''or less'' causes the datum to be skipped.
    * 
    * If `Selections` are specified for nested structures, they will be multiplied to provide the final weight. A value of 0.0 at any level skips descent of the subtree.
    */
  trait Selection[-DATUM] extends Serializable {
    def apply[SUB <: DATUM](x: SUB): Double
  }
  implicit class SelectionFromBoolean[-DATUM](f: DATUM => Boolean) extends Selection[DATUM] {
    def apply[SUB <: DATUM](x: SUB): Double = if (f(x)) 1.0 else 0.0
  }
  implicit class SelectionFromByte[-DATUM](f: DATUM => Byte) extends Selection[DATUM] {
    def apply[SUB <: DATUM](x: SUB): Double = f(x).toDouble
  }
  implicit class SelectionFromShort[-DATUM](f: DATUM => Short) extends Selection[DATUM] {
    def apply[SUB <: DATUM](x: SUB): Double = f(x).toDouble
  }
  implicit class SelectionFromInt[-DATUM](f: DATUM => Int) extends Selection[DATUM] {
    def apply[SUB <: DATUM](x: SUB): Double = f(x).toDouble
  }
  implicit class SelectionFromLong[-DATUM](f: DATUM => Long) extends Selection[DATUM] {
    def apply[SUB <: DATUM](x: SUB): Double = f(x).toDouble
  }
  implicit class SelectionFromFloat[-DATUM](f: DATUM => Float) extends Selection[DATUM] {
    def apply[SUB <: DATUM](x: SUB): Double = f(x).toDouble
  }
  implicit class SelectionFromDouble[-DATUM](f: DATUM => Double) extends Selection[DATUM] {
    def apply[SUB <: DATUM](x: SUB): Double = f(x)
  }

  /** Default weighting function that always returns 1.0. */
  def unweighted[DATUM] = new Selection[DATUM] {
    def apply[SUB <: DATUM](x: SUB) = 1.0
  }

  /** (Sealed) base trait for user functions. */
  sealed trait UserFcn[-DOMAIN, +RANGE] extends Serializable {
    def apply[SUB <: DOMAIN](x: SUB): RANGE
  }

  /** Wraps a user's function for extracting numbers from the input data type. */
  trait NumericalFcn[-DATUM] extends UserFcn[DATUM, Double] {
    def apply[SUB <: DATUM](x: SUB): Double
  }
  implicit class NumericalFcnFromByte[-DATUM](f: DATUM => Byte) extends NumericalFcn[DATUM] {
    def apply[SUB <: DATUM](x: SUB): Double = f(x).toDouble
  }
  implicit class NumericalFcnFromShort[-DATUM](f: DATUM => Short) extends NumericalFcn[DATUM] {
    def apply[SUB <: DATUM](x: SUB): Double = f(x).toDouble
  }
  implicit class NumericalFcnFromInt[-DATUM](f: DATUM => Int) extends NumericalFcn[DATUM] {
    def apply[SUB <: DATUM](x: SUB): Double = f(x).toDouble
  }
  implicit class NumericalFcnFromLong[-DATUM](f: DATUM => Long) extends NumericalFcn[DATUM] {
    def apply[SUB <: DATUM](x: SUB): Double = f(x).toDouble
  }
  implicit class NumericalFcnFromFloat[-DATUM](f: DATUM => Float) extends NumericalFcn[DATUM] {
    def apply[SUB <: DATUM](x: SUB): Double = f(x).toDouble
  }
  implicit class NumericalFcnFromDouble[-DATUM](f: DATUM => Double) extends NumericalFcn[DATUM] {
    def apply[SUB <: DATUM](x: SUB): Double = f(x)
  }

  /** Wraps a user's function for extracting strings (categories) from the input data type. */
  implicit class CategoricalFcn[-DATUM](f: DATUM => String) extends UserFcn[DATUM, String] {
    def apply[SUB <: DATUM](x: SUB): String = f(x)
  }

  /** Wraps a user's function for extracting multidimensional numeric data from the input data type. */
  implicit class MultivariateFcn[-DATUM](f: DATUM => Iterable[Double]) extends UserFcn[DATUM, Vector[Double]] {
    def apply[SUB <: DATUM](x: SUB): Vector[Double] = f(x).toVector
  }

  /** Introduces a `===` operator for `Double` precision numbers in which `NaN === NaN`. */
  implicit class nanEquality(val x: Double) extends AnyVal {
    def ===(that: Double) = (this.x.isNaN  &&  that.isNaN)  ||  this.x == that
  }

  /** Add `i0`, `i1`, etc. methods to `Branched` containers. */
  implicit class Branched0[C0 <: Container[C0], TAIL <: BranchedList](x: Branched[C0, TAIL]) {
    def i0 = x.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branched` containers. */
  implicit class Branched1[C0 <: Container[C0], C1 <: Container[C1], TAIL <: BranchedList](x: Branched[C0, Branched[C1, TAIL]]) {
    def i1 = x.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branched` containers. */
  implicit class Branched2[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, TAIL]]]) {
    def i2 = x.tail.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branched` containers. */
  implicit class Branched3[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, Branched[C3, TAIL]]]]) {
    def i3 = x.tail.tail.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branched` containers. */
  implicit class Branched4[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, Branched[C3, Branched[C4, TAIL]]]]]) {
    def i4 = x.tail.tail.tail.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branched` containers. */
  implicit class Branched5[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, Branched[C3, Branched[C4, Branched[C5, TAIL]]]]]]) {
    def i5 = x.tail.tail.tail.tail.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branched` containers. */
  implicit class Branched6[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, Branched[C3, Branched[C4, Branched[C5, Branched[C6, TAIL]]]]]]]) {
    def i6 = x.tail.tail.tail.tail.tail.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branched` containers. */
  implicit class Branched7[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, Branched[C3, Branched[C4, Branched[C5, Branched[C6, Branched[C7, TAIL]]]]]]]]) {
    def i7 = x.tail.tail.tail.tail.tail.tail.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branched` containers. */
  implicit class Branched8[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, Branched[C3, Branched[C4, Branched[C5, Branched[C6, Branched[C7, Branched[C8, TAIL]]]]]]]]]) {
    def i8 = x.tail.tail.tail.tail.tail.tail.tail.tail.head
  }
  /** Add `i0`, `i1`, etc. methods to `Branched` containers. */
  implicit class Branched9[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, Branched[C3, Branched[C4, Branched[C5, Branched[C6, Branched[C7, Branched[C8, Branched[C9, TAIL]]]]]]]]]]) {
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

}
