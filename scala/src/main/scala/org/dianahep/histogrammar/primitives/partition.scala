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

import scala.collection.immutable.SortedSet

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Partition/Partitioned/Partitioning

  /** Accumulate a suite containers, filling the one that is between a pair of given cuts on a given expression.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Partitioning]] and immutable [[org.dianahep.histogrammar.Partitioned]] objects.
    */
  object Partition extends Factory {
    val name = "Partition"
    val help = "Accumulate a suite containers, filling the one that is between a pair of given cuts on a given expression."
    val detailedHelp = """Partition(value: => V, expression: NumericalFcn[DATUM], cuts: Double*)"""

    /** Create an immutable [[org.dianahep.histogrammar.Partitioned]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param cuts Lower thresholds and their associated containers, starting with negative infinity.
      */
    def ed[V <: Container[V]](entries: Double, cuts: (Double, V)*) = new Partitioned(entries, cuts: _*)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Partitioning]].
      * 
      * @param value Template used to create zero values (by calling this `value`'s `zero` method).
      * @param expression Numerical expression whose value is compared with the given thresholds.
      * @param cuts Thresholds that will be used to determine which datum goes into a given container; this list gets sorted, duplicates get removed, and negative infinity gets added as the first element.
      */
    def apply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](value: => V, expression: NumericalFcn[DATUM], cuts: Double*) =
      new Partitioning(expression, 0.0, (java.lang.Double.NEGATIVE_INFINITY +: SortedSet(cuts: _*).toList).map((_, value.zero)): _*)

    /** Synonym for `apply`. */
    def ing[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](value: => V, expression: NumericalFcn[DATUM], cuts: Double*) =
      apply(value, expression, cuts: _*)

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("entries", "type", "data")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val factory = get("type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, name + ".type")
        }

        get("data") match {
          case JsonArray(elements @ _*) if (elements.size >= 1) =>
            new Partitioned[Container[_]](entries, elements.zipWithIndex map {case (element, i) =>
              element match {
                case JsonObject(elementPairs @ _*) if (elementPairs.keySet == Set("atleast", "data")) =>
                  val elementGet = elementPairs.toMap
                  val atleast = elementGet("atleast") match {
                    case JsonNumber(x) => x
                    case x => throw new JsonFormatException(x, name + s".data element $i atleast")
                  }
                  (atleast, factory.fromJsonFragment(elementGet("data")))

                case x => throw new JsonFormatException(x, name + s".data element $i")
              }
            }: _*)

          case x => throw new JsonFormatException(x, name + ".data")
        }

      case _ => throw new JsonFormatException(json, name)
    }
  }

  /** An accumulated suite of containers, each collecting data between a pair of given cuts on a given expression.
    * 
    * Use the factory [[org.dianahep.histogrammar.Partition]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all weights).
    * @param cuts Lower thresholds and their associated containers, starting with negative infinity.
    */
  class Partitioned[V <: Container[V]] private[histogrammar](val entries: Double, val cuts: (Double, V)*) extends Container[Partitioned[V]] {
    type Type = Partitioned[V]
    def factory = Partition

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (cuts.size < 1)
      throw new ContainerException(s"number of cuts (${cuts.size}) must be at least 1 (including the implicit >= -inf, which the Partition.ing factory method adds)")

    def thresholds = cuts.map(_._1)
    def values = cuts.map(_._2)

    def zero = new Partitioned[V](0.0, cuts map {case (c, v) => (c, v.zero)}: _*)
    def +(that: Partitioned[V]) =
      if (this.thresholds != that.thresholds)
        throw new ContainerException(s"cannot add Partitioned because cut thresholds differ")
      else
        new Partitioned(
          this.entries + that.entries,
          this.cuts zip that.cuts map {case ((mycut, me), (yourcut, you)) => (mycut, me + you)}: _*)

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "type" -> JsonString(cuts.head._2.factory.name),
      "data" -> JsonArray(cuts map {case (atleast, sub) => JsonObject("atleast" -> JsonFloat(atleast), "data" -> sub.toJsonFragment)}: _*))

    override def toString() = s"""Partitioned[${cuts.head._2}, thresholds=[${cuts.map(_._1).mkString(", ")}]]"""
    override def equals(that: Any) = that match {
      case that: Partitioned[V] => this.entries === that.entries  &&  (this.cuts zip that.cuts forall {case (me, you) => me._1 === you._1  &&  me._2 == you._2})
      case _ => false
    }
    override def hashCode() = (entries, cuts).hashCode()
  }

  /** Accumulating a suite of containers, each collecting data between a pair of given cuts on a given expression.
    * 
    * Use the factory [[org.dianahep.histogrammar.Partition]] to construct an instance.
    * 
    * @param expression Numerical expression whose value is compared with the given thresholds.
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param cuts Lower thresholds and their associated containers, starting with negative infinity.
    */
  class Partitioning[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}] private[histogrammar](val expression: NumericalFcn[DATUM], var entries: Double, val cuts: (Double, V)*) extends Container[Partitioning[DATUM, V]] with AggregationOnData {
    type Type = Partitioning[DATUM, V]
    type Datum = DATUM
    def factory = Partition

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (cuts.size < 1)
      throw new ContainerException(s"number of cuts (${cuts.size}) must be at least 1 (including the implicit >= -inf, which the Partition.ing factory method adds)")

    private val range = cuts zip (cuts.tail :+ (java.lang.Double.NaN, null))

    def thresholds = cuts.map(_._1)
    def values = cuts.map(_._2)

    def zero = new Partitioning[DATUM, V](expression, 0.0, cuts map {case (c, v) => (c, v.zero)}: _*)
    def +(that: Partitioning[DATUM, V]) =
      if (this.thresholds != that.thresholds)
        throw new ContainerException(s"cannot add Partitioning because cut thresholds differ")
      else
        new Partitioning(
          this.expression,
          this.entries + that.entries,
          this.cuts zip that.cuts map {case ((mycut, me), (yourcut, you)) => (mycut, me + you)}: _*)

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      if (weight > 0.0) {
        val value = expression(datum)
        entries += weight
        // !(value >= high) is true when high == NaN (even if value == +inf)
        range find {case ((low, sub), (high, _)) => value >= low  &&  !(value >= high)} foreach {case ((_, sub), (_, _)) =>
          sub.fill(datum, weight)
        }
      }
    }

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "type" -> JsonString(cuts.head._2.factory.name),
      "data" -> JsonArray(cuts map {case (atleast, sub) => JsonObject("atleast" -> JsonFloat(atleast), "data" -> sub.toJsonFragment)}: _*))

    override def toString() = s"""Partitioning[${cuts.head._2}, thresholds=[${cuts.map(_._1).mkString(", ")}]]"""
    override def equals(that: Any) = that match {
      case that: Partitioning[DATUM, V] => this.expression == that.expression  &&  this.entries === that.entries  &&  (this.cuts zip that.cuts forall {case (me, you) => me._1 === you._1  &&  me._2 == you._2})
      case _ => false
    }
    override def hashCode() = (expression, entries, cuts).hashCode()
  }
}
