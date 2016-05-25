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
import org.dianahep.histogrammar.util._

package histogrammar {
  //////////////////////////////////////////////////////////////// Partition/Partitioned/Partitioning

  /** Accumulate a suite containers, filling the one that is between a pair of given cuts on a given quantity.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Partitioning]] and immutable [[org.dianahep.histogrammar.Partitioned]] objects.
    */
  object Partition extends Factory {
    val name = "Partition"
    val help = "Accumulate a suite containers, filling the one that is between a pair of given cuts on a given quantity."
    val detailedHelp = """Partition(value: => V, quantity: UserFcn[DATUM, Double], cuts: Double*)"""

    /** Create an immutable [[org.dianahep.histogrammar.Partitioned]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param cuts Lower thresholds and their associated containers, starting with negative infinity.
      */
    def ed[V <: Container[V] with NoAggregation](entries: Double, cuts: (Double, V)*) = new Partitioned(entries, None, cuts: _*)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Partitioning]].
      * 
      * @param value Template used to create zero values (by calling this `value`'s `zero` method).
      * @param quantity Numerical quantity whose value is compared with the given thresholds.
      * @param cuts Thresholds that will be used to determine which datum goes into a given container; this list gets sorted, duplicates get removed, and negative infinity gets added as the first element.
      */
    def apply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](quantity: UserFcn[DATUM, Double], value: => V, cuts: Double*) =
      new Partitioning(quantity, 0.0, (java.lang.Double.NEGATIVE_INFINITY +: SortedSet(cuts: _*).toList).map((_, value.zero)): _*)

    /** Synonym for `apply`. */
    def ing[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](quantity: UserFcn[DATUM, Double], value: => V, cuts: Double*) =
      apply(quantity, value, cuts: _*)

    import KeySetComparisons._
    def fromJsonFragment(json: Json, nameFromParent: Option[String]): Container[_] with NoAggregation = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("entries", "type", "data").maybe("name").maybe("data:name")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val quantityName = get.getOrElse("name", JsonNull) match {
          case JsonString(x) => Some(x)
          case JsonNull => None
          case x => throw new JsonFormatException(x, name + ".name")
        }

        val factory = get("type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, name + ".type")
        }

        val dataName = get.getOrElse("data:name", JsonNull) match {
          case JsonString(x) => Some(x)
          case JsonNull => None
          case x => throw new JsonFormatException(x, name + ".data:name")
        }

        get("data") match {
          case JsonArray(elements @ _*) if (elements.size >= 1) =>
            new Partitioned[Container[_]](entries, (nameFromParent ++ quantityName).lastOption, elements.zipWithIndex map {case (element, i) =>
              element match {
                case JsonObject(elementPairs @ _*) if (elementPairs.keySet has Set("atleast", "data")) =>
                  val elementGet = elementPairs.toMap
                  val atleast = elementGet("atleast") match {
                    case JsonNumber(x) => x
                    case x => throw new JsonFormatException(x, name + s".data element $i atleast")
                  }
                  (atleast, factory.fromJsonFragment(elementGet("data"), dataName))

                case x => throw new JsonFormatException(x, name + s".data element $i")
              }
            }: _*)

          case x => throw new JsonFormatException(x, name + ".data")
        }

      case _ => throw new JsonFormatException(json, name)
    }
  }

  /** An accumulated suite of containers, each collecting data between a pair of given cuts on a given quantity.
    * 
    * Use the factory [[org.dianahep.histogrammar.Partition]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param quantityName Optional name given to the quantity function, passed for bookkeeping.
    * @param cuts Lower thresholds and their associated containers, starting with negative infinity.
    */
  class Partitioned[V <: Container[V] with NoAggregation] private[histogrammar](val entries: Double, val quantityName: Option[String], val cuts: (Double, V)*) extends Container[Partitioned[V]] with NoAggregation with QuantityName {
    type Type = Partitioned[V]
    def factory = Partition

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (cuts.size < 1)
      throw new ContainerException(s"number of cuts (${cuts.size}) must be at least 1 (including the implicit >= -inf, which the Partition.ing factory method adds)")

    def thresholds = cuts.map(_._1)
    def values = cuts.map(_._2)

    def zero = new Partitioned[V](0.0, quantityName, cuts map {case (c, v) => (c, v.zero)}: _*)
    def +(that: Partitioned[V]) = {
      if (this.thresholds != that.thresholds)
        throw new ContainerException(s"cannot add ${getClass.getName} because cut thresholds differ")
      if (this.quantityName != that.quantityName)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantityName differs (${this.quantityName} vs ${that.quantityName})")
      new Partitioned(
        this.entries + that.entries,
        this.quantityName,
        this.cuts zip that.cuts map {case ((mycut, me), (yourcut, you)) => (mycut, me + you)}: _*)
    }

    def children = values.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "type" -> JsonString(cuts.head._2.factory.name),
      "data" -> JsonArray(cuts map {case (atleast, sub) => JsonObject("atleast" -> JsonFloat(atleast), "data" -> sub.toJsonFragment(true))}: _*)).
      maybe(JsonString("name") -> (if (suppressName) None else quantityName.map(JsonString(_)))).
      maybe(JsonString("data:name") -> (cuts.head match {case (atleast, sub: QuantityName) => sub.quantityName.map(JsonString(_)); case _ => None}))

    override def toString() = s"""Partitioned[${cuts.head._2}, thresholds=[${cuts.map(_._1).mkString(", ")}]]"""
    override def equals(that: Any) = that match {
      case that: Partitioned[V] => this.entries === that.entries  &&  this.quantityName == that.quantityName  &&  (this.cuts zip that.cuts forall {case (me, you) => me._1 === you._1  &&  me._2 == you._2})
      case _ => false
    }
    override def hashCode() = (entries, quantityName, cuts).hashCode()
  }

  /** Accumulating a suite of containers, each collecting data between a pair of given cuts on a given quantity.
    * 
    * Use the factory [[org.dianahep.histogrammar.Partition]] to construct an instance.
    * 
    * @param quantity Numerical quantity whose value is compared with the given thresholds.
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param cuts Lower thresholds and their associated containers, starting with negative infinity.
    */
  class Partitioning[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}] private[histogrammar](val quantity: UserFcn[DATUM, Double], var entries: Double, val cuts: (Double, V)*) extends Container[Partitioning[DATUM, V]] with AggregationOnData with NumericalQuantity[DATUM] {
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

    def zero = new Partitioning[DATUM, V](quantity, 0.0, cuts map {case (c, v) => (c, v.zero)}: _*)
    def +(that: Partitioning[DATUM, V]) = {
      if (this.thresholds != that.thresholds)
        throw new ContainerException(s"cannot add ${getClass.getName} because cut thresholds differ")
      if (this.quantity.name != that.quantity.name)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantity name differs (${this.quantity.name} vs ${that.quantity.name})")
        new Partitioning(
          this.quantity,
          this.entries + that.entries,
          this.cuts zip that.cuts map {case ((mycut, me), (yourcut, you)) => (mycut, me + you)}: _*)
    }

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      if (weight > 0.0) {
        val value = quantity(datum)
        // !(value >= high) is true when high == NaN (even if value == +inf)
        range find {case ((low, sub), (high, _)) => value >= low  &&  !(value >= high)} foreach {case ((_, sub), (_, _)) =>
          sub.fill(datum, weight)
        }

        // no possibility of exception from here on out (for rollback)
        entries += weight
      }
    }

    def children = values.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "type" -> JsonString(cuts.head._2.factory.name),
      "data" -> JsonArray(cuts map {case (atleast, sub) => JsonObject("atleast" -> JsonFloat(atleast), "data" -> sub.toJsonFragment(true))}: _*)).
      maybe(JsonString("name") -> (if (suppressName) None else quantity.name.map(JsonString(_)))).
      maybe(JsonString("data:name") -> (cuts.head match {case (atleast, sub: AnyQuantity[_, _]) => sub.quantity.name.map(JsonString(_)); case _ => None}))

    override def toString() = s"""Partitioning[${cuts.head._2}, thresholds=[${cuts.map(_._1).mkString(", ")}]]"""
    override def equals(that: Any) = that match {
      case that: Partitioning[DATUM, V] => this.quantity == that.quantity  &&  this.entries === that.entries  &&  (this.cuts zip that.cuts forall {case (me, you) => me._1 === you._1  &&  me._2 == you._2})
      case _ => false
    }
    override def hashCode() = (quantity, entries, cuts).hashCode()
  }
}
