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
  //////////////////////////////////////////////////////////////// Stack/Stacked/Stacking

  /** Accumulate a suite containers, filling all that are above a given cut on a given quantity.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Stacking]] and immutable [[org.dianahep.histogrammar.Stacked]] objects.
    */
  object Stack extends Factory {
    val name = "Stack"
    val help = "Accumulate a suite containers, filling all that are above a given cut on a given quantity."
    val detailedHelp = """Stack(value: => V, quantity: UserFcn[DATUM, Double], cuts: Double*)"""

    /** Create an immutable [[org.dianahep.histogrammar.Stacked]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param cuts Lower thresholds and their associated containers, starting with negative infinity.
      * @param nanflow Container for data that resulted in `NaN`.
      */
    def ed[V <: Container[V] with NoAggregation, N <: Container[N] with NoAggregation](entries: Double, cuts: Iterable[(Double, V)], nanflow: N): Stacked[V, N] = new Stacked(entries, None, cuts.toSeq, nanflow)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Stacking]].
      * 
      * @param cuts Thresholds that will be used to determine which datum goes into a given container; this list gets sorted, duplicates get removed, and negative infinity gets added as the first element.
      * @param quantity Numerical quantity whose value is compared with the given thresholds.
      * @param value Template used to create zero values (by calling this `value`'s `zero` method).
      * @param nanflow Container for data that resulted in `NaN`.
      */
    def apply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](cuts: Iterable[Double], quantity: UserFcn[DATUM, Double], value: => V, nanflow: N = Count()) =
      new Stacking((java.lang.Double.NEGATIVE_INFINITY +: SortedSet(cuts.toSeq: _*).toSeq).map((_, value.zero)), quantity, nanflow, 0.0)

    /** Synonym for `apply`. */
    def ing[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](cuts: Iterable[Double], quantity: UserFcn[DATUM, Double], value: => V, nanflow: N = Count()) =
      apply(cuts, quantity, value, nanflow)

    /** Alternate constructor for [[org.dianahep.histogrammar.Stacked]] that builds from N pre-aggregated primitives (N > 0).
      * 
      * The ''first'' result is the one that gets filled with contributions from all others, and should be plotted ''behind'' all others (''first,'' if overlays cover each other in the usual order).
      * 
      * Since this kind of stacked plot is not made from numerical cuts, the numerical values of the `cuts` are all `NaN`.
      */
    def build[V <: Container[V]](x: V, xs: Container[_]*): Stacked[V, Counted] = {
      val ys = x +: xs.map(_.asInstanceOf[V])
      val entries = ys.map(_.entries).sum
      val cuts = ys.init.scanRight(ys.last)(_ + _).map((java.lang.Double.NaN, _))
      new Stacked(entries, None, cuts, Count.ed(0.0))
    }

    import KeySetComparisons._
    def fromJsonFragment(json: Json, nameFromParent: Option[String]): Container[_] with NoAggregation = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("entries", "type", "data", "nanflow:type", "nanflow").maybe("name").maybe("data:name")) =>
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

        val nanflowFactory = get("nanflow:type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, name + ".nanflow:type")
        }
        val nanflow = nanflowFactory.fromJsonFragment(get("nanflow"), None)

        get("data") match {
          case JsonArray(elements @ _*) if (elements.size >= 1) =>
            new Stacked[Container[_], Container[_]](entries, (nameFromParent ++ quantityName).lastOption, elements.zipWithIndex map {case (element, i) =>
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
            }, nanflow)

          case x => throw new JsonFormatException(x, name + ".data")
        }

      case _ => throw new JsonFormatException(json, name)
    }
  }

  /** An accumulated suite of containers, each collecting data above a given cut on a given quantity.
    * 
    * Use the factory [[org.dianahep.histogrammar.Stack]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param quantityName Optional name given to the quantity function, passed for bookkeeping.
    * @param cuts Lower thresholds and their associated containers, starting with negative infinity.
    * @param nanflow Container for data that resulted in `NaN`.
    */
  class Stacked[V <: Container[V], N <: Container[N]] private[histogrammar](val entries: Double, val quantityName: Option[String], val cuts: Seq[(Double, V)], val nanflow: N) extends Container[Stacked[V, N]] with NoAggregation with QuantityName {
    // NOTE: The type bounds ought to be V <: Container[V] with NoAggregation, but this constraint has
    //       been relaxed to allow the alternate constructor. The standard constructor applies this
    //       constraint, so normal Stacked objects will have the correct types. HOWEVER, this class
    //       no longer "knows" that. I am not sure if this lack of knowledge will ever become a problem.

    type Type = Stacked[V, N]
    type EdType = Stacked[V, N]
    def factory = Stack

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (cuts.size < 1)
      throw new ContainerException(s"number of cuts (${cuts.size}) must be at least 1 (including the implicit >= -inf, which the Stack.ing factory method adds)")

    def thresholds = cuts.map(_._1)
    def values = cuts.map(_._2)

    def zero = new Stacked[V, N](0.0, quantityName, cuts map {case (c, v) => (c, v.zero)}, nanflow.zero)
    def +(that: Stacked[V, N]) = {
      if (this.thresholds != that.thresholds)
        throw new ContainerException(s"cannot add ${getClass.getName} because cut thresholds differ")
      if (this.quantityName != that.quantityName)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantityName differs (${this.quantityName} vs ${that.quantityName})")
      new Stacked(
        this.entries + that.entries,
        this.quantityName,
        this.cuts zip that.cuts map {case ((mycut, me), (yourcut, you)) => (mycut, me + you)},
        this.nanflow + that.nanflow)
    }

    def children = values.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "type" -> JsonString(cuts.head._2.factory.name),
      "data" -> JsonArray(cuts map {case (atleast, sub) => JsonObject("atleast" -> JsonFloat(atleast), "data" -> sub.toJsonFragment(true))}: _*),
      "nanflow:type" -> JsonString(nanflow.factory.name),
      "nanflow" -> nanflow.toJsonFragment(false)).
      maybe(JsonString("name") -> (if (suppressName) None else quantityName.map(JsonString(_)))).
      maybe(JsonString("data:name") -> (cuts.head match {case (atleast, sub: QuantityName) => sub.quantityName.map(JsonString(_)); case _ => None}))

    override def toString() = s"""<Stacked bins=${cuts.head._2.factory.name} thresholds=(${cuts.map(_._1).mkString(", ")}) nanflow=${nanflow.factory.name}>"""
    override def equals(that: Any) = that match {
      case that: Stacked[V, N] => this.entries === that.entries  &&  this.quantityName == that.quantityName  &&  (this.cuts zip that.cuts forall {case (me, you) => me._1 === you._1  &&  me._2 == you._2})  &&  this.nanflow == that.nanflow
      case _ => false
    }
    override def hashCode() = (entries, quantityName, cuts, nanflow).hashCode()
  }

  /** Accumulating a suite of containers, each collecting data above a given cut on a given quantity.
    * 
    * Use the factory [[org.dianahep.histogrammar.Stack]] to construct an instance.
    * 
    * @param cuts Lower thresholds and their associated containers, starting with negative infinity.
    * @param quantity Numerical quantity whose value is compared with the given thresholds.
    * @param nanflow Container for data that resulted in `NaN`.
    * @param entries Weighted number of entries (sum of all observed weights).
    */
  class Stacking[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}] private[histogrammar](val cuts: Seq[(Double, V)], val quantity: UserFcn[DATUM, Double], val nanflow: N, var entries: Double) extends Container[Stacking[DATUM, V, N]] with AggregationOnData with NumericalQuantity[DATUM] {
    protected val v = cuts.head._2
    type Type = Stacking[DATUM, V, N]
    type EdType = Stacked[v.EdType, nanflow.EdType]
    type Datum = DATUM
    def factory = Stack

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (cuts.size < 1)
      throw new ContainerException(s"number of cuts (${cuts.size}) must be at least 1 (including the implicit >= -inf, which the Stack.ing factory method adds)")

    def thresholds = cuts.map(_._1)
    def values = cuts.map(_._2)

    def zero = new Stacking[DATUM, V, N](cuts map {case (c, v) => (c, v.zero)}, quantity, nanflow.zero, 0.0)
    def +(that: Stacking[DATUM, V, N]) = {
      if (this.thresholds != that.thresholds)
        throw new ContainerException(s"cannot add ${getClass.getName} because cut thresholds differ")
      if (this.quantity.name != that.quantity.name)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantity name differs (${this.quantity.name} vs ${that.quantity.name})")
        new Stacking(
          this.cuts zip that.cuts map {case ((mycut, me), (yourcut, you)) => (mycut, me + you)},
          this.quantity,
          this.nanflow + that.nanflow,
          this.entries + that.entries)
    }

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      checkForCrossReferences()
      if (weight > 0.0) {
        val q = quantity(datum)
        if (q.isNaN)
          nanflow.fill(datum, weight)
        else
          cuts foreach {case (threshold, sub) =>
            if (q >= threshold)
              sub.fill(datum, weight)
          }

        // no possibility of exception from here on out (for rollback)
        entries += weight
      }
    }

    def children = nanflow :: values.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "type" -> JsonString(cuts.head._2.factory.name),
      "data" -> JsonArray(cuts map {case (atleast, sub) => JsonObject("atleast" -> JsonFloat(atleast), "data" -> sub.toJsonFragment(true))}: _*),
      "nanflow:type" -> JsonString(nanflow.factory.name),
      "nanflow" -> nanflow.toJsonFragment(false)).
      maybe(JsonString("name") -> (if (suppressName) None else quantity.name.map(JsonString(_)))).
      maybe(JsonString("data:name") -> (cuts.head match {case (atleast, sub: AnyQuantity[_, _]) => sub.quantity.name.map(JsonString(_)); case _ => None}))

    override def toString() = s"""<Stacking bins=${cuts.head._2.factory.name} thresholds=(${cuts.map(_._1).mkString(", ")}) nanflow=${nanflow.factory.name}>"""
    override def equals(that: Any) = that match {
      case that: Stacking[DATUM, V, N] => this.quantity == that.quantity  &&  this.entries === that.entries  &&  (this.cuts zip that.cuts forall {case (me, you) => me._1 === you._1  &&  me._2 == you._2})  &&  this.nanflow == that.nanflow
      case _ => false
    }
    override def hashCode() = (quantity, entries, cuts, nanflow).hashCode()
  }
}
