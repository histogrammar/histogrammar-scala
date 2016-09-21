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

import scala.collection.immutable.SortedSet
import scala.language.existentials

import org.dianahep.histogrammar.json._
import org.dianahep.histogrammar.util._

package histogrammar {
  //////////////////////////////////////////////////////////////// Stack/Stacked/Stacking

  /** Accumulates a suite of aggregators, each filtered with a tighter selection on the same quantity.
    * 
    * This is a generalization of [[org.dianahep.histogrammar.Fraction]], which fills two aggregators, one with a cut, the other without. Stack fills `N + 1` aggregators with `N` successively tighter cut thresholds. The first is always filled (like the denominator of Fraction), the second is filled if the computed quantity exceeds its threshold, the next is filled if the computed quantity exceeds a higher threshold, and so on.
    * 
    * The thresholds are presented in increasing order and the computed value must be greater than or equal to a threshold to fill the corresponding bin, and therefore the number of entries in each filled bin is greatest in the first and least in the last.
    * 
    * Although this aggregation could be visualized as a stack of histograms, stacked histograms usually represent a different thing: data from different sources, rather than different cuts on the same source. For example, it is common to stack Monte Carlo samples from different backgrounds to show that they add up to the observed data. The Stack aggregator does not make plots of this type because aggregation trees in Histogrammar draw data from exactly one source.
    * 
    * To make plots from different sources in Histogrammar, one must perform separate aggregation runs. It may then be convenient to stack the results of those runs as though they were created with a Stack aggregation, so that plotting code can treat both cases uniformly. For this reason, Stack has an alternate constructor to build a Stack manually from distinct aggregators, even if those aggregators came from different aggregation runs.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Stacking]] and immutable [[org.dianahep.histogrammar.Stacked]] objects.
    */
  object Stack extends Factory {
    val name = "Stack"
    val help = "Accumulates a suite of aggregators, each filtered with a tighter selection on the same quantity."
    val detailedHelp = """This is a generalization of [[org.dianahep.histogrammar.Fraction]], which fills two aggregators, one with a cut, the other without. Stack fills `N + 1` aggregators with `N` successively tighter cut thresholds. The first is always filled (like the denominator of Fraction), the second is filled if the computed quantity exceeds its threshold, the next is filled if the computed quantity exceeds a higher threshold, and so on.

The thresholds are presented in increasing order and the computed value must be greater than or equal to a threshold to fill the corresponding bin, and therefore the number of entries in each filled bin is greatest in the first and least in the last.

Although this aggregation could be visualized as a stack of histograms, stacked histograms usually represent a different thing: data from different sources, rather than different cuts on the same source. For example, it is common to stack Monte Carlo samples from different backgrounds to show that they add up to the observed data. The Stack aggregator does not make plots of this type because aggregation trees in Histogrammar draw data from exactly one source.

To make plots from different sources in Histogrammar, one must perform separate aggregation runs. It may then be convenient to stack the results of those runs as though they were created with a Stack aggregation, so that plotting code can treat both cases uniformly. For this reason, Stack has an alternate constructor to build a Stack manually from distinct aggregators, even if those aggregators came from different aggregation runs."""

    /** Create an immutable [[org.dianahep.histogrammar.Stacked]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param bins Lower thresholds and their associated containers, starting with negative infinity.
      * @param nanflow Container for data that resulted in `NaN`.
      */
    def ed[V <: Container[V] with NoAggregation, N <: Container[N] with NoAggregation](entries: Double, bins: Iterable[(Double, V)], nanflow: N): Stacked[V, N] = new Stacked(entries, None, bins.toSeq, nanflow)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Stacking]].
      * 
      * @param bins Thresholds that will be used to determine which datum goes into a given container; this list gets sorted, duplicates get removed, and negative infinity gets added as the first element.
      * @param quantity Numerical quantity whose value is compared with the given thresholds.
      * @param value Template used to create zero values (by calling this `value`'s `zero` method).
      * @param nanflow Container for data that resulted in `NaN`.
      */
    def apply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](bins: Iterable[Double], quantity: UserFcn[DATUM, Double], value: => V = Count(), nanflow: N = Count()) =
      new Stacking((java.lang.Double.NEGATIVE_INFINITY +: SortedSet(bins.toSeq: _*).toSeq).map((_, value.zero)), quantity, nanflow, 0.0)

    /** Synonym for `apply`. */
    def ing[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](bins: Iterable[Double], quantity: UserFcn[DATUM, Double], value: => V = Count(), nanflow: N = Count()) =
      apply(bins, quantity, value, nanflow)

    /** Alternate constructor for [[org.dianahep.histogrammar.Stacked]] that builds from N pre-aggregated primitives (N > 0).
      * 
      * The ''first'' result is the one that gets filled with contributions from all others, and should be plotted ''behind'' all others (''first,'' if overlays cover each other in the usual order).
      * 
      * Since this kind of stacked plot is not made from numerical bins, the numerical values of the `bins` are all `NaN`.
      */
    def build[V <: Container[V]](x: V, xs: Container[_]*): Stacked[V, Counted] = {
      val ys = x +: xs.map(_.asInstanceOf[V])
      val entries = ys.map(_.entries).sum
      val bins = ys.init.scanRight(ys.last)(_ + _).map((java.lang.Double.NaN, _))
      new Stacked(entries, None, bins, Count.ed(0.0))
    }

    import KeySetComparisons._
    def fromJsonFragment(json: Json, nameFromParent: Option[String]): Container[_] with NoAggregation = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("entries", "bins:type", "bins", "nanflow:type", "nanflow").maybe("name").maybe("bins:name")) =>
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

        val factory = get("bins:type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, name + ".bins:type")
        }
        val dataName = get.getOrElse("bins:name", JsonNull) match {
          case JsonString(x) => Some(x)
          case JsonNull => None
          case x => throw new JsonFormatException(x, name + ".bins:name")
        }

        val nanflowFactory = get("nanflow:type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, name + ".nanflow:type")
        }
        val nanflow = nanflowFactory.fromJsonFragment(get("nanflow"), None)

        val thebins =
          get("bins") match {
            case JsonArray(elements @ _*) if (elements.size >= 1) =>
              elements.zipWithIndex map {case (element, i) =>
                element match {
                  case JsonObject(elementPairs @ _*) if (elementPairs.keySet has Set("atleast", "data")) =>
                    val elementGet = elementPairs.toMap
                    val atleast = elementGet("atleast") match {
                      case JsonNumber(x) => x
                      case x => throw new JsonFormatException(x, name + s".bins element $i atleast")
                    }
                    (atleast, factory.fromJsonFragment(elementGet("data"), dataName))
                  case x => throw new JsonFormatException(x, name + s".bins element $i")
                }
              }
          case x => throw new JsonFormatException(x, name + ".bins")
        }

        new Stacked(entries, (nameFromParent ++ quantityName).lastOption, thebins.asInstanceOf[Seq[(Double, C)] forSome {type C <: Container[C] with NoAggregation}], nanflow.asInstanceOf[C forSome {type C <: Container[C] with NoAggregation}])

      case _ => throw new JsonFormatException(json, name)
    }
  }

  /** An accumulated suite of containers, each collecting data above a given cut on a given quantity.
    * 
    * Use the factory [[org.dianahep.histogrammar.Stack]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param quantityName Optional name given to the quantity function, passed for bookkeeping.
    * @param bins Lower thresholds and their associated containers, starting with negative infinity.
    * @param nanflow Container for data that resulted in `NaN`.
    */
  class Stacked[V <: Container[V], N <: Container[N]] private[histogrammar](val entries: Double, val quantityName: Option[String], val bins: Seq[(Double, V)], val nanflow: N) extends Container[Stacked[V, N]] with NoAggregation with QuantityName {
    // NOTE: The type bounds ought to be V <: Container[V] with NoAggregation, but this constraint has
    //       been relaxed to allow the alternate constructor. The standard constructor applies this
    //       constraint, so normal Stacked objects will have the correct types. HOWEVER, this class
    //       no longer "knows" that. I am not sure if this lack of knowledge will ever become a problem.

    type Type = Stacked[V, N]
    type EdType = Stacked[V, N]
    def factory = Stack

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (bins.size < 1)
      throw new ContainerException(s"number of bins (${bins.size}) must be at least 1 (including the implicit >= -inf, which the Stack.ing factory method adds)")

    def thresholds = bins.map(_._1)
    def values = bins.map(_._2)

    def zero = new Stacked[V, N](0.0, quantityName, bins map {case (c, v) => (c, v.zero)}, nanflow.zero)
    def +(that: Stacked[V, N]) = {
      if (this.thresholds != that.thresholds)
        throw new ContainerException(s"cannot add ${getClass.getName} because cut thresholds differ")
      if (this.quantityName != that.quantityName)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantityName differs (${this.quantityName} vs ${that.quantityName})")
      new Stacked(
        this.entries + that.entries,
        this.quantityName,
        this.bins zip that.bins map {case ((mycut, me), (yourcut, you)) => (mycut, me + you)},
        this.nanflow + that.nanflow)
    }
    def *(factor: Double) =
      if (factor.isNaN  ||  factor <= 0.0)
        zero
      else
        new Stacked[V, N](factor * entries, quantityName, bins map {case (c, v) => (c, v * factor)}, nanflow * factor)

    def children = values.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "bins:type" -> JsonString(bins.head._2.factory.name),
      "bins" -> JsonArray(bins map {case (atleast, sub) => JsonObject("atleast" -> JsonFloat(atleast), "data" -> sub.toJsonFragment(true))}: _*),
      "nanflow:type" -> JsonString(nanflow.factory.name),
      "nanflow" -> nanflow.toJsonFragment(false)).
      maybe(JsonString("name") -> (if (suppressName) None else quantityName.map(JsonString(_)))).
      maybe(JsonString("bins:name") -> (bins.head match {case (atleast, sub: QuantityName) => sub.quantityName.map(JsonString(_)); case _ => None}))

    override def toString() = s"""<Stacked values=${bins.head._2.factory.name} thresholds=(${bins.map(_._1).mkString(", ")}) nanflow=${nanflow.factory.name}>"""
    override def equals(that: Any) = that match {
      case that: Stacked[V, N] => this.entries === that.entries  &&  this.quantityName == that.quantityName  &&  (this.bins zip that.bins forall {case (me, you) => me._1 === you._1  &&  me._2 == you._2})  &&  this.nanflow == that.nanflow
      case _ => false
    }
    override def hashCode() = (entries, quantityName, bins, nanflow).hashCode()
  }

  /** Accumulating a suite of containers, each collecting data above a given cut on a given quantity.
    * 
    * Use the factory [[org.dianahep.histogrammar.Stack]] to construct an instance.
    * 
    * @param bins Lower thresholds and their associated containers, starting with negative infinity.
    * @param quantity Numerical quantity whose value is compared with the given thresholds.
    * @param nanflow Container for data that resulted in `NaN`.
    * @param entries Weighted number of entries (sum of all observed weights).
    */
  class Stacking[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}] private[histogrammar](val bins: Seq[(Double, V)], val quantity: UserFcn[DATUM, Double], val nanflow: N, var entries: Double) extends Container[Stacking[DATUM, V, N]] with AggregationOnData with NumericalQuantity[DATUM] {
    protected val v = bins.head._2
    type Type = Stacking[DATUM, V, N]
    type EdType = Stacked[v.EdType, nanflow.EdType]
    type Datum = DATUM
    def factory = Stack

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (bins.size < 1)
      throw new ContainerException(s"number of bins (${bins.size}) must be at least 1 (including the implicit >= -inf, which the Stack.ing factory method adds)")

    def thresholds = bins.map(_._1)
    def values = bins.map(_._2)

    def zero = new Stacking[DATUM, V, N](bins map {case (c, v) => (c, v.zero)}, quantity, nanflow.zero, 0.0)
    def +(that: Stacking[DATUM, V, N]) = {
      if (this.thresholds != that.thresholds)
        throw new ContainerException(s"cannot add ${getClass.getName} because cut thresholds differ")
      if (this.quantity.name != that.quantity.name)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantity name differs (${this.quantity.name} vs ${that.quantity.name})")
        new Stacking(
          this.bins zip that.bins map {case ((mycut, me), (yourcut, you)) => (mycut, me + you)},
          this.quantity,
          this.nanflow + that.nanflow,
          this.entries + that.entries)
    }
    def *(factor: Double) =
      if (factor.isNaN  ||  factor <= 0.0)
        zero
      else
        new Stacking[DATUM, V, N](bins map {case (c, v) => (c, v * factor)}, quantity, nanflow * factor, factor * entries)

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      checkForCrossReferences()
      if (weight > 0.0) {
        val q = quantity(datum)
        if (q.isNaN)
          nanflow.fill(datum, weight)
        else
          bins foreach {case (threshold, sub) =>
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
      "bins:type" -> JsonString(bins.head._2.factory.name),
      "bins" -> JsonArray(bins map {case (atleast, sub) => JsonObject("atleast" -> JsonFloat(atleast), "data" -> sub.toJsonFragment(true))}: _*),
      "nanflow:type" -> JsonString(nanflow.factory.name),
      "nanflow" -> nanflow.toJsonFragment(false)).
      maybe(JsonString("name") -> (if (suppressName) None else quantity.name.map(JsonString(_)))).
      maybe(JsonString("bins:name") -> (bins.head match {case (atleast, sub: AnyQuantity[_, _]) => sub.quantity.name.map(JsonString(_)); case _ => None}))

    override def toString() = s"""<Stacking values=${bins.head._2.factory.name} thresholds=(${bins.map(_._1).mkString(", ")}) nanflow=${nanflow.factory.name}>"""
    override def equals(that: Any) = that match {
      case that: Stacking[DATUM, V, N] => this.quantity == that.quantity  &&  this.entries === that.entries  &&  (this.bins zip that.bins forall {case (me, you) => me._1 === you._1  &&  me._2 == you._2})  &&  this.nanflow == that.nanflow
      case _ => false
    }
    override def hashCode() = (quantity, entries, bins, nanflow).hashCode()
  }
}
