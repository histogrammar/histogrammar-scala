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
  //////////////////////////////////////////////////////////////// IrregularlyBin/IrregularlyBinned/IrregularlyBinning

  /** Accumulate a suite of aggregators, each between two thresholds, filling exactly one per datum.
    * 
    * This is a variation on [[org.dianahep.histogrammar.Stack]], which fills `N + 1` aggregators with `N` successively tighter cut thresholds. IrregularlyBin fills `N + 1` aggregators in the non-overlapping intervals between `N` thresholds.
    * 
    * IrregularlyBin is also similar to [[org.dianahep.histogrammar.CentrallyBin]], in that they both partition a space into irregular subdomains with no gaps and no overlaps. However, CentrallyBin is defined by bin centers and IrregularlyBin is defined by bin edges, the first and last of which are at negative and positive infinity.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.IrregularlyBinning]] and immutable [[org.dianahep.histogrammar.IrregularlyBinned]] objects.
    */
  object IrregularlyBin extends Factory {
    val name = "IrregularlyBin"
    val help = "Accumulate a suite of aggregators, each between two thresholds, filling exactly one per datum."
    val detailedHelp = """This is a variation on [[org.dianahep.histogrammar.Stack]], which fills `N + 1` aggregators with `N` successively tighter cut thresholds. IrregularlyBin fills `N + 1` aggregators in the non-overlapping intervals between `N` thresholds.

IrregularlyBin is also similar to [[org.dianahep.histogrammar.CentrallyBin]], in that they both partition a space into irregular subdomains with no gaps and no overlaps. However, CentrallyBin is defined by bin centers and IrregularlyBin is defined by bin edges, the first and last of which are at negative and positive infinity."""

    /** Create an immutable [[org.dianahep.histogrammar.IrregularlyBinned]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param bins Lower thresholds and their associated containers, starting with negative infinity.
      * @param nanflow Container for data that resulted in `NaN`.
      */
    def ed[V <: Container[V] with NoAggregation, N <: Container[N] with NoAggregation](entries: Double, bins: Iterable[(Double, V)], nanflow: N) = new IrregularlyBinned(entries, None, bins.toSeq, nanflow)

    /** Create an empty, mutable [[org.dianahep.histogrammar.IrregularlyBinning]].
      * 
      * @param bins Thresholds that will be used to determine which datum goes into a given container; this list gets sorted, duplicates get removed, and negative infinity gets added as the first element.
      * @param quantity Numerical quantity whose value is compared with the given thresholds.
      * @param value Template used to create zero values (by calling this `value`'s `zero` method).
      */
    def apply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](bins: Iterable[Double], quantity: UserFcn[DATUM, Double], value: => V = Count(), nanflow: N = Count()) =
      new IrregularlyBinning((java.lang.Double.NEGATIVE_INFINITY +: SortedSet(bins.toSeq: _*).toSeq).map((_, value.zero)), quantity, nanflow, 0.0)

    /** Synonym for `apply`. */
    def ing[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](bins: Iterable[Double], quantity: UserFcn[DATUM, Double], value: => V = Count(), nanflow: N = Count()) =
      apply(bins, quantity, value, nanflow)

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
                      case x => throw new JsonFormatException(x, name + s".data element $i atleast")
                    }
                    (atleast, factory.fromJsonFragment(elementGet("data"), dataName))
                  case x => throw new JsonFormatException(x, name + s".data element $i")
                }
              }
            case x => throw new JsonFormatException(x, name + ".data")
          }

        new IrregularlyBinned(entries, (nameFromParent ++ quantityName).lastOption, thebins.asInstanceOf[Seq[(Double, C)] forSome {type C <: Container[C] with NoAggregation}], nanflow.asInstanceOf[C forSome {type C <: Container[C] with NoAggregation}])

      case _ => throw new JsonFormatException(json, name)
    }
  }

  /** An accumulated suite of containers, each collecting data between a pair of given cuts on a given quantity.
    * 
    * Use the factory [[org.dianahep.histogrammar.IrregularlyBin]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param quantityName Optional name given to the quantity function, passed for bookkeeping.
    * @param bins Lower thresholds and their associated containers, starting with negative infinity.
    * @param nanflow Container for data that resulted in `NaN`.
    */
  class IrregularlyBinned[V <: Container[V] with NoAggregation, N <: Container[N] with NoAggregation] private[histogrammar](val entries: Double, val quantityName: Option[String], val bins: Seq[(Double, V)], val nanflow: N) extends Container[IrregularlyBinned[V, N]] with NoAggregation with QuantityName {
    type Type = IrregularlyBinned[V, N]
    type EdType = IrregularlyBinned[V, N]
    def factory = IrregularlyBin

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (bins.size < 1)
      throw new ContainerException(s"number of bins (${bins.size}) must be at least 1 (including the implicit >= -inf, which the IrregularlyBin.ing factory method adds)")

    def thresholds = bins.map(_._1)
    def values = bins.map(_._2)

    def zero = new IrregularlyBinned[V, N](0.0, quantityName, bins map {case (c, v) => (c, v.zero)}, nanflow.zero)
    def +(that: IrregularlyBinned[V, N]) = {
      if (this.thresholds != that.thresholds)
        throw new ContainerException(s"cannot add ${getClass.getName} because cut thresholds differ")
      if (this.quantityName != that.quantityName)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantityName differs (${this.quantityName} vs ${that.quantityName})")
      new IrregularlyBinned(
        this.entries + that.entries,
        this.quantityName,
        this.bins zip that.bins map {case ((mycut, me), (yourcut, you)) => (mycut, me + you)},
        this.nanflow + that.nanflow)
    }
    def reweight(factor: Double) = new IrregularlyBinned[V, N](factor * entries, quantityName, bins map {case (c, v) => (c, v.reweight(factor))}, nanflow.reweight(factor))

    def children = values.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "bins:type" -> JsonString(bins.head._2.factory.name),
      "bins" -> JsonArray(bins map {case (atleast, sub) => JsonObject("atleast" -> JsonFloat(atleast), "data" -> sub.toJsonFragment(true))}: _*),
      "nanflow:type" -> JsonString(nanflow.factory.name),
      "nanflow" -> nanflow.toJsonFragment(false)).
      maybe(JsonString("name") -> (if (suppressName) None else quantityName.map(JsonString(_)))).
      maybe(JsonString("bins:name") -> (bins.head match {case (atleast, sub: QuantityName) => sub.quantityName.map(JsonString(_)); case _ => None}))

    override def toString() = s"""<IrregularlyBinned values=${bins.head._2.factory.name} thresholds=(${bins.map(_._1).mkString(", ")}) nanflow=${nanflow.factory.name}>"""
    override def equals(that: Any) = that match {
      case that: IrregularlyBinned[V, N] => this.entries === that.entries  &&  this.quantityName == that.quantityName  &&  (this.bins zip that.bins forall {case (me, you) => me._1 === you._1  &&  me._2 == you._2})  &&  this.nanflow == that.nanflow
      case _ => false
    }
    override def hashCode() = (entries, quantityName, bins, nanflow).hashCode()
  }

  /** Accumulating a suite of containers, each collecting data between a pair of given bins on a given quantity.
    * 
    * Use the factory [[org.dianahep.histogrammar.IrregularlyBin]] to construct an instance.
    * 
    * @param bins Lower thresholds and their associated containers, starting with negative infinity.
    * @param quantity Numerical quantity whose value is compared with the given thresholds.
    * @param nanflow Container for data that resulted in `NaN`.
    * @param entries Weighted number of entries (sum of all observed weights).
    */
  class IrregularlyBinning[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}] private[histogrammar](val bins: Seq[(Double, V)], val quantity: UserFcn[DATUM, Double], val nanflow: N, var entries: Double) extends Container[IrregularlyBinning[DATUM, V, N]] with AggregationOnData with NumericalQuantity[DATUM] {
    protected val v = bins.head._2
    type Type = IrregularlyBinning[DATUM, V, N]
    type EdType = IrregularlyBinned[v.EdType, nanflow.EdType]
    type Datum = DATUM
    def factory = IrregularlyBin

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (bins.size < 1)
      throw new ContainerException(s"number of bins (${bins.size}) must be at least 1 (including the implicit >= -inf, which the IrregularlyBin.ing factory method adds)")

    private val range = bins zip (bins.tail :+ (java.lang.Double.NaN, null))

    def thresholds = bins.map(_._1)
    def values = bins.map(_._2)

    def zero = new IrregularlyBinning[DATUM, V, N](bins map {case (c, v) => (c, v.zero)}, quantity, nanflow.zero, 0.0)
    def +(that: IrregularlyBinning[DATUM, V, N]) = {
      if (this.thresholds != that.thresholds)
        throw new ContainerException(s"cannot add ${getClass.getName} because cut thresholds differ")
      if (this.quantity.name != that.quantity.name)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantity name differs (${this.quantity.name} vs ${that.quantity.name})")
        new IrregularlyBinning(
          this.bins zip that.bins map {case ((mycut, me), (yourcut, you)) => (mycut, me + you)},
          this.quantity,
          this.nanflow + that.nanflow,
          this.entries + that.entries)
    }
    def reweight(factor: Double) = new IrregularlyBinning[DATUM, V, N](bins map {case (c, v) => (c, v.reweight(factor))}, quantity, nanflow.reweight(factor), factor * entries)

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      checkForCrossReferences()
      if (weight > 0.0) {
        val q = quantity(datum)
        if (q.isNaN)
          nanflow.fill(datum, weight)
        else
          // !(q >= high) is true when high == NaN (even if q == +inf)
          range find {case ((low, sub), (high, _)) => q >= low  &&  !(q >= high)} foreach {case ((_, sub), (_, _)) =>
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

    override def toString() = s"""<IrregularlyBinning values=${bins.head._2.factory.name} thresholds=(${bins.map(_._1).mkString(", ")}) nanflow=${nanflow.factory.name}>"""
    override def equals(that: Any) = that match {
      case that: IrregularlyBinning[DATUM, V, N] => this.quantity == that.quantity  &&  this.entries === that.entries  &&  (this.bins zip that.bins forall {case (me, you) => me._1 === you._1  &&  me._2 == you._2})  &&  this.nanflow == that.nanflow
      case _ => false
    }
    override def hashCode() = (quantity, entries, bins, nanflow).hashCode()
  }
}
