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

import scala.language.existentials

import org.dianahep.histogrammar.json._
import org.dianahep.histogrammar.util._

package histogrammar {
  //////////////////////////////////////////////////////////////// AdaptivelyBin/AdaptivelyBinned/AdaptivelyBinning

  /** Split a quanity into bins dynamically with a clustering algorithm, filling only one datum per bin with no overflows or underflows.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.AdaptivelyBinning]] and immutable [[org.dianahep.histogrammar.AdaptivelyBinned]] objects.
    */
  object AdaptivelyBin extends Factory {
    val name = "AdaptivelyBin"
    val help = "Split a quanity into bins dynamically with a clustering algorithm, filling only one datum per bin with no overflows or underflows."
    val detailedHelp = """AdaptivelyBin(quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM], num: Int = 100, tailDetail: Double = 0.2, value: => V = Count(), nanflow: N = Count())"""

    /** Create an immutable [[org.dianahep.histogrammar.AdaptivelyBinned]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param num Maximum number of bins (used as a constraint when merging).
      * @param tailDetail Between 0.0 and 1.0 inclusive: use 0.0 to focus on the bulk of the distribution and 1.0 to focus on the tails; see [[org.dianahep.histogrammar.util.mutable.Clustering1D]] for details.
      * @param contentType Name of the intended content; used as a placeholder in cases with zero bins (due to no observed data).
      * @param bins Centers and values of each bin.
      * @param min Lowest observed value; used to interpret the first bin as a finite PDF (since the first bin technically extends to minus infinity).
      * @param max Highest observed value; used to interpret the last bin as a finite PDF (since the last bin technically extends to plus infinity).
      * @param nanflow Container for data that resulted in `NaN`.
      */
    def ed[V <: Container[V], N <: Container[N]](entries: Double, num: Int, tailDetail: Double, contentType: String, bins: Iterable[(Double, V)], min: Double, max: Double, nanflow: N) =
      new AdaptivelyBinned[V, N](contentType, new mutable.Clustering1D[V](num, tailDetail, null.asInstanceOf[V], mutable.MetricSortedMap[Double, V](bins.toSeq: _*), min, max, entries), nanflow)

    /** Create an empty, mutable [[org.dianahep.histogrammar.AdaptivelyBinning]].
      * 
      * @param quantity Numerical function to split into bins.
      * @param selection Boolean or non-negative function that cuts or weights entries.
      * @param num Maximum number of bins (used as a constraint when growing or merging).
      * @param tailDetail Between 0.0 and 1.0 inclusive: use 0.0 to focus on the bulk of the distribution and 1.0 to focus on the tails; see [[org.dianahep.histogrammar.util.mutable.Clustering1D]] for details.
      * @param value New value (note the `=>`: expression is reevaluated every time a new value is needed).
      * @param nanflow Container for data that result in `NaN`.
      */
    def apply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}]
      (quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM], num: Int = 100, tailDetail: Double = 0.2, value: => V = Count(), nanflow: N = Count()) =
      new AdaptivelyBinning[DATUM, V, N](quantity, selection, value, mutable.Clustering1D[V](num, tailDetail, value, mutable.Clustering1D.values[V](), java.lang.Double.NaN, java.lang.Double.NaN, 0.0), nanflow)

    /** Synonym for `apply`. */
    def ing[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}]
      (quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM], num: Int = 100, tailDetail: Double = 0.2, value: => V = Count(), nanflow: N = Count()) =
      apply(quantity, selection, num, tailDetail, value, nanflow)

    /** Use [[org.dianahep.histogrammar.AdaptivelyBinned]] in Scala pattern-matching. */
    def unapply[V <: Container[V], N <: Container[N]](x: AdaptivelyBinned[V, N]) =
      Some((x.entries, x.num, x.tailDetail, x.bins, x.min, x.max, x.nanflow))
    /** Use [[org.dianahep.histogrammar.AdaptivelyBinning]] in Scala pattern-matching. */
    def unapply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](x: AdaptivelyBinning[DATUM, V, N]) =
      Some((x.entries, x.num, x.tailDetail, x.bins, x.min, x.max, x.nanflow))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("entries", "num", "bins:type", "bins", "min", "max", "nanflow:type", "nanflow", "tailDetail")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val num = get("num") match {
          case JsonInt(x) => x
          case x => throw new JsonFormatException(x, name + ".num")
        }

        val (contentType, binsFactory) = get("bins:type") match {
          case JsonString(name) => (name, Factory(name))
          case x => throw new JsonFormatException(x, name + ".bins:type")
        }
        val bins = get("bins") match {
          case JsonArray(bins @ _*) =>
            mutable.MetricSortedMap[Double, Container[_]](bins.zipWithIndex map {
              case (JsonObject(binpair @ _*), i) if (binpair.keySet == Set("center", "value")) =>
                val binget = binpair.toMap

                val center = binget("center") match {
                  case JsonNumber(x) => x
                  case x => throw new JsonFormatException(x, name + s".bins $i center")
                }

                val value = binsFactory.fromJsonFragment(binget("value"))
                (center, value.asInstanceOf[Container[_]])

              case (x, i) => throw new JsonFormatException(x, name + s".bins $i")
            }: _*)

          case x => throw new JsonFormatException(x, name + ".bins")
        }

        val min = get("min") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".min")
        }

        val max = get("max") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".max")
        }

        val nanflowFactory = get("nanflow:type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, name + ".nanflow:type")
        }
        val nanflow = nanflowFactory.fromJsonFragment(get("nanflow"))

        val tailDetail = get("tailDetail") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".tailDetail")
        }

        new AdaptivelyBinned[Container[_], Container[_]](contentType, new mutable.Clustering1D[Container[_]](num.toInt, tailDetail, null.asInstanceOf[Container[_]], bins.asInstanceOf[mutable.MetricSortedMap[Double, Container[_]]], min, max, entries), nanflow)
    }
  }

  /** An accumulated quantity that was split dynamically into bins with a clustering algorithm, with only one datum filled per bin and no overflows or underflows.
    * 
    * @param contentType Name of the intended content; used as a placeholder in cases with zero bins (due to no observed data).
    * @param clustering Performs the adative binning.
    * @param nanflow Container for data that resulted in `NaN`.
    */
  class AdaptivelyBinned[V <: Container[V], N <: Container[N]](contentType: String, clustering: mutable.Clustering1D[V], val nanflow: N)
    extends Container[AdaptivelyBinned[V, N]] with CentrallyBin.Methods[V] {

    type Type = AdaptivelyBinned[V, N]
    def factory = AdaptivelyBin

    if (clustering.entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (clustering.num < 2)
      throw new ContainerException(s"number of bins ($num) must be at least two")
    if (clustering.tailDetail < 0.0  ||  clustering.tailDetail > 1.0)
      throw new ContainerException(s"tailDetail parameter ($tailDetail) must be between 0.0 and 1.0 inclusive")

    /** Maximum number of bins (used as a constraint when merging). */
    def num = clustering.num
    /** Clustering hyperparameter, between 0.0 and 1.0 inclusive: use 0.0 to focus on the bulk of the distribution and 1.0 to focus on the tails; see [[org.dianahep.histogrammar.util.mutable.Clustering1D]] for details. */
    def tailDetail = clustering.tailDetail
    def entries = clustering.entries
    def bins = clustering.values
    def min = clustering.min
    def max = clustering.max
    private[histogrammar] def getClustering = clustering

    def zero = new AdaptivelyBinned[V, N](contentType, mutable.Clustering1D[V](num, tailDetail, null.asInstanceOf[V], mutable.Clustering1D.values[V](), java.lang.Double.NaN, java.lang.Double.NaN, 0.0), nanflow.zero)
    def +(that: AdaptivelyBinned[V, N]) = {
      if (this.num != that.num)
        throw new ContainerException(s"cannot add AdaptivelyBinned because number of bins is different (${this.num} vs ${that.num})")
      if (this.tailDetail != that.tailDetail)
        throw new ContainerException(s"cannot add AdaptivelyBinned because tailDetail parameter is different (${this.tailDetail} vs ${that.tailDetail})")

      new AdaptivelyBinned[V, N](contentType, clustering.merge(that.getClustering), this.nanflow + that.nanflow)
    }

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "num" -> JsonInt(num),
      "bins:type" -> JsonString(contentType),
      "bins" -> JsonArray(bins.toSeq map {case (c, v) => JsonObject("center" -> JsonFloat(c), "value" -> v.toJsonFragment)}: _*),
      "min" -> JsonFloat(min),
      "max" -> JsonFloat(max),
      "nanflow:type" -> JsonString(nanflow.factory.name),
      "nanflow" -> nanflow.toJsonFragment,
      "tailDetail" -> JsonFloat(tailDetail))

    override def toString() = s"""AdaptivelyBinned[bins=[${if (bins.isEmpty) contentType else bins.head._2.toString}..., size=${bins.size}, num=$num], nanflow=$nanflow]"""
    override def equals(that: Any) = that match {
      case that: AdaptivelyBinned[V, N] => this.clustering == that.getClustering  &&  this.nanflow == that.nanflow
      case _ => false
    }
    override def hashCode() = (clustering, nanflow).hashCode()
  }

  /** Accumulating a quantity by splitting it dynamically into bins with a clustering algorithm, filling only one datum per bin with no overflows or underflows.
    * 
    * @param quantity Numerical function to track.
    * @param selection Boolean or non-negative function that cuts or weights entries.
    * @param value New value (note the `=>`: expression is reevaluated every time a new value is needed).
    * @param clustering Performs the adative binning.
    * @param nanflow Container for data that result in `NaN`.
    */
  class AdaptivelyBinning[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}]
    (val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], value: => V, clustering: mutable.Clustering1D[V], val nanflow: N)
      extends Container[AdaptivelyBinning[DATUM, V, N]] with AggregationOnData with CentrallyBin.Methods[V] {

    type Type = AdaptivelyBinning[DATUM, V, N]
    type Datum = DATUM
    def factory = AdaptivelyBin

    if (clustering.entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (clustering.num < 2)
      throw new ContainerException(s"number of bins ($num) must be at least two")
    if (clustering.tailDetail < 0.0  ||  clustering.tailDetail > 1.0)
      throw new ContainerException(s"tailDetail parameter ($tailDetail) must be between 0.0 and 1.0 inclusive")

    /** Maximum number of bins (used as a constraint when filling and merging). */
    def num = clustering.num
    /** Clustering hyperparameter, between 0.0 and 1.0 inclusive: use 0.0 to focus on the bulk of the distribution and 1.0 to focus on the tails; see [[org.dianahep.histogrammar.util.mutable.Clustering1D]] for details. */
    def tailDetail = clustering.tailDetail
    def entries = clustering.entries
    def bins = clustering.values
    def min = clustering.min
    def max = clustering.max
    def entries_=(x: Double) {clustering.entries = x}
    def min_=(x: Double) {clustering.min = x}
    def max_=(x: Double) {clustering.max = x}
    private[histogrammar] def getClustering = clustering

    def zero = new AdaptivelyBinning[DATUM, V, N](quantity, selection, value, mutable.Clustering1D[V](num, tailDetail, value, mutable.Clustering1D.values[V](), java.lang.Double.NaN, java.lang.Double.NaN, 0.0), nanflow.zero)
    def +(that: AdaptivelyBinning[DATUM, V, N]) = {
      if (this.num != that.num)
        throw new ContainerException(s"cannot add AdaptivelyBinning because number of bins is different (${this.num} vs ${that.num})")
      if (this.tailDetail != that.tailDetail)
        throw new ContainerException(s"cannot add AdaptivelyBinning because tailDetail parameter is different (${this.tailDetail} vs ${that.tailDetail})")

      new AdaptivelyBinning[DATUM, V, N](quantity, selection, value, clustering.merge(that.getClustering), this.nanflow + that.nanflow)
    }

    def fill[SUB <: DATUM](datum: SUB, weight: Double = 1.0) {
      val w = weight * selection(datum)
      if (w >= 0.0) {
        val q = quantity(datum)
        clustering.update(q, datum, w)
      }
    }

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "num" -> JsonInt(num),
      "bins:type" -> JsonString(value.factory.name),
      "bins" -> JsonArray(bins.toSeq map {case (c, v) => JsonObject("center" -> JsonFloat(c), "value" -> v.toJsonFragment)}: _*),
      "min" -> JsonFloat(min),
      "max" -> JsonFloat(max),
      "nanflow:type" -> JsonString(nanflow.factory.name),
      "nanflow" -> nanflow.toJsonFragment,
      "tailDetail" -> JsonFloat(tailDetail))

    override def toString() = s"""AdaptivelyBinning[bins=[${if (bins.isEmpty) value.factory.name else bins.head._2.toString}..., size=${bins.size}, num=$num], nanflow=$nanflow]"""
    override def equals(that: Any) = that match {
      case that: AdaptivelyBinning[DATUM, V, N] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.clustering == that.getClustering  &&  this.nanflow == that.nanflow
      case _ => false
    }
    override def hashCode() = (quantity, selection, clustering, nanflow).hashCode()
  }

}
