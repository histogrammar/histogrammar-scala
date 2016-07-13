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

import scala.language.existentials

import org.dianahep.histogrammar.json._
import org.dianahep.histogrammar.util._

package histogrammar {
  //////////////////////////////////////////////////////////////// AdaptivelyBin/AdaptivelyBinned/AdaptivelyBinning

  /** Adaptively partition a domain into bins and fill them at the same time using a clustering algorithm. Each input datum contributes to exactly one final bin.
    * 
    * The algorithm is based on [[http://www.jmlr.org/papers/volume11/ben-haim10a/ben-haim10a.pdf "A streaming parallel decision tree algorithm"]] ''J. Machine Learning Research 11 (2010)'' with a small modification for display histograms.
    * 
    * Yael Ben-Haim and Elad Tom-Tov's algorithm adds each new data point as a new bin containing a single value, then merges the closest bins if the total number of bins exceeds a maximum (like hierarchical clustering in one dimension).
    * 
    * This tends to provide the most detail on the tails of a distribution (which have the most widely spaced bins), and is therefore a good alternative to [[org.dianahep.histogrammar.Quantile]] for estimating extreme quantiles like 0.01 and 0.99.
    * 
    * However, a histogram binned this way is less interesting for visualizing a distribution. Usually, the least interesting bins are the ones with the fewest entries, so one can consider merging the bins with the fewest entries, giving no detail on the tails.
    * 
    * As a compromise, we introduce a "tail detail" hyperparameter that strikes a balance between the two extremes: the bins that are merged minimize
    * 
    * 
    * {{{tailDetail*(x2 - x1)/(max - min) + (1.0 - tailDetail)*(v1 + v2)/entries}}}
    * 
    * where `pos1` and `pos2` are the (ordered) positions of the two bins, `min` and `max` are the minimum and maximum positions of all entries, `entries1` and `entries2` are the number of entries in the two bins, and `entries` is the total number of entries in all bins. The denominators normalize the scales of domain position and number of entries so that `tailDetail` may be unitless and between 0.0 and 1.0 (inclusive).
    * 
    * A value of `tailDetail = 0.2` is a good default.
    * 
    * This algorithm is deterministic; the same input data yield the same histogram.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.AdaptivelyBinning]] and immutable [[org.dianahep.histogrammar.AdaptivelyBinned]] objects.
    */
  object AdaptivelyBin extends Factory {
    val name = "AdaptivelyBin"
    val help = "Adaptively partition a domain into bins and fill them at the same time using a clustering algorithm. Each input datum contributes to exactly one final bin."
    val detailedHelp = """The algorithm is based on [[http://www.jmlr.org/papers/volume11/ben-haim10a/ben-haim10a.pdf "A streaming parallel decision tree algorithm"]] ''J. Machine Learning Research 11 (2010)'' with a small modification for display histograms.

Yael Ben-Haim and Elad Tom-Tov's algorithm adds each new data point as a new bin containing a single value, then merges the closest bins if the total number of bins exceeds a maximum (like hierarchical clustering in one dimension).

This tends to provide the most detail on the tails of a distribution (which have the most widely spaced bins), and is therefore a good alternative to [[org.dianahep.histogrammar.Quantile]] for estimating extreme quantiles like 0.01 and 0.99.

However, a histogram binned this way is less interesting for visualizing a distribution. Usually, the least interesting bins are the ones with the fewest entries, so one can consider merging the bins with the fewest entries, giving no detail on the tails.

As a compromise, we introduce a "tail detail" hyperparameter that strikes a balance between the two extremes: the bins that are merged minimize


{{{tailDetail*(x2 - x1)/(max - min) + (1.0 - tailDetail)*(v1 + v2)/entries}}}

where `pos1` and `pos2` are the (ordered) positions of the two bins, `min` and `max` are the minimum and maximum positions of all entries, `entries1` and `entries2` are the number of entries in the two bins, and `entries` is the total number of entries in all bins. The denominators normalize the scales of domain position and number of entries so that `tailDetail` may be unitless and between 0.0 and 1.0 (inclusive).

A value of `tailDetail = 0.2` is a good default.

This algorithm is deterministic; the same input data yield the same histogram."""

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
    def ed[V <: Container[V] with NoAggregation, N <: Container[N] with NoAggregation](entries: Double, num: Int, tailDetail: Double, contentType: String, bins: Iterable[(Double, V)], min: Double, max: Double, nanflow: N) =
      new AdaptivelyBinned[V, N](contentType, new mutable.Clustering1D[V](num, tailDetail, null.asInstanceOf[V], mutable.MetricSortedMap[Double, V](bins.toSeq: _*), min, max, entries), None, nanflow)

    /** Create an empty, mutable [[org.dianahep.histogrammar.AdaptivelyBinning]].
      * 
      * @param quantity Numerical function to split into bins.
      * @param num Maximum number of bins (used as a constraint when growing or merging).
      * @param tailDetail Between 0.0 and 1.0 inclusive: use 0.0 to focus on the bulk of the distribution and 1.0 to focus on the tails; see [[org.dianahep.histogrammar.util.mutable.Clustering1D]] for details.
      * @param value Template used to create zero values (by calling this `value`'s `zero` method).
      * @param nanflow Container for data that result in `NaN`.
      */
    def apply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}]
      (quantity: UserFcn[DATUM, Double], num: Int = 100, tailDetail: Double = 0.2, value: => V = Count(), nanflow: N = Count()) =
      new AdaptivelyBinning[DATUM, V, N](quantity, value, mutable.Clustering1D[V](num, tailDetail, value, mutable.Clustering1D.values[V](), java.lang.Double.NaN, java.lang.Double.NaN, 0.0), nanflow)

    /** Synonym for `apply`. */
    def ing[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}]
      (quantity: UserFcn[DATUM, Double], num: Int = 100, tailDetail: Double = 0.2, value: => V = Count(), nanflow: N = Count()) =
      apply(quantity, num, tailDetail, value, nanflow)

    import KeySetComparisons._
    def fromJsonFragment(json: Json, nameFromParent: Option[String]): Container[_] with NoAggregation = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("entries", "num", "bins:type", "bins", "min", "max", "nanflow:type", "nanflow", "tailDetail").maybe("name").maybe("bins:name")) =>
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

        val num = get("num") match {
          case JsonInt(x) => x
          case x => throw new JsonFormatException(x, name + ".num")
        }

        val (contentType, binsFactory) = get("bins:type") match {
          case JsonString(name) => (name, Factory(name))
          case x => throw new JsonFormatException(x, name + ".bins:type")
        }
        val binsName = get.getOrElse("bins:name", JsonNull) match {
          case JsonString(x) => Some(x)
          case JsonNull => None
          case x => throw new JsonFormatException(x, name + ".bins:name")
        }
        val bins = get("bins") match {
          case JsonArray(bins @ _*) =>
            mutable.MetricSortedMap[Double, Container[_]](bins.zipWithIndex map {
              case (JsonObject(binpair @ _*), i) if (binpair.keySet has Set("center", "value")) =>
                val binget = binpair.toMap

                val center = binget("center") match {
                  case JsonNumber(x) => x
                  case x => throw new JsonFormatException(x, name + s".bins $i center")
                }

                val value = binsFactory.fromJsonFragment(binget("value"), binsName)
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
        val nanflow = nanflowFactory.fromJsonFragment(get("nanflow"), None)

        val tailDetail = get("tailDetail") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".tailDetail")
        }

        new AdaptivelyBinned[Container[_], Container[_]](contentType, new mutable.Clustering1D[Container[_]](num.toInt, tailDetail, null.asInstanceOf[Container[_]], bins.asInstanceOf[mutable.MetricSortedMap[Double, Container[_]]], min, max, entries), (nameFromParent ++ quantityName).lastOption, nanflow)

      case _ => throw new JsonFormatException(json, name)
    }
  }

  /** An accumulated quantity that was split dynamically into bins with a clustering algorithm, with only one datum filled per bin and no overflows or underflows.
    * 
    * Use the factory [[org.dianahep.histogrammar.AdaptivelyBin]] to construct an instance.
    * 
    * @param contentType Name of the intended content; used as a placeholder in cases with zero bins (due to no observed data).
    * @param clustering Performs the adative binning.
    * @param quantityName Optional name given to the quantity function, passed for bookkeeping.
    * @param nanflow Container for data that resulted in `NaN`.
    */
  class AdaptivelyBinned[V <: Container[V] with NoAggregation, N <: Container[N] with NoAggregation] private[histogrammar](contentType: String, clustering: mutable.Clustering1D[V], val quantityName: Option[String], val nanflow: N)
    extends Container[AdaptivelyBinned[V, N]] with NoAggregation with CentrallyBin.Methods[V] with QuantityName {

    type Type = AdaptivelyBinned[V, N]
    type EdType = AdaptivelyBinned[V, N]
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

    def zero = new AdaptivelyBinned[V, N](contentType, mutable.Clustering1D[V](num, tailDetail, null.asInstanceOf[V], mutable.Clustering1D.values[V](), java.lang.Double.NaN, java.lang.Double.NaN, 0.0), quantityName, nanflow.zero)
    def +(that: AdaptivelyBinned[V, N]) = {
      if (this.quantityName != that.quantityName)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantityName differs (${this.quantityName} vs ${that.quantityName})")
      if (this.num != that.num)
        throw new ContainerException(s"cannot add ${getClass.getName} because number of bins is different (${this.num} vs ${that.num})")
      if (this.tailDetail != that.tailDetail)
        throw new ContainerException(s"cannot add ${getClass.getName} because tailDetail parameter is different (${this.tailDetail} vs ${that.tailDetail})")

      new AdaptivelyBinned[V, N](contentType, clustering.merge(that.getClustering), this.quantityName, this.nanflow + that.nanflow)
    }

    def children = nanflow :: values.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "num" -> JsonInt(num),
      "bins:type" -> JsonString(contentType),
      "bins" -> JsonArray(bins.toSeq map {case (c, v) => JsonObject("center" -> JsonFloat(c), "value" -> v.toJsonFragment(true))}: _*),
      "min" -> JsonFloat(min),
      "max" -> JsonFloat(max),
      "nanflow:type" -> JsonString(nanflow.factory.name),
      "nanflow" -> nanflow.toJsonFragment(false),
      "tailDetail" -> JsonFloat(tailDetail)).
      maybe(JsonString("name") -> (if (suppressName) None else quantityName.map(JsonString(_)))).
      maybe(JsonString("bins:name") -> (bins.headOption match {case Some((c, v: QuantityName)) => v.quantityName.map(JsonString(_)); case _ => None}))

    override def toString() = s"""<AdaptivelyBinned bins=$contentType size=${bins.size} nanflow=${nanflow.factory.name}>"""
    override def equals(that: Any) = that match {
      case that: AdaptivelyBinned[V, N] => this.clustering == that.getClustering  &&  this.quantityName == that.quantityName  &&  this.nanflow == that.nanflow
      case _ => false
    }
    override def hashCode() = (clustering, quantityName, nanflow).hashCode()
  }

  /** Accumulating a quantity by splitting it dynamically into bins with a clustering algorithm, filling only one datum per bin with no overflows or underflows.
    * 
    * Use the factory [[org.dianahep.histogrammar.AdaptivelyBin]] to construct an instance.
    * 
    * @param quantity Numerical function to track.
    * @param value New value (note the `=>`: expression is reevaluated every time a new value is needed).
    * @param clustering Performs the adative binning.
    * @param nanflow Container for data that result in `NaN`.
    */
  class AdaptivelyBinning[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}] private[histogrammar]
    (val quantity: UserFcn[DATUM, Double], value: => V, clustering: mutable.Clustering1D[V], val nanflow: N)
      extends Container[AdaptivelyBinning[DATUM, V, N]] with AggregationOnData with NumericalQuantity[DATUM] with CentrallyBin.Methods[V] {

    protected val v = value
    type Type = AdaptivelyBinning[DATUM, V, N]
    type EdType = AdaptivelyBinned[v.EdType, nanflow.EdType]
    type Datum = DATUM
    def factory = AdaptivelyBin

    if (clustering.entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (clustering.num < 2)
      throw new ContainerException(s"number of bins ($num) must be at least two")
    if (clustering.tailDetail < 0.0  ||  clustering.tailDetail > 1.0)
      throw new ContainerException(s"tailDetail parameter ($tailDetail) must be between 0.0 and 1.0 inclusive")

    def entries = clustering.entries
    /** Maximum number of bins (used as a constraint when filling and merging). */
    def num = clustering.num
    /** Clustering hyperparameter, between 0.0 and 1.0 inclusive: use 0.0 to focus on the bulk of the distribution and 1.0 to focus on the tails; see [[org.dianahep.histogrammar.util.mutable.Clustering1D]] for details. */
    def tailDetail = clustering.tailDetail
    def bins = clustering.values
    def min = clustering.min
    def max = clustering.max
    def entries_=(x: Double) {clustering.entries = x}
    def min_=(x: Double) {clustering.min = x}
    def max_=(x: Double) {clustering.max = x}
    private[histogrammar] def getClustering = clustering

    def zero = new AdaptivelyBinning[DATUM, V, N](quantity, value, mutable.Clustering1D[V](num, tailDetail, value, mutable.Clustering1D.values[V](), java.lang.Double.NaN, java.lang.Double.NaN, 0.0), nanflow.zero)
    def +(that: AdaptivelyBinning[DATUM, V, N]) = {
      if (this.quantity.name != that.quantity.name)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantity name differs (${this.quantity.name} vs ${that.quantity.name})")
      if (this.num != that.num)
        throw new ContainerException(s"cannot add ${getClass.getName} because number of bins is different (${this.num} vs ${that.num})")
      if (this.tailDetail != that.tailDetail)
        throw new ContainerException(s"cannot add ${getClass.getName} because tailDetail parameter is different (${this.tailDetail} vs ${that.tailDetail})")

      new AdaptivelyBinning[DATUM, V, N](quantity, value, clustering.merge(that.getClustering), this.nanflow + that.nanflow)
    }

    def fill[SUB <: DATUM](datum: SUB, weight: Double = 1.0) {
      checkForCrossReferences()
      if (weight > 0.0) {
        val q = quantity(datum)
        clustering.update(q, datum, weight)
      }
    }

    def children = value :: nanflow :: values.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "num" -> JsonInt(num),
      "bins:type" -> JsonString(value.factory.name),
      "bins" -> JsonArray(bins.toSeq map {case (c, v) => JsonObject("center" -> JsonFloat(c), "value" -> v.toJsonFragment(true))}: _*),
      "min" -> JsonFloat(min),
      "max" -> JsonFloat(max),
      "nanflow:type" -> JsonString(nanflow.factory.name),
      "nanflow" -> nanflow.toJsonFragment(false),
      "tailDetail" -> JsonFloat(tailDetail)).
      maybe(JsonString("name") -> (if (suppressName) None else quantity.name.map(JsonString(_)))).
      maybe(JsonString("bins:name") -> List(value).collect({case v: AnyQuantity[_, _] => v.quantity.name}).headOption.flatten.map(JsonString(_)))

    override def toString() = s"""<AdaptivelyBinning bins=${value.factory.name} size=${bins.size} nanflow=${nanflow.factory.name}>"""
    override def equals(that: Any) = that match {
      case that: AdaptivelyBinning[DATUM, V, N] => this.quantity == that.quantity  &&  this.clustering == that.getClustering  &&  this.nanflow == that.nanflow
      case _ => false
    }
    override def hashCode() = (quantity, clustering, nanflow).hashCode()
  }
}