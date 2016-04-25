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
  //////////////////////////////////////////////////////////////// Quantile/Quantiled/Quantiling

  /** Accumulate an adaptively binned histogram to compute approximate quantiles, such as the median.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Quantiling]] and immutable [[org.dianahep.histogrammar.Quantiled]] objects.
    */
  object Quantile extends Factory {
    val name = "Quantile"
    val help = "Accumulate an adaptively binned histogram to compute approximate quantiles, such as the median."
    val detailedHelp = """Quantile(quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM])"""

    /** Create an immutable [[org.dianahep.histogrammar.Quantiled]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param bins Centers and values of bins used to approximate and summarize the distribution.
      * @param min Lowest observed value; used to interpret the first bin as a finite PDF (since the first bin technically extends to minus infinity).
      * @param max Highest observed value; used to interpret the last bin as a finite PDF (since the last bin technically extends to plus infinity).
      */
    def ed(entries: Double, bins: Iterable[(Double, Counted)], min: Double, max: Double) =
      new Quantiled(new mutable.Clustering1D[Counted](100, 1.0, null.asInstanceOf[Counted], mutable.MetricSortedMap[Double, Counted](bins.toSeq: _*), min, max, entries))

    /** Create an empty, mutable [[org.dianahep.histogrammar.Quantiling]].
      * 
      * @param quantity Numerical function to track.
      * @param selection Boolean or non-negative function that cuts or weights entries.
      */
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) =
      new Quantiling[DATUM](quantity, selection, mutable.Clustering1D[Counting](100, 1.0, Count(), mutable.Clustering1D.values[Counting](), java.lang.Double.NaN, java.lang.Double.NaN, 0.0))

    /** Synonym for `apply`. */
    def ing[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) =
      apply(quantity, selection)

    /** Use [[org.dianahep.histogrammar.Quantiled]] in Scala pattern-matching. */
    def unapply(x: Quantiled) = Some((x.entries, x.bins, x.min, x.max))
    /** Use [[org.dianahep.histogrammar.Quantiling]] in Scala pattern-matching. */
    def unapply[DATUM](x: Quantiling[DATUM]) = Some((x.entries, x.bins, x.min, x.max))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("entries", "bins", "min", "max")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val factory = Factory("Count")
        val bins = get("bins") match {
          case JsonArray(bins @ _*) =>
            mutable.MetricSortedMap[Double, Counted](bins.zipWithIndex map {
              case (JsonObject(binpair @ _*), i) if (binpair.keySet == Set("center", "value")) =>
                val binget = binpair.toMap

                val center = binget("center") match {
                  case JsonNumber(x) => x
                  case x => throw new JsonFormatException(x, name + s".bins $i center")
                }

                val value = factory.fromJsonFragment(binget("value"))
                (center, value.asInstanceOf[Counted])

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

        new Quantiled(new mutable.Clustering1D[Counted](100, 1.0, null.asInstanceOf[Counted], bins.asInstanceOf[mutable.MetricSortedMap[Double, Counted]], min, max, entries))
    }
  }

  /** An accumulated adaptive histogram, used to compute approximate quantiles, such as the median.
    * 
    * Use the factory [[org.dianahep.histogrammar.Quantile]] to construct an instance.
    * 
    * @param clustering Performs the adative binning.
    */
  class Quantiled private[histogrammar](clustering: mutable.Clustering1D[Counted]) extends Container[Quantiled] with CentralBinsDistribution[Counted] {

    type Type = Quantiled
    def factory = Quantile

    if (clustering.entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    /** Weighted number of entries (sum of all observed weights). */
    def entries = clustering.entries
    /** Centers and values of bins used to approximate and summarize the distribution. */
    def bins = clustering.values
    /** Lowest observed value; used to interpret the first bin as a finite PDF (since the first bin technically extends to minus infinity). */
    def min = clustering.min
    /** Highest observed value; used to interpret the last bin as a finite PDF (since the last bin technically extends to plus infinity). */
    def max = clustering.max

    /** Location of the top of the first quartile (25% of the data have smaller values). */
    def quartile1 = qf(0.25)
    /** Location of median (50% of the data have smaller values; 50% of the data have larger values). */
    def median = qf(0.5)
    /** Location of the top of the third quartile (25% of the data have larger values). */
    def quartile3 = qf(0.75)

    /** Location of the top of the first quintile (20% of the data have smaller values). */
    def quintile1 = qf(0.20)
    /** Location of the top of the second quintile (40% of the data have smaller values). */
    def quintile2 = qf(0.40)
    /** Location of the top of the third quintile (60% of the data have smaller values). */
    def quintile3 = qf(0.60)
    /** Location of the top of the fourth quintile (80% of the data have smaller values). */
    def quintile4 = qf(0.80)

    /** Location of a given percentile (`qf` scaled by 100). */
    def percentile(x: Double) = qf(x / 100.0)
    /** Location of the given percentiles (`qf` scaled by 100). */
    def percentile(xs: Double*) = qf(xs.map(_ / 100.0): _*)

    private[histogrammar] def getClustering = clustering

    def zero = new Quantiled(mutable.Clustering1D[Counted](100, 1.0, null.asInstanceOf[Counted], mutable.Clustering1D.values[Counted](), java.lang.Double.NaN, java.lang.Double.NaN, 0.0))
    def +(that: Quantiled) =
      new Quantiled(clustering.merge(that.getClustering))

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "bins" -> JsonArray(bins.toSeq map {case (c, v) => JsonObject("center" -> JsonFloat(c), "value" -> v.toJsonFragment)}: _*),
      "min" -> JsonFloat(min),
      "max" -> JsonFloat(max))

    override def toString() = s"""Quantiled[bins=[${if (bins.isEmpty) "Counted" else bins.head._2.toString}..., size=${bins.size}]]"""
    override def equals(that: Any) = that match {
      case that: Quantiled => this.clustering == that.getClustering
      case _ => false
    }
    override def hashCode() = clustering.hashCode()
  }

  /** Accumulating an adaptive histogram, used to compute approximate quantiles, such as the median.
    * 
    * Use the factory [[org.dianahep.histogrammar.Quantile]] to construct an instance.
    * 
    * @param quantity Numerical function to track.
    * @param selection Boolean or non-negative function that cuts or weights entries.
    * @param clustering Performs the adative binning.
    */
  class Quantiling[DATUM] private[histogrammar](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], clustering: mutable.Clustering1D[Counting])
      extends Container[Quantiling[DATUM]] with AggregationOnData with CentralBinsDistribution[Counting] {

    type Type = Quantiling[DATUM]
    type Datum = DATUM
    def factory = Quantile

    if (clustering.entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    /** Weighted number of entries (sum of all observed weights). */
    def entries = clustering.entries
    /** Centers and values of bins used to approximate and summarize the distribution. */
    def bins = clustering.values
    /** Lowest observed value; used to interpret the first bin as a finite PDF (since the first bin technically extends to minus infinity). */
    def min = clustering.min
    /** Highest observed value; used to interpret the last bin as a finite PDF (since the last bin technically extends to plus infinity). */
    def max = clustering.max

    def entries_=(x: Double) {clustering.entries = x}
    def min_=(x: Double) {clustering.min = x}
    def max_=(x: Double) {clustering.max = x}

    /** Location of the top of the first quartile (25% of the data have smaller values). */
    def quartile1 = qf(0.25)
    /** Location of median (50% of the data have smaller values; 50% of the data have larger values). */
    def median = qf(0.5)
    /** Location of the top of the third quartile (25% of the data have larger values). */
    def quartile3 = qf(0.75)

    /** Location of the top of the first quintile (20% of the data have smaller values). */
    def quintile1 = qf(0.20)
    /** Location of the top of the second quintile (40% of the data have smaller values). */
    def quintile2 = qf(0.40)
    /** Location of the top of the third quintile (60% of the data have smaller values). */
    def quintile3 = qf(0.60)
    /** Location of the top of the fourth quintile (80% of the data have smaller values). */
    def quintile4 = qf(0.80)

    /** Location of a given percentile (`qf` scaled by 100). */
    def percentile(x: Double) = qf(x / 100.0)
    /** Location of the given percentiles (`qf` scaled by 100). */
    def percentile(xs: Double*) = qf(xs.map(_ / 100.0): _*)

    private[histogrammar] def getClustering = clustering

    def zero = new Quantiling[DATUM](quantity, selection, mutable.Clustering1D[Counting](100, 1.0, Count(), mutable.Clustering1D.values[Counting](), java.lang.Double.NaN, java.lang.Double.NaN, 0.0))
    def +(that: Quantiling[DATUM]) =
      new Quantiling[DATUM](quantity, selection, clustering.merge(that.getClustering))

    def fill[SUB <: DATUM](datum: SUB, weight: Double = 1.0) {
      val w = weight * selection(datum)
      if (w >= 0.0) {
        val q = quantity(datum)
        clustering.update(q, datum, w)
      }
    }

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "bins" -> JsonArray(bins.toSeq map {case (c, v) => JsonObject("center" -> JsonFloat(c), "value" -> v.toJsonFragment)}: _*),
      "min" -> JsonFloat(min),
      "max" -> JsonFloat(max))

    override def toString() = s"""Quantiling[bins=[${if (bins.isEmpty) "Counting" else bins.head._2.toString}..., size=${bins.size}]]"""
    override def equals(that: Any) = that match {
      case that: Quantiling[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.clustering == that.getClustering
      case _ => false
    }
    override def hashCode() = (quantity, selection, clustering).hashCode()
  }

}
