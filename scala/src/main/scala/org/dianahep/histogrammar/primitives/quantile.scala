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

  object Quantile extends Factory {
    val name = "Quantile"
    val help = "Accumulate an adaptively binned histogram to compute approximate quantiles, such as the median."
    val detailedHelp = """Quantile"""

    def ed(entries: Double, bins: Iterable[(Double, Counted)], min: Double, max: Double) =
      new Quantiled(new mutable.Clustering1D[Counted](100, 1.0, null.asInstanceOf[Counted], mutable.MetricSortedMap[Double, Counted](bins.toSeq: _*), min, max, entries))

    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) =
      new Quantiling[DATUM](quantity, selection, mutable.Clustering1D[Counting](100, 1.0, Count(), mutable.Clustering1D.values[Counting](), java.lang.Double.NaN, java.lang.Double.NaN, 0.0))

    def unapply(x: Quantiled) = Some((x.entries, x.bins, x.min, x.max))
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

  class Quantiled(clustering: mutable.Clustering1D[Counted]) extends Container[Quantiled] with CentralBinsDistribution[Counted] {

    type Type = Quantiled
    def factory = Quantile

    if (clustering.entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def entries = clustering.entries
    def bins = clustering.values
    def min = clustering.min
    def max = clustering.max
    def quartile1 = qf(0.25)
    def median = qf(0.5)
    def quartile3 = qf(0.75)
    def percentile(x: Double) = qf(x / 100.0)
    private[histogrammar] def getClustering = clustering

    def zero = new Quantiled(mutable.Clustering1D[Counted](100, 1.0, null.asInstanceOf[Counted], mutable.Clustering1D.values[Counted](), java.lang.Double.NaN, java.lang.Double.NaN, 0.0))
    def +(that: Quantiled) =
      new Quantiled(clustering.merge(that.getClustering))

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "bins" -> JsonArray(bins.toSeq map {case (c, v) => JsonObject("center" -> JsonFloat(c), "value" -> v.toJsonFragment)}: _*),
      "min" -> JsonFloat(min),
      "max" -> JsonFloat(max))

    override def toString() = s"""Quantiled[entries=$entries, bins=[${if (bins.isEmpty) "Counted" else bins.head._2.toString}..., size=${bins.size}]]"""
    override def equals(that: Any) = that match {
      case that: Quantiled => this.clustering == that.getClustering
      case _ => false
    }
    override def hashCode() = clustering.hashCode()
  }

  class Quantiling[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], clustering: mutable.Clustering1D[Counting])
      extends Container[Quantiling[DATUM]] with AggregationOnData with CentralBinsDistribution[Counting] {

    type Type = Quantiling[DATUM]
    type Datum = DATUM
    def factory = Quantile

    if (clustering.entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def entries = clustering.entries
    def bins = clustering.values
    def min = clustering.min
    def max = clustering.max
    def entries_=(x: Double) {clustering.entries = x}
    def min_=(x: Double) {clustering.min = x}
    def max_=(x: Double) {clustering.max = x}
    def quartile1 = qf(0.25)
    def median = qf(0.5)
    def quartile3 = qf(0.75)
    def percentile(x: Double) = qf(x / 100.0)
    private[histogrammar] def getClustering = clustering

    def zero = new Quantiling[DATUM](quantity, selection, mutable.Clustering1D[Counting](100, 1.0, Count(), mutable.Clustering1D.values[Counting](), java.lang.Double.NaN, java.lang.Double.NaN, 0.0))
    def +(that: Quantiling[DATUM]) =
      new Quantiling[DATUM](quantity, selection, clustering.merge(that.getClustering))

    def fillWeighted[SUB <: DATUM](datum: SUB, weight: Double) {
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

    override def toString() = s"""Quantiling[entries=$entries, bins=[${if (bins.isEmpty) "Counting" else bins.head._2.toString}..., size=${bins.size}]]"""
    override def equals(that: Any) = that match {
      case that: Quantiling[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.clustering == that.getClustering
      case _ => false
    }
    override def hashCode() = (quantity, selection, clustering).hashCode()
  }

}
