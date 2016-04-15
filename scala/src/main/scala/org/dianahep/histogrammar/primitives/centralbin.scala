package org.dianahep

import org.dianahep.histogrammar.json._
import org.dianahep.histogrammar.util._

package histogrammar {
  //////////////////////////////////////////////////////////////// CentrallyBin/CentrallyBinned/CentrallyBinning

  object CentrallyBin extends Factory {
    val name = "CentrallyBin"
    val help = "Split a quantity into bins defined by a set of bin centers, filling only one datum per bin with no overflows or underflows."
    val detailedHelp = """HERE"""

    def fixed[V <: Container[V], N <: Container[N]](entries: Double, bins: Iterable[(Double, V)], min: Double, max: Double, nanflow: N) =
      new CentrallyBinned[V, N](entries, immutable.MetricSortedMap(bins: _*), min, max, nanflow)

    def apply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}]
      (bins: Iterable[Double], quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM], value: => V = Count(), nanflow: N = Count())
      new CentrallyBinning[DATUM, V, N](quantity, selection, 0.0, value, mutable.MetricSortedMap(bins.map((_, value)): _*), bins.min, bins.max, nanflow)

    def unapply[V <: Container[V], N <: Container[N]](x: CentrallyBinned[V, N]) = Some((x.entries, x.bins, x.min, x.max, x.nanflow))
    def unapply[V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](x: CentrallyBinning[V, N]) = Some((x.entries, x.bins, x.min, x.max, x.nanflow))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("entries", "bins:type", "bins", "min", "max", "nanflow:type", "nanflow")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val binsFactory = get("bins:type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, name + ".bins:type")
        }
        val bins = get("bins") match {
          case JsonArray(bins @ _*) if (bins.size >= 2) =>
            immutable.MetricSortedMap(bins.zipWithIndex map {
              case (JsonObject(binpair @ _*), i) if (binpair.keySet == Set("center", "value")) =>
                val binget = binpair.toMap

                val center = binget("center") match {
                  case JsonNumber(x) => x
                  case x => throw new JsonFormatException(x, name + s".bins $i center")
                }

                val value = binsFactory.fromJsonFragment(binget("value"))

                (center, value)

              case x => throw new JsonFormatException(x, name + s".bins $i")
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

        new CentrallyBinned[V, N](entries, bins, min, max, nanflow)

      case _ => throw new JsonFormatException(json, name)
    }
  }

  class CentrallyBinned[V <: Container[V], N <: Container[N]](val entries: Double, val bins: immutable.MetricSortedMap[Double, V], val min: Double, val max: Double, val nanflow: N) with CentralBinsDistribution[V] {
    type Type = CentrallyBinned[V, N]
    def factory = CentrallyBinned

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def centersSet = bins.iterator.map(_._1).toSet
    def centers = bins.iterator.map(_._1).toSeq
    def values = bins.iterator.map(_._2).toSeq

    def zero = new CentrallyBinned[V, N](0.0, immutable.MetricSortedMap[Double, V](bins.map({case (c, v) => (c, v.zero)}): _*), bins.head._1, bins.last._1, nanflow.zero)
    def +(that: CentrallyBinned[V, N]) = {
      if (this.centers != that.centers)
        throw new ContainerException(s"cannot add CentrallyBinned because centers are different:\n    ${this.centers}\nvs\n    ${that.centers}")

      val newbins = immutable.MetricSortedMap(this.bins zip that.bins map {case ((c1, v1), (_, v2)) => (c1, v1 + v2)}: _*)

      new CentrallyBinned[V, N](this.entries + that.entries, newbins, Math.min(this.min, that.min), Math.max(this.max, that.max), this.nanflow + that.nanflow)
    }

    def toJsonFragment = JsonObject(
      // HERE !!!


    )

  }

}
