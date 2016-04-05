package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Averaged/Averaging

  object Averaged extends ContainerFactory {
    val name = "Averaged"

    def apply(count: Double, mean: Double) = new Averaged(count, mean)
    def unapply(x: Averaged) = Some((x.count, x.mean))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("count", "mean")) =>
        val get = pairs.toMap

        val count = get("count") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, "Averaged.count")
        }

        val mean = get("mean") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, "Averaged.mean")
        }

        new Averaged(count, mean)

      case _ => throw new JsonFormatException(json, "Averaged")
    }
  }
  class Averaged(val count: Double, val mean: Double) extends Container[Averaged] {
    def factory = Averaged

    def +(that: Averaged) = new Averaged(
      this.count + that.count,
      (this.mean*this.count + that.mean*that.count) / (this.count + that.count))

    def toJsonFragment = JsonObject("count" -> JsonFloat(count), "mean" -> JsonFloat(mean))
    override def toString() = s"Averaged"
    override def equals(that: Any) = that match {
      case that: Averaged => this.count == that.count  &&  this.mean == that.mean
      case _ => false
    }
    override def hashCode() = (count, mean).hashCode
  }

  object Averaging extends AggregatorFactory {
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new Averaging(quantity, selection, 0.0, 0.0)
    def unapply(x: Averaging[_]) = Some((x.count, x.mean))
  }
  class Averaging[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var count: Double, var mean: Double) extends Aggregator[DATUM, Averaged] {
    def fill(x: Weighted[DATUM]) {
      val y = quantity(x) reweight selection(x)

      if (y.contributes) {
        count += y.weight

        val delta = y.datum - mean
        val shift = delta * y.weight / count

        mean += shift
      }
    }

    def fix = new Averaged(count, mean)
    override def toString() = s"Averaging"
    override def equals(that: Any) = that match {
      case that: Averaging[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.count == that.count  &&  this.mean == that.mean
      case _ => false
    }
    override def hashCode() = (quantity, selection, count, mean).hashCode
  }
}
