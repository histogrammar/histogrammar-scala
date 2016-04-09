package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Average/Averaged/Averaging

  object Average extends Factory {
    val name = "Average"
    val help = "Accumulate a count and a weighted mean of a given quantity."
    val detailedHelp = """Average(quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM])"""

    def container(count: Double, mean: Double) = new Averaged(count, mean)
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new Averaging(quantity, selection, 0.0, 0.0)

    def unapply(x: Averaged) = Some((x.count, x.mean))
    def unapply(x: Averaging[_]) = Some((x.count, x.mean))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("count", "mean")) =>
        val get = pairs.toMap

        val count = get("count") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".count")
        }

        val mean = get("mean") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".mean")
        }

        new Averaged(count, mean)

      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def plus(ca: Double, mua: Double, cb: Double, mub: Double) =
      (ca + cb, (ca*mua + cb*mub)/(ca + cb))
  }

  class Averaged(val count: Double, val mean: Double) extends Container[Averaged] {
    def factory = Average

    def +(that: Averaged) = {
      val (newcount, newmean) = Average.plus(this.count, this.mean, that.count, that.mean)
      new Averaged(newcount, newmean)
    }

    def toJsonFragment = JsonObject("count" -> JsonFloat(count), "mean" -> JsonFloat(mean))

    override def toString() = s"Averaged"
    override def equals(that: Any) = that match {
      case that: Averaged => this.count === that.count  &&  this.mean === that.mean
      case _ => false
    }
    override def hashCode() = (count, mean).hashCode
  }

  class Averaging[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var count: Double, var mean: Double) extends Aggregator[DATUM, Averaging[DATUM]] {
    def factory = Average

    def +(that: Averaging[DATUM]) = {
      val (newcount, newmean) = Average.plus(this.count, this.mean, that.count, that.mean)
      new Averaging(this.quantity, this.selection, newcount, newmean)
    }

    def fillWeighted[SUB <: DATUM](datum: SUB, weight: Double) {
      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)

        count += w

        val delta = q - mean
        val shift = delta * w / count

        mean += shift
      }
    }

    def toJsonFragment = JsonObject("count" -> JsonFloat(count), "mean" -> JsonFloat(mean))

    override def toString() = s"Averaging"
    override def equals(that: Any) = that match {
      case that: Averaging[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.count === that.count  &&  this.mean === that.mean
      case _ => false
    }
    override def hashCode() = (quantity, selection, count, mean).hashCode
  }
}
