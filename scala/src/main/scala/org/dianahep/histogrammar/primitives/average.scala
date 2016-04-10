package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Average/Averaged/Averaging

  object Average extends Factory {
    val name = "Average"
    val help = "Accumulate a weighted mean and total weight of a given quantity."
    val detailedHelp = """Average(quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM])"""

    def fixed(totalWeight: Double, mean: Double) = new Averaged(totalWeight, mean)
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new Averaging(quantity, selection, 0.0, 0.0)

    def unapply(x: Averaged) = Some((x.totalWeight, x.mean))
    def unapply[DATUM](x: Averaging[DATUM]) = Some((x.totalWeight, x.mean))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("totalWeight", "mean")) =>
        val get = pairs.toMap

        val totalWeight = get("totalWeight") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".totalWeight")
        }

        val mean = get("mean") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".mean")
        }

        new Averaged(totalWeight, mean)

      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def plus(ca: Double, mua: Double, cb: Double, mub: Double) =
      (ca + cb, (ca*mua + cb*mub)/(ca + cb))
  }

  class Averaged(val totalWeight: Double, val mean: Double) extends Container[Averaged] {
    type Type = Averaged
    def factory = Average

    def +(that: Averaged) = {
      val (newtotalWeight, newmean) = Average.plus(this.totalWeight, this.mean, that.totalWeight, that.mean)
      new Averaged(newtotalWeight, newmean)
    }

    def toJsonFragment = JsonObject("totalWeight" -> JsonFloat(totalWeight), "mean" -> JsonFloat(mean))

    override def toString() = s"Averaged($totalWeight, $mean)"
    override def equals(that: Any) = that match {
      case that: Averaged => this.totalWeight === that.totalWeight  &&  this.mean === that.mean
      case _ => false
    }
    override def hashCode() = (totalWeight, mean).hashCode
  }

  class Averaging[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var totalWeight: Double, var mean: Double) extends Container[Averaging[DATUM]] with AggregationOnData {
    type Type = Averaging[DATUM]
    type Datum = DATUM
    def factory = Average

    def +(that: Averaging[DATUM]) = {
      val (newtotalWeight, newmean) = Average.plus(this.totalWeight, this.mean, that.totalWeight, that.mean)
      new Averaging(this.quantity, this.selection, newtotalWeight, newmean)
    }

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)

        totalWeight += w

        val delta = q - mean
        val shift = delta * w / totalWeight

        mean += shift
      }
    }

    def toJsonFragment = JsonObject("totalWeight" -> JsonFloat(totalWeight), "mean" -> JsonFloat(mean))

    override def toString() = s"Averaging($totalWeight, $mean)"
    override def equals(that: Any) = that match {
      case that: Averaging[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.totalWeight === that.totalWeight  &&  this.mean === that.mean
      case _ => false
    }
    override def hashCode() = (quantity, selection, totalWeight, mean).hashCode
  }
}
