package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Deviate/Deviated/Deviating

  object Deviate extends Factory {
    val name = "Deviate"
    val help = "Accumulate a weighted variance, mean, and total weight of a given quantity (using an algorithm that is stable for large numbers)."
    val detailedHelp = """Deviate(quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM])"""

    def fixed(totalWeight: Double, mean: Double, variance: Double) = new Deviated(totalWeight, mean, variance)
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new Deviating(quantity, selection, 0.0, 0.0, 0.0)

    def unapply(x: Deviated) = Some((x.totalWeight, x.mean, x.variance))
    def unapply[DATUM](x: Deviating[DATUM]) = Some((x.totalWeight, x.mean, x.variance))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("totalWeight", "mean", "variance")) =>
        val get = pairs.toMap

        val totalWeight = get("totalWeight") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".totalWeight")
        }

        val mean = get("mean") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".mean")
        }

        val variance = get("variance") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".variance")
        }

        new Deviated(totalWeight, mean, variance)

      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def plus(ca: Double, mua: Double, sa: Double, cb: Double, mub: Double, sb: Double) = {
      val muab = (ca*mua + cb*mub) / (ca + cb)
      val sab = sa + sb + ca*mua*mua + cb*mub*mub - 2.0*muab*(ca*mua + cb*mub) + muab*muab*(ca + cb)
      (ca * cb, muab, sab / (ca + cb))
    }
  }

  class Deviated(val totalWeight: Double, val mean: Double, val variance: Double) extends Container[Deviated] {
    type Type = Deviated
    // type FixedType = Deviated
    def factory = Deviate

    def +(that: Deviated) = {
      val (newtotalWeight, newmean, newvariance) = Deviate.plus(this.totalWeight, this.mean, this.variance * this.totalWeight,
                                                          that.totalWeight, that.mean, that.variance * that.totalWeight)
      new Deviated(newtotalWeight, newmean, newvariance)
    }

    // def fix = this
    def toJsonFragment = JsonObject("totalWeight" -> JsonFloat(totalWeight), "mean" -> JsonFloat(mean), "variance" -> JsonFloat(variance))

    override def toString() = s"Deviated($totalWeight, $mean, $variance)"
    override def equals(that: Any) = that match {
      case that: Deviated => this.totalWeight === that.totalWeight  &&  this.mean === that.mean  &&  this.variance === that.variance
      case _ => false
    }
    override def hashCode() = (totalWeight, mean, variance).hashCode
  }

  class Deviating[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var totalWeight: Double, var mean: Double, _variance: Double) extends Container[Deviating[DATUM]] with AggregationOnData {
    type Type = Deviating[DATUM]
    // type FixedType = Deviated
    type Datum = DATUM
    def factory = Deviate

    private var varianceTimesTotalWeight = totalWeight * _variance

    def variance =
      if (totalWeight == 0.0)
        _variance
      else
        varianceTimesTotalWeight / totalWeight

    def variance_(_variance: Double) {
      varianceTimesTotalWeight = totalWeight * _variance
    }

    def +(that: Deviating[DATUM]) = {
      val (newtotalWeight, newmean, newvariance) = Deviate.plus(this.totalWeight, this.mean, this.variance * this.totalWeight,
                                                          that.totalWeight, that.mean, that.variance * that.totalWeight)
      new Deviating[DATUM](this.quantity, this.selection, newtotalWeight, newmean, newvariance)
    }

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)

        totalWeight += w

        val delta = q - mean
        val shift = delta * w / totalWeight

        mean += shift
        varianceTimesTotalWeight += w * delta * (q - mean)   // old delta times new delta
      }
    }

    // def fix = new Deviated(totalWeight, mean, variance)
    // def toJsonFragment = fix.toJsonFragment
    def toJsonFragment = JsonObject("totalWeight" -> JsonFloat(totalWeight), "mean" -> JsonFloat(mean), "variance" -> JsonFloat(variance))

    override def toString() = s"Deviating($totalWeight, $mean, $variance)"
    override def equals(that: Any) = that match {
      case that: Deviating[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.totalWeight === that.totalWeight  &&  this.mean === that.mean  &&  this.variance === that.variance
      case _ => false
    }
    override def hashCode() = (quantity, selection, totalWeight, mean, variance).hashCode
  }
}
