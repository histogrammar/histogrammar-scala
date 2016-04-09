package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Deviate/Deviated/Deviating

  object Deviate extends Factory {
    val name = "Deviate"
    val help = "Accumulate a count, a mean, and a variance of a given quantity (using an algorithm that is stable for large numbers)."
    val detailedHelp = """Deviate(quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM])"""

    def container(count: Double, mean: Double, variance: Double) = new Deviated(count, mean, variance)
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new Deviating(quantity, selection, 0.0, 0.0, 0.0)

    def unapply(x: Deviated) = Some((x.count, x.mean, x.variance))
    def unapply(x: Deviating[_]) = Some((x.count, x.mean, x.variance))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("count", "mean", "variance")) =>
        val get = pairs.toMap

        val count = get("count") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".count")
        }

        val mean = get("mean") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".mean")
        }

        val variance = get("variance") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".variance")
        }

        new Deviated(count, mean, variance)

      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def plus(ca: Double, mua: Double, sa: Double, cb: Double, mub: Double, sb: Double) = {
      val muab = (ca*mua + cb*mub) / (ca + cb)
      val sab = sa + sb + ca*mua*mua + cb*mub*mub - 2.0*muab*(ca*mua + cb*mub) + muab*muab*(ca + cb)
      (ca * cb, muab, sab / (ca + cb))
    }
  }

  class Deviated(val count: Double, val mean: Double, val variance: Double) extends Container[Deviated] {
    def factory = Deviate

    def +(that: Deviated) = {
      val (newcount, newmean, newvariance) = Deviate.plus(this.count, this.mean, this.variance * this.count,
                                                          that.count, that.mean, that.variance * that.count)
      new Deviated(newcount, newmean, newvariance)
    }

    def toJsonFragment = JsonObject("count" -> JsonFloat(count), "mean" -> JsonFloat(mean), "variance" -> JsonFloat(variance))

    override def toString() = s"Deviated"
    override def equals(that: Any) = that match {
      case that: Deviated => this.count === that.count  &&  this.mean === that.mean  &&  this.variance === that.variance
      case _ => false
    }
    override def hashCode() = (count, mean, variance).hashCode
  }

  class Deviating[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var count: Double, var mean: Double, _variance: Double) extends Aggregator[DATUM, Deviating[DATUM]] {
    def factory = Deviate

    private var varianceTimesCount = count * _variance

    def variance =
      if (count == 0.0)
        _variance
      else
        varianceTimesCount / count

    def variance_(_variance: Double) {
      varianceTimesCount = count * _variance
    }

    def +(that: Deviating[DATUM]) = {
      val (newcount, newmean, newvariance) = Deviate.plus(this.count, this.mean, this.variance * this.count,
                                                          that.count, that.mean, that.variance * that.count)
      new Deviating[DATUM](this.quantity, this.selection, newcount, newmean, newvariance)
    }

    def fillWeighted[SUB <: DATUM](datum: SUB, weight: Double) {
      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)

        count += w

        val delta = q - mean
        val shift = delta * w / count

        mean += shift
        varianceTimesCount += w * delta * (q - mean)   // old delta times new delta
      }
    }

    def toJsonFragment = JsonObject("count" -> JsonFloat(count), "mean" -> JsonFloat(mean), "variance" -> JsonFloat(variance))

    override def toString() = s"Deviating"
    override def equals(that: Any) = that match {
      case that: Deviating[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.count === that.count  &&  this.mean === that.mean  &&  this.variance === that.variance
      case _ => false
    }
    override def hashCode() = (quantity, selection, count, mean, variance).hashCode
  }
}
