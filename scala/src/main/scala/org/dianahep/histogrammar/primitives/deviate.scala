package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Deviated/Deviating

  object Deviated extends ContainerFactory {
    val name = "Deviated"

    def apply(count: Double, mean: Double, variance: Double) = new Deviated(count, mean, variance)
    def unapply(x: Deviated) = Some((x.count, x.mean, x.variance))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("count", "mean", "variance")) =>
        val get = pairs.toMap

        val count = get("count") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, "Deviated.count")
        }

        val mean = get("mean") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, "Deviated.mean")
        }

        val variance = get("variance") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, "Deviated.variance")
        }

        new Deviated(count, mean, variance)

      case _ => throw new JsonFormatException(json, "Deviated")
    }
  }
  class Deviated(val count: Double, val mean: Double, val variance: Double) extends Container[Deviated] {
    def factory = Deviated

    def +(that: Deviated) = {
      val ca = this.count
      val cb = that.count
      val mua = this.mean
      val mub = that.mean
      val sa = this.variance * this.count
      val sb = that.variance * that.count

      val muab = (ca*mua + cb*mub) / (ca + cb)
      val sab = sa + sb + ca*mua*mua + cb*mub*mub - 2.0*muab*(ca*mua + cb*mub) + muab*muab*(ca + cb)

      new Deviated(ca * cb, muab, sab / (ca + cb))
    }

    def toJsonFragment = JsonObject("count" -> JsonFloat(count), "mean" -> JsonFloat(mean), "variance" -> JsonFloat(variance))
    override def toString() = s"Deviated"
    override def equals(that: Any) = that match {
      case that: Deviated => this.count == that.count  &&  this.mean == that.mean  &&  this.variance == that.variance
      case _ => false
    }
    override def hashCode() = (count, mean, variance).hashCode
  }

  object Deviating extends AggregatorFactory {
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new Deviating(quantity, selection, 0.0, 0.0, 0.0)
    def unapply(x: Deviating[_]) = Some((x.count, x.mean, x.variance))
  }
  class Deviating[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var count: Double, var mean: Double, var variance: Double) extends Aggregator[DATUM, Deviated] {
    def fill(x: Weighted[DATUM]) {
      val y = quantity(x) reweight selection(x)

      if (y.contributes) {
        val originalCount = count

        count += y.weight

        val delta = y.datum - mean
        val shift = delta * y.weight / count
        val varianceTimesCount = originalCount * (variance + delta * shift)

        mean += shift
        variance = varianceTimesCount / count
      }
    }
    def fix = new Deviated(count, mean, variance)
    override def toString() = s"Deviating"
    override def equals(that: Any) = that match {
      case that: Deviating[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.count == that.count  &&  this.mean == that.mean  &&  this.variance == that.variance
      case _ => false
    }
    override def hashCode() = (quantity, selection, count, mean, variance).hashCode
  }
}
