package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Deviate/Deviated/Deviating

  object Deviate extends Factory {
    val name = "Deviate"
    val help = "Accumulate a weighted variance, mean, and total weight of a given quantity (using an algorithm that is stable for large numbers)."
    val detailedHelp = """Deviate(quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM])"""

    def fixed(entries: Double, mean: Double, variance: Double) = new Deviated(entries, mean, variance)
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new Deviating(quantity, selection, 0.0, 0.0, 0.0)

    def unapply(x: Deviated) = Some((x.entries, x.mean, x.variance))
    def unapply[DATUM](x: Deviating[DATUM]) = Some((x.entries, x.mean, x.variance))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("entries", "mean", "variance")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val mean = get("mean") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".mean")
        }

        val variance = get("variance") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".variance")
        }

        new Deviated(entries, mean, variance)

      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def plus(ca: Double, mua: Double, sa: Double, cb: Double, mub: Double, sb: Double) = {
      val muab = (ca*mua + cb*mub) / (ca + cb)
      val sab = sa + sb + ca*mua*mua + cb*mub*mub - 2.0*muab*(ca*mua + cb*mub) + muab*muab*(ca + cb)
      (ca * cb, muab, sab / (ca + cb))
    }
  }

  class Deviated(val entries: Double, val mean: Double, val variance: Double) extends Container[Deviated] {
    type Type = Deviated
    def factory = Deviate

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def zero = new Deviated(0.0, 0.0, 0.0)
    def +(that: Deviated) = {
      val (newentries, newmean, newvariance) = Deviate.plus(this.entries, this.mean, this.variance * this.entries,
                                                            that.entries, that.mean, that.variance * that.entries)
      new Deviated(newentries, newmean, newvariance)
    }

    def toJsonFragment = JsonObject("entries" -> JsonFloat(entries), "mean" -> JsonFloat(mean), "variance" -> JsonFloat(variance))

    override def toString() = s"Deviated($entries, $mean, $variance)"
    override def equals(that: Any) = that match {
      case that: Deviated => this.entries === that.entries  &&  this.mean === that.mean  &&  this.variance === that.variance
      case _ => false
    }
    override def hashCode() = (entries, mean, variance).hashCode
  }

  class Deviating[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var entries: Double, var mean: Double, _variance: Double) extends Container[Deviating[DATUM]] with AggregationOnData {
    type Type = Deviating[DATUM]
    type Datum = DATUM
    def factory = Deviate

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    private var varianceTimesTotalWeight = entries * _variance

    def variance =
      if (entries == 0.0)
        _variance
      else
        varianceTimesTotalWeight / entries

    def variance_=(_variance: Double) {
      varianceTimesTotalWeight = entries * _variance
    }

    def zero = new Deviating[DATUM](quantity, selection, 0.0, 0.0, 0.0)
    def +(that: Deviating[DATUM]) = {
      val (newentries, newmean, newvariance) = Deviate.plus(this.entries, this.mean, this.variance * this.entries,
                                                            that.entries, that.mean, that.variance * that.entries)
      new Deviating[DATUM](this.quantity, this.selection, newentries, newmean, newvariance)
    }

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)

        entries += w
        val delta = q - mean
        val shift = delta * w / entries
        mean += shift
        varianceTimesTotalWeight += w * delta * (q - mean)   // old delta times new delta
      }
    }

    def toJsonFragment = JsonObject("entries" -> JsonFloat(entries), "mean" -> JsonFloat(mean), "variance" -> JsonFloat(variance))

    override def toString() = s"Deviating($entries, $mean, $variance)"
    override def equals(that: Any) = that match {
      case that: Deviating[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.entries === that.entries  &&  this.mean === that.mean  &&  this.variance === that.variance
      case _ => false
    }
    override def hashCode() = (quantity, selection, entries, mean, variance).hashCode
  }
}
