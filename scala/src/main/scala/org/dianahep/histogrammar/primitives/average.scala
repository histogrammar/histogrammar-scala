package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Average/Averaged/Averaging

  object Average extends Factory {
    val name = "Average"
    val help = "Accumulate the weighted mean of a given quantity."
    val detailedHelp = """Average(quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM])"""

    def fixed(entries: Double, mean: Double) = new Averaged(entries, mean)
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new Averaging(quantity, selection, 0.0, 0.0)

    def unapply(x: Averaged) = Some((x.entries, x.mean))
    def unapply[DATUM](x: Averaging[DATUM]) = Some((x.entries, x.mean))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("entries", "mean")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val mean = get("mean") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".mean")
        }

        new Averaged(entries, mean)

      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def plus(ca: Double, mua: Double, cb: Double, mub: Double) =
      (ca + cb, (ca*mua + cb*mub)/(ca + cb))
  }

  class Averaged(val entries: Double, val mean: Double) extends Container[Averaged] {
    type Type = Averaged
    def factory = Average

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def zero = new Averaged(0.0, 0.0)
    def +(that: Averaged) = {
      val (newentries, newmean) = Average.plus(this.entries, this.mean, that.entries, that.mean)
      new Averaged(newentries, newmean)
    }

    def toJsonFragment = JsonObject("entries" -> JsonFloat(entries), "mean" -> JsonFloat(mean))

    override def toString() = s"Averaged($mean)"
    override def equals(that: Any) = that match {
      case that: Averaged => this.entries === that.entries  &&  this.mean === that.mean
      case _ => false
    }
    override def hashCode() = (entries, mean).hashCode
  }

  class Averaging[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var entries: Double, var mean: Double) extends Container[Averaging[DATUM]] with AggregationOnData {
    type Type = Averaging[DATUM]
    type FixedType = Averaged
    type Datum = DATUM
    def factory = Average

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def zero = new Averaging[DATUM](quantity, selection, 0.0, 0.0)
    def +(that: Averaging[DATUM]) = {
      val (newentries, newmean) = Average.plus(this.entries, this.mean, that.entries, that.mean)
      new Averaging(this.quantity, this.selection, newentries, newmean)
    }

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)

        entries += w
        val delta = q - mean
        val shift = delta * w / entries
        mean += shift
      }
    }

    def toJsonFragment = JsonObject("entries" -> JsonFloat(entries), "mean" -> JsonFloat(mean))

    override def toString() = s"Averaging($mean)"
    override def equals(that: Any) = that match {
      case that: Averaging[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.entries === that.entries  &&  this.mean === that.mean
      case _ => false
    }
    override def hashCode() = (quantity, selection, entries, mean).hashCode
  }
}
