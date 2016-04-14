package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Average/Averaged/Averaging

  object Average extends Factory {
    val name = "Average"
    val help = "Accumulate the weighted mean of a given quantity."
    val detailedHelp = """Average(quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM])"""

    def fixed(mean: Double, entries: Double) = new Averaged(mean, entries)
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new Averaging(quantity, selection, 0.0, 0.0)

    def unapply(x: Averaged) = Some((x.mean, x.entries))
    def unapply[DATUM](x: Averaging[DATUM]) = Some((x.mean, x.entries))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("mean", "entries")) =>
        val get = pairs.toMap

        val mean = get("mean") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".mean")
        }

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        new Averaged(mean, entries)

      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def plus(mua: Double, ca: Double, mub: Double, cb: Double) =
      ((ca*mua + cb*mub)/(ca + cb), ca + cb)
  }

  class Averaged(val mean: Double, val entries: Double) extends Container[Averaged] {
    type Type = Averaged
    // type FixedType = Averaged
    def factory = Average

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def zero = new Averaged(0.0, 0.0)
    def +(that: Averaged) = {
      val (newmean, newentries) = Average.plus(this.mean, this.entries, that.mean, that.entries)
      new Averaged(newmean, newentries)
    }

    // def fix = this
    def toJsonFragment = JsonObject("mean" -> JsonFloat(mean), "entries" -> JsonFloat(entries))

    override def toString() = s"Averaged($mean)"
    override def equals(that: Any) = that match {
      case that: Averaged => this.mean === that.mean  &&  this.entries === that.entries
      case _ => false
    }
    override def hashCode() = (mean, entries).hashCode
  }

  class Averaging[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var mean: Double, var entries: Double) extends Container[Averaging[DATUM]] with AggregationOnData {
    type Type = Averaging[DATUM]
    type FixedType = Averaged
    type Datum = DATUM
    def factory = Average

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def zero = new Averaging[DATUM](quantity, selection, 0.0, 0.0)
    def +(that: Averaging[DATUM]) = {
      val (newmean, newentries) = Average.plus(this.mean, this.entries, that.mean, that.entries)
      new Averaging(this.quantity, this.selection, newmean, newentries)
    }

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)

        val delta = q - mean
        val shift = delta * w / entries

        mean += shift
        entries += w
      }
    }

    // def fix = new Averaged(entries, mean)
    // def toJsonFragment = fix.toJsonFragment
    def toJsonFragment = JsonObject("mean" -> JsonFloat(mean), "entries" -> JsonFloat(entries))

    override def toString() = s"Averaging($mean)"
    override def equals(that: Any) = that match {
      case that: Averaging[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.mean === that.mean  &&  this.entries === that.entries
      case _ => false
    }
    override def hashCode() = (quantity, selection, mean, entries).hashCode
  }
}
