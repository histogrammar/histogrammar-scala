package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// AbsoluteErr/AbsoluteErred/AbsoluteErring

  object AbsoluteErr extends Factory {
    val name = "AbsoluteErr"
    val help = "Accumulate the weighted Mean Absolute Error (MAE) of a quantity whose nominal value is zero."
    val detailedHelp = """AbsoluteErr(quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM])"""

    def fixed(entries: Double, mae: Double) = new AbsoluteErred(entries, mae)
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new AbsoluteErring(quantity, selection, 0.0, 0.0)

    def unapply(x: AbsoluteErred) = Some((x.entries, x.mae))
    def unapply[DATUM](x: AbsoluteErring[DATUM]) = Some((x.entries, x.mae))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("entries", "mae")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val mae = get("mae") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".mae")
        }

        new AbsoluteErred(entries, mae)

      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def plus(ca: Double, ma: Double, cb: Double, mb: Double) =
      (ca + cb, (ca*ma + cb*mb)/(ca + cb))
  }

  class AbsoluteErred(val entries: Double, val mae: Double) extends Container[AbsoluteErred] {
    type Type = AbsoluteErred
    def factory = AbsoluteErr

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def zero = new AbsoluteErred(0.0, 0.0)
    def +(that: AbsoluteErred) = {
      val (newentries, newmae) = AbsoluteErr.plus(this.entries, this.mae, that.entries, that.mae)
      new AbsoluteErred(newentries, newmae)
    }

    def toJsonFragment = JsonObject("entries" -> JsonFloat(entries), "mae" -> JsonFloat(mae))

    override def toString() = s"AbsoluteErred($mae)"
    override def equals(that: Any) = that match {
      case that: AbsoluteErred => this.entries === that.entries  &&  this.mae === that.mae
      case _ => false
    }
    override def hashCode() = (entries, mae).hashCode
  }

  class AbsoluteErring[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var entries: Double, _mae: Double) extends Container[AbsoluteErring[DATUM]] with AggregationOnData {
    type Type = AbsoluteErring[DATUM]
    type Datum = DATUM
    def factory = AbsoluteErr

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    private var absoluteSum = entries * _mae

    def mae =
      if (entries == 0.0)
        _mae
      else
        absoluteSum / entries

    def mae_=(_mae: Double) {
      absoluteSum = entries * _mae
    }

    def zero = new AbsoluteErring[DATUM](quantity, selection, 0.0, 0.0)
    def +(that: AbsoluteErring[DATUM]) = {
      val (newentries, newmae) = AbsoluteErr.plus(this.entries, this.mae, that.entries, that.mae)
      new AbsoluteErring[DATUM](this.quantity, this.selection, newentries, newmae)
    }

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)
        entries += w
        absoluteSum += Math.abs(q)
      }
    }

    def toJsonFragment = JsonObject("entries" -> JsonFloat(entries), "mae" -> JsonFloat(mae))

    override def toString() = s"AbsoluteErring($mae)"
    override def equals(that: Any) = that match {
      case that: AbsoluteErring[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.entries === that.entries  &&  this.mae === that.mae
      case _ => false
    }
    override def hashCode() = (quantity, selection, entries, mae).hashCode
  }
}
