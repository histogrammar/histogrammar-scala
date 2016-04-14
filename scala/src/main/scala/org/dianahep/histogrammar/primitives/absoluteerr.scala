package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// AbsoluteErr/AbsoluteErred/AbsoluteErring

  object AbsoluteErr extends Factory {
    val name = "AbsoluteErr"
    val help = "Accumulate the weighted Mean Absolute Error (MAE) of a quantity whose nominal value is zero."
    val detailedHelp = """AbsoluteErr(quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM])"""

    def fixed(mae: Double, entries: Double) = new AbsoluteErred(mae, entries)
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new AbsoluteErring(quantity, selection, 0.0, 0.0)

    def unapply(x: AbsoluteErred) = Some((x.mae, x.entries))
    def unapply[DATUM](x: AbsoluteErring[DATUM]) = Some((x.mae, x.entries))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("mae", "entries")) =>
        val get = pairs.toMap

        val mae = get("mae") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".mae")
        }

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        new AbsoluteErred(mae, entries)

      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def plus(ma: Double, ca: Double, mb: Double, cb: Double) =
      ((ca*ma + cb*mb)/(ca + cb), ca + cb)
  }

  class AbsoluteErred(val mae: Double, val entries: Double) extends Container[AbsoluteErred] {
    type Type = AbsoluteErred
    // type FixedType = AbsoluteErred
    def factory = AbsoluteErr

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def zero = new AbsoluteErred(0.0, 0.0)
    def +(that: AbsoluteErred) = {
      val (newmae, newentries) = AbsoluteErr.plus(this.mae, this.entries, that.mae, that.entries)
      new AbsoluteErred(newmae, newentries)
    }

    // def fix = this
    def toJsonFragment = JsonObject("mae" -> JsonFloat(mae), "entries" -> JsonFloat(entries))

    override def toString() = s"AbsoluteErred($mae)"
    override def equals(that: Any) = that match {
      case that: AbsoluteErred => this.mae === that.mae  &&  this.entries === that.entries
      case _ => false
    }
    override def hashCode() = (mae, entries).hashCode
  }

  class AbsoluteErring[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], _mae: Double, var entries: Double) extends Container[AbsoluteErring[DATUM]] with AggregationOnData {
    type Type = AbsoluteErring[DATUM]
    // type FixedType = AbsoluteErred
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

    def mae_(_mae: Double) {
      absoluteSum = entries * _mae
    }

    def zero = new AbsoluteErring[DATUM](quantity, selection, 0.0, 0.0)
    def +(that: AbsoluteErring[DATUM]) = {
      val (newmae, newentries) = AbsoluteErr.plus(this.mae, this.entries, that.mae, that.entries)
      new AbsoluteErring[DATUM](this.quantity, this.selection, newmae, newentries)
    }

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)
        absoluteSum += Math.abs(q)
        entries += w
      }
    }

    // def fix = new AbsoluteErred(entries, mae)
    // def toJsonFragment = fix.toJsonFragment
    def toJsonFragment = JsonObject("mae" -> JsonFloat(mae), "entries" -> JsonFloat(entries))

    override def toString() = s"AbsoluteErring($mae)"
    override def equals(that: Any) = that match {
      case that: AbsoluteErring[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.mae === that.mae  &&  this.entries === that.entries
      case _ => false
    }
    override def hashCode() = (quantity, selection, mae, entries).hashCode
  }
}
