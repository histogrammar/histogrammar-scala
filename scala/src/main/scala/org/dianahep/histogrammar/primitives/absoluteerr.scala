package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// AbsoluteErr/AbsoluteErred/AbsoluteErring

  object AbsoluteErr extends Factory {
    val name = "AbsoluteErr"
    val help = "Accumulate a weighted mean absolute error (MAE) and total weight of a given quantity whose nominal value is zero."
    val detailedHelp = """AbsoluteErr(quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM])"""

    def fixed(totalWeight: Double, mae: Double) = new AbsoluteErred(totalWeight, mae)
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new AbsoluteErring(quantity, selection, 0.0, 0.0)

    def unapply(x: AbsoluteErred) = Some((x.totalWeight, x.mae))
    def unapply[DATUM](x: AbsoluteErring[DATUM]) = Some((x.totalWeight, x.mae))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("totalWeight", "mae")) =>
        val get = pairs.toMap

        val totalWeight = get("totalWeight") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".totalWeight")
        }

        val mae = get("mae") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".mae")
        }

        new AbsoluteErred(totalWeight, mae)

      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def plus(ca: Double, ma: Double, cb: Double, mb: Double) =
      (ca + cb, (ca*ma + cb*mb)/(ca + cb))
  }

  class AbsoluteErred(val totalWeight: Double, val mae: Double) extends Container[AbsoluteErred] {
    type Type = AbsoluteErred
    def factory = AbsoluteErr

    def +(that: AbsoluteErred) = {
      val (newtotalWeight, newmae) = AbsoluteErr.plus(this.totalWeight, this.mae, that.totalWeight, that.mae)
      new AbsoluteErred(newtotalWeight, newmae)
    }

    def toJsonFragment = JsonObject("totalWeight" -> JsonFloat(totalWeight), "mae" -> JsonFloat(mae))

    override def toString() = s"AbsoluteErred($totalWeight, $mae)"
    override def equals(that: Any) = that match {
      case that: AbsoluteErred => this.totalWeight === that.totalWeight  &&  this.mae === that.mae
      case _ => false
    }
    override def hashCode() = (totalWeight, mae).hashCode
  }

  class AbsoluteErring[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var totalWeight: Double, _mae: Double) extends Container[AbsoluteErring[DATUM]] with Aggregation {
    type Type = AbsoluteErring[DATUM]
    type Datum = DATUM
    def factory = AbsoluteErr

    private var absoluteSum = totalWeight * _mae

    def mae =
      if (totalWeight == 0.0)
        _mae
      else
        absoluteSum / totalWeight

    def mae_(_mae: Double) {
      absoluteSum = totalWeight * _mae
    }

    def +(that: AbsoluteErring[DATUM]) = {
      val (newtotalWeight, newmae) = AbsoluteErr.plus(this.totalWeight, this.mae, that.totalWeight, that.mae)
      new AbsoluteErring[DATUM](this.quantity, this.selection, newtotalWeight, newmae)
    }

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)
        absoluteSum += Math.abs(q)
        totalWeight += w
      }
    }

    def toJsonFragment = JsonObject("totalWeight" -> JsonFloat(totalWeight), "mae" -> JsonFloat(mae))

    override def toString() = s"AbsoluteErring($totalWeight, $mae)"
    override def equals(that: Any) = that match {
      case that: AbsoluteErring[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.totalWeight === that.totalWeight  &&  this.mae === that.mae
      case _ => false
    }
    override def hashCode() = (quantity, selection, totalWeight, mae).hashCode
  }
}
