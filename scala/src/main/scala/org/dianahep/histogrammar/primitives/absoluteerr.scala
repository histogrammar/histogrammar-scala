package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// AbsoluteErr/AbsoluteErred/AbsoluteErring

  object AbsoluteErr extends Factory {
    val name = "AbsoluteErr"

    def ed(count: Double, mae: Double) = new AbsoluteErred(count, mae)
    def ing[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new AbsoluteErring(quantity, selection, 0.0, 0.0)
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = ing(quantity, selection)

    def unapply(x: AbsoluteErred) = Some((x.count, x.mae))
    def unapply(x: AbsoluteErring[_]) = Some((x.count, x.mae))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("count", "mae")) =>
        val get = pairs.toMap

        val count = get("count") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".count")
        }

        val mae = get("mae") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".mae")
        }

        new AbsoluteErred(count, mae)

      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def plus(one: (Double, Double), two: (Double, Double)) = (one._1 + two._1, (one._1*one._2 + two._1*two._2) / (one._1 + two._1))
  }

  class AbsoluteErred(val count: Double, val mae: Double) extends Container[AbsoluteErred] {
    def factory = AbsoluteErr

    def +(that: AbsoluteErred) = {
      val (newcount, newmae) = AbsoluteErr.plus((this.count, this.mae), (that.count, that.mae))
      new AbsoluteErred(newcount, newmae)
    }
    def +[DATUM](that: AbsoluteErring[DATUM]) = {
      val (newcount, newmae) = AbsoluteErr.plus((this.count, this.mae), (that.count, that.mae))
      new AbsoluteErring[DATUM](that.quantity, that.selection, newcount, newmae)
    }

    def toJsonFragment = JsonObject("count" -> JsonFloat(count), "mae" -> JsonFloat(mae))
    override def toString() = s"AbsoluteErred"
    override def equals(that: Any) = that match {
      case that: AbsoluteErred => this.count === that.count  &&  this.mae === that.mae
      case _ => false
    }
    override def hashCode() = (count, mae).hashCode
  }

  class AbsoluteErring[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var count: Double, _mae: Double) extends Aggregator[DATUM, AbsoluteErred] {
    def factory = AbsoluteErr

    def +(that: AbsoluteErred) = {
      val (newcount, newmae) = AbsoluteErr.plus((this.count, this.mae), (that.count, that.mae))
      new AbsoluteErring[DATUM](this.quantity, this.selection, newcount, newmae)
    }
    def +(that: AbsoluteErring[DATUM]) = {
      val (newcount, newmae) = AbsoluteErr.plus((this.count, this.mae), (that.count, that.mae))
      new AbsoluteErring[DATUM](this.quantity, this.selection, newcount, newmae)
    }

    private var absoluteSum = count * _mae

    def mae =
      if (count == 0.0)
        _mae
      else
        absoluteSum / count

    def mae_(_mae: Double) {
      absoluteSum = count * _mae
    }

    def fill(x: Weighted[DATUM]) {
      val y = quantity(x) reweight selection(x)

      if (y.contributes) {
        absoluteSum += Math.abs(y.datum)
        count += y.weight
      }
    }

    def toContainer = new AbsoluteErred(count, mae)

    override def toString() = s"AbsoluteErring"
    override def equals(that: Any) = that match {
      case that: AbsoluteErring[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.count === that.count  &&  this.mae === that.mae
      case _ => false
    }
    override def hashCode() = (quantity, selection, count, mae).hashCode
  }
}
