package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Sum/Summed/Summing

  object Sum extends Factory {
    val name = "Sum"

    def ed(value: Double) = new Summed(value)
    def ing[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new Summing(quantity, selection, 0.0)
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = ing(quantity, selection)

    def unapply(x: Summed) = Some(x.value)
    def unapply(x: Summing[_]) = Some(x.value)

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonFloat(value) => new Summed(value)
      case _ => throw new JsonFormatException(json, name)
    }
  }

  class Summed(val value: Double) extends Container[Summed] {
    def factory = Sum

    def +(that: Summed) = new Summed(this.value + that.value)

    def toJsonFragment = JsonFloat(value)
    override def toString() = s"Summed"
    override def equals(that: Any) = that match {
      case that: Summed => this.value == that.value
      case _ => false
    }
    override def hashCode() = value.hashCode
  }

  class Summing[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var value: Double) extends Aggregator[DATUM, Summed] {
    def factory = Sum

    def fill(x: Weighted[DATUM]) {
      val y = quantity(x) reweight selection(x)
      if (y.contributes)
        value += y.datum * y.weight
    }

    def fix = new Summed(value)

    override def toString() = s"Summing"
    override def equals(that: Any) = that match {
      case that: Summing[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.value == that.value
      case _ => false
    }
    override def hashCode() = (quantity, selection, value).hashCode
  }
}
