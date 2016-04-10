package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Sum/Summed/Summing

  object Sum extends Factory {
    val name = "Sum"
    val help = "Accumulate the sum of a given quantity."
    val detailedHelp = """Sum(quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM])"""

    def fixed(value: Double) = new Summed(value)
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new Summing[DATUM](quantity, selection, 0.0)

    def unapply(x: Summed) = Some(x.value)
    def unapply(x: Summing[_]) = Some(x.value)

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonFloat(value) => new Summed(value)
      case _ => throw new JsonFormatException(json, name)
    }
  }

  class Summed(val value: Double) extends Container[Summed] {
    type Type = Summed
    type FixedType = Summed
    def factory = Sum

    def +(that: Summed) = new Summed(this.value + that.value)

    def fix = this
    def toJsonFragment = JsonFloat(value)

    override def toString() = s"Summed($value)"
    override def equals(that: Any) = that match {
      case that: Summed => this.value === that.value
      case _ => false
    }
    override def hashCode() = value.hashCode
  }

  class Summing[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var value: Double) extends Container[Summing[DATUM]] with AggregationOnData {
    type Type = Summing[DATUM]
    type FixedType = Summed
    type Datum = DATUM
    def factory = Sum

    def +(that: Summing[DATUM]) = new Summing(this.quantity, this.selection, this.value + that.value)

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)
        value += q * w
      }
    }

    def fix = new Summed(value)
    def toJsonFragment = fix.toJsonFragment

    override def toString() = s"Summing($value)"
    override def equals(that: Any) = that match {
      case that: Summing[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.value === that.value
      case _ => false
    }
    override def hashCode() = (quantity, selection, value).hashCode
  }
}
