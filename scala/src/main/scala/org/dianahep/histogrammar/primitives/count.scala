package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Count/Counted/Counting

  object Count extends Factory {
    val name = "Count"

    def ed(value: Double) = new Counted(value)
    def ing[DATUM](selection: Selection[DATUM] = unweighted[DATUM]) = new Counting(selection, 0.0)
    def apply[DATUM](selection: Selection[DATUM] = unweighted[DATUM]) = ing(selection)

    def unapply(x: Counted) = Some(x.value)
    def unapply(x: Counting[_]) = Some(x.value)

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonFloat(value) => new Counted(value)
      case _ => throw new JsonFormatException(json, name)
    }
  }

  class Counted(val value: Double) extends Container[Counted] {
    def factory = Count

    def +(that: Counted) = new Counted(this.value + that.value)

    def toJsonFragment = JsonFloat(value)
    override def toString() = s"Counted"
    override def equals(that: Any) = that match {
      case that: Counted => this.value == that.value
      case _ => false
    }
    override def hashCode() = value.hashCode
  }

  class Counting[DATUM](val selection: Selection[DATUM], var value: Double) extends Aggregator[DATUM, Counted] {
    def factory = Count

    def fill(x: Weighted[DATUM]) {
      val y = x reweight selection(x)
      if (y.contributes)
        value += y.weight
    }
    def fix = new Counted(value)
    override def toString() = s"Counting"
    override def equals(that: Any) = that match {
      case that: Counting[DATUM] => this.selection == that.selection  &&  this.value == that.value
      case _ => false
    }
    override def hashCode() = (selection, value).hashCode
  }
}
