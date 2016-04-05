package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Counted/Counting

  object Counted extends ContainerFactory {
    val name = "Counted"

    def apply(value: Double) = new Counted(value)
    def unapply(x: Counted) = Some(x.value)

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonFloat(value) => new Counted(value)
      case _ => throw new JsonFormatException(json, "Counted")
    }
  }
  class Counted(val value: Double) extends Container[Counted] {
    def factory = Counted

    def +(that: Counted) = new Counted(this.value + that.value)

    def toJsonFragment = JsonFloat(value)
    override def toString() = s"Counted"
    override def equals(that: Any) = that match {
      case that: Counted => this.value == that.value
      case _ => false
    }
    override def hashCode() = value.hashCode
  }

  object Counting extends AggregatorFactory {
    def apply[DATUM](selection: Selection[DATUM] = unweighted[DATUM]) = new Counting(selection, 0.0)
    def unapply(x: Counting[_]) = Some(x.value)
  }
  class Counting[DATUM](val selection: Selection[DATUM], var value: Double) extends Aggregator[DATUM, Counted] {
    def fill(x: Weighted[DATUM]) {
      val y = x reweight selection(x)
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
