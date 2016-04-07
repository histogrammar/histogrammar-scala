package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Count/Counted/Counting

  object Count extends Factory {
    val name = "Count"
    val help = "Count data, ignoring their content (actually, sum their weights)."
    val detailedHelp = """Count(selection: Selection[DATUM] = unweighted[DATUM])"""

    def container(value: Double) = new Counted(value)
    def apply[DATUM](selection: Selection[DATUM] = unweighted[DATUM]) = new Counting(selection, 0.0)

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
      case that: Counted => this.value === that.value
      case _ => false
    }
    override def hashCode() = value.hashCode
  }

  class Counting[DATUM](val selection: Selection[DATUM], var value: Double) extends Container[Counting[DATUM]] with Aggregation[DATUM] {
    def factory = Count

    def +(that: Counting[DATUM]) = new Counting[DATUM](this.selection, this.value + that.value)

    def fillWeighted(x: Weighted[DATUM]) {
      val Weighted(datum, weight) = x

      val w = weight * selection(datum)
      if (w > 0.0)
        value += w
    }

    def toJsonFragment = JsonFloat(value)

    override def toString() = s"Counting"
    override def equals(that: Any) = that match {
      case that: Counting[DATUM] => this.selection == that.selection  &&  this.value === that.value
      case _ => false
    }
    override def hashCode() = (selection, value).hashCode
  }
}
