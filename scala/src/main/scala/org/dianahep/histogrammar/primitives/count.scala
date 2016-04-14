package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Count/Counted/Counting

  object Count extends Factory {
    val name = "Count"
    val help = "Count data, ignoring their content."
    val detailedHelp = """Count()"""

    def fixed(entries: Double) = new Counted(entries)
    def apply() = new Counting(0.0)

    def unapply(x: Counted) = Some(x.entries)
    def unapply(x: Counting) = Some(x.entries)

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonFloat(entries) => new Counted(entries)
      case _ => throw new JsonFormatException(json, name)
    }
  }

  class Counted(val entries: Double) extends Container[Counted] {
    type Type = Counted
    def factory = Count

    def zero = new Counted(0.0)
    def +(that: Counted): Counted = new Counted(this.entries + that.entries)

    def toJsonFragment = JsonFloat(entries)

    override def toString() = s"Counted($entries)"
    override def equals(that: Any) = that match {
      case that: Counted => this.entries == that.entries
      case _ => false
    }
    override def hashCode() = entries.hashCode
  }

  class Counting(var entries: Double) extends Container[Counting] with Aggregation {
    type Type = Counting
    type Datum = Any
    def factory = Count

    def zero = new Counting(0.0)
    def +(that: Counting): Counting = new Counting(this.entries + that.entries)

    def fillWeighted[SUB <: Any](datum: SUB, weight: Double) {
      entries += weight
    }

    def toJsonFragment = JsonFloat(entries)

    override def toString() = s"Counting($entries)"
    override def equals(that: Any) = that match {
      case that: Counting => this.entries == that.entries
      case _ => false
    }
    override def hashCode() = entries.hashCode
  }
}
