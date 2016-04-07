package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Minimize/Minimized/Minimizing

  object Minimize extends Factory {
    val name = "Minimize"

    def ed(min: Double) = new Minimized(min)
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new Minimizing(quantity, selection, java.lang.Double.NaN)

    def unapply(x: Minimized) = Some(x.min)
    def unapply(x: Minimizing[_]) = Some(x.min)

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonNumber(x) => new Minimized(x)
      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def plus(one: Double, two: Double) =
      if (one.isNaN)
        two
      else if (two.isNaN)
        one
      else if (one < two)
        one
      else
        two
  }

  class Minimized(val min: Double) extends Container[Minimized] {
    def factory = Minimize

    def +(that: Minimized) = new Minimized(Minimize.plus(this.min, that.min))

    def toJsonFragment = JsonFloat(min)

    override def toString() = s"Minimized"
    override def equals(that: Any) = that match {
      case that: Minimized => this.min === that.min
      case _ => false
    }
    override def hashCode() = min.hashCode
  }

  class Minimizing[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var min: Double) extends Aggregator[DATUM, Minimizing[DATUM]] {
    def factory = Minimize

    def +(that: Minimizing[DATUM]) = new Minimizing[DATUM](this.quantity, this.selection, Minimize.plus(this.min, that.min))

    def fill(x: Weighted[DATUM]) {
      val y = quantity(x) reweight selection(x)

      if (y.contributes  &&  (min.isNaN  ||  y.datum < min))
        min = y.datum
    }

    def toJsonFragment = JsonFloat(min)

    override def toString() = s"Minimizing"
    override def equals(that: Any) = that match {
      case that: Minimizing[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.min === that.min
      case _ => false
    }
    override def hashCode() = (quantity, selection, min).hashCode
  }

  //////////////////////////////////////////////////////////////// Maximize/Maximized/Maximizing

  object Maximize extends Factory {
    val name = "Maximize"

    def ed(max: Double) = new Maximized(max)
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new Maximizing(quantity, selection, java.lang.Double.NaN)

    def unapply(x: Maximized) = Some(x.max)
    def unapply(x: Maximizing[_]) = Some(x.max)

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonNumber(x) => new Maximized(x)
      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def plus(one: Double, two: Double) =
      if (one.isNaN)
        two
      else if (two.isNaN)
        one
      else if (one > two)
        one
      else
        two
  }

  class Maximized(val max: Double) extends Container[Maximized] {
    def factory = Maximize

    def +(that: Maximized) = new Maximized(Maximize.plus(this.max, that.max))

    def toJsonFragment = JsonFloat(max)

    override def toString() = s"Maximized"
    override def equals(that: Any) = that match {
      case that: Maximized => this.max === that.max
      case _ => false
    }
    override def hashCode() = max.hashCode
  }

  class Maximizing[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var max: Double) extends Aggregator[DATUM, Maximizing[DATUM]] {
    def factory = Maximize

    def +(that: Maximizing[DATUM]) = new Maximizing[DATUM](this.quantity, this.selection, Maximize.plus(this.max, that.max))

    def fill(x: Weighted[DATUM]) {
      val y = quantity(x) reweight selection(x)

      if (y.contributes  &&  (max.isNaN  ||  y.datum > max))
        max = y.datum
    }

    def toJsonFragment = JsonFloat(max)

    override def toString() = s"Maximizing"
    override def equals(that: Any) = that match {
      case that: Maximizing[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.max === that.max
      case _ => false
    }
    override def hashCode() = (quantity, selection, max).hashCode
  }
}
