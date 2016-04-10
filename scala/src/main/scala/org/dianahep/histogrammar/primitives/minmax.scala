package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Minimize/Minimized/Minimizing

  object Minimize extends Factory {
    val name = "Minimize"
    val help = "Find the minimum value of a given quantity. If no data are observed, the result is NaN."
    val detailedHelp = """Minimize(quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM])"""

    def container(min: Double) = new Minimized(min)
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new Minimizing(quantity, selection, java.lang.Double.NaN)

    def unapply(x: Minimized) = Some(x.min)
    def unapply[DATUM](x: Minimizing[DATUM]) = Some(x.min)

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
    type Type = Minimized
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

  class Minimizing[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var min: Double) extends Container[Minimizing[DATUM]] with Aggregation {
    type Type = Minimizing[DATUM]
    type Datum = DATUM
    def factory = Minimize

    def +(that: Minimizing[DATUM]) = new Minimizing[DATUM](this.quantity, this.selection, Minimize.plus(this.min, that.min))

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)
        if (min.isNaN  ||  q < min)
          min = q
      }
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
    val help = "Find the maximum value of a given quantity. If no data are observed, the result is NaN."
    val detailedHelp = """Maximize(quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM])"""

    def container(max: Double) = new Maximized(max)
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new Maximizing(quantity, selection, java.lang.Double.NaN)

    def unapply(x: Maximized) = Some(x.max)
    def unapply[DATUM](x: Maximizing[DATUM]) = Some(x.max)

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
    type Type = Maximized
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

  class Maximizing[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var max: Double) extends Container[Maximizing[DATUM]] with Aggregation {
    type Type = Maximizing[DATUM]
    type Datum = DATUM
    def factory = Maximize

    def +(that: Maximizing[DATUM]) = new Maximizing[DATUM](this.quantity, this.selection, Maximize.plus(this.max, that.max))

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)
        if (max.isNaN  ||  q > max)
          max = q
      }
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
