package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Minimize/Minimized/Minimizing

  object Minimize extends Factory {
    val name = "Minimize"
    val help = "Find the minimum value of a given quantity. If no data are observed, the result is NaN."
    val detailedHelp = """Minimize(quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM])"""

    def ed(entries: Double, min: Double) = new Minimized(entries, min)
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new Minimizing(quantity, selection, 0.0, java.lang.Double.NaN)

    def unapply(x: Minimized) = Some((x.entries, x.min))
    def unapply[DATUM](x: Minimizing[DATUM]) = Some((x.entries, x.min))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("entries", "min")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        get("min") match {
          case JsonNumber(x) => new Minimized(entries, x)
          case x => throw new JsonFormatException(x, name)
        }

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

  class Minimized(val entries: Double, val min: Double) extends Container[Minimized] {
    type Type = Minimized
    def factory = Minimize

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def zero = new Minimized(0.0, java.lang.Double.NaN)
    def +(that: Minimized) = new Minimized(this.entries + that.entries, Minimize.plus(this.min, that.min))

    def toJsonFragment = JsonObject("entries" -> JsonFloat(entries), "min" -> JsonFloat(min))

    override def toString() = s"Minimized($min)"
    override def equals(that: Any) = that match {
      case that: Minimized => this.entries === that.entries  &&  this.min === that.min
      case _ => false
    }
    override def hashCode() = (entries, min).hashCode
  }

  class Minimizing[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var entries: Double, var min: Double) extends Container[Minimizing[DATUM]] with AggregationOnData {
    type Type = Minimizing[DATUM]
    type Datum = DATUM
    def factory = Minimize

    def zero = new Minimizing[DATUM](quantity, selection, 0.0, java.lang.Double.NaN)
    def +(that: Minimizing[DATUM]) = new Minimizing[DATUM](this.quantity, this.selection, this.entries + that.entries, Minimize.plus(this.min, that.min))

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)
        entries += w
        if (min.isNaN  ||  q < min)
          min = q
      }
    }

    def toJsonFragment = JsonObject("entries" -> JsonFloat(entries), "min" -> JsonFloat(min))

    override def toString() = s"Minimizing($min)"
    override def equals(that: Any) = that match {
      case that: Minimizing[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.entries === that.entries  &&  this.min === that.min
      case _ => false
    }
    override def hashCode() = (quantity, selection, entries, min).hashCode
  }

  //////////////////////////////////////////////////////////////// Maximize/Maximized/Maximizing

  object Maximize extends Factory {
    val name = "Maximize"
    val help = "Find the maximum value of a given quantity. If no data are observed, the result is NaN."
    val detailedHelp = """Maximize(quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM])"""

    def ed(entries: Double, max: Double) = new Maximized(entries, max)
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new Maximizing(quantity, selection, 0.0, java.lang.Double.NaN)

    def unapply(x: Maximized) = Some((x.entries, x.max))
    def unapply[DATUM](x: Maximizing[DATUM]) = Some((x.entries, x.max))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("entries", "max")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        get("max") match {
          case JsonNumber(x) => new Maximized(entries, x)
          case x => throw new JsonFormatException(x, name)
        }

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

  class Maximized(val entries: Double, val max: Double) extends Container[Maximized] {
    type Type = Maximized
    def factory = Maximize

    def zero = new Maximized(0.0, java.lang.Double.NaN)
    def +(that: Maximized) = new Maximized(this.entries + that.entries, Maximize.plus(this.max, that.max))

    def toJsonFragment = JsonObject("entries" -> JsonFloat(entries), "max" -> JsonFloat(max))

    override def toString() = s"Maximized($max)"
    override def equals(that: Any) = that match {
      case that: Maximized => this.entries === that.entries  &&  this.max === that.max
      case _ => false
    }
    override def hashCode() = (entries, max).hashCode
  }

  class Maximizing[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var entries: Double, var max: Double) extends Container[Maximizing[DATUM]] with AggregationOnData {
    type Type = Maximizing[DATUM]
    type Datum = DATUM
    def factory = Maximize

    def zero = new Maximizing[DATUM](quantity, selection, 0.0, java.lang.Double.NaN)
    def +(that: Maximizing[DATUM]) = new Maximizing[DATUM](this.quantity, this.selection, this.entries + that.entries, Maximize.plus(this.max, that.max))

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)
        entries += w
        if (max.isNaN  ||  q > max)
          max = q
      }
    }

    def toJsonFragment = JsonObject("entries" -> JsonFloat(entries), "max" -> JsonFloat(max))

    override def toString() = s"Maximizing($max)"
    override def equals(that: Any) = that match {
      case that: Maximizing[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.entries === that.entries  &&  this.max === that.max
      case _ => false
    }
    override def hashCode() = (quantity, selection, entries, max).hashCode
  }
}
