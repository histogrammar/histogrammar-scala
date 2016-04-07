package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Fraction/Fractioned/Fractioning

  object Fraction extends Factory {
    val name = "Fraction"

    def container[V <: Container[V]](numerator: V, denominator: V) = new Fractioned(numerator, denominator)
    def apply[DATUM, V <: Container[V]](numeratorSelection: Selection[DATUM], value: => V = Count()) =
      new Fractioning(numeratorSelection, value, value)

    def unapply[V <: Container[V]](x: Fractioned[V]) = Some((x.numerator, x.denominator))
    def unapply[DATUM, V <: Container[V]](x: Fractioning[DATUM, V]) = Some((x.numerator, x.denominator))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("type", "numerator", "denominator")) =>
        val get = pairs.toMap

        val factory = get("type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, name + ".type")
        }

        val numerator = factory.fromJsonFragment(get("numerator"))
        val denominator = factory.fromJsonFragment(get("denominator"))

        new Fractioned[Container[_]](numerator, denominator)

      case _ => throw new JsonFormatException(json, name)
    }
  }

  class Fractioned[V <: Container[V]](val numerator: V, val denominator: V) extends Container[Fractioned[V]] {
    def factory = Fraction

    def +(that: Fractioned[V]) = new Fractioned(this.numerator + that.numerator, this.denominator + that.denominator)

    def toJsonFragment = JsonObject(
      "type" -> JsonString(numerator.factory.name),
      "numerator" -> numerator.toJsonFragment,
      "denominator" -> denominator.toJsonFragment)

    override def toString() = s"Fractioned[numerator=$numerator, denominator=$denominator]"
    override def equals(that: Any) = that match {
      case that: Fractioned[V] => this.numerator == that.numerator  &&  this.denominator == that.denominator
      case _ => false
    }
  }

  class Fractioning[DATUM, V <: Container[V]](val numeratorSelection: Selection[DATUM], val numerator: V, val denominator: V) extends Container[Fractioning[DATUM, V]] with Aggregation[DATUM] {
    def factory = Fraction

    if (!numerator.isInstanceOf[Aggregation[DATUM]])
      throw new AggregatorException(s"Fractioning should be built with Aggregation-enabled numerator (ending in -ing)")
    if (!denominator.isInstanceOf[Aggregation[DATUM]])
      throw new AggregatorException(s"Fractioning should be built with Aggregation-enabled denominator (ending in -ing)")

    def +(that: Fractioning[DATUM, V]) = new Fractioning(this.numeratorSelection, this.numerator + that.numerator, this.denominator + that.denominator)

    def fillWeighted(x: Weighted[DATUM]) {
      val Weighted(datum, weight) = x

      val w = weight * numeratorSelection(datum)

      if (weight > 0.0)
        denominator.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
      if (w > 0.0)
        numerator.asInstanceOf[Aggregation[DATUM]].fillWeighted(Weighted(datum, w))
    }

    def toJsonFragment = JsonObject(
      "type" -> JsonString(numerator.factory.name),
      "numerator" -> numerator.toJsonFragment,
      "denominator" -> denominator.toJsonFragment)

    override def toString() = s"Fractioning[numerator=$numerator, denominator=$denominator]"
    override def equals(that: Any) = that match {
      case that: Fractioning[DATUM, V] => this.numeratorSelection == that.numeratorSelection  &&  this.numerator == that.numerator  &&  this.denominator == that.denominator
      case _ => false
    }
  }
}
