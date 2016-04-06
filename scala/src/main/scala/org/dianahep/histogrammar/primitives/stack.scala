package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Stack/Stacked/Stacking

  object Stack extends Factory {
    val name = "Stack"

    def ed[V <: Container[V]](cuts: (Double, V)*) = new Stacked(cuts: _*)
    def ing[DATUM, V <: Container[V]](value: => Aggregator[DATUM, V], expression: NumericalFcn[DATUM], cuts: Double*) =
      new Stacking(expression, (java.lang.Double.NEGATIVE_INFINITY +: cuts).map((_, value)): _*)
    def apply[DATUM, V <: Container[V]](value: => Aggregator[DATUM, V], expression: NumericalFcn[DATUM], cuts: Double*) =
      ing(value, expression, cuts: _*)

    def unapplySeq[V <: Container[V]](x: Stacked[V]) = Some(x.cuts)
    def unapplySeq[DATUM, V <: Container[V]](x: Stacking[DATUM, V]) = Some(x.cuts)

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("type", "data")) =>
        val get = pairs.toMap

        val factory = get("type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, name + ".type")
        }

        get("data") match {
          case JsonArray(elements @ _*) if (elements.size >= 1) =>
            new Stacked[Container[_]](elements.zipWithIndex map {case (element, i) =>
              element match {
                case JsonObject(elementPairs @ _*) if (elementPairs.keySet == Set("atleast", "data")) =>
                  val elementGet = elementPairs.toMap
                  val atleast = elementGet("atleast") match {
                    case JsonNumber(x) => x
                    case x => throw new JsonFormatException(x, name + s".data element $i atleast")
                  }
                  (atleast, factory.fromJsonFragment(elementGet("data")))

                case x => throw new JsonFormatException(x, name + s".data element $i")
              }
            }: _*)

          case x => throw new JsonFormatException(x, name + ".data")
        }

      case _ => throw new JsonFormatException(json, name)
    }
  }

  class Stacked[V <: Container[V]](val cuts: (Double, V)*) extends Container[Stacked[V]] {
    def factory = Stack

    if (cuts.size < 1)
      throw new AggregatorException(s"number of cuts (${cuts.size}) must be at least 1 (including the implicit >= -inf, which the Stack.ing factory method adds)")

    def +(that: Stacked[V]) =
      if (this.cuts.size != that.cuts.size)
        throw new AggregatorException(s"cannot add Stacks because the number of cut differs (${this.cuts.size} vs ${that.cuts.size})")
      else
        new Stacked[V](this.cuts zip that.cuts map {case ((mycut, me), (yourcut, you)) =>
          if (mycut != yourcut)
            throw new AggregatorException(s"cannot add Stacks because cut differs ($mycut vs $yourcut)")
          (mycut, me + you)
        }: _*)
    def +[DATUM](that: Stacking[DATUM, V]) =
      if (this.cuts.size != that.cuts.size)
        throw new AggregatorException(s"cannot add Stacks because the number of cut differs (${this.cuts.size} vs ${that.cuts.size})")
      else
        new Stacking[DATUM, V](that.expression, this.cuts zip that.cuts map {case ((mycut, me), (yourcut, you)) =>
          if (mycut != yourcut)
            throw new AggregatorException(s"cannot add Stacks because cut differs ($mycut vs $yourcut)")
          (mycut, me + you)
        }: _*)

    def toJsonFragment = JsonObject(
      "type" -> JsonString(cuts.head._2.factory.name),
      "data" -> JsonArray(cuts map {case (atleast, sub) => JsonObject("atleast" -> JsonFloat(atleast), "data" -> sub.toJsonFragment)}: _*))

    override def toString() = s"""Stacked[${cuts.head._2}, cuts=[${cuts.map(_._1).mkString(", ")}]]"""
    override def equals(that: Any) = that match {
      case that: Stacked[V] => this.cuts zip that.cuts forall {case (me, you) => me._1 === you._1  &&  me._2 == you._2}
      case _ => false
    }
  }

  class Stacking[DATUM, V <: Container[V]](val expression: NumericalFcn[DATUM], val cuts: (Double, Aggregator[DATUM, V])*) extends Aggregator[DATUM, Stacked[V]] {
    def factory = Stack

    if (cuts.size < 1)
      throw new AggregatorException(s"number of cuts (${cuts.size}) must be at least 1 (including the implicit >= -inf, which the Stack.ing factory method adds)")

    def +(that: Stacked[V]) =
      if (this.cuts.size != that.cuts.size)
        throw new AggregatorException(s"cannot add Stacks because the number of cut differs (${this.cuts.size} vs ${that.cuts.size})")
      else
        new Stacking[DATUM, V](this.expression, this.cuts zip that.cuts map {case ((mycut, me), (yourcut, you)) =>
          if (mycut != yourcut)
            throw new AggregatorException(s"cannot add Stacks because cut differs ($mycut vs $yourcut)")
          (mycut, me + you)
        }: _*)
    def +(that: Stacking[DATUM, V]) =
      if (this.cuts.size != that.cuts.size)
        throw new AggregatorException(s"cannot add Stacks because the number of cut differs (${this.cuts.size} vs ${that.cuts.size})")
      else
        new Stacking[DATUM, V](this.expression, this.cuts zip that.cuts map {case ((mycut, me), (yourcut, you)) =>
          if (mycut != yourcut)
            throw new AggregatorException(s"cannot add Stacks because cut differs ($mycut vs $yourcut)")
          (mycut, me + you)
        }: _*)

    def fill(x: Weighted[DATUM]) {
      if (x.contributes) {
        val value = expression(x)
        cuts foreach {case (threshold, sub) =>
          if (value >= threshold)
            sub.fill(x)
        }
      }
    }

    def toContainer = new Stacked(cuts map {case (atleast, sub) => (atleast, sub.toContainer)}: _*)

    override def toString() = s"""Stacking[${cuts.head._2}, cuts=[${cuts.map(_._1).mkString(", ")}]]"""
    override def equals(that: Any) = that match {
      case that: Stacking[DATUM, V] => this.expression == that.expression  &&  (this.cuts zip that.cuts forall {case (me, you) => me._1 === you._1  &&  me._2 == you._2})
      case _ => false
    }
  }
}
