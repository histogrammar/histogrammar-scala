package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Partition/Partitioned/Partitioning

  object Partition extends Factory {
    val name = "Partition"

    def ed[V <: Container[V]](cuts: (Double, V)*) = new Partitioned(cuts: _*)
    def ing[DATUM, V <: Container[V]](value: => Aggregator[DATUM, V], expression: NumericalFcn[DATUM], cuts: Double*) =
      new Partitioning(expression, (java.lang.Double.NEGATIVE_INFINITY +: cuts).map((_, value)): _*)
    def apply[DATUM, V <: Container[V]](value: => Aggregator[DATUM, V], expression: NumericalFcn[DATUM], cuts: Double*) =
      ing(value, expression, cuts: _*)

    def unapplySeq[V <: Container[V]](x: Partitioned[V]) = Some(x.cuts)
    def unapplySeq[DATUM, V <: Container[V]](x: Partitioning[DATUM, V]) = Some(x.cuts)

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("type", "data")) =>
        val get = pairs.toMap

        val factory = get("type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, name + ".type")
        }

        get("data") match {
          case JsonArray(elements @ _*) if (elements.size >= 1) =>
            new Partitioned[Container[_]](elements.zipWithIndex map {case (element, i) =>
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

  class Partitioned[V <: Container[V]](val cuts: (Double, V)*) extends Container[Partitioned[V]] {
    def factory = Partition

    if (cuts.size < 1)
      throw new AggregatorException(s"number of cuts (${cuts.size}) must be at least 1 (including the implicit >= -inf, which the Partition.ing factory method adds)")

    def +(that: Partitioned[V]) =
      if (this.cuts.size != that.cuts.size)
        throw new AggregatorException(s"cannot add Partitioned because the number of cut differs (${this.cuts.size} vs ${that.cuts.size})")
      else
        new Partitioned(this.cuts zip that.cuts map {case ((mycut, me), (yourcut, you)) =>
          if (mycut != yourcut)
            throw new AggregatorException(s"cannot add Partitioned because cut differs ($mycut vs $yourcut)")
          (mycut, me + you)
        }: _*)

    def toJsonFragment = JsonObject(
      "type" -> JsonString(cuts.head._2.factory.name),
      "data" -> JsonArray(cuts map {case (atleast, sub) => JsonObject("atleast" -> JsonFloat(atleast), "data" -> sub.toJsonFragment)}: _*))

    override def toString() = s"""Partitioned[${cuts.head._2}, cuts=[${cuts.map(_._1).mkString(", ")}]]"""
    override def equals(that: Any) = that match {
      case that: Partitioned[V] => this.cuts == that.cuts
      case _ => false
    }
  }

  class Partitioning[DATUM, V <: Container[V]](val expression: NumericalFcn[DATUM], val cuts: (Double, Aggregator[DATUM, V])*) extends Aggregator[DATUM, Partitioned[V]] {
    def factory = Partition

    if (cuts.size < 1)
      throw new AggregatorException(s"number of cuts (${cuts.size}) must be at least 1 (including the implicit >= -inf, which the Partition.ing factory method adds)")

    private val range = cuts zip (cuts.tail :+ (java.lang.Double.NaN, null))

    def fill(x: Weighted[DATUM]) {
      if (x.contributes) {
        val value = expression(x)
        // !(value >= high) is true when high == NaN (even if value == +inf)
        range find {case ((low, sub), (high, _)) => value >= low  &&  !(value >= high)} foreach {case ((_, sub), (_, _)) =>
          sub.fill(x)
        }
      }
    }

    def fix = new Partitioned(cuts map {case (atleast, sub) => (atleast, sub.fix)}: _*)
    override def toString() = s"""Partitioning[${cuts.head._2}, cuts=[${cuts.map(_._1).mkString(", ")}]]"""
    override def equals(that: Any) = that match {
      case that: Partitioning[DATUM, V] => this.expression == that.expression  &&  this.cuts == that.cuts
      case _ => false
    }
  }
}
