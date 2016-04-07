package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Partition/Partitioned/Partitioning

  object Partition extends Factory {
    val name = "Partition"

    def ed[V <: Container[V]](cuts: (Double, V)*) = new Partitioned(cuts: _*)
    def apply[DATUM, V <: Container[V]](value: => V, expression: NumericalFcn[DATUM], cuts: Double*) =
      new Partitioning(expression, (java.lang.Double.NEGATIVE_INFINITY +: cuts).map((_, value)): _*)

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
      case that: Partitioned[V] => this.cuts zip that.cuts forall {case (me, you) => me._1 === you._1  &&  me._2 == you._2}
      case _ => false
    }
  }

  class Partitioning[DATUM, V <: Container[V]](val expression: NumericalFcn[DATUM], val cuts: (Double, V)*) extends Container[Partitioning[DATUM, V]] with Aggregation[DATUM] {
    def factory = Partition

    if (cuts.size < 1)
      throw new AggregatorException(s"number of cuts (${cuts.size}) must be at least 1 (including the implicit >= -inf, which the Partition.ing factory method adds)")

    if (!cuts.forall(_._2.isInstanceOf[Aggregation[DATUM]]))
      throw new AggregatorException(s"Partitioning should be built with Aggregation-enabled cuts (ending in -ing)")

    private val range = cuts zip (cuts.tail :+ (java.lang.Double.NaN, null))

    def +(that: Partitioning[DATUM, V]) =
      if (this.cuts.size != that.cuts.size)
        throw new AggregatorException(s"cannot add Partitioning because the number of cut differs (${this.cuts.size} vs ${that.cuts.size})")
      else
        new Partitioning(this.expression,
          this.cuts zip that.cuts map {case ((mycut, me), (yourcut, you)) =>
            if (mycut != yourcut)
              throw new AggregatorException(s"cannot add Partitioning because cut differs ($mycut vs $yourcut)")
            (mycut, me + you)
          }: _*)

    def fillWeighted(x: Weighted[DATUM]) {
      val Weighted(datum, weight) = x
      if (weight > 0.0) {
        val value = expression(datum)
        // !(value >= high) is true when high == NaN (even if value == +inf)
        range find {case ((low, sub), (high, _)) => value >= low  &&  !(value >= high)} foreach {case ((_, sub), (_, _)) =>
          sub.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        }
      }
    }

    def toJsonFragment = JsonObject(
      "type" -> JsonString(cuts.head._2.factory.name),
      "data" -> JsonArray(cuts map {case (atleast, sub) => JsonObject("atleast" -> JsonFloat(atleast), "data" -> sub.toJsonFragment)}: _*))

    override def toString() = s"""Partitioning[${cuts.head._2}, cuts=[${cuts.map(_._1).mkString(", ")}]]"""
    override def equals(that: Any) = that match {
      case that: Partitioning[DATUM, V] => this.expression == that.expression  &&  (this.cuts zip that.cuts forall {case (me, you) => me._1 === you._1  &&  me._2 == you._2})
      case _ => false
    }
  }
}
