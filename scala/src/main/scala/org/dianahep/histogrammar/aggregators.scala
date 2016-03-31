package org.dianahep

import scala.language.implicitConversions

import org.dianahep.histogrammar.json._

package object histogrammar {
  type Weighting[DATUM] = Weighted[DATUM] => Double

  implicit def toWeighted[DATUM](datum: DATUM) = Weighted(datum)
  implicit def domainToWeighted[DOMAIN, RANGE](f: DOMAIN => RANGE) = {x: Weighted[DOMAIN] => f(x.datum)}

  implicit def filterToWeighting[WEIGHTED <: Weighted[_]](filter: WEIGHTED => Boolean) = {x: WEIGHTED => if (filter(x)) 1.0 else 0.0}
  implicit def noWeighting[DATUM] = {x: Weighted[DATUM] => 1.0}
  def identity[DATUM](x: DATUM) = x

  AggregatorFactory.register(Count)
  AggregatorFactory.register(Binned)
}

package histogrammar {
  //////////////////////////////////////////////////////////////// utility classes

  case class Cache[DOMAIN, RANGE](function: DOMAIN => RANGE) extends Function1[DOMAIN, RANGE] {
    private var last: Option[(DOMAIN, RANGE)] = None
    def apply(x: DOMAIN): RANGE = (x, last) match {
      case (xref: AnyRef, Some((oldx: AnyRef, oldy))) if (xref eq oldx) => oldy
      case (_,            Some((oldx, oldy)))         if (x == oldx)    => oldy
      case _ =>
        val y = function(x)
        last = Some(x -> y)
        y
    }
    def clear() { last = None }
  }

  case class Weighted[DATUM](datum: DATUM, weight: Double = 1.0) {
    def *(w: Double): Weighted[DATUM] = copy(weight = weight*w)
  }

  //////////////////////////////////////////////////////////////// general definition of an aggregator/container

  trait AggregatorFactory[T <: Aggregator[_, _]] {
    def name: String
    def prototype: T
    def fromJson(str: String) = Json.parse(str) match {
      case Some(json) => fromJson(json)
      case None => throw new IllegalArgumentException(s"could not parse JSON: $str")
    }
    def fromJson(json: Json): T
  }
  object AggregatorFactory {
    private var known = Map[String, AggregatorFactory[_]]()
    def register(factory: AggregatorFactory[_]) {
      known = known.update(factory.name, factory)
    }
    def apply(name: String) = known.get(name) match {
      case Some(x) => x
      case None => throw new IllegalArgumentException(s"not a known type of aggregator: $name")
    }
  }

  trait Aggregator[DATUM, SELF] {
    def apply(x: Weighted[DATUM]): Unit
    def copy: SELF
    def +(that: SELF): SELF
    def weighting: Weighting[DATUM]

    def constructor: String
    def state: String

    def toJson: Json
    def factory: AggregatorFactory[SELF]

    override def toString() = s"$name($constructor)($state)"
  }

  trait Container[DATUM, SELF, SUB <: Aggregator[DATUM, SUB]] extends Aggregator[DATUM, SELF]

  //////////////////////////////////////////////////////////////// specific aggregators

  class Count[DATUM]
             (val weighting: Weighting[DATUM])
             (var value: Double = 0.0)
             extends Aggregator[DATUM, Count[DATUM]] {
    def apply(x: Weighted[DATUM]) {
      val y = x * weighting(x)
      value += y.weight
    }
    def copy = new Count(weighting)(value)
    def +(that: Count[DATUM]) = new Count(weighting)(this.value + that.value)

    def constructor = ""
    def state = value.toString

    def toJson: Json = JsonFloat(value)
    def factory = Count
  }
  object Count extends AggregatorFactory[Count] {
    val name = "Count"
    def prototype = Count[Nothing]

    def fromJson(json: Json) = json match {
      case JsonFloat(value) => new Count[Nothing](value)
      case _ => throw new IllegalArgumentException(s"wrong JSON format for Count: $json")
    }

    def apply[DATUM](implicit weighting: Weighting[DATUM]) = new Count(weighting)()
  }

  //////////////////////////////////////////////////////////////// specific containers

  class Binned[DATUM, SUB <: Aggregator[DATUM, SUB]]
              (sub: SUB, val num: Int, val low: Double, val high: Double, val key: Weighted[DATUM] => Double, val weighting: Weighting[DATUM])
              (val values: Vector[SUB] = Vector.fill(num)(sub.copy), val underflow: SUB = sub.copy, val overflow: SUB = sub.copy, val nanflow: SUB = sub.copy)
              extends Container[DATUM, Binned[DATUM, SUB], SUB] {
    if (low >= high)
      throw new IllegalArgumentException(s"low ($low) must be less than high ($high)")
    if (num < 1)
      throw new IllegalArgumentException(s"num ($num) must be greater than zero")

    def bin(k: Double): Int =
      if (under(k)  ||  over(k)  ||  nan(k))
        -1
      else
        Math.floor(num * (k - low) / (high - low)).toInt

    def under(k: Double): Boolean = !k.isNaN  &&  k < low
    def over(k: Double): Boolean = !k.isNaN  &&  k >= high
    def nan(k: Double): Boolean = k.isNaN

    def apply(x: Weighted[DATUM]) {
      val k = key(x)
      val y = x * weighting(x)
      if (under(k))
        underflow(y)
      else if (over(k))
        overflow(y)
      else if (nan(k))
        nanflow(y)
      else
        values(bin(k))(y)
    }

    def copy = new Binned(sub.copy, num, low, high, key, weighting)(values.map(_.copy), underflow.copy, overflow.copy, nanflow.copy)

    def +(that: Binned[DATUM, SUB]) = {
      if (that.num != this.num)
        throw new IllegalArgumentException(s"cannot add Binned because num differs (${this.num} vs ${that.num})")
      if (that.low != this.low)
        throw new IllegalArgumentException(s"cannot add Binned because low differs (${this.low} vs ${that.low})")
      if (that.high != this.high)
        throw new IllegalArgumentException(s"cannot add Binned because high differs (${this.high} vs ${that.high})")

      new Binned[DATUM, SUB](sub, num, low, high, key, weighting)(this.values zip that.values map {case (x, y) => x + y}, this.underflow + that.underflow, this.overflow + that.overflow, this.nanflow + that.nanflow)
    }

    def constructor = s"""${sub.name}(${sub.constructor}), $num, $low, $high"""
    def state = s"""Vector(${values.map(_.state).mkString(", ")}), ${underflow.state}, ${overflow.state}, ${nanflow.state}"""

    def toJson: Json = JsonObject(
      "sub" -> JsonString(sub.factory.name),
      "low" -> JsonFloat(low),
      "high" -> JsonFloat(high),
      "values" -> JsonArray(values.map(_.toJson): _*),
      "underflow" -> underflow.toJson,
      "overflow" -> overflow.toJson,
      "nanflow" -> nanflow.toJson)
    def factory = Binned
  }
  object Binned extends AggregatorFactory[Binned] {
    val name = "Binned"
    def prototype = Binned[Nothing, Count[Nothing]](Count.prototype, 1, 0.0, 1.0, {x: Nothing => 0.0})

    def fromJson(json: Json) = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("sub", "low", "high", "values", "underflow", "overflow", "nanflow")) =>
        val subFactory = pairs("sub") match {
          case JsonString(name) => AggregatorFactory(name)
          case None => throw new IllegalArgumentException(s"""wrong sub type for Binned: ${pairs("sub")}""")
        }

        val low = pairs("low") match {
          case JsonNumber(x) => x
          case _ => throw new IllegalArgumentException(s"""wrong low type for Binned: ${pairs("low")}""")
        }

        val high = pairs("high") match {
          case JsonNumber(x) => x
          case _ => throw new IllegalArgumentException(s"""wrong high type for Binned: ${pairs("high")}""")
        }

        val values = pairs("values") match {
          case JsonArray(subJsons @ _*) => subJsons.map(subFactory.fromJson(_)).toVector
          case _ => throw new IllegalArgumentException(s"""wrong values type for Binned: ${pairs("values")}""")
        }

        val underflow = subFactory.fromJson(pairs("underflow"))
        val overflow = subFactory.fromJson(pairs("overflow"))
        val nanflow = subFactory.fromJson(pairs("nanflow"))

        new Binned(Count.prototype, values.size, low, high, {x: Nothing => 0.0}, noWeighting)(values, underflow, overflow, nanflow)

      case _ => throw new IllegalArgumentException(s"wrong type or keys for Binned: $json")
    }

    def apply[DATUM, SUB <: Aggregator[DATUM, SUB]](sub: SUB, num: Int, low: Double, high: Double, key: Weighted[DATUM] => Double, weighting: Weighting[DATUM] = noWeighting[DATUM]) =
      new Binned[DATUM, SUB](sub, num, low, high, key, weighting)()
  }
}
