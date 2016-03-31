package org.dianahep

import scala.language.implicitConversions

package object histogrammar {
  type Weighting[DATUM] = Weighted[DATUM] => Double

  implicit def toWeighted[DATUM](datum: DATUM) = Weighted(datum)
  implicit def domainToWeighted[DOMAIN, RANGE](f: DOMAIN => RANGE) = {x: Weighted[DOMAIN] => f(x.datum)}

  implicit def filterToWeighting[WEIGHTED <: Weighted[_]](filter: WEIGHTED => Boolean) = {x: WEIGHTED => if (filter(x)) 1.0 else 0.0}

  implicit def noWeighting[DATUM] = {x: Weighted[DATUM] => 1.0}
}

package histogrammar {
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

  trait Aggregator[DATUM, SELF] {
    def apply(x: Weighted[DATUM]): Unit
    def copy: SELF
    def +(that: SELF): SELF
    def weighting: Weighting[DATUM]

    def name = getClass.getSimpleName
    def constructor: String
    def state: String
    override def toString() = s"$name($constructor)($state)"
  }

  trait Container[DATUM, SELF, SUB <: Aggregator[DATUM, SUB]] extends Aggregator[DATUM, SELF]

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
  }
  object Count {
    def apply[DATUM](implicit weighting: Weighting[DATUM]) = new Count(weighting)()
  }

  class Binned[DATUM, SUB <: Aggregator[DATUM, SUB]]
              (sub: SUB, val num: Int, val low: Double, val high: Double, val key: Weighted[DATUM] => Double, val weighting: Weighting[DATUM])
              (val values: Vector[SUB] = Vector.fill(num)(sub.copy), val underflow: SUB = sub.copy, val overflow: SUB = sub.copy)
              extends Container[DATUM, Binned[DATUM, SUB], SUB] {
    if (low >= high)
      throw new IllegalArgumentException(s"low ($low) must be less than high ($high)")
    if (num < 1)
      throw new IllegalArgumentException(s"num ($num) must be greater than zero")

    def bin(k: Double): Int = {
      val out = Math.floor(num * (k - low) / (high - low)).toInt
      if (out < 0  ||  out >= num)
        -1
      else
        out
    }
    def under(k: Double): Boolean = k < low
    def over(k: Double): Boolean = k >= high

    def apply(x: Weighted[DATUM]) {
      val k = key(x)
      val y = x * weighting(x)
      if (under(k))
        underflow(y)
      else if (over(k))
        overflow(y)
      else
        values(bin(k))(y)
    }

    def copy = new Binned(sub.copy, num, low, high, key, weighting)(values.map(_.copy), underflow.copy, overflow.copy)

    def +(that: Binned[DATUM, SUB]) = {
      if (that.num != this.num)
        throw new IllegalArgumentException(s"cannot add Binned because num differs (${this.num} vs ${that.num})")
      if (that.low != this.low)
        throw new IllegalArgumentException(s"cannot add Binned because low differs (${this.low} vs ${that.low})")
      if (that.high != this.high)
        throw new IllegalArgumentException(s"cannot add Binned because high differs (${this.high} vs ${that.high})")

      new Binned[DATUM, SUB](sub, num, low, high, key, weighting)(this.values zip that.values map {case (x, y) => x + y}, this.underflow + that.underflow, this.overflow + that.overflow)
    }

    def constructor = s"""${sub.name}(${sub.constructor}), $num, $low, $high"""
    def state = s"""Vector(${values.map(_.state).mkString(", ")}), ${underflow.state}, ${overflow.state}"""
  }
  object Binned {
    def apply[DATUM, SUB <: Aggregator[DATUM, SUB]](sub: SUB, num: Int, low: Double, high: Double, key: Weighted[DATUM] => Double, weighting: Weighting[DATUM] = noWeighting[DATUM]) =
      new Binned[DATUM, SUB](sub, num, low, high, key, weighting)()
  }
}
