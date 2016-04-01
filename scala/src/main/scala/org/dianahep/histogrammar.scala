package org.dianahep

import scala.language.implicitConversions

import org.dianahep.histogrammar.json._

package object histogrammar {
  implicit def toWeighted[DATUM](datum: DATUM) = Weighted(datum)
  implicit def domainToWeighted[DOMAIN, RANGE](f: DOMAIN => RANGE) = {x: Weighted[DOMAIN] => f(x.datum)}

  implicit def filterToSelection[DATUM](f: DATUM => Boolean) = Selection({x: Weighted[DATUM] => if (f(x.datum)) 1.0 else 0.0})
  implicit def weightToSelection[DATUM](f: DATUM => Double) = Selection({x: Weighted[DATUM] => f(x.datum)})
  implicit def weightedFilterToSelection[DATUM](f: Weighted[DATUM] => Boolean) = Selection({x: Weighted[DATUM] => if (f(x)) 1.0 else 0.0})
  implicit def weightedWeightToSelection[DATUM](f: Weighted[DATUM] => Double) = Selection({x: Weighted[DATUM] => f(x)})

  type ToNumeric[DATUM] = Weighted[DATUM] => Double
  type ToCategory[DATUM] = Weighted[DATUM] => String

  def uncut[DATUM] = Selection({x: Weighted[DATUM] => 1.0})

  Factory.register(Counted)
  Factory.register(Binned)
}

package histogrammar {
  //////////////////////////////////////////////////////////////// utility classes

  case class Weighted[DATUM](datum: DATUM, weight: Double = 1.0) {
    def reweight(w: Double): Weighted[DATUM] = copy(weight = weight*w)
    def nonzero = weight != 0.0
  }

  case class Selection[DATUM](f: Weighted[DATUM] => Double) extends Function1[Weighted[DATUM], Double] {
    def apply(x: Weighted[DATUM]) = f(x)
  }

  case class Cached[DOMAIN, RANGE](f: DOMAIN => RANGE) extends Function1[DOMAIN, RANGE] {
    private var last: Option[(DOMAIN, RANGE)] = None
    def apply(x: DOMAIN): RANGE = (x, last) match {
      case (xref: AnyRef, Some((oldx: AnyRef, oldy))) if (xref eq oldx) => oldy
      case (_,            Some((oldx, oldy)))         if (x == oldx)    => oldy
      case _ =>
        val y = f(x)
        last = Some(x -> y)
        y
    }
    def clear() { last = None }
  }

  class AggregatorException(message: String, cause: Exception = null) extends Exception(message, cause)

  //////////////////////////////////////////////////////////////// general definition of an container/aggregator

  // creates aggregators (from provided functions) and containers (from JSON)
  trait Factory {
    def name: String
    def fromJsonFragment(json: Json): Container[_]
  }
  object Factory {
    private var known = Map[String, Factory]()

    def register(factory: Factory) {
      known = known.updated(factory.name, factory)
    }

    def apply(name: String) = known.get(name) match {
      case Some(x) => x
      case None => throw new AggregatorException(s"unrecognized aggregator (is it a custom aggregator that hasn't been registered?): $name")
    }

    def fromJson[CONTAINER <: Container[CONTAINER]](str: String): CONTAINER = Json.parse(str) match {
      case Some(json) => fromJson(json).asInstanceOf[CONTAINER]
      case None => throw new InvalidJsonException(str)
    }

    def fromJson[CONTAINER <: Container[CONTAINER]](json: Json): CONTAINER = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("type", "data")) =>
        val get = pairs.toMap

        val name = get("type") match {
          case JsonString(x) => x
          case x => throw new JsonFormatException(x, "type")
        }

        Factory(name).fromJsonFragment(get("data")).asInstanceOf[CONTAINER]

      case _ => throw new JsonFormatException(json, "Factory")
    }
  }

  // immutable container of data; the result of an aggregation
  trait Container[CONTAINER <: Container[CONTAINER]] extends Serializable {
    def factory: Factory

    def +(that: CONTAINER): CONTAINER

    def toJson: Json = JsonObject("type" -> JsonString(factory.name), "data" -> toJsonFragment)
    def toJsonFragment: Json
  }

  // mutable aggregator of data; produces a container
  trait Aggregator[DATUM, CONTAINER <: Container[CONTAINER]] extends Container[CONTAINER] {
    def selection: Selection[DATUM]

    def fill(x: Weighted[DATUM])

    def fix: CONTAINER

    // satisfy Container[CONTAINER] contract by passing everything through fix
    def factory = fix.factory
    def +(that: CONTAINER) = fix.+(that)
    def toJsonFragment = fix.toJsonFragment
  }

  //////////////////////////////////////////////////////////////// Counted/Counting

  object Counted extends Factory {
    val name = "Counted"

    def apply(value: Double) = new Counted(value)

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonFloat(value) => new Counted(value)
      case _ => throw new JsonFormatException(json, "Counted")
    }
  }
  class Counted(val value: Double) extends Container[Counted] {
    def factory = Counted

    def +(that: Counted) = new Counted(this.value + that.value)

    def toJsonFragment = JsonFloat(value)
    override def toString() = s"Counted"
  }

  object Counting {
    def apply[DATUM](selection: Selection[DATUM] = uncut[DATUM]) = new Counting(selection, 0.0)
  }
  class Counting[DATUM](val selection: Selection[DATUM], var value: Double) extends Aggregator[DATUM, Counted] {
    def fill(x: Weighted[DATUM]) {
      val y = x reweight selection(x)
      value += y.weight
    }
    def fix = new Counted(value)
    override def toString() = s"Counting"
  }

  //////////////////////////////////////////////////////////////// Binned/Binning

  object Binned extends Factory {
    val name = "Binned"

    def apply[V <: Container[V], U <: Container[U], O <: Container[O], N <: Container[N]]
      (low: Double,
        high: Double)
      (values: Seq[V],
        underflow: U,
        overflow: O,
        nanflow: N) =
      new Binned[V, U, O, N](low, high, values, underflow, overflow, nanflow)

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("low", "high", "values:type", "values", "underflow:type", "underflow", "overflow:type", "overflow", "nanflow:type", "nanflow")) =>
        val get = pairs.toMap

        val low = get("low") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, "Binned.low")
        }

        val high = get("high") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, "Binned.high")
        }

        val valuesFactory = get("values:type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, "Binned.values:type")
        }
        val values = get("values") match {
          case JsonArray(sub @ _*) => sub.map(valuesFactory.fromJsonFragment(_))
          case x => throw new JsonFormatException(x, "Binned.values")
        }

        val underflowFactory = get("underflow:type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, "Binned.underflow:type")
        }
        val underflow = underflowFactory.fromJsonFragment(get("underflow"))

        val overflowFactory = get("overflow:type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, "Binned.overflow:type")
        }
        val overflow = overflowFactory.fromJsonFragment(get("overflow"))

        val nanflowFactory = get("nanflow:type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, "Binned.nanflow:type")
        }
        val nanflow = nanflowFactory.fromJsonFragment(get("nanflow"))

        new Binned[Container[_], Container[_], Container[_], Container[_]](low, high, values, underflow, overflow, nanflow)

      case _ => throw new JsonFormatException(json, "Binned")
    }
  }
  class Binned[V <: Container[V], U <: Container[U], O <: Container[O], N <: Container[N]](
    val low: Double,
    val high: Double,
    val values: Seq[V],
    val underflow: U,
    val overflow: O,
    val nanflow: N) extends Container[Binned[V, U, O, N]] {

    if (low >= high)
      throw new AggregatorException(s"low ($low) must be less than high ($high)")
    if (values.size < 1)
      throw new AggregatorException(s"values ($values) must have at least one element")

    def factory = Binned

    def +(that: Binned[V, U, O, N]) = {
      if (this.low != that.low)
        throw new AggregatorException(s"cannot add Binned because low differs (${this.low} vs ${that.low})")
      if (this.high != that.high)
        throw new AggregatorException(s"cannot add Binned because high differs (${this.high} vs ${that.high})")
      if (this.values.size != that.values.size)
        throw new AggregatorException(s"cannot add Binned because number of values differs (${this.values.size} vs ${that.values.size})")

      new Binned(
        low,
        high,
        this.values zip that.values map {case (me, you) => me + you},
        this.underflow + that.underflow,
        this.overflow + that.overflow,
        this.nanflow + that.nanflow)
    }

    def toJsonFragment = JsonObject(
      "low" -> JsonFloat(low),
      "high" -> JsonFloat(high),
      "values:type" -> JsonString(values.head.factory.name),
      "values" -> JsonArray(values.map(_.toJsonFragment): _*),
      "underflow:type" -> JsonString(underflow.factory.name),
      "underflow" -> underflow.toJsonFragment,
      "overflow:type" -> JsonString(overflow.factory.name),
      "overflow" -> overflow.toJsonFragment,
      "nanflow:type" -> JsonString(nanflow.factory.name),
      "nanflow" -> nanflow.toJsonFragment)

    override def toString() = s"Binned[low=$low, high=$high, values=[${values.head.toString}, size=${values.size}], underflow=$underflow, overflow=$overflow, nanflow=$nanflow]"
  }

  object Binning {
    def apply[DATUM, V <: Container[V], U <: Container[U], O <: Container[O], N <: Container[N]]
      (num: Int,
        low: Double,
        high: Double,
        key: ToNumeric[DATUM],
        selection: Selection[DATUM] = uncut[DATUM])
      (value: => Aggregator[DATUM, V] = Counting[DATUM](),
        underflow: Aggregator[DATUM, U] = Counting[DATUM](),
        overflow: Aggregator[DATUM, O] = Counting[DATUM](),
        nanflow: Aggregator[DATUM, N] = Counting[DATUM]()) =
      new Binning[DATUM, V, U, O, N](low, high, key, selection, Array.fill(num)(value).toSeq, underflow, overflow, nanflow)
  }
  class Binning[DATUM, V <: Container[V], U <: Container[U], O <: Container[O], N <: Container[N]](
    val low: Double,
    val high: Double,
    val key: ToNumeric[DATUM],
    val selection: Selection[DATUM],
    val values: Seq[Aggregator[DATUM, V]],
    val underflow: Aggregator[DATUM, U],
    val overflow: Aggregator[DATUM, O],
    val nanflow: Aggregator[DATUM, N]) extends Aggregator[DATUM, Binned[V, U, O, N]] {

    if (low >= high)
      throw new AggregatorException(s"low ($low) must be less than high ($high)")
    if (values.size < 1)
      throw new AggregatorException(s"values ($values) must have at least one element")

    def bin(k: Double): Int =
      if (under(k)  ||  over(k)  ||  nan(k))
        -1
      else
        Math.floor(values.size * (k - low) / (high - low)).toInt

    def under(k: Double): Boolean = !k.isNaN  &&  k < low
    def over(k: Double): Boolean = !k.isNaN  &&  k >= high
    def nan(k: Double): Boolean = k.isNaN

    def fill(x: Weighted[DATUM]) {
      val k = key(x)
      val y = x reweight selection(x)
      if (y.nonzero) {
        if (under(k))
          underflow.fill(y)
        else if (over(k))
          overflow.fill(y)
        else if (nan(k))
          nanflow.fill(y)
        else
          values(bin(k)).fill(y)
      }
    }

    def fix = new Binned(low, high, values.map(_.fix), underflow.fix, overflow.fix, nanflow.fix)
    override def toString() = s"Binning[low=$low, high=$high, values=[${values.head.toString}, size=${values.size}], underflow=$underflow, overflow=$overflow, nanflow=$nanflow]"
  }
}
