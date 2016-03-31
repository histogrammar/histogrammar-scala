package org.dianahep

import scala.language.implicitConversions
import scala.reflect.runtime.universe.Type
import scala.reflect.runtime.universe.WeakTypeTag
import scala.reflect.runtime.universe.weakTypeOf

import org.dianahep.histogrammar.json._

package object histogrammar {
  implicit def toWeighted[DATUM](datum: DATUM) = Weighted(datum)
  implicit def domainToWeighted[DOMAIN, RANGE](f: DOMAIN => RANGE) = {x: Weighted[DOMAIN] => f(x.datum)}

  type Selection[DATUM] = Weighted[DATUM] => Double
  implicit def filterToSelection[DATUM](filter: Weighted[DATUM] => Boolean): Selection[DATUM] =
    {x: Weighted[DATUM] => if (filter(x)) 1.0 else 0.0}

  type ToNumeric[DATUM] = Weighted[DATUM] => Double
  type ToCategory[DATUM] = Weighted[DATUM] => String

  implicit def aggregatorToContainer[CONTAINER <: Container[CONTAINER]](aggregator: Aggregator[_, CONTAINER]): CONTAINER = aggregator.fix

  implicit def uncut[DATUM] = {x: Weighted[DATUM] => 1.0}
  def identity[DATUM](x: DATUM): DATUM = x

  Factory.register(Count)
  Factory.register(Bin)
}

package histogrammar {
  //////////////////////////////////////////////////////////////// utility classes

  case class Weighted[DATUM](datum: DATUM, weight: Double = 1.0) {
    def reweight(w: Double): Weighted[DATUM] = copy(weight = weight*w)
    def nonzero = weight != 0.0
  }

  case class Cached[DOMAIN, RANGE](function: DOMAIN => RANGE) extends Function1[DOMAIN, RANGE] {
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

  class AggregatorException(message: String, cause: Exception = null) extends Exception(message, cause)

  //////////////////////////////////////////////////////////////// general definition of an container/aggregator

  // creates aggregators (from provided functions) and containers (from JSON); names are infinitive (no ending)
  trait Factory {
    def name: String

    def fromJson(str: String): Container[_] = Json.parse(str) match {
      case Some(json) => fromJson(json)
      case None => throw new InvalidJsonException(str)
    }
    def fromJson(json: Json): Container[_]
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
  }

  // immutable container of data; the result of an aggregation; names are past-tense (-ed)
  trait Container[CONTAINER <: Container[CONTAINER]] extends Serializable {
    def factory: Factory

    def +(that: CONTAINER): CONTAINER

    def toJson: Json
    override def toString() = s"${factory.name}($toStringParams)"
    def toStringParams: String
  }

  // mutable aggregator of data; produces a container; names are gerunds (-ing)
  trait Aggregator[DATUM, CONTAINER <: Container[CONTAINER]] extends Container[CONTAINER] {
    def datumType: Type
    def selection: Selection[DATUM]

    def fill(x: Weighted[DATUM])

    def fix: CONTAINER

    // satisfy Container[CONTAINER] contract by passing everything through fix
    def factory = fix.factory
    def +(that: CONTAINER) = fix.+(that)
    def toJson = fix.toJson

    override def toString() = s"${factory.name}[${datumType.toString}]($toStringParams)"
  }

  //////////////////////////////////////////////////////////////// Count/Counted/Counting

  object Count extends Factory {
    val name = "Count"

    def apply(value: Double) = new Counted(value)
    def apply[DATUM : WeakTypeTag](implicit selection: Selection[DATUM]) = new Counting(selection, 0.0)

    def fromJson(json: Json): Container[_] = json match {
      case JsonFloat(value) => new Counted(value)
      case _ => throw new JsonFormatException(json, "Count")
    }
  }

  class Counted(val value: Double) extends Container[Counted] {
    def factory = Count

    def +(that: Counted) = new Counted(this.value + that.value)

    def toJson = JsonFloat(value)
    def toStringParams = value.toString
  }

  class Counting[DATUM : WeakTypeTag](val selection: Selection[DATUM], var value: Double) extends Aggregator[DATUM, Counted] {
    def datumType = weakTypeOf[DATUM]
    def fill(x: Weighted[DATUM]) {
      val y = x reweight selection(x)
      value += y.weight
    }
    def fix = new Counted(value)
    def toStringParams = selection.toString
  }

  //////////////////////////////////////////////////////////////// Bin/Binned/Binning

  object Bin extends Factory {
    val name = "Bin"

    def apply[V <: Container[V], U <: Container[U], O <: Container[O], N <: Container[N]](
      low: Double,
      high: Double,
      values: Seq[V],
      underflow: U,
      overflow: O,
      nanflow: N) =
      new Binned[V, U, O, N](low, high, values, underflow, overflow, nanflow)

    def apply[DATUM : WeakTypeTag, V <: Container[V], U <: Container[U], O <: Container[O], N <: Container[N]](
      key: ToNumeric[DATUM],
      selection: Selection[DATUM],
      low: Double,
      high: Double,
      values: Seq[Aggregator[DATUM, V]] = Array.fill(100)(Count[DATUM]).toSeq,
      underflow: Aggregator[DATUM, U] = Count[DATUM],
      overflow: Aggregator[DATUM, O] = Count[DATUM],
      nanflow: Aggregator[DATUM, N] = Count[DATUM]) =
      new Binning[DATUM, V, U, O, N](key, selection, low, high, values, underflow, overflow, nanflow)

    def fromJson(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("low", "high", "values:type", "values", "underflow:type", "underflow", "overflow:type", "overflow", "nanflow:type", "nanflow")) =>
        val get = pairs.toMap

        val low = get("low") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, "Bin.low")
        }

        val high = get("high") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, "Bin.high")
        }

        val valuesFactory = get("values:type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, "Bin.values:type")
        }
        val values = get("values") match {
          case JsonArray(sub @ _*) => sub.map(valuesFactory.fromJson(_))
          case x => throw new JsonFormatException(x, "Bin.values")
        }

        val underflowFactory = get("underflow:type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, "Bin.underflow:type")
        }
        val underflow = underflowFactory.fromJson(get("underflow"))

        val overflowFactory = get("overflow:type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, "Bin.overflow:type")
        }
        val overflow = overflowFactory.fromJson(get("overflow"))

        val nanflowFactory = get("nanflow:type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, "Bin.nanflow:type")
        }
        val nanflow = nanflowFactory.fromJson(get("nanflow"))

        new Binned[Container[_], Container[_], Container[_], Container[_]](low, high, values, underflow, overflow, nanflow)

      case _ => throw new JsonFormatException(json, "Bin")
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

    def factory = Bin

    def +(that: Binned[V, U, O, N]) = {
      if (this.low != that.low)
        throw new AggregatorException(s"cannot add Bins because low differs (${this.low} vs ${that.low})")
      if (this.high != that.high)
        throw new AggregatorException(s"cannot add Bins because high differs (${this.high} vs ${that.high})")
      if (this.values.size != that.values.size)
        throw new AggregatorException(s"cannot add Bins because number of values differs (${this.values.size} vs ${that.values.size})")

      new Binned(
        low,
        high,
        this.values zip that.values map {case (me, you) => me + you},
        this.underflow + that.underflow,
        this.overflow + that.overflow,
        this.nanflow + that.nanflow)
    }

    def toJson = JsonObject(
      "low" -> JsonFloat(low),
      "high" -> JsonFloat(high),
      "values:type" -> JsonString(values.head.factory.name),
      "values" -> JsonArray(values.map(_.toJson): _*),
      "underflow:type" -> JsonString(underflow.factory.name),
      "underflow" -> underflow.toJson,
      "overflow:type" -> JsonString(overflow.factory.name),
      "overflow" -> overflow.toJson,
      "nanflow:type" -> JsonString(nanflow.factory.name),
      "nanflow" -> nanflow.toJson)

    def toStringParams = s"""$low, $high, Seq(${values.map(_.toString).mkString(", ")}), $underflow, $overflow, $nanflow"""
  }

  class Binning[DATUM : WeakTypeTag, V <: Container[V], U <: Container[U], O <: Container[O], N <: Container[N]](
    val key: ToNumeric[DATUM],
    val selection: Selection[DATUM],
    val low: Double,
    val high: Double,
    val values: Seq[Aggregator[DATUM, V]],
    val underflow: Aggregator[DATUM, U],
    val overflow: Aggregator[DATUM, O],
    val nanflow: Aggregator[DATUM, N]) extends Aggregator[DATUM, Binned[V, U, O, N]] {

    if (low >= high)
      throw new AggregatorException(s"low ($low) must be less than high ($high)")
    if (values.size < 1)
      throw new AggregatorException(s"values ($values) must have at least one element")

    def datumType = weakTypeOf[DATUM]

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

    def toStringParams = s"""$key, $selection, $low, $high, Seq(${values.map(_.toString).mkString(", ")}), $underflow, $overflow, $nanflow"""
  }
}

