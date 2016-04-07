package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Bin/Binned/Binning
 
  object Bin extends Factory {
    val name = "Bin"

    def container[V <: Container[V], U <: Container[U], O <: Container[O], N <: Container[N]]
      (low: Double,
       high: Double,
       values: Seq[V],
       underflow: U,
       overflow: O,
       nanflow: N) = new Binned[V, U, O, N](low, high, values, underflow, overflow, nanflow)

    def apply[DATUM, V <: Container[V], U <: Container[U], O <: Container[O], N <: Container[N]]
      (num: Int,
       low: Double,
       high: Double,
       quantity: NumericalFcn[DATUM],
       selection: Selection[DATUM] = unweighted[DATUM],
       value: => V = Count(),
       underflow: U = Count(),
       overflow: O = Count(),
       nanflow: N = Count()) =
      new Binning[DATUM, V, U, O, N](low, high, quantity, selection, Seq.fill(num)(value), underflow, overflow, nanflow)

    def unapply[V <: Container[V], U <: Container[U], O <: Container[O], N <: Container[N]](x: Binned[V, U, O, N]) = Some((x.values, x.underflow, x.overflow, x.nanflow))
    def unapply[DATUM, V <: Container[V], U <: Container[U], O <: Container[O], N <: Container[N]](x: Binning[DATUM, V, U, O, N]) = Some((x.values, x.underflow, x.overflow, x.nanflow))

    trait Methods {
      def num: Int
      def low: Double
      def high: Double

      def bin(k: Double): Int =
        if (under(k)  ||  over(k)  ||  nan(k))
          -1
        else
          Math.floor(num * (k - low) / (high - low)).toInt

      def under(k: Double): Boolean = !k.isNaN  &&  k < low
      def over(k: Double): Boolean = !k.isNaN  &&  k >= high
      def nan(k: Double): Boolean = k.isNaN
    }

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("low", "high", "values:type", "values", "underflow:type", "underflow", "overflow:type", "overflow", "nanflow:type", "nanflow")) =>
        val get = pairs.toMap

        val low = get("low") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".low")
        }

        val high = get("high") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".high")
        }

        val valuesFactory = get("values:type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, name + ".values:type")
        }
        val values = get("values") match {
          case JsonArray(sub @ _*) => sub.map(valuesFactory.fromJsonFragment(_))
          case x => throw new JsonFormatException(x, name + ".values")
        }

        val underflowFactory = get("underflow:type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, name + ".underflow:type")
        }
        val underflow = underflowFactory.fromJsonFragment(get("underflow"))

        val overflowFactory = get("overflow:type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, name + ".overflow:type")
        }
        val overflow = overflowFactory.fromJsonFragment(get("overflow"))

        val nanflowFactory = get("nanflow:type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, name + ".nanflow:type")
        }
        val nanflow = nanflowFactory.fromJsonFragment(get("nanflow"))

        new Binned[Container[_], Container[_], Container[_], Container[_]](low, high, values, underflow, overflow, nanflow)

      case _ => throw new JsonFormatException(json, name)
    }
  }

  class Binned[V <: Container[V], U <: Container[U], O <: Container[O], N <: Container[N]](
    val low: Double,
    val high: Double,
    val values: Seq[V],
    val underflow: U,
    val overflow: O,
    val nanflow: N) extends Container[Binned[V, U, O, N]] with Bin.Methods {

    if (low >= high)
      throw new AggregatorException(s"low ($low) must be less than high ($high)")
    if (values.size < 1)
      throw new AggregatorException(s"values ($values) must have at least one element")
    def num = values.size

    def factory = Bin

    def +(that: Binned[V, U, O, N]) = {
      if (this.low != that.low)
        throw new AggregatorException(s"cannot add Binned because low differs (${this.low} vs ${that.low})")
      if (this.high != that.high)
        throw new AggregatorException(s"cannot add Binned because high differs (${this.high} vs ${that.high})")
      if (this.values.size != that.values.size)
        throw new AggregatorException(s"cannot add Binned because number of values differs (${this.values.size} vs ${that.values.size})")
      if (this.values.isEmpty)
        throw new AggregatorException(s"cannot add Binned because number of values is zero")
      if (this.values.head.factory != that.values.head.factory)
        throw new AggregatorException(s"cannot add Binned because values type differs (${this.values.head.factory.name} vs ${that.values.head.factory.name})")
      if (this.underflow.factory != that.underflow.factory)
        throw new AggregatorException(s"cannot add Binned because underflow type differs (${this.underflow.factory.name} vs ${that.underflow.factory.name})")
      if (this.overflow.factory != that.overflow.factory)
        throw new AggregatorException(s"cannot add Binned because overflow type differs (${this.overflow.factory.name} vs ${that.overflow.factory.name})")
      if (this.nanflow.factory != that.nanflow.factory)
        throw new AggregatorException(s"cannot add Binned because nanflow type differs (${this.nanflow.factory.name} vs ${that.nanflow.factory.name})")

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
    override def equals(that: Any) = that match {
      case that: Binned[V, U, O, N] => this.low === that.low  &&  this.high === that.high  &&  this.values == that.values  &&  this.underflow == that.underflow  &&  this.overflow == that.overflow  &&  this.nanflow == that.nanflow
      case _ => false
    }
    override def hashCode() = (low, high, values, underflow, overflow, nanflow).hashCode
  }

  class Binning[DATUM, V <: Container[V], U <: Container[U], O <: Container[O], N <: Container[N]](
    val low: Double,
    val high: Double,
    val quantity: NumericalFcn[DATUM],
    val selection: Selection[DATUM],
    val values: Seq[V],
    val underflow: U,
    val overflow: O,
    val nanflow: N) extends Container[Binning[DATUM, V, U, O, N]] with Aggregation[DATUM] with Bin.Methods {

    def factory = Bin

    if (low >= high)
      throw new AggregatorException(s"low ($low) must be less than high ($high)")
    if (values.size < 1)
      throw new AggregatorException(s"values ($values) must have at least one element")
    def num = values.size

    if (!values.forall(_.isInstanceOf[Aggregation[DATUM]]))
      throw new AggregatorException(s"Binning should be built with Aggregation-enabled values (ending in -ing)")
    if (!underflow.isInstanceOf[Aggregation[DATUM]])
      throw new AggregatorException(s"Binning should be built with Aggregation-enabled underflow (ending in -ing)")
    if (!overflow.isInstanceOf[Aggregation[DATUM]])
      throw new AggregatorException(s"Binning should be built with Aggregation-enabled overflow (ending in -ing)")
    if (!nanflow.isInstanceOf[Aggregation[DATUM]])
      throw new AggregatorException(s"Binning should be built with Aggregation-enabled nanflow (ending in -ing)")

    def +(that: Binning[DATUM, V, U, O, N]) = {
      if (this.low != that.low)
        throw new AggregatorException(s"cannot add Binning because low differs (${this.low} vs ${that.low})")
      if (this.high != that.high)
        throw new AggregatorException(s"cannot add Binning because high differs (${this.high} vs ${that.high})")
      if (this.values.size != that.values.size)
        throw new AggregatorException(s"cannot add Binning because number of values differs (${this.values.size} vs ${that.values.size})")
      if (this.values.isEmpty)
        throw new AggregatorException(s"cannot add Binning because number of values is zero")
      if (this.values.head.factory != that.values.head.factory)
        throw new AggregatorException(s"cannot add Binning because values type differs (${this.values.head.factory.name} vs ${that.values.head.factory.name})")
      if (this.underflow.factory != that.underflow.factory)
        throw new AggregatorException(s"cannot add Binning because underflow type differs (${this.underflow.factory.name} vs ${that.underflow.factory.name})")
      if (this.overflow.factory != that.overflow.factory)
        throw new AggregatorException(s"cannot add Binning because overflow type differs (${this.overflow.factory.name} vs ${that.overflow.factory.name})")
      if (this.nanflow.factory != that.nanflow.factory)
        throw new AggregatorException(s"cannot add Binning because nanflow type differs (${this.nanflow.factory.name} vs ${that.nanflow.factory.name})")

      new Binning[DATUM, V, U, O, N](
        low,
        high,
        this.quantity,
        this.selection,
        this.values zip that.values map {case (me, you) => me + you},
        this.underflow + that.underflow,
        this.overflow + that.overflow,
        this.nanflow + that.nanflow)
    }

    def fillWeighted(x: Weighted[DATUM]) {
      val k = quantity(x)
      val y = x reweight selection(x)
      if (y.contributes) {
        if (under(k))
          underflow.asInstanceOf[Aggregation[DATUM]].fillWeighted(y)
        else if (over(k))
          overflow.asInstanceOf[Aggregation[DATUM]].fillWeighted(y)
        else if (nan(k))
          nanflow.asInstanceOf[Aggregation[DATUM]].fillWeighted(y)
        else
          values(bin(k)).asInstanceOf[Aggregation[DATUM]].fillWeighted(y)
      }
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

    override def toString() = s"Binning[low=$low, high=$high, values=[${values.head.toString}, size=${values.size}], underflow=$underflow, overflow=$overflow, nanflow=$nanflow]"
    override def equals(that: Any) = that match {
      case that: Binning[DATUM, V, U, O, N] => this.low === that.low  &&  this.high === that.high  &&  this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.values == that.values  &&  this.underflow == that.underflow  &&  this.overflow == that.overflow  &&  this.nanflow == that.nanflow
      case _ => false
    }
    override def hashCode() = (low, high, quantity, selection, values, underflow, overflow, nanflow).hashCode
  }
}
