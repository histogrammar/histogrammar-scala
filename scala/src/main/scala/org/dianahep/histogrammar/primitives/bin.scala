package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Bin/Binned/Binning
 
  object Bin extends Factory {
    val name = "Bin"

    def default[DATUM] = Count[DATUM]()

    def ed[V <: Container[V], U <: Container[U], O <: Container[O], N <: Container[N]]
      (low: Double,
       high: Double,
       values: Seq[V],
       underflow: U,
       overflow: O,
       nanflow: N) = new Binned[V, U, O, N](low, high, values, underflow, overflow, nanflow)

    def ing[DATUM, V <: Container[V], U <: Container[U], O <: Container[O], N <: Container[N]]
      (num: Int,
       low: Double,
       high: Double,
       quantity: NumericalFcn[DATUM],
       selection: Selection[DATUM] = unweighted[DATUM],
       value: => Aggregator[DATUM, V] = default[DATUM],
       underflow: Aggregator[DATUM, U] = default[DATUM],
       overflow: Aggregator[DATUM, O] = default[DATUM],
       nanflow: Aggregator[DATUM, N] = default[DATUM]) =
      new Binning[DATUM, V, U, O, N](low, high, quantity, selection, Array.fill(num)(value).toSeq, underflow, overflow, nanflow)

    def apply[DATUM, V <: Container[V], U <: Container[U], O <: Container[O], N <: Container[N]]
      (num: Int,
       low: Double,
       high: Double,
       quantity: NumericalFcn[DATUM],
       selection: Selection[DATUM] = unweighted[DATUM],
       value: => Aggregator[DATUM, V] = default[DATUM],
       underflow: Aggregator[DATUM, U] = default[DATUM],
       overflow: Aggregator[DATUM, O] = default[DATUM],
       nanflow: Aggregator[DATUM, N] = default[DATUM]) =
      ing(num, low, high, quantity, selection, value, underflow, overflow, nanflow)

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

    private[histogrammar] def plusErrors(
      thislow: Double, thishigh: Double, thisSize: Int, thisValues: Factory, thisUnderflow: Factory, thisOverflow: Factory, thisNanflow: Factory,
      thatlow: Double, thathigh: Double, thatSize: Int, thatValues: Factory, thatUnderflow: Factory, thatOverflow: Factory, thatNanflow: Factory) {
      if (thislow != thatlow)
        throw new AggregatorException(s"cannot add Bins because low differs ($thislow vs $thatlow)")
      if (thishigh != thathigh)
        throw new AggregatorException(s"cannot add Bins because high differs ($thishigh vs $thathigh)")
      if (thisSize != thatSize)
        throw new AggregatorException(s"cannot add Bins because number of values differs ($thisSize vs $thatSize)")
      if (thisValues != thatValues)
        throw new AggregatorException(s"cannot add Bins because values type differs ($thisValues vs $thatValues)")
      if (thisUnderflow != thatUnderflow)
        throw new AggregatorException(s"cannot add Bins because underflow type differs (${thisUnderflow.name} vs ${thatUnderflow.name})")
      if (thisOverflow != thatOverflow)
        throw new AggregatorException(s"cannot add Bins because overflow type differs (${thisOverflow.name} vs ${thatOverflow.name})")
      if (thisNanflow != thatNanflow)
        throw new AggregatorException(s"cannot add Bins because nanflow type differs (${thisNanflow.name} vs ${thatNanflow.name})")
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
      Bin.plusErrors(this.low, this.high, this.values.size, this.values.head.factory, this.underflow.factory, this.overflow.factory, this.nanflow.factory,
                     that.low, that.high, that.values.size, that.values.head.factory, that.underflow.factory, that.overflow.factory, that.nanflow.factory)
      new Binned[V, U, O, N](
        low,
        high,
        this.values zip that.values map {case (me, you) => me + you},
        this.underflow + that.underflow,
        this.overflow + that.overflow,
        this.nanflow + that.nanflow)
    }
    def +[DATUM](that: Binning[DATUM, V, U, O, N]) = {
      Bin.plusErrors(this.low, this.high, this.values.size, this.values.head.factory, this.underflow.factory, this.overflow.factory, this.nanflow.factory,
                     that.low, that.high, that.values.size, that.values.head.factory, that.underflow.factory, that.overflow.factory, that.nanflow.factory)
      new Binning[DATUM, V, U, O, N](
        low,
        high,
        that.quantity,
        that.selection,
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
    val values: Seq[Aggregator[DATUM, V]],
    val underflow: Aggregator[DATUM, U],
    val overflow: Aggregator[DATUM, O],
    val nanflow: Aggregator[DATUM, N]) extends Aggregator[DATUM, Binned[V, U, O, N]] with Bin.Methods {

    def factory = Bin

    if (low >= high)
      throw new AggregatorException(s"low ($low) must be less than high ($high)")
    if (values.size < 1)
      throw new AggregatorException(s"values ($values) must have at least one element")
    def num = values.size

    def +(that: Binned[V, U, O, N]) = {
      Bin.plusErrors(this.low, this.high, this.values.size, this.values.head.factory, this.underflow.factory, this.overflow.factory, this.nanflow.factory,
                     that.low, that.high, that.values.size, that.values.head.factory, that.underflow.factory, that.overflow.factory, that.nanflow.factory)
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
    def +(that: Binning[DATUM, V, U, O, N]) = {
      Bin.plusErrors(this.low, this.high, this.values.size, this.values.head.factory, this.underflow.factory, this.overflow.factory, this.nanflow.factory,
                     that.low, that.high, that.values.size, that.values.head.factory, that.underflow.factory, that.overflow.factory, that.nanflow.factory)
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

    def fill(x: Weighted[DATUM]) {
      val k = quantity(x)
      val y = x reweight selection(x)

      if (y.contributes) {
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

    def toContainer = new Binned(low, high, values.map(_.toContainer), underflow.toContainer, overflow.toContainer, nanflow.toContainer)

    override def toString() = s"Binning[low=$low, high=$high, values=[${values.head.toString}, size=${values.size}], underflow=$underflow, overflow=$overflow, nanflow=$nanflow]"
    override def equals(that: Any) = that match {
      case that: Binning[DATUM, V, U, O, N] => this.low === that.low  &&  this.high === that.high  &&  this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.values == that.values  &&  this.underflow == that.underflow  &&  this.overflow == that.overflow  &&  this.nanflow == that.nanflow
      case _ => false
    }
    override def hashCode() = (low, high, quantity, selection, values, underflow, overflow, nanflow).hashCode
  }
}
