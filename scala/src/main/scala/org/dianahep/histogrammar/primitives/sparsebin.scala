package org.dianahep

import scala.collection.mutable
import scala.collection.immutable.SortedSet
import scala.language.existentials

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// SparselyBinned/SparselyBinning

  object SparselyBinned extends ContainerFactory {
    val name = "SparselyBinned"

    private val integerPattern = "-?[0-9]+".r

    def apply[V <: Container[V], N <: Container[N]](binWidth: Double, origin: Double)(values: SortedSet[(Long, V)] = SortedSet[(Long, V)]()(Ordering.by[(Long, V), Long](_._1)), nanflow: N) =
      new SparselyBinned[V, N](binWidth, origin, values, nanflow)

    def unapply[V <: Container[V], N <: Container[N]](x: SparselyBinned[V, N]) = Some((x.binWidth, x.origin, x.values, x.nanflow))

    trait Methods[V <: Container[V]] {
      def binWidth: Double
      def origin: Double

      def numFilled: Int
      def num: Long
      def minBin: Long
      def maxBin: Long
      def low: Double
      def high: Double
      def at(i: Long): Option[V]

      def bin(k: Double): Long =
        if (nan(k))
          java.lang.Long.MIN_VALUE
        else
          Math.floor((k - origin) / binWidth).toLong

      def nan(k: Double): Boolean = k.isNaN
    }

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("binWidth", "origin", "values:type", "values", "nanflow:type", "nanflow")) =>
        val get = pairs.toMap

        val binWidth = get("binWidth") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, "SparselyBinned.binWidth")
        }

        val origin = get("origin") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, "SparselyBinned.origin")
        }

        val valuesFactory = get("values:type") match {
          case JsonString(name) => ContainerFactory(name)
          case x => throw new JsonFormatException(x, "SparselyBinned.values:type")
        }
        val values = get("values") match {
          case JsonObject(indexValues @ _*) =>
            SortedSet(indexValues map {
              case (JsonString(i), v) if (integerPattern.pattern.matcher(i).matches) => (i.toLong, valuesFactory.fromJsonFragment(v))
              case (i, _) => throw new JsonFormatException(i, s"SparselyBinned.values key $i must be an integer")
            }: _*)(Ordering.by(_._1))
          case x => throw new JsonFormatException(x, "SparselyBinned.values")
        }

        val nanflowFactory = get("nanflow:type") match {
          case JsonString(name) => ContainerFactory(name)
          case x => throw new JsonFormatException(x, "SparselyBinned.nanflow:type")
        }
        val nanflow = nanflowFactory.fromJsonFragment(get("nanflow"))

        new SparselyBinned[Container[_], Container[_]](binWidth, origin, values.asInstanceOf[SortedSet[(Long, Container[_])]], nanflow)

      case _ => throw new JsonFormatException(json, "SparselyBinned")
    }
  }
  class SparselyBinned[V <: Container[V], N <: Container[N]](val binWidth: Double, val origin: Double, val values: SortedSet[(Long, V)], val nanflow: N) extends Container[SparselyBinned[V, N]] with SparselyBinned.Methods[V] {
    if (binWidth <= 0.0)
      throw new AggregatorException(s"binWidth ($binWidth) must be greater than zero")

    def factory = SparselyBinned

    def +(that: SparselyBinned[V, N]) = {
      if (this.origin != that.origin)
        throw new AggregatorException(s"cannot add SparselyBinned because origin differs (${this.origin} vs ${that.origin})")

      val thislookup = this.values.toMap
      val thatlookup = that.values.toMap

      val newvalues =
        SortedSet((this.values union that.values).toSeq map {case (i, v) =>
          (thislookup.get(i), thatlookup.get(i)) match {
            case (Some(v1), Some(v2)) => i -> (v1 + v2)
            case _ => i -> v
          }
        }: _*)(Ordering.by[(Long, V), Long](_._1))

      new SparselyBinned(binWidth, origin, newvalues, this.nanflow + that.nanflow)
    }

    def numFilled = values.size
    def num = if (values.isEmpty) -1L else values.last._1 - values.head._1
    def minBin = if (values.isEmpty) java.lang.Long.MIN_VALUE else values.head._1
    def maxBin = if (values.isEmpty) java.lang.Long.MIN_VALUE else values.last._1
    def low = if (values.isEmpty) java.lang.Double.NaN else minBin * binWidth + origin
    def high = if (values.isEmpty) java.lang.Double.NaN else (maxBin + 1L) * binWidth + origin
    def at(i: Long) = values.find(_._1 == i).map(_._2)

    def toJsonFragment = JsonObject(
      "binWidth" -> JsonFloat(binWidth),
      "origin" -> JsonFloat(origin),
      "values:type" -> JsonString(if (values.isEmpty) "?" else values.head._2.factory.name),
      "values" -> JsonObject(values.toSeq map {case (i, v) => (JsonString(i.toString), v.toJsonFragment)}: _*),
      "nanflow:type" -> JsonString(nanflow.factory.name),
      "nanflow" -> nanflow.toJsonFragment)

    override def toString() = s"""SparselyBinned[binWidth=$binWidth, origin=$origin, values=[${if (values.isEmpty) "?" else values.head._2.toString}, size=${values.size}], nanflow=$nanflow]"""
    override def equals(that: Any) = that match {
      case that: SparselyBinned[V, N] => this.binWidth == that.binWidth  &&  this.origin == that.origin  &&  this.values == that.values  &&  this.nanflow == that.nanflow
      case _ => false
    }
    override def hashCode() = (binWidth, origin, values, nanflow).hashCode
  }

  object SparselyBinning extends AggregatorFactory {
    def apply[DATUM, V <: Container[V], N <: Container[N]]
      (binWidth: Double,
       key: NumericalFcn[DATUM],
       selection: Selection[DATUM] = unweighted[DATUM],
       origin: Double = 0.0)
      (value: => Aggregator[DATUM, V] = Counting[DATUM](),
       nanflow: Aggregator[DATUM, N] = Counting[DATUM]()) =
      new SparselyBinning[DATUM, V, N](binWidth, origin, key, selection, value, mutable.HashMap[Long, Aggregator[DATUM, V]](), nanflow)

    def unapply[DATUM, V <: Container[V], N <: Container[N]](x: SparselyBinning[DATUM, V, N]) = Some((x.binWidth, x.origin, x.values, x.nanflow))
  }
  class SparselyBinning[DATUM, V <: Container[V], N <: Container[N]](val binWidth: Double, val origin: Double, val key: NumericalFcn[DATUM], val selection: Selection[DATUM], value: => Aggregator[DATUM, V], val values: mutable.Map[Long, Aggregator[DATUM, V]], val nanflow: Aggregator[DATUM, N]) extends Aggregator[DATUM, SparselyBinned[V, N]] with SparselyBinned.Methods[V] {
    if (binWidth <= 0.0)
      throw new AggregatorException(s"binWidth ($binWidth) must be greater than zero")

    def fill(x: Weighted[DATUM]) {
      val k = key(x)
      val y = x reweight selection(x)
      if (y.contributes) {
        if (nan(k))
          nanflow.fill(y)
        else {
          val b = bin(k)
          if (!(values contains b))
            values.update(b, value)
          values(b).fill(y)
        }
      }
    }

    def numFilled = values.size
    def num = if (values.isEmpty) -1L else values.map(_._1).max - values.map(_._1).min
    def minBin = if (values.isEmpty) java.lang.Long.MIN_VALUE else values.map(_._1).min
    def maxBin = if (values.isEmpty) java.lang.Long.MIN_VALUE else values.map(_._1).max
    def low = if (values.isEmpty) java.lang.Double.NaN else minBin * binWidth + origin
    def high = if (values.isEmpty) java.lang.Double.NaN else (maxBin + 1L) * binWidth + origin
    def at(i: Long) = values.get(i).map(_.fix)

    def fix = new SparselyBinned(binWidth, origin, SortedSet(values.toSeq map {case (i, v) => (i -> v.fix)}: _*)(Ordering.by[(Long, V), Long](_._1)), nanflow.fix)

    override def toString() = s"""SparselyBinning[binWidth=$binWidth, origin=$origin, values=[${value.toString}, size=${values.size}], nanflow=$nanflow]"""
    override def equals(that: Any) = that match {
      case that: SparselyBinning[DATUM, V, N] => this.binWidth == that.binWidth  &&  this.origin == that.origin  &&  this.values == that.values  &&  this.nanflow == that.nanflow
      case _ => false
    }
    override def hashCode() = (binWidth, origin, values, nanflow).hashCode
  }
}
