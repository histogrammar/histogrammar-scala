package org.dianahep

import scala.collection.mutable
import scala.collection.immutable.SortedMap
import scala.language.existentials

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// SparselyBin/SparselyBinned/SparselyBinning

  object SparselyBin extends Factory {
    val name = "SparselyBin"
    val help = "Split a quantity into equally spaced bins, filling only one bin per datum and creating new bins as necessary."
    val detailedHelp ="""SparselyBin(binWidth: Double, quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM],
            value: => V = Count(), nanflow: N = Count(), origin: Double = 0.0)"""

    private val integerPattern = "-?[0-9]+".r

    def fixed[V <: Container[V], N <: Container[N]](binWidth: Double, values: SortedMap[Long, V], nanflow: N, origin: Double) =
      new SparselyBinned[V, N](binWidth, values, nanflow, origin)

    def apply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}]
      (binWidth: Double,
       quantity: NumericalFcn[DATUM],
       selection: Selection[DATUM] = unweighted[DATUM],
       value: => V = Count(),
       nanflow: N = Count(),
       origin: Double = 0.0) =
      new SparselyBinning[DATUM, V, N](binWidth, quantity, selection, value, mutable.HashMap[Long, V](), nanflow, origin)

    def unapply[V <: Container[V], N <: Container[N]](x: SparselyBinned[V, N]) = Some((x.binWidth, x.values, x.nanflow, x.origin))
    def unapply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](x: SparselyBinning[DATUM, V, N]) = Some((x.binWidth, x.values, x.nanflow, x.origin))

    trait Methods {
      def binWidth: Double
      def origin: Double

      def numFilled: Int
      def num: Long
      def minBin: Long
      def maxBin: Long
      def low: Double
      def high: Double

      def bin(k: Double): Long =
        if (nan(k))
          java.lang.Long.MIN_VALUE
        else
          Math.floor((k - origin) / binWidth).toLong

      def nan(k: Double): Boolean = k.isNaN
    }

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("binWidth", "values:type", "values", "nanflow:type", "nanflow", "origin")) =>
        val get = pairs.toMap

        val binWidth = get("binWidth") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".binWidth")
        }

        val valuesFactory = get("values:type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, name + ".values:type")
        }
        val values = get("values") match {
          case JsonObject(indexValues @ _*) =>
            SortedMap(indexValues map {
              case (JsonString(i), v) if (integerPattern.pattern.matcher(i).matches) => (i.toLong, valuesFactory.fromJsonFragment(v))
              case (i, _) => throw new JsonFormatException(i, name + s".values key $i must be an integer")
            }: _*)
          case x => throw new JsonFormatException(x, name + ".values")
        }

        val nanflowFactory = get("nanflow:type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, name + ".nanflow:type")
        }
        val nanflow = nanflowFactory.fromJsonFragment(get("nanflow"))

        val origin = get("origin") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".origin")
        }

        new SparselyBinned[Container[_], Container[_]](binWidth, values.asInstanceOf[SortedMap[Long, Container[_]]], nanflow, origin)

      case _ => throw new JsonFormatException(json, name)
    }
  }

  class SparselyBinned[V <: Container[V], N <: Container[N]](val binWidth: Double, val values: SortedMap[Long, V], val nanflow: N, val origin: Double) extends Container[SparselyBinned[V, N]] with SparselyBin.Methods {
    type Type = SparselyBinned[V, N]
    // type FixedType = SparselyBinned[V, N]
    def factory = SparselyBin

    if (binWidth <= 0.0)
      throw new ContainerException(s"binWidth ($binWidth) must be greater than zero")

    def +(that: SparselyBinned[V, N]) = {
      if (this.binWidth != that.binWidth)
        throw new ContainerException(s"cannot add SparselyBinned because binWidth differs (${this.binWidth} vs ${that.binWidth})")
      if (this.origin != that.origin)
        throw new ContainerException(s"cannot add SparselyBinned because origin differs (${this.origin} vs ${that.origin})")

      val newvalues =
        SortedMap[Long, V]((this.values.keySet union that.values.keySet).toSeq map {case i =>
          (this.values.get(i), that.values.get(i)) match {
            case (Some(v1), Some(v2)) => i -> (v1 + v2)
            case (Some(v1), None) => i -> v1
            case (None, Some(v2)) => i -> v2
            case _ => throw new Exception("can't get here")
          }
        }: _*)

      new SparselyBinned[V, N](binWidth, newvalues, this.nanflow + that.nanflow, origin)
    }

    def numFilled = values.size
    def num = if (values.isEmpty) -1L else values.last._1 - values.head._1
    def minBin = if (values.isEmpty) java.lang.Long.MIN_VALUE else values.head._1
    def maxBin = if (values.isEmpty) java.lang.Long.MIN_VALUE else values.last._1
    def low = if (values.isEmpty) java.lang.Double.NaN else minBin * binWidth + origin
    def high = if (values.isEmpty) java.lang.Double.NaN else (maxBin + 1L) * binWidth + origin
    def at(i: Long) = values.find(_._1 == i).map(_._2)

    // def fix = this
    def toJsonFragment = JsonObject(
      "binWidth" -> JsonFloat(binWidth),
      "values:type" -> JsonString(if (values.isEmpty) "?" else values.head._2.factory.name),
      "values" -> JsonObject(values.toSeq map {case (i, v) => (JsonString(i.toString), v.toJsonFragment)}: _*),
      "nanflow:type" -> JsonString(nanflow.factory.name),
      "nanflow" -> nanflow.toJsonFragment,
      "origin" -> JsonFloat(origin))

    override def toString() = s"""SparselyBinned[binWidth=$binWidth, values=[${if (values.isEmpty) "?" else values.head._2.toString}, size=${values.size}], nanflow=$nanflow, origin=$origin]"""
    override def equals(that: Any) = that match {
      case that: SparselyBinned[V, N] => this.binWidth === that.binWidth  &&  this.values == that.values  &&  this.nanflow == that.nanflow  &&  this.origin === that.origin
      case _ => false
    }
    override def hashCode() = (binWidth, values, nanflow, origin).hashCode
  }

  class SparselyBinning[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](
    val binWidth: Double,
    val quantity: NumericalFcn[DATUM],
    val selection: Selection[DATUM],
    value: => V,
    val values: mutable.Map[Long, V],
    val nanflow: N,
    val origin: Double) extends Container[SparselyBinning[DATUM, V, N]] with AggregationOnData with SparselyBin.Methods {

    type Type = SparselyBinning[DATUM, V, N]
    // type FixedType = SparselyBinned[V#FixedType, N#FixedType]
    type Datum = DATUM
    def factory = SparselyBin

    if (binWidth <= 0.0)
      throw new ContainerException(s"binWidth ($binWidth) must be greater than zero")

    def +(that: SparselyBinning[DATUM, V, N]) = {
      if (this.binWidth != that.binWidth)
        throw new ContainerException(s"cannot add SparselyBinning because binWidth differs (${this.binWidth} vs ${that.binWidth})")
      if (this.origin != that.origin)
        throw new ContainerException(s"cannot add SparselyBinning because origin differs (${this.origin} vs ${that.origin})")

      val newvalues =
        mutable.Map[Long, V]((this.values.keySet union that.values.keySet).toSeq map {case i =>
          (this.values.get(i), that.values.get(i)) match {
            case (Some(v1), Some(v2)) => i -> (v1 + v2)
            case (Some(v1), None) => i -> v1
            case (None, Some(v2)) => i -> v2
            case _ => throw new Exception("can't get here")
          }
        }: _*)

      new SparselyBinning[DATUM, V, N](binWidth, this.quantity, this.selection, this.value, newvalues, this.nanflow + that.nanflow, origin)
    }

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)

        if (nan(q))
          nanflow.fillWeighted(datum, w)
        else {
          val b = bin(q)
          if (!(values contains b))
            values.update(b, value)
          values(b).fillWeighted(datum, w)
        }
      }
    }

    def numFilled = values.size
    def num = if (values.isEmpty) -1L else values.map(_._1).max - values.map(_._1).min
    def minBin = if (values.isEmpty) java.lang.Long.MIN_VALUE else values.map(_._1).min
    def maxBin = if (values.isEmpty) java.lang.Long.MIN_VALUE else values.map(_._1).max
    def low = if (values.isEmpty) java.lang.Double.NaN else minBin * binWidth + origin
    def high = if (values.isEmpty) java.lang.Double.NaN else (maxBin + 1L) * binWidth + origin
    def at(i: Long) = values.get(i)

    // def fix = new SparselyBinned(binWidth, SortedMap(values.toSeq map {case (b, v) => (b, v.fix)}: _*), nanflow.fix, origin)
    // def toJsonFragment = fix.toJsonFragment
    def toJsonFragment = JsonObject(
      "binWidth" -> JsonFloat(binWidth),
      "values:type" -> JsonString(if (values.isEmpty) "?" else values.head._2.factory.name),
      "values" -> JsonObject(values.toSeq map {case (i, v) => (JsonString(i.toString), v.toJsonFragment)}: _*),
      "nanflow:type" -> JsonString(nanflow.factory.name),
      "nanflow" -> nanflow.toJsonFragment,
      "origin" -> JsonFloat(origin))

    override def toString() = s"""SparselyBinning[binWidth=$binWidth, values=[${value.toString}, size=${values.size}], nanflow=$nanflow, origin=$origin]"""
    override def equals(that: Any) = that match {
      case that: SparselyBinning[DATUM, V, N] => this.binWidth === that.binWidth  &&  this.values == that.values  &&  this.nanflow == that.nanflow  &&  this.origin === that.origin
      case _ => false
    }
    override def hashCode() = (binWidth, values, nanflow, origin).hashCode
  }
}
