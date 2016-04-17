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
    val detailedHelp = """SparselyBin(binWidth: Double, quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM],
            value: => V = Count(), nanflow: N = Count(), origin: Double = 0.0)"""

    private val integerPattern = "-?[0-9]+".r

    def ed[V <: Container[V], N <: Container[N]](binWidth: Double, entries: Double, contentType: String, bins: SortedMap[Long, V], nanflow: N, origin: Double) =
      new SparselyBinned[V, N](binWidth, entries, contentType, bins, nanflow, origin)

    def apply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}]
      (binWidth: Double,
       quantity: NumericalFcn[DATUM],
       selection: Selection[DATUM] = unweighted[DATUM],
       value: => V = Count(),
       nanflow: N = Count(),
       origin: Double = 0.0) =
      new SparselyBinning[DATUM, V, N](binWidth, quantity, selection, 0.0, value, mutable.HashMap[Long, V](), nanflow, origin)

    def unapply[V <: Container[V], N <: Container[N]](x: SparselyBinned[V, N]) = Some((x.binWidth, x.entries, x.bins, x.nanflow, x.origin))
    def unapply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](x: SparselyBinning[DATUM, V, N]) = Some((x.binWidth, x.entries, x.bins, x.nanflow, x.origin))

    trait Methods {
      def binWidth: Double
      def origin: Double

      def numFilled: Int
      def num: Long
      def minBin: Long
      def maxBin: Long
      def low: Double
      def high: Double
      def range(index: Long): (Double, Double)
      def indexes: Seq[Long]

      def bin(k: Double): Long =
        if (nan(k))
          java.lang.Long.MIN_VALUE
        else
          Math.floor((k - origin) / binWidth).toLong

      def nan(k: Double): Boolean = k.isNaN
    }

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("binWidth", "entries", "bins:type", "bins", "nanflow:type", "nanflow", "origin")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val binWidth = get("binWidth") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".binWidth")
        }

        val (contentType, binsFactory) = get("bins:type") match {
          case JsonString(name) => (name, Factory(name))
          case x => throw new JsonFormatException(x, name + ".bins:type")
        }
        val bins = get("bins") match {
          case JsonObject(indexBins @ _*) =>
            SortedMap(indexBins map {
              case (JsonString(i), v) if (integerPattern.pattern.matcher(i).matches) => (i.toLong, binsFactory.fromJsonFragment(v))
              case (i, _) => throw new JsonFormatException(i, name + s".bins key $i must be an integer")
            }: _*)
          case x => throw new JsonFormatException(x, name + ".bins")
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

        new SparselyBinned[Container[_], Container[_]](binWidth, entries, contentType, bins.asInstanceOf[SortedMap[Long, Container[_]]], nanflow, origin)

      case _ => throw new JsonFormatException(json, name)
    }
  }

  class SparselyBinned[V <: Container[V], N <: Container[N]](val binWidth: Double, val entries: Double, contentType: String, val bins: SortedMap[Long, V], val nanflow: N, val origin: Double) extends Container[SparselyBinned[V, N]] with SparselyBin.Methods {
    type Type = SparselyBinned[V, N]
    def factory = SparselyBin

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (binWidth <= 0.0)
      throw new ContainerException(s"binWidth ($binWidth) must be greater than zero")

    def zero = new SparselyBinned[V, N](binWidth, 0.0, contentType, SortedMap(bins.toSeq map {case (b, v) => (b, v.zero)}: _*), nanflow.zero, origin)
    def +(that: SparselyBinned[V, N]) = {
      if (this.binWidth != that.binWidth)
        throw new ContainerException(s"cannot add SparselyBinned because binWidth differs (${this.binWidth} vs ${that.binWidth})")
      if (this.origin != that.origin)
        throw new ContainerException(s"cannot add SparselyBinned because origin differs (${this.origin} vs ${that.origin})")

      val newbins =
        SortedMap[Long, V]((this.bins.keySet union that.bins.keySet).toSeq map {case i =>
          (this.bins.get(i), that.bins.get(i)) match {
            case (Some(v1), Some(v2)) => i -> (v1 + v2)
            case (Some(v1), None) => i -> v1
            case (None, Some(v2)) => i -> v2
            case _ => throw new Exception("can't get here")
          }
        }: _*)

      new SparselyBinned[V, N](binWidth, this.entries + that.entries, contentType, newbins, this.nanflow + that.nanflow, origin)
    }

    def numFilled = bins.size
    def num = if (bins.isEmpty) -1L else bins.last._1 - bins.head._1
    def minBin = if (bins.isEmpty) java.lang.Long.MIN_VALUE else bins.head._1
    def maxBin = if (bins.isEmpty) java.lang.Long.MIN_VALUE else bins.last._1
    def low = if (bins.isEmpty) java.lang.Double.NaN else minBin * binWidth + origin
    def high = if (bins.isEmpty) java.lang.Double.NaN else (maxBin + 1L) * binWidth + origin
    def at(index: Long) = bins.find(_._1 == index).map(_._2)
    def indexes = bins.map(_._1).toSeq
    def range(index: Long) = (index * binWidth + origin, (index + 1) * binWidth + origin)
    def values = bins.map(_._2)

    def toJsonFragment = JsonObject(
      "binWidth" -> JsonFloat(binWidth),
      "entries" -> JsonFloat(entries),
      "bins:type" -> JsonString(if (bins.isEmpty) contentType else bins.head._2.factory.name),
      "bins" -> JsonObject(bins.toSeq map {case (i, v) => (JsonString(i.toString), v.toJsonFragment)}: _*),
      "nanflow:type" -> JsonString(nanflow.factory.name),
      "nanflow" -> nanflow.toJsonFragment,
      "origin" -> JsonFloat(origin))

    override def toString() = s"""SparselyBinned[binWidth=$binWidth, entries=$entries, bins=[${if (bins.isEmpty) contentType else bins.head.toString}..., size=${bins.size}], nanflow=$nanflow, origin=$origin]"""
    override def equals(that: Any) = that match {
      case that: SparselyBinned[V, N] => this.binWidth === that.binWidth  &&  this.entries === that.entries  &&  this.bins == that.bins  &&  this.nanflow == that.nanflow  &&  this.origin === that.origin
      case _ => false
    }
    override def hashCode() = (binWidth, entries, bins, nanflow, origin).hashCode
  }

  class SparselyBinning[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}]
    (val binWidth: Double,
     val quantity: NumericalFcn[DATUM],
     val selection: Selection[DATUM],
     var entries: Double,
     value: => V,
     val bins: mutable.Map[Long, V],
     val nanflow: N,
     val origin: Double) extends Container[SparselyBinning[DATUM, V, N]] with AggregationOnData with SparselyBin.Methods {

    type Type = SparselyBinning[DATUM, V, N]
    type Datum = DATUM
    def factory = SparselyBin

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (binWidth <= 0.0)
      throw new ContainerException(s"binWidth ($binWidth) must be greater than zero")

    def zero = new SparselyBinning[DATUM, V, N](binWidth, quantity, selection, 0.0, value, mutable.Map(bins.toSeq map {case (b, v) => (b, v.zero)}: _*), nanflow.zero, origin)
    def +(that: SparselyBinning[DATUM, V, N]) = {
      if (this.binWidth != that.binWidth)
        throw new ContainerException(s"cannot add SparselyBinning because binWidth differs (${this.binWidth} vs ${that.binWidth})")
      if (this.origin != that.origin)
        throw new ContainerException(s"cannot add SparselyBinning because origin differs (${this.origin} vs ${that.origin})")

      val newbins =
        mutable.Map[Long, V]((this.bins.keySet union that.bins.keySet).toSeq map {case i =>
          (this.bins.get(i), that.bins.get(i)) match {
            case (Some(v1), Some(v2)) => i -> (v1 + v2)
            case (Some(v1), None) => i -> v1
            case (None, Some(v2)) => i -> v2
            case _ => throw new Exception("can't get here")
          }
        }: _*)

      new SparselyBinning[DATUM, V, N](binWidth, this.quantity, this.selection, this.entries + that.entries, this.value, newbins, this.nanflow + that.nanflow, origin)
    }

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)

        entries += w
        if (nan(q))
          nanflow.fillWeighted(datum, w)
        else {
          val b = bin(q)
          if (!(bins contains b))
            bins.update(b, value)
          bins(b).fillWeighted(datum, w)
        }
      }
    }

    def numFilled = bins.size
    def num = if (bins.isEmpty) -1L else bins.map(_._1).max - bins.map(_._1).min
    def minBin = if (bins.isEmpty) java.lang.Long.MIN_VALUE else bins.map(_._1).min
    def maxBin = if (bins.isEmpty) java.lang.Long.MIN_VALUE else bins.map(_._1).max
    def low = if (bins.isEmpty) java.lang.Double.NaN else minBin * binWidth + origin
    def high = if (bins.isEmpty) java.lang.Double.NaN else (maxBin + 1L) * binWidth + origin
    def at(index: Long) = bins.get(index)
    def indexes = bins.map(_._1).toSeq
    def range(index: Long) = (index * binWidth + origin, (index + 1) * binWidth + origin)
    def values = bins.map(_._2)

    def toJsonFragment = JsonObject(
      "binWidth" -> JsonFloat(binWidth),
      "entries" -> JsonFloat(entries),
      "bins:type" -> JsonString(value.factory.name),
      "bins" -> JsonObject(bins.toSeq map {case (i, v) => (JsonString(i.toString), v.toJsonFragment)}: _*),
      "nanflow:type" -> JsonString(nanflow.factory.name),
      "nanflow" -> nanflow.toJsonFragment,
      "origin" -> JsonFloat(origin))

    override def toString() = s"""SparselyBinning[binWidth=$binWidth, entries=$entries, bins=[${if (bins.isEmpty) value.factory.name else bins.head.toString}, size=${bins.size}], nanflow=$nanflow, origin=$origin]"""
    override def equals(that: Any) = that match {
      case that: SparselyBinning[DATUM, V, N] => this.binWidth === that.binWidth  &&  this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.entries === that.entries  &&  this.bins == that.bins  &&  this.nanflow == that.nanflow  &&  this.origin === that.origin
      case _ => false
    }
    override def hashCode() = (binWidth, quantity, selection, entries, bins, nanflow, origin).hashCode
  }
}
