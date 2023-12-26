// Copyright 2016 DIANA-HEP
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.dianahep

import scala.collection.mutable
import scala.collection.immutable.SortedMap
import scala.language.existentials

import org.dianahep.histogrammar.json._
import org.dianahep.histogrammar.util._

package histogrammar {
  //////////////////////////////////////////////////////////////// SparselyBin/SparselyBinned/SparselyBinning

  /** Split a quantity into equally spaced bins, creating them whenever their `entries` would be non-zero. Exactly one sub-aggregator is filled per datum.
    * 
    * Use this when you have a distribution of known scale (bin width) but unknown domain (lowest and highest bin index).
    * 
    * Unlike fixed-domain binning, this aggregator has the potential to use unlimited memory. A large number of ''distinct'' outliers can generate many unwanted bins.
    * 
    * Like fixed-domain binning, the bins are indexed by integers, though they are 64-bit and may be negative. Bin indexes below `-(2**63 - 1)` are put in the `-(2**63 - 1)` are bin and indexes above `(2**63 - 1)` are put in the `(2**63 - 1)` bin.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.SparselyBinning]] and immutable [[org.dianahep.histogrammar.SparselyBinned]] objects.
    */
  object SparselyBin extends Factory {
    val name = "SparselyBin"
    val help = "Split a quantity into equally spaced bins, creating them whenever their `entries` would be non-zero. Exactly one sub-aggregator is filled per datum."
    val detailedHelp = """Use this when you have a distribution of known scale (bin width) but unknown domain (lowest and highest bin index).

Unlike fixed-domain binning, this aggregator has the potential to use unlimited memory. A large number of _distinct_ outliers can generate many unwanted bins.

Like fixed-domain binning, the bins are indexed by integers, though they are 64-bit and may be negative. Bin indexes below `-(2**63 - 1)` are put in the `-(2**63 - 1)` are bin and indexes above `(2**63 - 1)` are put in the `(2**63 - 1)` bin."""

    private val integerPattern = "-?[0-9]+".r

    /** Create an immutable [[org.dianahep.histogrammar.SparselyBinned]] from arguments (instead of JSON).
      * 
      * @param binWidth Width of the equally sized bins.
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param contentType Name of the intended content; used as a placeholder in cases with zero bins (due to no observed data).
      * @param bins Centers and values of each bin.
      * @param nanflow Container for data that resulted in `NaN`.
      * @param origin Left edge of the bin whose index is zero.
      */
    def ed[V <: Container[V] with NoAggregation, N <: Container[N] with NoAggregation](binWidth: Double, entries: Double, contentType: String, bins: SortedMap[Long, V], nanflow: N, origin: Double) =
      new SparselyBinned[V, N](binWidth, entries, None, contentType, bins, nanflow, origin)

    /** Create an empty, mutable [[org.dianahep.histogrammar.SparselyBinning]].
      * 
      * @param binWidth Width of the equally sized bins.
      * @param quantity Numerical function to split into bins.
      * @param value Template used to create zero values (by calling this `value`'s `zero` method).
      * @param nanflow Container for data that resulted in `NaN`.
      * @param origin Left edge of the bin whose index is zero.
      */
    def apply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}]
      (binWidth: Double,
       quantity: UserFcn[DATUM, Double],
       value: => V = Count(),
       nanflow: N = Count(),
       origin: Double = 0.0) =
      new SparselyBinning[DATUM, V, N](binWidth, quantity, 0.0, value, mutable.HashMap[Long, V](), nanflow, origin)

    /** Synonym for `apply`. */
    def ing[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}]
      (binWidth: Double,
       quantity: UserFcn[DATUM, Double],
       value: => V = Count(),
       nanflow: N = Count(),
       origin: Double = 0.0) = apply(binWidth, quantity, value, nanflow, origin)

    trait Methods {
      def binWidth: Double
      def origin: Double

      /** The number of non-empty bins. */
      def numFilled: Int
      /** The number of bins between the first non-empty one (inclusive) and the last non-empty one (exclusive). */
      def num: Long
      /** The first non-empty bin. */
      def minBin: Option[Long]
      /** The last non-empty bin. */
      def maxBin: Option[Long]
      def low: Option[Double]
      def high: Option[Double]
      /** Get a sequence of filled indexes. */
      def indexes: Seq[Long]
      /** Get the low and high edge of a bin (given by index number). */
      def range(index: Long): (Double, Double)

      /** Find the bin index associated with numerical value `x`.
        * 
        * @return `Long.MIN_VALUE` if `x` is `NaN`, the bin index if it is between `Long.MIN_VALUE + 1` and `Long.MAX_VALUE`, otherwise saturate at the endpoints.
        */
      def bin(x: Double): Long =
        if (nan(x))
          java.lang.Long.MIN_VALUE
        else {
          val out = Math.floor((x - origin) / binWidth)
          if (out < java.lang.Long.MIN_VALUE + 1)
            java.lang.Long.MIN_VALUE + 1
          else if (out > java.lang.Long.MAX_VALUE)
            java.lang.Long.MAX_VALUE
          else
            out.toLong
        }

      /** Return `true` iff `x` is in the nanflow region (equal to `NaN`). */
      def nan(x: Double): Boolean = x.isNaN
    }

    import KeySetComparisons._
    def fromJsonFragment(json: Json, nameFromParent: Option[String]): Container[_] with NoAggregation = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("binWidth", "entries", "bins:type", "bins", "nanflow:type", "nanflow", "origin").maybe("name").maybe("bins:name")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val quantityName = get.getOrElse("name", JsonNull) match {
          case JsonString(x) => Some(x)
          case JsonNull => None
          case x => throw new JsonFormatException(x, name + ".name")
        }

        val binWidth = get("binWidth") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".binWidth")
        }

        val (contentType, binsFactory) = get("bins:type") match {
          case JsonString(name) => (name, Factory(name))
          case x => throw new JsonFormatException(x, name + ".bins:type")
        }
        val binsName = get.getOrElse("bins:name", JsonNull) match {
          case JsonString(x) => Some(x)
          case JsonNull => None
          case x => throw new JsonFormatException(x, name + ".bins:name")
        }
        val bins = get("bins") match {
          case JsonObject(indexBins @ _*) =>
            SortedMap(indexBins map {
              case (JsonString(i), v) if (integerPattern.pattern.matcher(i).matches) => (i.toLong, binsFactory.fromJsonFragment(v, binsName))
              case (i, _) => throw new JsonFormatException(i, name + s".bins key must be an integer")
            }: _*)
          case x => throw new JsonFormatException(x, name + ".bins")
        }

        val nanflowFactory = get("nanflow:type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, name + ".nanflow:type")
        }
        val nanflow = nanflowFactory.fromJsonFragment(get("nanflow"), None)

        val origin = get("origin") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".origin")
        }

        new SparselyBinned(binWidth, entries, (nameFromParent ++ quantityName).lastOption, contentType, bins.asInstanceOf[SortedMap[Long, C] forSome {type C <: Container[C] with NoAggregation}], nanflow.asInstanceOf[C forSome {type C <: Container[C] with NoAggregation}], origin)

      case _ => throw new JsonFormatException(json, name)
    }
  }

  /** An accumulated quantity that was split into equally spaced bins, filling only one bin per datum and creating new bins as necessary.
    * 
    * Use the factory [[org.dianahep.histogrammar.SparselyBin]] to construct an instance.
    * 
    * @param binWidth Width of the equally sized bins.
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param quantityName Optional name given to the quantity function, passed for bookkeeping.
    * @param contentType Name of the intended content; used as a placeholder in cases with zero bins (due to no observed data).
    * @param bins Centers and values of each bin.
    * @param nanflow Container for data that resulted in `NaN`.
    * @param origin Left edge of the bin whose index is zero.
    */
  class SparselyBinned[V <: Container[V] with NoAggregation, N <: Container[N] with NoAggregation] private[histogrammar](val binWidth: Double, val entries: Double, val quantityName: Option[String], val contentType: String, val bins: SortedMap[Long, V], val nanflow: N, val origin: Double) extends Container[SparselyBinned[V, N]] with NoAggregation with QuantityName with SparselyBin.Methods {
    type Type = SparselyBinned[V, N]
    type EdType = SparselyBinned[V, N]
    def factory = SparselyBin

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (binWidth <= 0.0)
      throw new ContainerException(s"binWidth ($binWidth) must be greater than zero")

    def zero = new SparselyBinned[V, N](binWidth, 0.0, quantityName, contentType, SortedMap[Long, V](), nanflow.zero, origin)
    def +(that: SparselyBinned[V, N]) = {
      if (this.quantityName != that.quantityName)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantityName differs (${this.quantityName} vs ${that.quantityName})")
      if (this.binWidth != that.binWidth)
        throw new ContainerException(s"cannot add ${getClass.getName} because binWidth differs (${this.binWidth} vs ${that.binWidth})")
      if (this.origin != that.origin)
        throw new ContainerException(s"cannot add ${getClass.getName} because origin differs (${this.origin} vs ${that.origin})")

      val newbins =
        SortedMap[Long, V]((this.bins.keySet union that.bins.keySet).toSeq map {case i =>
          (this.bins.get(i), that.bins.get(i)) match {
            case (Some(v1), Some(v2)) => i -> (v1 + v2)
            case (Some(v1), None) => i -> v1
            case (None, Some(v2)) => i -> v2
            case _ => throw new Exception("can't get here")
          }
        }: _*)

      new SparselyBinned[V, N](binWidth, this.entries + that.entries, quantityName, contentType, newbins, this.nanflow + that.nanflow, origin)
    }
    def *(factor: Double) =
      if (factor.isNaN  ||  factor <= 0.0)
        zero
      else
        new SparselyBinned[V, N](
          binWidth,
          factor * entries,
          quantityName,
          contentType,
          SortedMap[Long, V](bins.toSeq map {case (i, x) => (i, x * factor)}: _*),
          nanflow * factor,
          origin)

    def numFilled = bins.size
    def num = if (bins.isEmpty) 0L else 1L + bins.last._1 - bins.head._1
    def minBin = if (bins.isEmpty) None else Some(bins.head._1)
    def maxBin = if (bins.isEmpty) None else Some(bins.last._1)
    def low = if (bins.isEmpty) None else Some(minBin.get * binWidth + origin)
    def high = if (bins.isEmpty) None else Some((maxBin.get + 1L) * binWidth + origin)
    /** Extract the container at a given index, if it exists. */
    def at(index: Long) = bins.find(_._1 == index).map(_._2)
    def indexes = bins.map(_._1).toSeq
    def range(index: Long) = (index * binWidth + origin, (index + 1) * binWidth + origin)
    def values = bins.map(_._2)

    def children = nanflow :: values.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "binWidth" -> JsonFloat(binWidth),
      "entries" -> JsonFloat(entries),
      "bins:type" -> JsonString(if (bins.isEmpty) contentType else bins.head._2.factory.name),
      "bins" -> JsonObject(bins.toSeq map {case (i, v) => (JsonString(i.toString), v.toJsonFragment(true))}: _*),
      "nanflow:type" -> JsonString(nanflow.factory.name),
      "nanflow" -> nanflow.toJsonFragment(false),
      "origin" -> JsonFloat(origin)).
      maybe(JsonString("name") -> (if (suppressName) None else quantityName.map(JsonString(_)))).
      maybe(JsonString("bins:name") -> (bins.headOption match {case Some((i, v: QuantityName)) => v.quantityName.map(JsonString(_)); case _ => None}))

    override def toString() = s"""<SparselyBinned binWidth=$binWidth bins=$contentType nanflow=${nanflow.factory.name}>"""
    override def equals(that: Any) = that match {
      case that: SparselyBinned[V, N] => this.binWidth === that.binWidth  &&  this.entries === that.entries  &&  this.quantityName == that.quantityName  &&  this.bins == that.bins  &&  this.nanflow == that.nanflow  &&  this.origin === that.origin
      case _ => false
    }
    override def hashCode() = (binWidth, entries, quantityName, bins, nanflow, origin).hashCode
  }

  /** Accumulating a quantity by splitting it into equally spaced bins, filling only one bin per datum and creating new bins as necessary.
    * 
    * Use the factory [[org.dianahep.histogrammar.SparselyBin]] to construct an instance.
    * 
    * @param binWidth Width of the equally sized bins.
    * @param quantity Numerical function to split into bins.
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param value New value (note the `=>`: expression is reevaluated every time a new value is needed).
    * @param bins Centers and values of each bin.
    * @param nanflow Container for data that resulted in `NaN`.
    * @param origin Left edge of the bin whose index is zero.
    */
  class SparselyBinning[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}] private[histogrammar]
    (val binWidth: Double,
     val quantity: UserFcn[DATUM, Double],
     var entries: Double,
     value: => V,
     val bins: mutable.Map[Long, V],
     val nanflow: N,
     val origin: Double) extends Container[SparselyBinning[DATUM, V, N]] with AggregationOnData with NumericalQuantity[DATUM] with SparselyBin.Methods {

    protected val v = value
    type Type = SparselyBinning[DATUM, V, N]
    type EdType = SparselyBinned[v.EdType, nanflow.EdType]
    type Datum = DATUM
    def factory = SparselyBin

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (binWidth <= 0.0)
      throw new ContainerException(s"binWidth ($binWidth) must be greater than zero")

    def zero = new SparselyBinning[DATUM, V, N](binWidth, quantity, 0.0, value, mutable.Map[Long, V](), nanflow.zero, origin)
    def +(that: SparselyBinning[DATUM, V, N]) = {
      if (this.quantity.name != that.quantity.name)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantity name differs (${this.quantity.name} vs ${that.quantity.name})")
      if (this.binWidth != that.binWidth)
        throw new ContainerException(s"cannot add ${getClass.getName} because binWidth differs (${this.binWidth} vs ${that.binWidth})")
      if (this.origin != that.origin)
        throw new ContainerException(s"cannot add ${getClass.getName} because origin differs (${this.origin} vs ${that.origin})")

      val newbins =
        mutable.Map[Long, V]((this.bins.keySet union that.bins.keySet).toSeq map {case i =>
          (this.bins.get(i), that.bins.get(i)) match {
            case (Some(v1), Some(v2)) => i -> (v1 + v2)
            case (Some(v1), None) => i -> v1
            case (None, Some(v2)) => i -> v2
            case _ => throw new Exception("can't get here")
          }
        }: _*)

      new SparselyBinning[DATUM, V, N](binWidth, this.quantity, this.entries + that.entries, this.value, newbins, this.nanflow + that.nanflow, origin)
    }
    def *(factor: Double) =
      if (factor.isNaN  ||  factor <= 0.0)
        zero
      else
        new SparselyBinning[DATUM, V, N](
          binWidth,
          quantity,
          factor * entries,
          value,
          mutable.Map[Long, V](bins.toSeq map {case (i, x) => (i, x * factor)}: _*),
          nanflow * factor,
          origin)

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0): Unit = {
      checkForCrossReferences()
      if (weight > 0.0) {
        val q = quantity(datum)

        if (nan(q))
          nanflow.fill(datum, weight)
        else {
          val b = bin(q)
          if (!(bins contains b))
            bins.update(b, value.zero)
          bins(b).fill(datum, weight)
        }

        // no possibility of exception from here on out (for rollback)
        entries += weight
      }
    }

    def numFilled = bins.size
    def num = if (bins.isEmpty) 0L else 1L + bins.map(_._1).max - bins.map(_._1).min
    def minBin = if (bins.isEmpty) None else Some(bins.map(_._1).min)
    def maxBin = if (bins.isEmpty) None else Some(bins.map(_._1).max)
    def low = if (bins.isEmpty) None else Some(minBin.get * binWidth + origin)
    def high = if (bins.isEmpty) None else Some((maxBin.get + 1L) * binWidth + origin)
    /** Extract the container at a given index, if it exists. */
    def at(index: Long) = bins.get(index)
    def indexes = bins.map(_._1).toSeq
    def range(index: Long) = (index * binWidth + origin, (index + 1) * binWidth + origin)
    def values = bins.map(_._2)

    def children = value :: nanflow :: values.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "binWidth" -> JsonFloat(binWidth),
      "entries" -> JsonFloat(entries),
      "bins:type" -> JsonString(value.factory.name),
      "bins" -> JsonObject(bins.toSeq map {case (i, v) => (JsonString(i.toString), v.toJsonFragment(true))}: _*),
      "nanflow:type" -> JsonString(nanflow.factory.name),
      "nanflow" -> nanflow.toJsonFragment(false),
      "origin" -> JsonFloat(origin)).
      maybe(JsonString("name") -> (if (suppressName) None else quantity.name.map(JsonString(_)))).
      maybe(JsonString("bins:name") -> List(value).collect({case v: AnyQuantity[_, _] => v.quantity.name}).headOption.flatten.map(JsonString(_)))

    override def toString() = s"""<SparselyBinning binWidth=$binWidth bins=${value.factory.name} nanflow=${nanflow.factory.name}>"""
    override def equals(that: Any) = that match {
      case that: SparselyBinning[DATUM, V, N] => this.binWidth === that.binWidth  &&  this.quantity == that.quantity  &&  this.entries === that.entries  &&  this.bins == that.bins  &&  this.nanflow == that.nanflow  &&  this.origin === that.origin
      case _ => false
    }
    override def hashCode() = (binWidth, quantity, entries, bins, nanflow, origin).hashCode
  }
}
