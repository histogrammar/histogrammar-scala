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

import scala.language.existentials

import org.dianahep.histogrammar.json._
import org.dianahep.histogrammar.util._

package histogrammar {
  //////////////////////////////////////////////////////////////// Limit/Limited/Limiting

  /** Accumulate an aggregator until its number of entries reaches a predefined limit.
    * 
    * Limit is intended to roll high-detail descriptions of small datasets over into low-detail descriptions of large datasets. For instance, a scatter plot is useful for small numbers of data points and heatmaps are useful for large ones. The following construction
    * 
    * {{{Bin(xbins, xlow, xhigh, {d: Datum => d.x},
    *   Bin(ybins, ylow, yhigh, {d: Datum => d.y},
    *     Limit(10.0, Bag({d: Datum => Vector(d.x, d.y)}))))}}}
    * 
    * fills a scatter plot in all x-y bins that have fewer than 10 entries and only a number of entries above that. Postprocessing code would use the bin-by-bin numbers of entries to color a heatmap and the raw data points to show outliers in the nearly empty bins.
    * 
    * Limit can effectively swap between two descriptions if it is embedded in a collection, such as [[org.dianahep.histogrammar.Branch]]. All elements of the collection would be filled until the Limit saturates, leaving only the low-detail one. For instance, one could aggregate several [[org.dianahep.histogrammar.SparselyBin]] histograms, each with a different `binWidth`, and progressively eliminate them in order of increasing `binWidth`.
    * 
    * Note that Limit saturates when it reaches a specified ''total weight,'' not the number of data points in a [[org.dianahep.histogrammar.Bag]], so it is not certain to control memory use. However, the total weight is of more use to data analysis. ([[org.dianahep.histogrammar.Sample]] puts a strict limit on memory use.)
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Limiting]] and immutable [[org.dianahep.histogrammar.Limited]] objects.
    */
  object Limit extends Factory {
    val name = "Limit"
    val help = "Accumulate an aggregator until its number of entries reaches a predefined limit."
    val detailedHelp = """Limit is intended to roll high-detail descriptions of small datasets over into low-detail descriptions of large datasets. For instance, a scatter plot is useful for small numbers of data points and heatmaps are useful for large ones. The following construction

{{{Bin(xbins, xlow, xhigh, {d: Datum => d.x},
  Bin(ybins, ylow, yhigh, {d: Datum => d.y},
    Limit(10.0, Bag({d: Datum => Vector(d.x, d.y)}))))}}}

fills a scatter plot in all x-y bins that have fewer than 10 entries and only a number of entries above that. Postprocessing code would use the bin-by-bin numbers of entries to color a heatmap and the raw data points to show outliers in the nearly empty bins.

Limit can effectively swap between two descriptions if it is embedded in a collection, such as [[org.dianahep.histogrammar.Branch]]. All elements of the collection would be filled until the Limit saturates, leaving only the low-detail one. For instance, one could aggregate several [[org.dianahep.histogrammar.SparselyBin]] histograms, each with a different `binWidth`, and progressively eliminate them in order of increasing `binWidth`.

Note that Limit saturates when it reaches a specified ''total weight,'' not the number of data points in a [[org.dianahep.histogrammar.Bag]], so it is not certain to control memory use. However, the total weight is of more use to data analysis. ([[org.dianahep.histogrammar.Sample]] puts a strict limit on memory use.)"""

    /** Create an immutable [[org.dianahep.histogrammar.Limited]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param limit Maximum sum of weights to keep; above this, `value` goes to `None`.
      * @param contentType Name of the Factory for `value`.
      * @param value Some aggregator or `None`.
      */
    def ed[V <: Container[V] with NoAggregation](entries: Double, limit: Double, contentType: String, value: Option[V]) = new Limited[V](entries, limit, contentType, value)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Limiting]].
      * 
      * @param value Aggregator to apply a limit to.
      * @param limit Maximum sum of weights to keep; above this, `value` goes to `None`.
      */
    def apply[V <: Container[V] with Aggregation](limit: Double, value: V) = new Limiting[V](0.0, limit, value.factory.name, Some(value))

    /** Synonym for `apply`. */
    def ing[V <: Container[V] with Aggregation](limit: Double, value: V) = apply[V](limit, value)

    /** Use [[org.dianahep.histogrammar.Limited]] in Scala pattern-matching. */
    def unapply[V <: Container[V] with NoAggregation](x: Limited[V]) = x.value
    /** Use [[org.dianahep.histogrammar.Limiting]] in Scala pattern-matching. */
    def unapply[V <: Container[V] with Aggregation](x: Limiting[V]) = x.value

    import KeySetComparisons._
    def fromJsonFragment(json: Json, nameFromParent: Option[String]): Container[_] with NoAggregation = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("entries", "limit", "type", "data")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val limit = get("limit") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".limit")
        }

        val contentType = get("type") match {
          case JsonString(x) => x
          case x => throw new JsonFormatException(x, name + ".type")
        }
        val factory = Factory(contentType)

        get("data") match {
          case JsonNull => new Limited[Container[_]](entries, limit, contentType, None)
          case x => new Limited[Container[_]](entries, limit, contentType, Some(factory.fromJsonFragment(x, None)))
        }

      case _ => throw new JsonFormatException(json, name)
    }
  }

  /** An accumulated aggregator or `None` if the number of entries exceeded the limit.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param limit Maximum sum of weights to keep; above this, `value` goes to `None`.
    * @param contentType Name of the Factory for `value`.
    * @param value Some aggregator or `None`.
    */
  class Limited[V <: Container[V] with NoAggregation] private[histogrammar](val entries: Double, val limit: Double, val contentType: String, val value: Option[V]) extends Container[Limited[V]] with NoAggregation {
    type Type = Limited[V]
    type EdType = Limited[V]
    def factory = Limit

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    /** True if `entries` exceeds `limit` and `value` is `None`. */
    def saturated = value.isEmpty
    /** Get the value of `value` or raise an error if it is `None`. */
    def get = value.get
    /** Get the value of `value` or return a default if it is `None`. */
    def getOrElse(default: => V) = value.getOrElse(default)

    def zero = new Limited[V](0.0, limit, contentType, value.map(_.zero))
    def +(that: Limited[V]) =
      if (this.limit != that.limit)
        throw new ContainerException(s"""cannot add Limited because they have different limits (${this.limit} vs ${that.limit}))""")
      else {
        val newentries = this.entries + that.entries
        val newvalue =
          if (newentries > limit)
            None
          else
            Some(this.value.get + that.value.get)
        new Limited[V](newentries, limit, contentType, newvalue)
      }

    def children = value.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "limit" -> JsonFloat(limit),
      "type" -> JsonString(contentType),
      "data" -> (value match {
        case None => JsonNull
        case Some(x) => x.toJsonFragment(false)
      }))

    override def toString() = s"""<Limited value=${if (saturated) "saturated" else value.get.factory.name}>"""
    override def equals(that: Any) = that match {
      case that: Limited[V] => this.entries === that.entries  &&  this.limit === that.limit  &&  this.contentType == that.contentType  &&  this.value == that.value
      case _ => false
    }
    override def hashCode() = (entries, limit, contentType, value).hashCode
  }

  /** Accumulating an aggregator or `None` if the number of entries exceeds the limit.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param limit Maximum sum of weights to keep; above this, `value` goes to `None`.
    * @param contentType Name of the Factory for `value`.
    * @param value Some aggregator or `None`.
    */
  class Limiting[V <: Container[V] with Aggregation] private[histogrammar](var entries: Double, val limit: Double, val contentType: String, var value: Option[V]) extends Container[Limiting[V]] with AggregationOnData {
    protected val v = value.get
    type Type = Limiting[V]
    type EdType = Limited[v.EdType]
    type Datum = V#Datum
    def factory = Limit

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    /** True if `entries` exceeds `limit` and `value` is `None`. */
    def saturated = value.isEmpty
    /** Get the value of `value` or raise an error if it is `None`. */
    def get = value.get
    /** Get the value of `value` or return a default if it is `None`. */
    def getOrElse(default: => V) = value.getOrElse(default)

    def zero = new Limiting[V](0.0, limit, contentType, value.map(_.zero))
    def +(that: Limiting[V]) =
      if (this.limit != that.limit)
        throw new ContainerException(s"""cannot add Limiting because they have different limits (${this.limit} vs ${that.limit}))""")
      else {
        val newentries = this.entries + that.entries
        val newvalue =
          if (newentries > limit)
            None
          else
            Some(this.value.get + that.value.get)
        new Limiting[V](newentries, limit, contentType, newvalue)
      }

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      checkForCrossReferences()
      if (weight > 0.0) {
        if (entries + weight > limit)
          value = None
        else
          value.foreach(v => v.fill(datum.asInstanceOf[v.Datum], weight))
        // no possibility of exception from here on out (for rollback)
        entries += weight
      }
    }

    def children = value.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "limit" -> JsonFloat(limit),
      "type" -> JsonString(contentType),
      "data" -> (value match {
        case None => JsonNull
        case Some(x) => x.toJsonFragment(false)
      }))

    override def toString() = s"""<Limiting value=${if (saturated) "saturated" else value.get.factory.name}>"""
    override def equals(that: Any) = that match {
      case that: Limited[V] => this.entries === that.entries  &&  this.limit === that.limit  &&  this.contentType == that.contentType  &&  this.value == that.value
      case _ => false
    }
    override def hashCode() = (entries, limit, contentType, value).hashCode
  }
}
