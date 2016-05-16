// Copyright 2016 Jim Pivarski
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

import org.dianahep.histogrammar.json._
import org.dianahep.histogrammar.util._

package histogrammar {
  //////////////////////////////////////////////////////////////// Sample

  /** Accumulate raw numbers, vectors of numbers, or strings, merging identical values.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Sampling]] and immutable [[org.dianahep.histogrammar.Sampled]] objects.
    */
  object Sample extends Factory {
    val name = "Sample"
    val help = "Accumulate raw numbers, vectors of numbers, or strings that are an unbiased sample of the observed distribution."
    val detailedHelp = "Sample(limit: Int, quantity: UserFcn[DATUM, RANGE])"

    /** Create an immutable [[org.dianahep.histogrammar.Sampled]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param limit Maximum number of data points in the sample.
      * @param values Distinct multidimensional vectors and their weights, sampled from the observed distribution.
      */
    def ed[RANGE](entries: Double, limit: Int, values: (RANGE, Double)*) = new Sampled(entries, None, limit, values: _*)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Sampling]].
      * 
      * @param limit Maximum number of data points in the sample.
      * @param quantity Function that produces numbers, vectors of numbers, or strings.
      */
    def apply[DATUM, RANGE](limit: Int, quantity: UserFcn[DATUM, RANGE]) =
      new Sampling[DATUM, RANGE](quantity, 0.0, new mutable.Reservoir[RANGE](limit))

    /** Synonym for `apply`. */
    def ing[DATUM, RANGE](limit: Int, quantity: UserFcn[DATUM, RANGE]) = apply(limit, quantity)

    /** Use [[org.dianahep.histogrammar.Sampled]] in Scala pattern-matching. */
    def unapply[RANGE](x: Sampled[RANGE]) = x.values
    /** Use [[org.dianahep.histogrammar.Sampling]] in Scala pattern-matching. */
    def unapply[DATUM, RANGE](x: Sampling[DATUM, RANGE]) = x.values

    import KeySetComparisons._
    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("entries", "limit", "values").maybe("name")) =>
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

        val limit = get("limit") match {
          case JsonInt(x) => x
          case x => throw new JsonFormatException(x, name + ".limit")
        }

        val values = get("values") match {
          case JsonArray(elems @ _*) => Seq[(Any, Double)](elems.zipWithIndex map {
            case (JsonObject(wv @ _*), i) if (wv.keySet has Set("w", "v")) =>
              val wvget = wv.toMap

              val n = wvget("w") match {
                case JsonNumber(x) => x
                case x => throw new JsonFormatException(x, name + s".values $i n")
              }

              val v: Any = wvget("v") match {
                case JsonString(x) => x
                case JsonNumber(x) => x
                case JsonArray(d @ _*) => Vector(d.zipWithIndex map {
                  case (JsonNumber(x), j) => x
                  case (x, j) => throw new JsonFormatException(x, name + s".values $i v $j")
                }: _*)
                case x => throw new JsonFormatException(x, name + s".values $i v")
              }

              (v -> n)

            case (x, i) => throw new JsonFormatException(x, name + s".values $i")
          }: _*)
          case x => throw new JsonFormatException(x, name + ".values")
        }

        new Sampled[Any](entries, quantityName, limit.toInt, values: _*)

      case _ => throw new JsonFormatException(json, name)
    }
  }

  /** An accumulated sample of numbers, vectors of numbers, or strings.
    * 
    * Use the factory [[org.dianahep.histogrammar.Sample]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param quantityName Optional name given to the quantity function, passed for bookkeeping.
    * @param limit Maximum number of data points in the sample.
    * @param values Distinct multidimensional vectors and their weights, sampled from the observed distribution.
    */
  class Sampled[RANGE] private[histogrammar](val entries: Double, val quantityName: Option[String], val limit: Int, val values: (RANGE, Double)*) extends Container[Sampled[RANGE]] {
    type Type = Sampled[RANGE]
    def factory = Sample

    if (limit <= 0)
      throw new ContainerException(s"limit ($limit) must be positive")

    /** Number of data points in the sample (saturates at `limit`). */
    def size = values.size
    /** Determine if the sample is empty. */
    def isEmpty = values.isEmpty

    def zero = new Sampled(0.0, quantityName, limit)
    def +(that: Sampled[RANGE]) = {
      if (this.limit != that.limit)
        throw new ContainerException(s"cannot add ${getClass.getName} because limit differs (${this.limit} vs ${that.limit})")
      if (this.quantityName != that.quantityName)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantityName differs (${this.quantityName} vs ${that.quantityName})")

      val reservoir = new mutable.Reservoir[RANGE](limit, values: _*)
      that.values foreach {case (y, weight) => reservoir.update(y, weight)}

      new Sampled[RANGE](this.entries + that.entries, this.quantityName, this.limit, reservoir.values: _*)
    }

    def children = Nil

    def toJsonFragment = {
      implicit val rangeOrdering = Bag.rangeOrdering[RANGE]
      JsonObject(
        "entries" -> JsonFloat(entries),
        "limit" -> JsonInt(limit),
        "values" -> JsonArray(values.toSeq.sortBy(_._1).map({
          case (v: String, n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonString(v))
          case (v: Double, n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonFloat(v))
          case (v: Vector[_], n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonArray(v.map({case vi: Double => JsonFloat(vi)}): _*))
        }): _*)).
      maybe(JsonString("name") -> quantityName.map(JsonString(_)))
    }

    override def toString() = s"""Sampled[${if (isEmpty) "empty" else values.head.toString + "..."}, size=${size}]"""
    override def equals(that: Any) = that match {
      case that: Sampled[RANGE] => this.entries === that.entries  &&  this.quantityName == that.quantityName  &&  this.limit == that.limit  &&  this.values == that.values
      case _ => false
    }
    override def hashCode() = (entries, quantityName, limit, values).hashCode()
  }

  /** An accumulated sample of numbers, vectors of numbers, or strings.
    * 
    * Use the factory [[org.dianahep.histogrammar.Sample]] to construct an instance.
    * 
    * @param quantity Function that produces numbers, vectors of numbers, or strings.
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param reservoir Data structure to perform weighted reservoir sampling.
    */
  class Sampling[DATUM, RANGE] private[histogrammar](val quantity: UserFcn[DATUM, RANGE], var entries: Double, reservoir: mutable.Reservoir[RANGE]) extends Container[Sampling[DATUM, RANGE]] with AggregationOnData with AnyQuantity[DATUM, RANGE] {
    type Type = Sampling[DATUM, RANGE]
    type Datum = DATUM
    def factory = Sample

    if (limit <= 0)
      throw new ContainerException(s"limit ($limit) must be positive")

    /** Maximum number of data points in the sample. */
    def limit = reservoir.limit
    /** Distinct multidimensional vectors and their weights, sampled from the observed distribution. */
    def values = reservoir.values
    /** Number of data points in the sample (saturates at `limit`). */
    def size = reservoir.size
    /** Determine if the sample is empty. */
    def isEmpty = reservoir.isEmpty

    def zero = new Sampling(quantity, 0.0, new mutable.Reservoir[RANGE](limit))
    def +(that: Sampling[DATUM, RANGE]) = {
      if (this.limit != that.limit)
        throw new ContainerException(s"cannot add ${getClass.getName} because limit differs (${this.limit} vs ${that.limit})")
      if (this.quantity.name != that.quantity.name)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantity name differs (${this.quantity.name} vs ${that.quantity.name})")

      val newreservoir = new mutable.Reservoir[RANGE](this.limit, this.values: _*)
      that.values foreach {case (y, weight) => newreservoir.update(y, weight)}

      new Sampling[DATUM, RANGE](quantity, this.entries + that.entries, newreservoir)
    }

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      if (weight > 0.0) {
        val q = quantity(datum)

        reservoir.update(q, weight)

        // no possibility of exception from here on out (for rollback)
        entries += weight
      }
    }

    def children = Nil

    def toJsonFragment = {
      implicit val rangeOrdering = Bag.rangeOrdering[RANGE]
      JsonObject(
        "entries" -> JsonFloat(entries),
        "limit" -> JsonInt(limit),
        "values" -> JsonArray(values.toSeq.sortBy(_._1).map({
          case (v: String, n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonString(v))
          case (v: Double, n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonFloat(v))
          case (v: Vector[_], n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonArray(v.map({case vi: Double => JsonFloat(vi)}): _*))
        }): _*)).
        maybe(JsonString("name") -> quantity.name.map(JsonString(_)))
    }

    override def toString() = s"""Sampling[${if (isEmpty) "empty" else reservoir.some.toString + "..."}, size=${size}]"""

    override def equals(that: Any) = that match {
      case that: Sampling[DATUM, RANGE] => this.quantity == that.quantity  &&  this.entries === that.entries  &&  this.limit == that.limit  &&  this.values == that.values
      case _ => false
    }
    override def hashCode() = (quantity, entries, limit, values).hashCode()
  }
}
