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

import scala.collection.mutable

import org.dianahep.histogrammar.json._
import org.dianahep.histogrammar.util._

package histogrammar {
  //////////////////////////////////////////////////////////////// Categorize/Categorized/Categorizing

  /** Split a given quantity by its categorical (string-based) value and fill only one category per datum.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Categorizing]] and immutable [[org.dianahep.histogrammar.Categorized]] objects.
    */
  object Categorize extends Factory {
    val name = "Categorize"
    val help = "Split a given quantity by its categorical (string-based) value and fill only one category per datum."
    val detailedHelp = """Categorize(quantity: UserFcn[DATUM, String], value: => V = Count())"""

    /** Create an immutable [[org.dianahep.histogrammar.Categorized]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param quantityName Optional name given to the quantity function, passed for bookkeeping.
      * @param contentType Name of the intended content; used as a placeholder in cases with zero bins (due to no observed data).
      * @param pairs String category and the associated container of values associated with it.
      */
    def ed[V <: Container[V]](entries: Double, quantityName: Option[String], contentType: String, pairs: (String, V)*) = new Categorized(entries, quantityName, contentType, pairs: _*)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Categorizing]].
      * 
      * @param quantity Numerical function to split into bins.
      * @param value New value (note the `=>`: expression is reevaluated every time a new value is needed).
      */
    def apply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](quantity: UserFcn[DATUM, String], value: => V = Count()) =
      new Categorizing(quantity, 0.0, value, mutable.HashMap[String, V]())

    /** Synonym for `apply`. */
    def ing[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](quantity: UserFcn[DATUM, String], value: => V = Count()) =
      apply(quantity, value)

    import KeySetComparisons._
    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("entries", "type", "data").maybe("name")) =>
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

        val (contentType, factory) = get("type") match {
          case JsonString(name) => (name, Factory(name))
          case x => throw new JsonFormatException(x, name + ".type")
        }

        get("data") match {
          case JsonObject(categoryPairs @ _*) =>
            new Categorized[Container[_]](entries, quantityName, contentType, categoryPairs map {
              case (JsonString(category), value) =>
                category -> factory.fromJsonFragment(value)
            }: _*)

          case x => throw new JsonFormatException(x, name + ".data")
        }

      case _ => throw new JsonFormatException(json, name)
    }
  }

  /** An accumulated quantity that was split by its categorical (string-based) values, filling only one category per datum.
    * 
    * Use the factory [[org.dianahep.histogrammar.Categorize]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param quantityName Optional name given to the quantity function, passed for bookkeeping.
    * @param contentType Name of the intended content; used as a placeholder in cases with zero bins (due to no observed data).
    * @param pairs String category and the associated container of values associated with it.
    */
  class Categorized[V <: Container[V]] private[histogrammar](val entries: Double, val quantityName: Option[String], contentType: String, val pairs: (String, V)*) extends Container[Categorized[V]] {
    type Type = Categorized[V]
    def factory = Categorize

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    /** Input `pairs` as a key-value map. */
    val pairsMap = pairs.toMap
    /** Number of `pairs`. */
    def size = pairs.size
    /** Iterable over the keys of the `pairs`. */
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    /** Iterable over the values of the `pairs`. */
    def values: Iterable[Container[V]] = pairs.toIterable.map(_._2)
    /** Set of keys among the `pairs`. */
    def keySet: Set[String] = keys.toSet
    /** Attempt to get key `x`, throwing an exception if it does not exist. */
    def apply(x: String) = pairsMap(x)
    /** Attempt to get key `x`, returning `None` if it does not exist. */
    def get(x: String) = pairsMap.get(x)
    /** Attempt to get key `x`, returning an alternative if it does not exist. */
    def getOrElse(x: String, default: => V) = pairsMap.getOrElse(x, default)

    def zero = new Categorized[V](0.0, quantityName, contentType)
    def +(that: Categorized[V]) =
      if (this.quantityName != that.quantityName)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantityName differs (${this.quantityName} vs ${that.quantityName})")
      else
        new Categorized(
          this.entries + that.entries,
          this.quantityName,
          contentType,
          (this.keySet union that.keySet).toSeq map {key =>
            if ((this.pairsMap contains key)  &&  (that.pairsMap contains key))
              (key, this.pairsMap(key) + that.pairsMap(key))
            else if (this.pairsMap contains key)
              (key, this.pairsMap(key))
            else
              (key, that.pairsMap(key))
          }: _*)

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "type" -> JsonString(contentType),
      "data" -> JsonObject(pairs map {case (k, v) => (JsonString(k), v.toJsonFragment)}: _*)).
      maybe(JsonString("name") -> quantityName.map(JsonString(_)))

    override def toString() = s"""Categorized[[${if (pairs.isEmpty) contentType else pairs.head._2.toString}..., size=${pairs.size}]]"""
    override def equals(that: Any) = that match {
      case that: Categorized[V] => this.entries === that.entries  &&  this.quantityName == that.quantityName  &&  this.pairs == that.pairs
      case _ => false
    }
    override def hashCode() = (entries, quantityName, pairs).hashCode()
  }

  /** Accumulating a quantity by splitting it by its categorical (string-based) value and filling only one category per datum.
    * 
    * Use the factory [[org.dianahep.histogrammar.Categorize]] to construct an instance.
    * 
    * @param quantity Numerical function to track.
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param value New value (note the `=>`: expression is reevaluated every time a new value is needed).
    * @param pairs Map of string category and the associated container of values associated with it.
    */
  class Categorizing[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}] private[histogrammar](val quantity: UserFcn[DATUM, String], var entries: Double, value: => V, val pairs: mutable.HashMap[String, V]) extends Container[Categorizing[DATUM, V]] with AggregationOnData {
    type Type = Categorizing[DATUM, V]
    type Datum = DATUM
    def factory = Categorize

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    /** Input `pairs` as a key-value map. */
    def pairsMap = pairs.toMap
    /** Number of `pairs`. */
    def size = pairs.size
    /** Iterable over the keys of the `pairs`. */
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    /** Iterable over the values of the `pairs`. */
    def values: Iterable[V] = pairs.toIterable.map(_._2)
    /** Set of keys among the `pairs`. */
    def keySet: Set[String] = keys.toSet
    /** Attempt to get key `x`, throwing an exception if it does not exist. */
    def apply(x: String) = pairsMap(x)
    /** Attempt to get key `x`, returning `None` if it does not exist. */
    def get(x: String) = pairsMap.get(x)
    /** Attempt to get key `x`, returning an alternative if it does not exist. */
    def getOrElse(x: String, default: => V) = pairsMap.getOrElse(x, default)

    def zero = new Categorizing[DATUM, V](quantity, 0.0, value, mutable.HashMap(pairs.toSeq map {case (c, v) => (c, v.zero)}: _*))
    def +(that: Categorizing[DATUM, V]) =
      if (this.quantity.name != that.quantity.name)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantity name differs (${this.quantity.name} vs ${that.quantity.name})")
      else
        new Categorizing[DATUM, V](
          this.quantity,
          this.entries + that.entries,
          this.value,
          mutable.HashMap[String, V]((this.keySet union that.keySet).toSeq map {key =>
            if ((this.pairsMap contains key)  &&  (that.pairsMap contains key))
              (key, this.pairsMap(key) + that.pairsMap(key))
            else if (this.pairsMap contains key)
              (key, this.pairsMap(key))
            else
              (key, that.pairsMap(key))
          }: _*))
    
    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      if (weight > 0.0) {
        val q = quantity(datum)

        if (!(pairs contains q))
          pairs(q) = value.zero
        pairs(q).fill(datum, weight)

        // no possibility of exception from here on out (for rollback)
        entries += weight
      }
    }

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "type" -> JsonString(value.factory.name),
      "data" -> JsonObject(pairs.toSeq map {case (k, v) => (JsonString(k), v.toJsonFragment)}: _*)).
      maybe(JsonString("name") -> quantity.name.map(JsonString(_)))

    override def toString() = s"Categorizing[[${if (values.isEmpty) value.factory.name else values.head.toString}..., size=${pairs.size}]]"
    override def equals(that: Any) = that match {
      case that: Categorizing[DATUM, V] => this.quantity == that.quantity  &&  this.entries === that.entries  &&  this.pairs == that.pairs
      case _ => false
    }
    override def hashCode() = (quantity, entries, pairs).hashCode()
  }
}
