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
import scala.language.postfixOps
import scala.language.existentials

import org.dianahep.histogrammar.json._
import org.dianahep.histogrammar.util._

package histogrammar {
  //////////////////////////////////////////////////////////////// Categorize/Categorized/Categorizing

  /** Split a given quantity by its categorical value and fill only one category per datum.
    * 
    * A bar chart may be thought of as a histogram with string-valued (categorical) bins, so this is the equivalent of [[org.dianahep.histogrammar.Bin]] for bar charts. The order of the strings is deferred to the visualization stage.
    * 
    * Unlike [[org.dianahep.histogrammar.SparselyBin]], this aggregator has the potential to use unlimited memory. A large number of ''distinct'' categories can generate many unwanted bins.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Categorizing]] and immutable [[org.dianahep.histogrammar.Categorized]] objects.
    */
  object Categorize extends Factory {
    val name = "Categorize"
    val help = "Split a given quantity by its categorical value and fill only one category per datum."
    val detailedHelp = """A bar chart may be thought of as a histogram with string-valued (categorical) bins, so this is the equivalent of [[org.dianahep.histogrammar.Bin]] for bar charts. The order of the strings is deferred to the visualization stage.

Unlike [[org.dianahep.histogrammar.SparselyBin]], this aggregator has the potential to use unlimited memory. A large number of ''distinct'' categories can generate many unwanted bins."""

    /** Create an immutable [[org.dianahep.histogrammar.Categorized]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param contentType Name of the intended content; used as a placeholder in cases with zero bins (due to no observed data).
      * @param bins String category and the associated container of values associated with it.
      */
    def ed[V <: Container[V] with NoAggregation](entries: Double, contentType: String, bins: Map[String, V]) = new Categorized(entries, None, contentType, bins)

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
    def fromJsonFragment(json: Json, nameFromParent: Option[String]): Container[_] with NoAggregation = json match {
      case JsonObject(bins @ _*) if (bins.keySet has Set("entries", "bins:type", "bins").maybe("name").maybe("bins:name")) =>
        val get = bins.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val quantityName = get.getOrElse("name", JsonNull) match {
          case JsonString(x) => Some(x)
          case JsonNull => None
          case x => throw new JsonFormatException(x, name + ".name")
        }

        val (contentType, factory) = get("bins:type") match {
          case JsonString(name) => (name, Factory(name))
          case x => throw new JsonFormatException(x, name + ".bins:type")
        }

        val dataName = get.getOrElse("bins:name", JsonNull) match {
          case JsonString(x) => Some(x)
          case JsonNull => None
          case x => throw new JsonFormatException(x, name + ".bins:name")
        }

        val thebins =
          get("bins") match {
            case JsonObject(categoryPairs @ _*) =>
              categoryPairs map {
                case (JsonString(category), value) =>
                  category -> factory.fromJsonFragment(value, dataName)
              } toMap
            case x => throw new JsonFormatException(x, name + ".bins")
          }

        new Categorized(entries, (nameFromParent ++ quantityName).lastOption, contentType, thebins.asInstanceOf[Map[String, C] forSome {type C <: Container[C] with NoAggregation}])

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
    * @param bins String category and the associated container of values associated with it.
    */
  class Categorized[V <: Container[V] with NoAggregation] private[histogrammar](val entries: Double, val quantityName: Option[String], contentType: String, val bins: Map[String, V]) extends Container[Categorized[V]] with NoAggregation with QuantityName {
    type Type = Categorized[V]
    type EdType = Categorized[V]
    def factory = Categorize

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    /** Number of `bins`. */
    def size = bins.size
    /** Iterable over the keys of the `bins`. */
    def keys: Iterable[String] = bins.toIterable.map(_._1)
    /** Iterable over the values of the `bins`. */
    def values: Iterable[Container[V]] = bins.toIterable.map(_._2)
    /** Set of keys among the `bins`. */
    def keySet: Set[String] = keys.toSet
    /** Attempt to get key `x`, throwing an exception if it does not exist. */
    def apply(x: String) = bins(x)
    /** Attempt to get key `x`, returning `None` if it does not exist. */
    def get(x: String) = bins.get(x)
    /** Attempt to get key `x`, returning an alternative if it does not exist. */
    def getOrElse(x: String, default: => V) = bins.getOrElse(x, default)

    def zero = new Categorized[V](0.0, quantityName, contentType, Map[String, V]())
    def +(that: Categorized[V]) =
      if (this.quantityName != that.quantityName)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantityName differs (${this.quantityName} vs ${that.quantityName})")
      else
        new Categorized(
          this.entries + that.entries,
          this.quantityName,
          contentType,
          (this.keySet union that.keySet).toSeq map {key =>
            if ((this.bins contains key)  &&  (that.bins contains key))
              (key, this.bins(key) + that.bins(key))
            else if (this.bins contains key)
              (key, this.bins(key))
            else
              (key, that.bins(key))
          } toMap)

    def children = values.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "bins:type" -> JsonString(contentType),
      "bins" -> JsonObject(bins.toSeq map {case (k, v) => (JsonString(k), v.toJsonFragment(true))}: _*)).
      maybe(JsonString("name") -> (if (suppressName) None else quantityName.map(JsonString(_)))).
      maybe(JsonString("bins:name") -> (bins.headOption match {case Some((k, v: QuantityName)) => v.quantityName.map(JsonString(_)); case _ => None}))

    override def toString() = s"""<Categorized values=$contentType size=${bins.size}>"""
    override def equals(that: Any) = that match {
      case that: Categorized[V] => this.entries === that.entries  &&  this.quantityName == that.quantityName  &&  this.bins == that.bins
      case _ => false
    }
    override def hashCode() = (entries, quantityName, bins).hashCode()
  }

  /** Accumulating a quantity by splitting it by its categorical (string-based) value and filling only one category per datum.
    * 
    * Use the factory [[org.dianahep.histogrammar.Categorize]] to construct an instance.
    * 
    * @param quantity Numerical function to track.
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param value New value (note the `=>`: expression is reevaluated every time a new value is needed).
    * @param bins Map of string category and the associated container of values associated with it.
    */
  class Categorizing[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}] private[histogrammar](val quantity: UserFcn[DATUM, String], var entries: Double, value: => V, val bins: mutable.HashMap[String, V]) extends Container[Categorizing[DATUM, V]] with AggregationOnData with CategoricalQuantity[DATUM] {

    protected val v = value
    type Type = Categorizing[DATUM, V]
    type EdType = Categorized[v.EdType]
    type Datum = DATUM
    def factory = Categorize

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    /** Number of `bins`. */
    def size = bins.size
    /** Iterable over the keys of the `bins`. */
    def keys: Iterable[String] = bins.toIterable.map(_._1)
    /** Iterable over the values of the `bins`. */
    def values: Iterable[V] = bins.toIterable.map(_._2)
    /** Set of keys among the `bins`. */
    def keySet: Set[String] = keys.toSet
    /** Attempt to get key `x`, throwing an exception if it does not exist. */
    def apply(x: String) = bins(x)
    /** Attempt to get key `x`, returning `None` if it does not exist. */
    def get(x: String) = bins.get(x)
    /** Attempt to get key `x`, returning an alternative if it does not exist. */
    def getOrElse(x: String, default: => V) = bins.getOrElse(x, default)

    def zero = new Categorizing[DATUM, V](quantity, 0.0, value, mutable.HashMap[String, V]())
    def +(that: Categorizing[DATUM, V]) =
      if (this.quantity.name != that.quantity.name)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantity name differs (${this.quantity.name} vs ${that.quantity.name})")
      else
        new Categorizing[DATUM, V](
          this.quantity,
          this.entries + that.entries,
          this.value,
          mutable.HashMap[String, V]((this.keySet union that.keySet).toSeq map {key =>
            if ((this.bins contains key)  &&  (that.bins contains key))
              (key, this.bins(key) + that.bins(key))
            else if (this.bins contains key)
              (key, this.bins(key))
            else
              (key, that.bins(key))
          }: _*))
    
    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      checkForCrossReferences()
      if (weight > 0.0) {
        val q = quantity(datum)

        if (!(bins contains q))
          bins(q) = value.zero
        bins(q).fill(datum, weight)

        // no possibility of exception from here on out (for rollback)
        entries += weight
      }
    }

    def children = value :: values.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "bins:type" -> JsonString(value.factory.name),
      "bins" -> JsonObject(bins.toSeq map {case (k, v) => (JsonString(k), v.toJsonFragment(true))}: _*)).
      maybe(JsonString("name") -> (if (suppressName) None else quantity.name.map(JsonString(_)))).
      maybe(JsonString("bins:name") -> List(value).collect({case v: AnyQuantity[_, _] => v.quantity.name}).headOption.flatten.map(JsonString(_)))

    override def toString() = s"""<Categorizing values=${value.factory.name} size=${bins.size}>"""
    override def equals(that: Any) = that match {
      case that: Categorizing[DATUM, V] => this.quantity == that.quantity  &&  this.entries === that.entries  &&  this.bins == that.bins
      case _ => false
    }
    override def hashCode() = (quantity, entries, bins).hashCode()
  }
}
