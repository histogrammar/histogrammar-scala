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

import scala.language.existentials

import org.dianahep.histogrammar.json._
import org.dianahep.histogrammar.util._

package histogrammar {
  //////////////////////////////////////////////////////////////// Select/Selected/Selecting

  /** Filter or weight data according to a given selection.
    * 
    * This primitive is a basic building block, intended to be used in conjunction with anything that needs a user-defined cut. In particular, a standard histogram often has a custom selection, and this can be built by nesting Select -> Bin -> Count.
    * 
    * Select also resembles [[org.dianahep.histogrammar.Fraction]], but without the `denominator`.
    * 
    * The efficiency of a cut in a Select aggregator named `x` is simply `x.cut.entries / x.entries` (because all aggregators have an `entries` member).
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Selecting]] and immutable [[org.dianahep.histogrammar.Selected]] (sic) objects.
    */
  object Select extends Factory {
    val name = "Select"
    val help = "Filter or weight data according to a given selection."
    val detailedHelp = """This primitive is a basic building block, intended to be used in conjunction with anything that needs a user-defined cut. In particular, a standard histogram often has a custom selection, and this can be built by nesting Select -> Bin -> Count.

Select also resembles [[org.dianahep.histogrammar.Fraction]], but without the `denominator`.

The efficiency of a cut in a Select aggregator named `x` is simply `x.cut.entries / x.entries` (because all aggregators have an `entries` member)."""

    /** Create an immutable [[org.dianahep.histogrammar.Selected]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights without the cut applied).
      * @param cut Aggregator that accumulated values that passed the cut.
      */
    def ed[V <: Container[V] with NoAggregation](entries: Double, cut: V) = new Selected[V](entries, None, cut)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Limiting]].
      * 
      * @param quantity Boolean or non-negative function that cuts or weights entries.
      * @param cut Aggregator to accumulate for values that pass the selection (`quantity`).
      */
    def apply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](quantity: UserFcn[DATUM, Double], cut: V) = new Selecting[DATUM, V](0.0, quantity, cut)

    /** Synonym for `apply`. */
    def ing[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](quantity: UserFcn[DATUM, Double], cut: V) = apply(quantity, cut)

    /** Use [[org.dianahep.histogrammar.Selected]] in Scala pattern-matching. */
    def unapply[V <: Container[V] with NoAggregation](x: Selected[V]) = Some(x.cut)
    /** Use [[org.dianahep.histogrammar.Selecting]] in Scala pattern-matching. */
    def unapply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](x: Selecting[DATUM, V]) = Some(x.cut)

    import KeySetComparisons._
    def fromJsonFragment(json: Json, nameFromParent: Option[String]): Container[_] with NoAggregation = json match {
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

        val factory = get("type") match {
          case JsonString(x) => Factory(x)
          case x => throw new JsonFormatException(x, name + ".type")
        }

        val cut = factory.fromJsonFragment(get("data"), None)

        new Selected[Container[_]](entries, (nameFromParent ++ quantityName).lastOption, cut)

      case _ => throw new JsonFormatException(json, name)
    }

    trait Methods {
      /** Fraction of weights that pass the quantity. */
      def fractionPassing: Double
    }
  }

  /** An accumulated aggregator of data that passed the cut.
    * 
    * @param entries Weighted number of entries (sum of all observed weights without the cut applied).
    * @param quantityName Optional name given to the quantity function, passed for bookkeeping.
    * @param cut Aggregator that accumulated values that passed the cut.
    */
  class Selected[V <: Container[V] with NoAggregation] private[histogrammar](val entries: Double, val quantityName: Option[String], val cut: V) extends Container[Selected[V]] with NoAggregation with QuantityName with Select.Methods {
    type Type = Selected[V]
    type EdType = Selected[V]
    def factory = Select

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def fractionPassing = cut.entries / entries

    def zero = new Selected[V](0.0, quantityName, cut.zero)
    def +(that: Selected[V]) =
      if (this.quantityName != that.quantityName)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantityName differs (${this.quantityName} vs ${that.quantityName})")
      else
        new Selected[V](this.entries + that.entries, this.quantityName, this.cut + that.cut)

    def children = cut :: Nil

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "type" -> JsonString(cut.factory.name),
      "data" -> cut.toJsonFragment(false)).
      maybe(JsonString("name") -> (if (suppressName) None else quantityName.map(JsonString(_))))

    override def toString() = s"""<Selected cut=${cut.factory.name}>"""
    override def equals(that: Any) = that match {
      case that: Selected[V] => this.entries === that.entries  &&  this.quantityName == that.quantityName  &&  this.cut == that.cut
      case _ => false
    }
    override def hashCode() = (entries, quantityName, cut).hashCode
  }

  /** Accumulating an aggregator of data that passes a cut.
    * 
    * @param entries Weighted number of entries (sum of all observed weights without the cut applied).
    * @param quantity Boolean or non-negative function that cuts or weights entries.
    * @param cut Aggregator to accumulate values that pass the cut.
    */
  class Selecting[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}] private[histogrammar](var entries: Double, val quantity: UserFcn[DATUM, Double], val cut: V) extends Container[Selecting[DATUM, V]] with AggregationOnData with NumericalQuantity[DATUM] with Select.Methods {
    type Type = Selecting[DATUM, V]
    type EdType = Selected[cut.EdType]
    type Datum = DATUM
    def factory = Select

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def fractionPassing = cut.entries / entries

    def zero = new Selecting[DATUM, V](0.0, quantity, cut.zero)
    def +(that: Selecting[DATUM, V]) =
      if (this.quantity.name != that.quantity.name)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantity name differs (${this.quantity.name} vs ${that.quantity.name})")
      else
        new Selecting[DATUM, V](this.entries + that.entries, this.quantity, this.cut + that.cut)

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      checkForCrossReferences()
      val w = weight * quantity(datum)
      if (w > 0.0)
        cut.fill(datum, w)

      // no possibility of exception from here on out (for rollback)
      entries += weight
    }

    def children = cut :: Nil

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "type" -> JsonString(cut.factory.name),
      "data" -> cut.toJsonFragment(false)).
      maybe(JsonString("name") -> (if (suppressName) None else quantity.name.map(JsonString(_))))

    override def toString() = s"""<Selecting cut=${cut.factory.name}>"""
    override def equals(that: Any) = that match {
      case that: Selecting[DATUM, V] => this.entries === that.entries  &&  this.quantity == that.quantity  &&  this.cut == that.cut
      case _ => false
    }
    override def hashCode() = (entries, quantity, cut).hashCode
  }
}
