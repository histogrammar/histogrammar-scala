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
  //////////////////////////////////////////////////////////////// Sum/Summed/Summing

  /** Accumulate the (weighted) sum of a given quantity, calculated from the data.
    * 
    * Sum differs from [[org.dianahep.histogrammar.Count]] in that it computes a quantity on the spot, rather than percolating a product of weight metadata from nested primitives. Also unlike weights, the sum can add both positive and negative quantities (weights are always non-negative).
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Summing]] and immutable [[org.dianahep.histogrammar.Summed]] objects.
    */
  object Sum extends Factory {
    val name = "Sum"
    val help = "Accumulate the (weighted) sum of a given quantity, calculated from the data."
    val detailedHelp = """Sum differs from [[org.dianahep.histogrammar.Count]] in that it computes a quantity on the spot, rather than percolating a product of weight metadata from nested primitives. Also unlike weights, the sum can add both positive and negative quantities (weights are always non-negative)."""

    /** Create an immutable [[org.dianahep.histogrammar.Summed]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param sum The sum of weight times quantity over all entries.
      */
    def ed(entries: Double, sum: Double) = new Summed(entries, None, sum)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Summing]].
      * 
      * @param quantity Numerical function to track.
      */
    def apply[DATUM](quantity: UserFcn[DATUM, Double]) = new Summing[DATUM](quantity, 0.0, 0.0)

    /** Synonym for `apply`. */
    def ing[DATUM](quantity: UserFcn[DATUM, Double]) = apply(quantity)

    /** Use [[org.dianahep.histogrammar.Summed]] in Scala pattern-matching. */
    def unapply(x: Summed) = Some(x.sum)
    /** Use [[org.dianahep.histogrammar.Summing]] in Scala pattern-matching. */
    def unapply(x: Summing[_]) = Some(x.sum)

    import KeySetComparisons._
    def fromJsonFragment(json: Json, nameFromParent: Option[String]): Container[_] with NoAggregation = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("entries", "sum").maybe("name")) =>
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

        val sum = get("sum") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".sum")
        }

        new Summed(entries, (nameFromParent ++ quantityName).lastOption, sum)

      case _ => throw new JsonFormatException(json, name)
    }
  }

  /** An accumulated weighted sum of a given quantity.
    * 
    * Use the factory [[org.dianahep.histogrammar.Sum]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param quantityName Optional name given to the quantity function, passed for bookkeeping.
    * @param sum The sum of weight times quantity over all entries.
    */
  class Summed private[histogrammar](val entries: Double, val quantityName: Option[String], val sum: Double) extends Container[Summed] with NoAggregation with QuantityName {
    type Type = Summed
    type EdType = Summed
    def factory = Sum

    def zero = new Summed(0.0, quantityName, 0.0)
    def +(that: Summed) =
      if (this.quantityName != that.quantityName)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantityName differs (${this.quantityName} vs ${that.quantityName})")
      else
        new Summed(this.entries + that.entries, this.quantityName, this.sum + that.sum)
    def *(factor: Double) =
      if (factor.isNaN  ||  factor <= 0.0)
        zero
      else
        new Summed(factor * entries, quantityName, factor * sum)

    def children = Nil

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "sum" -> JsonFloat(sum)).
      maybe(JsonString("name") -> (if (suppressName) None else quantityName.map(JsonString(_))))

    override def toString() = s"<Summed sum=$sum>"
    override def equals(that: Any) = that match {
      case that: Summed => this.entries === that.entries  &&  this.quantityName == that.quantityName  &&  this.sum === that.sum
      case _ => false
    }
    override def hashCode() = (entries, quantityName, sum).hashCode
  }

  /** Accumulating a weighted sum of a given quantity.
    * 
    * Use the factory [[org.dianahep.histogrammar.Sum]] to construct an instance.
    * 
    * @param quantity Numerical function to track.
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param sum The sum of weight times quantity over all entries.
    */
  class Summing[DATUM] private[histogrammar](val quantity: UserFcn[DATUM, Double], var entries: Double, var sum: Double) extends Container[Summing[DATUM]] with AggregationOnData with NumericalQuantity[DATUM] {
    type Type = Summing[DATUM]
    type EdType = Summed
    type Datum = DATUM
    def factory = Sum

    def zero = new Summing[DATUM](quantity, 0.0, 0.0)
    def +(that: Summing[DATUM]) =
      if (this.quantity.name != that.quantity.name)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantity name differs (${this.quantity.name} vs ${that.quantity.name})")
      else
        new Summing(this.quantity, this.entries + that.entries, this.sum + that.sum)
    def *(factor: Double) =
      if (factor.isNaN  ||  factor <= 0.0)
        zero
      else
        new Summing[DATUM](quantity, factor * entries, factor * sum)

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0): Unit = {
      checkForCrossReferences()
      if (weight > 0.0) {
        val q = quantity(datum)

        // no possibility of exception from here on out (for rollback)
        entries += weight
        sum += q * weight
      }
    }

    def children = Nil

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "sum" -> JsonFloat(sum)).
      maybe(JsonString("name") -> (if (suppressName) None else quantity.name.map(JsonString(_))))

    override def toString() = s"<Summing sum=$sum>"
    override def equals(that: Any) = that match {
      case that: Summing[DATUM] => this.quantity == that.quantity  &&  this.entries === that.entries  &&  this.sum === that.sum
      case _ => false
    }
    override def hashCode() = (quantity, entries, sum).hashCode
  }
}
