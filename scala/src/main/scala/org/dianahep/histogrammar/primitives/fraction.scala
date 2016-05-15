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
  //////////////////////////////////////////////////////////////// Fraction/Fractioned/Fractioning

  /** Accumulate two containers, one with all data (denominator), and one with data that pass a given selection (numerator).
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Fractioning]] and immutable [[org.dianahep.histogrammar.Fractioned]] objects.
    */
  object Fraction extends Factory {
    val name = "Fraction"
    val help = "Accumulate two containers, one with all data (denominator), and one with data that pass a given selection (numerator)."
    val detailedHelp = """Fraction(selection: UserFcn[DATUM, Double], value: => V = Count())"""

    /** Create an immutable [[org.dianahep.histogrammar.Fractioned]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights without the cut applied).
      * @param numerator Container for data that passed the given selection.
      * @param denominator Container for all data, regardless of whether it passed the given selection.
      */
    def ed[V <: Container[V]](entries: Double, numerator: V, denominator: V) = new Fractioned(entries, None, numerator, denominator)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Fractioning]].
      * 
      * @param selection Boolean or non-negative function that cuts or weights entries.
      * @param value Template used to create zero values (by calling this `value`'s `zero` method).
      */
    def apply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](selection: UserFcn[DATUM, Double], value: => V = Count()) =
      new Fractioning(selection, 0.0, value.zero, value.zero)

    /** Synonym for `apply`. */
    def ing[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](selection: UserFcn[DATUM, Double], value: => V = Count()) =
      apply(selection, value)

    import KeySetComparisons._
    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("entries", "type", "numerator", "denominator").maybe("name")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val selectionName = get.getOrElse("name", JsonNull) match {
          case JsonString(x) => Some(x)
          case JsonNull => None
          case x => throw new JsonFormatException(x, name + ".name")
        }

        val factory = get("type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, name + ".type")
        }

        val numerator = factory.fromJsonFragment(get("numerator"))
        val denominator = factory.fromJsonFragment(get("denominator"))

        new Fractioned[Container[_]](entries, selectionName, numerator, denominator)

      case _ => throw new JsonFormatException(json, name)
    }
  }

  /** An accumulated pair of containers, one with all data (denominator), and one with data that passed a given selection (numerator).
    * 
    * Use the factory [[org.dianahep.histogrammar.Fraction]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights without the cut applied).
    * @param selectionName Optional name given to the selection function, passed for bookkeeping.
    * @param numerator Container for data that passed the given selection.
    * @param denominator Container for all data, regardless of whether it passed the given selection.
    */
  class Fractioned[V <: Container[V]] private[histogrammar](val entries: Double, val selectionName: Option[String], val numerator: V, val denominator: V) extends Container[Fractioned[V]] with Cut.Methods {
    type Type = Fractioned[V]
    def factory = Fraction

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def fractionPassing = numerator.entries / entries

    def zero = new Fractioned[V](0.0, selectionName, numerator.zero, denominator.zero)
    def +(that: Fractioned[V]) =
      if (this.selectionName != that.selectionName)
        throw new ContainerException(s"cannot add ${getClass.getName} because selectionName differs (${this.selectionName} vs ${that.selectionName})")
      else
        new Fractioned(this.entries + that.entries, this.selectionName, this.numerator + that.numerator, this.denominator + that.denominator)

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "type" -> JsonString(numerator.factory.name),
      "numerator" -> numerator.toJsonFragment,
      "denominator" -> denominator.toJsonFragment).
      maybe(JsonString("name") -> selectionName.map(JsonString(_)))

    override def toString() = s"Fractioned[numerator=$numerator, denominator=$denominator]"
    override def equals(that: Any) = that match {
      case that: Fractioned[V] => this.entries === that.entries  &&  this.selectionName == that.selectionName  &&  this.numerator == that.numerator  &&  this.denominator == that.denominator
      case _ => false
    }
    override def hashCode() = (entries, selectionName, numerator, denominator).hashCode
  }

  /** Accumulating a pair of containers, one with all data (denominator), and one with data that passed a given selection (numerator).
    * 
    * Use the factory [[org.dianahep.histogrammar.Fraction]] to construct an instance.
    * 
    * @param selection Boolean or non-negative function that cuts or weights entries.
    * @param entries Weighted number of entries (sum of all observed weights without the cut applied).
    * @param numerator Container for data that passed the given selection.
    * @param denominator Container for all data, regardless of whether it passed the given selection.
    */
  class Fractioning[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}] private[histogrammar](val selection: UserFcn[DATUM, Double], var entries: Double, val numerator: V, val denominator: V) extends Container[Fractioning[DATUM, V]] with AggregationOnData with Cut.Methods {
    type Type = Fractioning[DATUM, V]
    type Datum = DATUM
    def factory = Fraction

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def fractionPassing = numerator.entries / entries

    def zero = new Fractioning[DATUM, V](selection, 0.0, numerator.zero, denominator.zero)
    def +(that: Fractioning[DATUM, V]) =
      if (this.selection.name != that.selection.name)
        throw new ContainerException(s"cannot add ${getClass.getName} because selection name differs (${this.selection.name} vs ${that.selection.name})")
      else
        new Fractioning(this.selection, this.entries + that.entries, this.numerator + that.numerator, this.denominator + that.denominator)

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      val w = weight * selection(datum)

      if (weight > 0.0)
        denominator.fill(datum, weight)
      if (w > 0.0)
        numerator.fill(datum, w)

      // no possibility of exception from here on out (for rollback)
      entries += weight
    }

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "type" -> JsonString(numerator.factory.name),
      "numerator" -> numerator.toJsonFragment,
      "denominator" -> denominator.toJsonFragment).
      maybe(JsonString("name") -> selection.name.map(JsonString(_)))

    override def toString() = s"Fractioning[$numerator, $denominator]"
    override def equals(that: Any) = that match {
      case that: Fractioning[DATUM, V] => this.selection == that.selection  &&  this.entries === that.entries  &&  this.numerator == that.numerator  &&  this.denominator == that.denominator
      case _ => false
    }
    override def hashCode() = (selection, entries, numerator, denominator).hashCode
  }
}
