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

package histogrammar {
  //////////////////////////////////////////////////////////////// Fraction/Fractioned/Fractioning

  /** Accumulate two containers, one with all data (denominator), and one with data that pass a given selection (numerator)."
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Fractioning]] and immutable [[org.dianahep.histogrammar.Fractioned]] objects.
    */
  object Fraction extends Factory {
    val name = "Fraction"
    val help = "Accumulate two containers, one with all data (denominator), and one with data that pass a given selection (numerator)."
    val detailedHelp = """Fraction(numeratorSelection: Selection[DATUM], value: => V = Count())"""

    /** Create an immutable [[org.dianahep.histogrammar.Fractioned]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param numerator Container for data that passed the given selection.
      * @param denominator Container for all data, regardless of whether it passed the given selection.
      */
    def ed[V <: Container[V]](entries: Double, numerator: V, denominator: V) = new Fractioned(entries, numerator, denominator)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Fractioning]].
      * 
      * @param numeratorSelection Boolean or non-negative function that cuts or weights entries.
      * @param value New value (note the `=>`: expression is reevaluated every time a new value is needed).
      */
    def apply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](numeratorSelection: Selection[DATUM], value: => V = Count()) =
      new Fractioning(numeratorSelection, 0.0, value, value)

    /** Synonym for `apply`. */
    def ing[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](numeratorSelection: Selection[DATUM], value: => V = Count()) =
      apply(numeratorSelection, value)

    /** Use [[org.dianahep.histogrammar.Fractioned]] in Scala pattern-matching. */
    def unapply[V <: Container[V]](x: Fractioned[V]) = Some((x.entries, x.numerator, x.denominator))
    /** Use [[org.dianahep.histogrammar.Fractioning]] in Scala pattern-matching. */
    def unapply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](x: Fractioning[DATUM, V]) = Some((x.entries, x.numerator, x.denominator))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("entries", "type", "numerator", "denominator")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val factory = get("type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, name + ".type")
        }

        val numerator = factory.fromJsonFragment(get("numerator"))
        val denominator = factory.fromJsonFragment(get("denominator"))

        new Fractioned[Container[_]](entries, numerator, denominator)

      case _ => throw new JsonFormatException(json, name)
    }
  }

  /** An accumulated pair of containers, one with all data (denominator), and one with data that passed a given selection (numerator).
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param numerator Container for data that passed the given selection.
    * @param denominator Container for all data, regardless of whether it passed the given selection.
    */
  class Fractioned[V <: Container[V]](val entries: Double, val numerator: V, val denominator: V) extends Container[Fractioned[V]] {
    type Type = Fractioned[V]
    def factory = Fraction

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def zero = new Fractioned[V](0.0, numerator.zero, denominator.zero)
    def +(that: Fractioned[V]) = new Fractioned(this.entries + that.entries, this.numerator + that.numerator, this.denominator + that.denominator)

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "type" -> JsonString(numerator.factory.name),
      "numerator" -> numerator.toJsonFragment,
      "denominator" -> denominator.toJsonFragment)

    override def toString() = s"Fractioned[entries=$entries, numerator=$numerator, denominator=$denominator]"
    override def equals(that: Any) = that match {
      case that: Fractioned[V] => this.entries === that.entries  &&  this.numerator == that.numerator  &&  this.denominator == that.denominator
      case _ => false
    }
  }

  /** Accumulating a pair of containers, one with all data (denominator), and one with data that passed a given selection (numerator).
    * 
    * @param numeratorSelection Boolean or non-negative function that cuts or weights entries.
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param numerator Container for data that passed the given selection.
    * @param denominator Container for all data, regardless of whether it passed the given selection.
    */
  class Fractioning[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](val numeratorSelection: Selection[DATUM], var entries: Double, val numerator: V, val denominator: V) extends Container[Fractioning[DATUM, V]] with AggregationOnData {
    type Type = Fractioning[DATUM, V]
    type Datum = DATUM
    def factory = Fraction

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def zero = new Fractioning[DATUM, V](numeratorSelection, 0.0, numerator.zero, denominator.zero)
    def +(that: Fractioning[DATUM, V]) = new Fractioning(this.numeratorSelection, this.entries + that.entries, this.numerator + that.numerator, this.denominator + that.denominator)

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      val w = weight * numeratorSelection(datum)

      entries += weight
      if (weight > 0.0)
        denominator.fillWeighted(datum, weight)
      if (w > 0.0)
        numerator.fillWeighted(datum, w)
    }

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "type" -> JsonString(numerator.factory.name),
      "numerator" -> numerator.toJsonFragment,
      "denominator" -> denominator.toJsonFragment)

    override def toString() = s"Fractioning[entries=$entries, numerator=$numerator, denominator=$denominator]"
    override def equals(that: Any) = that match {
      case that: Fractioning[DATUM, V] => this.numeratorSelection == that.numeratorSelection  &&  this.entries === that.entries  &&  this.numerator == that.numerator  &&  this.denominator == that.denominator
      case _ => false
    }
  }
}
