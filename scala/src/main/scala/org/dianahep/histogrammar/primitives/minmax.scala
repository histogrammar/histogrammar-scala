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
  //////////////////////////////////////////////////////////////// Minimize/Minimized/Minimizing

  /** Find the minimum value of a given quantity. If no data are observed, the result is `NaN`.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Minimizing]] and immutable [[org.dianahep.histogrammar.Minimized]] objects.
    */
  object Minimize extends Factory {
    val name = "Minimize"
    val help = "Find the minimum value of a given quantity. If no data are observed, the result is NaN."
    val detailedHelp = """Minimize(quantity: UserFcn[DATUM, Double])"""

    /** Create an immutable [[org.dianahep.histogrammar.Minimized]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param quantityName Optional name given to the quantity function, passed for bookkeeping.
      * @param min Lowest observed value.
      */
    def ed(entries: Double, quantityName: Option[String], min: Double) = new Minimized(entries, quantityName, min)

    /** Create an immutable [[org.dianahep.histogrammar.Minimizing]].
      * 
      * @param quantity Numerical function to track.
      */
    def apply[DATUM](quantity: UserFcn[DATUM, Double]) = new Minimizing(quantity, 0.0, java.lang.Double.NaN)

    /** Synonym for `apply`. */
    def ing[DATUM](quantity: UserFcn[DATUM, Double]) = apply(quantity)

    /** Use [[org.dianahep.histogrammar.Minimized]] in Scala pattern-matching. */
    def unapply(x: Minimized) = Some(x.min)
    /** Use [[org.dianahep.histogrammar.Minimizing]] in Scala pattern-matching. */
    def unapply[DATUM](x: Minimizing[DATUM]) = Some(x.min)

    import KeySetComparisons._
    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("entries", "min").maybe("name")) =>
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

        get("min") match {
          case JsonNumber(x) => new Minimized(entries, quantityName, x)
          case x => throw new JsonFormatException(x, name)
        }

      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def plus(one: Double, two: Double) =
      if (one.isNaN)
        two
      else if (two.isNaN)
        one
      else if (one < two)
        one
      else
        two
  }

  /** An accumulated minimum of a given quantity. If no data are observed, the result is `NaN`.
    * 
    * Use the factory [[org.dianahep.histogrammar.Minimize]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param quantityName Optional name given to the quantity function, passed for bookkeeping.
    * @param min Lowest observed value.
    */
  class Minimized private[histogrammar](val entries: Double, val quantityName: Option[String], val min: Double) extends Container[Minimized] {
    type Type = Minimized
    def factory = Minimize

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def zero = new Minimized(0.0, quantityName, java.lang.Double.NaN)
    def +(that: Minimized) =
      if (this.quantityName != that.quantityName)
        throw new ContainerException(s"cannot add Minimized because quantityName differs (${this.quantityName} vs ${that.quantityName})")
      else
        new Minimized(this.entries + that.entries, quantityName, Minimize.plus(this.min, that.min))

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "min" -> JsonFloat(min)).
      maybe(JsonString("name") -> quantityName.map(JsonString(_)))

    override def toString() = s"Minimized[$min]"
    override def equals(that: Any) = that match {
      case that: Minimized => this.entries === that.entries  &&  this.quantityName == that.quantityName  &&  this.min === that.min
      case _ => false
    }
    override def hashCode() = (entries, quantityName, min).hashCode
  }

  /** Accumulating the minimum of a given quantity. If no data are observed, the result is `NaN`.
    * 
    * Use the factory [[org.dianahep.histogrammar.Minimize]] to construct an instance.
    * 
    * @param quantity Numerical function to track.
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param min Lowest observed value.
    */
  class Minimizing[DATUM] private[histogrammar](val quantity: UserFcn[DATUM, Double], var entries: Double, var min: Double) extends Container[Minimizing[DATUM]] with AggregationOnData {
    type Type = Minimizing[DATUM]
    type Datum = DATUM
    def factory = Minimize

    def zero = new Minimizing[DATUM](quantity, 0.0, java.lang.Double.NaN)
    def +(that: Minimizing[DATUM]) =
      if (this.quantity.name != that.quantity.name)
        throw new ContainerException(s"cannot add Minimizing because quantity name differs (${this.quantity.name} vs ${that.quantity.name})")
      else
        new Minimizing[DATUM](this.quantity, this.entries + that.entries, Minimize.plus(this.min, that.min))

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      entries += weight
      if (weight > 0.0) {
        val q = quantity(datum)
        if (min.isNaN  ||  q < min)
          min = q
      }
    }

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "min" -> JsonFloat(min)).
      maybe(JsonString("name") -> quantity.name.map(JsonString(_)))

    override def toString() = s"Minimizing[$min]"
    override def equals(that: Any) = that match {
      case that: Minimizing[DATUM] => this.quantity == that.quantity  &&  this.entries === that.entries  &&  this.min === that.min
      case _ => false
    }
    override def hashCode() = (quantity, entries, min).hashCode
  }

  //////////////////////////////////////////////////////////////// Maximize/Maximized/Maximizing

  /** Find the maximum value of a given quantity. If no data are observed, the result is `NaN`.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Maximizing]] and immutable [[org.dianahep.histogrammar.Maximized]] objects.
    */
  object Maximize extends Factory {
    val name = "Maximize"
    val help = "Find the maximum value of a given quantity. If no data are observed, the result is NaN."
    val detailedHelp = """Maximize(quantity: UserFcn[DATUM, Double])"""

    /** Create an immutable [[org.dianahep.histogrammar.Maximized]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param quantityName Optional name given to the quantity function, passed for bookkeeping.
      * @param max Highest observed value.
      */
    def ed(entries: Double, quantityName: Option[String], max: Double) = new Maximized(entries, quantityName, max)

    /** Create an immutable [[org.dianahep.histogrammar.Maximizing]].
      * 
      * @param quantity Numerical function to track.
      */
    def apply[DATUM](quantity: UserFcn[DATUM, Double]) = new Maximizing(quantity, 0.0, java.lang.Double.NaN)

    /** Synonym for `apply`. */
    def ing[DATUM](quantity: UserFcn[DATUM, Double]) = apply(quantity)

    /** Use [[org.dianahep.histogrammar.Maximized]] in Scala pattern-matching. */
    def unapply(x: Maximized) = Some(x.max)
    /** Use [[org.dianahep.histogrammar.Maximizing]] in Scala pattern-matching. */
    def unapply[DATUM](x: Maximizing[DATUM]) = Some(x.max)

    import KeySetComparisons._
    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("entries", "max").maybe("name")) =>
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

        get("max") match {
          case JsonNumber(x) => new Maximized(entries, quantityName, x)
          case x => throw new JsonFormatException(x, name)
        }

      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def plus(one: Double, two: Double) =
      if (one.isNaN)
        two
      else if (two.isNaN)
        one
      else if (one > two)
        one
      else
        two
  }

  /** An accumulated maximum of a given quantity. If no data are observed, the result is `NaN`.
    * 
    * Use the factory [[org.dianahep.histogrammar.Maximize]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param quantityName Optional name given to the quantity function, passed for bookkeeping.
    * @param max Highest observed value.
    */
  class Maximized private[histogrammar](val entries: Double, val quantityName: Option[String], val max: Double) extends Container[Maximized] {
    type Type = Maximized
    def factory = Maximize

    def zero = new Maximized(0.0, quantityName, java.lang.Double.NaN)
    def +(that: Maximized) =
      if (this.quantityName != that.quantityName)
        throw new ContainerException(s"cannot add Maximized because quantityName differs (${this.quantityName} vs ${that.quantityName})")
    else
      new Maximized(this.entries + that.entries, this.quantityName, Maximize.plus(this.max, that.max))

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "max" -> JsonFloat(max)).
      maybe(JsonString("name") -> quantityName.map(JsonString(_)))

    override def toString() = s"Maximized[$max]"
    override def equals(that: Any) = that match {
      case that: Maximized => this.entries === that.entries  &&  this.quantityName == that.quantityName  &&  this.max === that.max
      case _ => false
    }
    override def hashCode() = (entries, quantityName, max).hashCode
  }

  /** Accumulating the maximum of a given quantity. If no data are observed, the result is `NaN`.
    * 
    * Use the factory [[org.dianahep.histogrammar.Maximize]] to construct an instance.
    * 
    * @param quantity Numerical function to track.
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param max Highest observed value.
    */
  class Maximizing[DATUM] private[histogrammar](val quantity: UserFcn[DATUM, Double], var entries: Double, var max: Double) extends Container[Maximizing[DATUM]] with AggregationOnData {
    type Type = Maximizing[DATUM]
    type Datum = DATUM
    def factory = Maximize

    def zero = new Maximizing[DATUM](quantity, 0.0, java.lang.Double.NaN)
    def +(that: Maximizing[DATUM]) =
      if (this.quantity.name != that.quantity.name)
        throw new ContainerException(s"cannot add Maximizing because quantity name differs (${this.quantity.name} vs ${that.quantity.name})")
      else
        new Maximizing[DATUM](this.quantity, this.entries + that.entries, Maximize.plus(this.max, that.max))

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      entries += weight
      if (weight > 0.0) {
        val q = quantity(datum)
        if (max.isNaN  ||  q > max)
          max = q
      }
    }

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "max" -> JsonFloat(max)).
      maybe(JsonString("name") -> quantity.name.map(JsonString(_)))

    override def toString() = s"Maximizing[$max]"
    override def equals(that: Any) = that match {
      case that: Maximizing[DATUM] => this.quantity == that.quantity  &&  this.entries === that.entries  &&  this.max === that.max
      case _ => false
    }
    override def hashCode() = (quantity, entries, max).hashCode
  }
}
