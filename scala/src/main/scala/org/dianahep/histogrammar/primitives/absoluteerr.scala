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
  //////////////////////////////////////////////////////////////// AbsoluteErr/AbsoluteErred/AbsoluteErring

  /** Accumulate the weighted Mean Absolute Error (MAE) of a quantity whose nominal value is zero.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.AbsoluteErring]] and immutable [[org.dianahep.histogrammar.AbsoluteErred]] objects.
    */
  object AbsoluteErr extends Factory {
    val name = "AbsoluteErr"
    val help = "Accumulate the weighted Mean Absolute Error (MAE) of a quantity whose nominal value is zero."
    val detailedHelp = """AbsoluteErr(quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM])"""

    /** Create an immutable [[org.dianahep.histogrammar.AbsoluteErred]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param mae Sum of absolute differences of the quantity from zero (Mean Absolute Error).
      */
    def ed(entries: Double, mae: Double) = new AbsoluteErred(entries, mae)

    /** Create an empty, mutable [[org.dianahep.histogrammar.AbsoluteErring]].
      * 
      * @param quantity Numerical function to track.
      * @param selection Boolean or non-negative function that cuts or weights entries.
      */
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new AbsoluteErring(quantity, selection, 0.0, 0.0)

    /** Synonym for `apply`. */
    def ing[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = apply(quantity, selection)

    /** Use [[org.dianahep.histogrammar.AbsoluteErred]] in Scala pattern-matching. */
    def unapply(x: AbsoluteErred) = Some((x.entries, x.mae))
    /** Use [[org.dianahep.histogrammar.AbsoluteErring]] in Scala pattern-matching. */
    def unapply[DATUM](x: AbsoluteErring[DATUM]) = Some((x.entries, x.mae))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("entries", "mae")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val mae = get("mae") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".mae")
        }

        new AbsoluteErred(entries, mae)

      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def plus(ca: Double, ma: Double, cb: Double, mb: Double) =
      (ca + cb, (ca*ma + cb*mb)/(ca + cb))
  }

  /** An accumulated weighted Mean Absolute Error (MAE) of a quantity whose nominal value is zero.
    * 
    * Use the factory [[org.dianahep.histogrammar.AbsoluteErr]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all weights).
    * @param mae Sum of absolute differences of the quantity from zero (Mean Absolute Error).
    */
  class AbsoluteErred private[histogrammar](val entries: Double, val mae: Double) extends Container[AbsoluteErred] {
    type Type = AbsoluteErred
    def factory = AbsoluteErr

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def zero = new AbsoluteErred(0.0, 0.0)
    def +(that: AbsoluteErred) = {
      val (newentries, newmae) = AbsoluteErr.plus(this.entries, this.mae, that.entries, that.mae)
      new AbsoluteErred(newentries, newmae)
    }

    def toJsonFragment = JsonObject("entries" -> JsonFloat(entries), "mae" -> JsonFloat(mae))

    override def toString() = s"AbsoluteErred[$mae]"
    override def equals(that: Any) = that match {
      case that: AbsoluteErred => this.entries === that.entries  &&  this.mae === that.mae
      case _ => false
    }
    override def hashCode() = (entries, mae).hashCode
  }

  /** Accumulating a weighted Mean Absolute Error (MAE) of a quantity whose nominal value is zero.
    * 
    * Use the factory [[org.dianahep.histogrammar.AbsoluteErr]] to construct an instance.
    * 
    * @param quantity Numerical function to track.
    * @param selection Boolean or non-negative function that cuts or weights entries.
    * @param entries Weighted number of entries (sum of all weights).
    * @param _mae Sum of absolute differences of the quantity from zero (Mean Absolute Error).
    */
  class AbsoluteErring[DATUM] private[histogrammar](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var entries: Double, _mae: Double) extends Container[AbsoluteErring[DATUM]] with AggregationOnData {
    type Type = AbsoluteErring[DATUM]
    type Datum = DATUM
    def factory = AbsoluteErr

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    private var absoluteSum = entries * _mae

    /** sum of absolute differences of the quantity from zero (Mean Absolute Error) */
    def mae =
      if (entries == 0.0)
        _mae
      else
        absoluteSum / entries

    def mae_=(_mae: Double) {
      absoluteSum = entries * _mae
    }

    def zero = new AbsoluteErring[DATUM](quantity, selection, 0.0, 0.0)
    def +(that: AbsoluteErring[DATUM]) = {
      val (newentries, newmae) = AbsoluteErr.plus(this.entries, this.mae, that.entries, that.mae)
      new AbsoluteErring[DATUM](this.quantity, this.selection, newentries, newmae)
    }

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)
        entries += w
        absoluteSum += Math.abs(q)
      }
    }

    def toJsonFragment = JsonObject("entries" -> JsonFloat(entries), "mae" -> JsonFloat(mae))

    override def toString() = s"AbsoluteErring[$mae]"
    override def equals(that: Any) = that match {
      case that: AbsoluteErring[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.entries === that.entries  &&  this.mae === that.mae
      case _ => false
    }
    override def hashCode() = (quantity, selection, entries, mae).hashCode
  }
}
