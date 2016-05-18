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
  //////////////////////////////////////////////////////////////// Count/Counted/Counting

  /** Count data, ignoring their content. (Actually a sum of weights.)
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Counting]] and immutable [[org.dianahep.histogrammar.Counted]] objects.
    */
  object Count extends Factory {
    val name = "Count"
    val help = "Count data, ignoring their content. (Actually a sum of weights.)"
    val detailedHelp = """Count()"""

    /** Create an immutable [[org.dianahep.histogrammar.Counted]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      */
    def ed(entries: Double) = new Counted(entries)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Counting]].
      * 
      * @param quantity Numerical function to track.
      */
    def apply() = new Counting(0.0)

    /** Synonym for `apply`. */
    def ing = apply()

    /** Use [[org.dianahep.histogrammar.Counted]] in Scala pattern-matching. */
    def unapply(x: Counted) = Some(x.entries)
    /** Use [[org.dianahep.histogrammar.Counting]] in Scala pattern-matching. */
    def unapply(x: Counting) = Some(x.entries)

    import KeySetComparisons._
    def fromJsonFragment(json: Json, nameFromParent: Option[String]): Container[_] = json match {
      case JsonFloat(entries) => new Counted(entries)
      case _ => throw new JsonFormatException(json, name)
    }
  }

  /** An accumulated count (sum of weights) of data, ignoring its content.
    * 
    * Use the factory [[org.dianahep.histogrammar.Count]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all weights).
    */
  class Counted private[histogrammar](val entries: Double) extends Container[Counted] {
    type Type = Counted
    def factory = Count

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def zero = new Counted(0.0)
    def +(that: Counted): Counted = new Counted(this.entries + that.entries)

    def children = Nil

    def toJsonFragment(suppressName: Boolean) = JsonFloat(entries)

    override def toString() = s"Counted[$entries]"
    override def equals(that: Any) = that match {
      case that: Counted => this.entries == that.entries
      case _ => false
    }
    override def hashCode() = entries.hashCode
  }

  /** Accumulating a count (sum of weights) of data, ignoring its content.
    * 
    * Use the factory [[org.dianahep.histogrammar.Count]] to construct an instance.
    * 
    * This is the only container with [[org.dianahep.histogrammar.Aggregation]] that doesn't have a configurable data type: its `Datum` is `Any`. It is primarily for the sake of this container that `Aggregation` is contravariant.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    */
  class Counting private[histogrammar](var entries: Double) extends Container[Counting] with Aggregation {
    type Type = Counting
    type Datum = Any
    def factory = Count

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def zero = new Counting(0.0)
    def +(that: Counting): Counting = new Counting(this.entries + that.entries)

    def fill[SUB <: Any](datum: SUB, weight: Double = 1.0) {
      // no possibility of exception from here on out (for rollback)
      if (weight > 0.0)
        entries += weight
    }

    def children = Nil

    def toJsonFragment(suppressName: Boolean) = JsonFloat(entries)

    override def toString() = s"Counting[$entries]"
    override def equals(that: Any) = that match {
      case that: Counting => this.entries == that.entries
      case _ => false
    }
    override def hashCode() = entries.hashCode
  }
}
