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
  //////////////////////////////////////////////////////////////// Quantile/Quantiled/Quantiling

  /** Accumulate an approximation to a given quantile, such as 0.5 for the median, (0.25, 0.75) for the first and third quartiles, or (0.2, 0.4, 0.6, 0.8) for quintiles.
    * 
    * Simple methods for computing quantiles sort the entire dataset and select an item by sorted index, but Histogrammar
    * 
    *   - aggregates in a single pass over the data and
    *   - does not load the entire dataset into memory as it passes over the data.
    * 
    * Therefore, sorting and selecting is out of the question. Instead, this primitive (in all languages) estimates the quantile with the following heuristic:
    * 
    * {{{
    * var entries = 0.0
    * var cumulativeDeviation = 0.0
    * var estimate = NaN
    * 
    * def fill(value: Double, weight: Double) {
    *   entries += weight
    * 
    *   if (datum.isNaN)
    *     estimate = value
    * 
    *   else {
    *     cumulativeDeviation += abs(value - estimate)
    *     val learningRate = 1.5 * cumulativeDeviation / entries**2
    * 
    *     estimate = weight * learningRate * (cmp(value, estimate) + 2.0*target - 1.0)
    *   }
    * }
    * }}}
    * 
    * where `target` is the intended quantile (e.g. 0.5 for median) and `cmp(x, y)` is `-1` for `x < y`, is `+1` for `x > y`, and is `0` for `x == y`. The `cumulativeDeviation` is only used to set a (usually decreasing) `learningRate` and is thread-local to a given aggregator. Only `entries` and `estimate` are persistent (serialized to/from JSON).
    * 
    * The combination function (overloaded `+` operator) simply averages the `estimates`, weighted by `entries`.
    * 
    * The accuracy of this heuristic is best in the bulk of the distribution, rather than the tails, and accuracy scales with the size of the dataset. '''TODO:''' formal study of the accuracy of this heuristic or a reference to literature.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Quantiling]] and immutable [[org.dianahep.histogrammar.Quantiled]] objects.
    */
  object Quantile extends Factory {
    val name = "Quantile"
    val help = "Estimate a quantile, such as 0.5 for median, (0.25, 0.75) for quartiles, or (0.2, 0.4, 0.6, 0.8) for quintiles."
    val detailedHelp = """Quantile(target: Double, quantity: UserFcn[DATUM, Double])"""

    /** Create an immutable [[org.dianahep.histogrammar.Quantiled]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param target Intended quantile (e.g. 0.5 for median).
      * @param estimate Estimated value of the quantile.
      */
    def ed(entries: Double, target: Double, estimate: Double) = new Quantiled(entries, None, target, estimate)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Quantiling]].
      * 
      * @param target Intended quantile (e.g. 0.5 for median).
      * @param quantity Numerical function to track.
      */
    def apply[DATUM](target: Double, quantity: UserFcn[DATUM, Double]) =
      new Quantiling[DATUM](target, quantity, 0.0, java.lang.Double.NaN, 0.0)

    /** Synonym for `apply`. */
    def ing[DATUM](target: Double, quantity: UserFcn[DATUM, Double]) = apply(target, quantity)

    /** Use [[org.dianahep.histogrammar.Quantiled]] in Scala pattern-matching. */
    def unapply(x: Quantiled) = Some(x.estimate)
    /** Use [[org.dianahep.histogrammar.Quantiling]] in Scala pattern-matching. */
    def unapply[DATUM](x: Quantiling[DATUM]) = Some(x.estimate)

    import KeySetComparisons._
    def fromJsonFragment(json: Json, nameFromParent: Option[String]): Container[_] with NoAggregation = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("entries", "target", "estimate").maybe("name")) =>
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

        val target = get("target") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".target")
        }

        val estimate = get("estimate") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".estimate")
        }

        new Quantiled(entries, (nameFromParent ++ quantityName).lastOption, target, estimate)
    }

    private[histogrammar] def estimateCombination(xestimate: Double, xentries: Double, yestimate: Double, yentries: Double) =
      if (xestimate.isNaN  &&  yestimate.isNaN)
        java.lang.Double.NaN
      else if (xestimate.isNaN)
        yestimate
      else if (yestimate.isNaN)
        xestimate
      else
        (xestimate*xentries + yestimate*yentries) / (xentries + yentries)
  }

  /** An estimated quantile, such as 0.5 for median, (0.25, 0.75) for quartiles, or (0.2, 0.4, 0.6, 0.8) for quintiles.
    * 
    * Use the factory [[org.dianahep.histogrammar.Quantile]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param quantityName Optional name given to the quantity function, passed for bookkeeping.
    * @param target Intended quantile (e.g. 0.5 for median).
    * @param estimate Estimated value of the quantile.
    */
  class Quantiled private[histogrammar](val entries: Double, val quantityName: Option[String], val target: Double, val estimate: Double) extends Container[Quantiled] with NoAggregation with QuantityName {

    type Type = Quantiled
    def factory = Quantile

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (target < 0.0  ||  target > 1.0)
      throw new ContainerException(s"target ($target) must be between 0 and 1, inclusive")

    def zero = new Quantiled(0.0, quantityName, target, java.lang.Double.NaN)
    def +(that: Quantiled) = {
      if (this.target != that.target)
        throw new ContainerException(s"cannot add ${getClass.getName} because targets do not match (${this.target} vs ${that.target})")
      if (this.quantityName != that.quantityName)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantityName differs (${this.quantityName} vs ${that.quantityName})")
      new Quantiled(this.entries + that.entries, this.quantityName, target, Quantile.estimateCombination(this.estimate, this.entries, that.estimate, that.entries))
    }

    def children = Nil

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "target" -> JsonFloat(target),
      "estimate" -> JsonFloat(estimate)).
      maybe(JsonString("name") -> (if (suppressName) None else quantityName.map(JsonString(_))))

    override def toString() = s"""Quantiled[$target, estimate=$estimate]"""
    override def equals(that: Any) = that match {
      case that: Quantiled => this.entries == that.entries  &&  this.quantityName == that.quantityName  &&  this.target == that.target  &&  this.estimate == that.estimate
      case _ => false
    }
    override def hashCode() = (entries, quantityName, target, estimate).hashCode()
  }

  /** Accumulating an adaptive histogram, used to compute approximate quantiles, such as the median.
    * 
    * Use the factory [[org.dianahep.histogrammar.Quantile]] to construct an instance.
    * 
    * @param quantity Numerical function to track.
    */
  class Quantiling[DATUM] private[histogrammar](val target: Double, val quantity: UserFcn[DATUM, Double], var entries: Double, var estimate: Double, var cumulativeDeviation: Double)
      extends Container[Quantiling[DATUM]] with AggregationOnData with NumericalQuantity[DATUM] {

    type Type = Quantiling[DATUM]
    type Datum = DATUM
    def factory = Quantile

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (target < 0.0  ||  target > 1.0)
      throw new ContainerException(s"target ($target) must be between 0 and 1, inclusive")

    def zero = new Quantiling[DATUM](target, quantity, 0.0, java.lang.Double.NaN, 0.0)
    def +(that: Quantiling[DATUM]) = {
      if (this.target != that.target)
        throw new ContainerException(s"cannot add ${getClass.getName} because targets do not match (${this.target} vs ${that.target})")
      if (this.quantity.name != that.quantity.name)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantity name differs (${this.quantity.name} vs ${that.quantity.name})")
      new Quantiling[DATUM](target, quantity, this.entries + that.entries, Quantile.estimateCombination(this.estimate, this.entries, that.estimate, that.entries), this.cumulativeDeviation + that.cumulativeDeviation)
    }

    def fill[SUB <: DATUM](datum: SUB, weight: Double = 1.0) {
      if (weight > 0.0) {
        val q = quantity(datum)

        // no possibility of exception from here on out (for rollback)
        entries += weight
        if (estimate.isNaN)
          estimate = q
        else {
          cumulativeDeviation += Math.abs(q - estimate)
          val learningRate = 1.5 * cumulativeDeviation / (entries * entries)
          val sgn =
            if      (q < estimate) -1.0
            else if (q > estimate)  1.0
            else                    0.0

          estimate = weight * learningRate * (sgn + 2.0*target - 1.0)
        }
      }
    }

    def children = Nil

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "target" -> JsonFloat(target),
      "estimate" -> JsonFloat(estimate)).
      maybe(JsonString("name") -> (if (suppressName) None else quantity.name.map(JsonString(_))))

    override def toString() = s"""Quantiling[$target, estimate=$estimate]"""
    override def equals(that: Any) = that match {
      case that: Quantiling[DATUM] => this.entries == that.entries  &&  this.target == that.target  &&  this.estimate == that.estimate  &&  this.cumulativeDeviation == that.cumulativeDeviation  &&  this.quantity == that.quantity
      case _ => false
    }
    override def hashCode() = (entries, target, estimate, cumulativeDeviation, quantity).hashCode()
  }
}
