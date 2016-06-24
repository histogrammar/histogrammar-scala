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

  /** Estimate a quantile, such as 0.5 for median, (0.25, 0.75) for quartiles, or (0.2, 0.4, 0.6, 0.8) for quintiles, etc.
    * 
    * '''Note:''' this is an inexact heuristic! In general, it is not possible to derive an exact quantile in a single pass over a dataset (without accumulating a large fraction of the dataset in memory). To interpret this statistic, refer to the fill and merge algorithms below.
    * 
    * The quantile aggregator dynamically minimizes the mean absolute error between the current estimate and the target quantile, with a learning rate that depends on the cumulative deviations. The algorithm is deterministic: the same data always yields the same final estimate.
    * 
    * This statistic has the best accuracy for quantiles near the middle of the distribution, such as the median (0.5), and the worst accuracy for quantiles near the edges, such as the first or last percentile (0.01 or 0.99). Use the specialized aggregators for the [[org.dianahep.histogrammar.Minimize]] (0.0) or [[org.dianahep.histogrammar.Maximize]] (1.0) of a distribution, since those aggregators are exact.
    * 
    * Another alternative is to use [[org.dianahep.histogrammar.AdaptivelyBin]] to histogram the distribution and then estimate quantiles from the histogram bins. AdaptivelyBin with `tailDetail == 1.0` maximizes detail on the tails of the distribution (Yael Ben-Haim and Elad Tom-Tov's original algorithm), providing the best estimates of extreme quantiles like 0.01 and 0.99.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Quantiling]] and immutable [[org.dianahep.histogrammar.Quantiled]] objects.
    */
  object Quantile extends Factory {
    val name = "Quantile"
    val help = "Estimate a quantile, such as 0.5 for median, (0.25, 0.75) for quartiles, or (0.2, 0.4, 0.6, 0.8) for quintiles, etc."
    val detailedHelp = """'''Note:''' this is an inexact heuristic! In general, it is not possible to derive an exact quantile in a single pass over a dataset (without accumulating a large fraction of the dataset in memory). To interpret this statistic, refer to the fill and merge algorithms below.

The quantile aggregator dynamically minimizes the mean absolute error between the current estimate and the target quantile, with a learning rate that depends on the cumulative deviations. The algorithm is deterministic: the same data always yields the same final estimate.

This statistic has the best accuracy for quantiles near the middle of the distribution, such as the median (0.5), and the worst accuracy for quantiles near the edges, such as the first or last percentile (0.01 or 0.99). Use the specialized aggregators for the [[org.dianahep.histogrammar.Minimum]] (0.0) or [[org.dianahep.histogrammar.Maximum]] (1.0) of a distribution, since those aggregators are exact.

Another alternative is to use [[org.dianahep.histogrammar.AdaptivelyBin]] to histogram the distribution and then estimate quantiles from the histogram bins. AdaptivelyBin with `tailDetail == 1.0` maximizes detail on the tails of the distribution (Yael Ben-Haim and Elad Tom-Tov's original algorithm), providing the best estimates of extreme quantiles like 0.01 and 0.99."""

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
      else if (xentries + yentries == 0.0)
        (xestimate + yestimate) / 2.0
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
    type EdType = Quantiled
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

    override def toString() = s"""<Quantiled target=$target estimate=$estimate>"""
    override def equals(that: Any) = that match {
      case that: Quantiled => this.entries === that.entries  &&  this.quantityName == that.quantityName  &&  this.target === that.target  &&  this.estimate === that.estimate
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
    type EdType = Quantiled
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
      checkForCrossReferences()
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

    override def toString() = s"""<Quantiling target=$target estimate=$estimate>"""
    override def equals(that: Any) = that match {
      case that: Quantiling[DATUM] => this.entries === that.entries  &&  this.target === that.target  &&  this.estimate === that.estimate  &&  this.cumulativeDeviation === that.cumulativeDeviation  &&  this.quantity == that.quantity
      case _ => false
    }
    override def hashCode() = (entries, target, estimate, cumulativeDeviation, quantity).hashCode()
  }
}
