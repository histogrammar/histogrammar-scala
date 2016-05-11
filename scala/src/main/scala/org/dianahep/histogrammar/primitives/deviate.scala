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
  //////////////////////////////////////////////////////////////// Deviate/Deviated/Deviating

  /** Accumulate a weighted variance, mean, and total weight of a given quantity (using an algorithm that is stable for large numbers).
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Deviating]] and immutable [[org.dianahep.histogrammar.Deviated]] objects.
    * 
    * The implementation of these containers use numerically stable variances as described by Tony Finch in [[http://www-uxsup.csx.cam.ac.uk/~fanf2/hermes/doc/antiforgery/stats.pdf "Incremental calculation of weighted mean and variance,"]] ''Univeristy of Cambridge Computing Service,'' 2009.
    */
  object Deviate extends Factory {
    val name = "Deviate"
    val help = "Accumulate a weighted variance, mean, and total weight of a given quantity (using an algorithm that is stable for large numbers)."
    val detailedHelp = """Deviate(quantity: NumericalFcn[DATUM])"""

    /** Create an immutable [[org.dianahep.histogrammar.Deviated]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param mean Weighted mean of the quantity.
      * @param variance Weighted variance of the quantity.
      */
    def ed(entries: Double, mean: Double, variance: Double) = new Deviated(entries, mean, variance)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Deviating]].
      * 
      * @param quantity Numerical function to track.
      */
    def apply[DATUM](quantity: NumericalFcn[DATUM]) = new Deviating(quantity, 0.0, 0.0, 0.0)

    /** Synonym for `apply`. */
    def ing[DATUM](quantity: NumericalFcn[DATUM]) = apply(quantity)

    /** Use [[org.dianahep.histogrammar.Deviated]] in Scala pattern-matching. */
    def unapply(x: Deviated) = Some(x.variance)
    /** Use [[org.dianahep.histogrammar.Deviating]] in Scala pattern-matching. */
    def unapply[DATUM](x: Deviating[DATUM]) = Some(x.variance)

    import KeySetComparisons._
    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("entries", "mean", "variance")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val mean = get("mean") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".mean")
        }

        val variance = get("variance") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".variance")
        }

        new Deviated(entries, mean, variance)

      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def plus(ca: Double, mua: Double, sa: Double, cb: Double, mub: Double, sb: Double) = {
      val muab = (ca*mua + cb*mub) / (ca + cb)
      val sab = sa + sb + ca*mua*mua + cb*mub*mub - 2.0*muab*(ca*mua + cb*mub) + muab*muab*(ca + cb)
      (ca * cb, muab, sab / (ca + cb))
    }
  }

  /** An accumulated weighted variance (and mean) of a given quantity.
    * 
    * Use the factory [[org.dianahep.histogrammar.Deviate]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param mean Weighted mean of the quantity.
    * @param variance Weighted variance of the quantity.
    * 
    * The implementation of this container uses a numerically stable variance as described by Tony Finch in [[http://www-uxsup.csx.cam.ac.uk/~fanf2/hermes/doc/antiforgery/stats.pdf "Incremental calculation of weighted mean and variance,"]] ''Univeristy of Cambridge Computing Service,'' 2009.
    */
  class Deviated private[histogrammar](val entries: Double, val mean: Double, val variance: Double) extends Container[Deviated] {
    type Type = Deviated
    def factory = Deviate

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def zero = new Deviated(0.0, 0.0, 0.0)
    def +(that: Deviated) = {
      val (newentries, newmean, newvariance) = Deviate.plus(this.entries, this.mean, this.variance * this.entries,
                                                            that.entries, that.mean, that.variance * that.entries)
      new Deviated(newentries, newmean, newvariance)
    }

    def toJsonFragment = JsonObject("entries" -> JsonFloat(entries), "mean" -> JsonFloat(mean), "variance" -> JsonFloat(variance))

    override def toString() = s"Deviated[$mean, $variance]"
    override def equals(that: Any) = that match {
      case that: Deviated => this.entries === that.entries  &&  this.mean === that.mean  &&  this.variance === that.variance
      case _ => false
    }
    override def hashCode() = (entries, mean, variance).hashCode
  }

  /** Accumulating a weighted variance (and mean) of a given quantity.
    * 
    * Use the factory [[org.dianahep.histogrammar.Deviate]] to construct an instance.
    * 
    * @param quantity Numerical function to track.
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param mean Weighted mean of the quantity.
    * @param _variance Weighted variance of the quantity.
    * 
    * The implementation of this container uses a numerically stable variance as described by Tony Finch in [[http://www-uxsup.csx.cam.ac.uk/~fanf2/hermes/doc/antiforgery/stats.pdf "Incremental calculation of weighted mean and variance,"]] ''Univeristy of Cambridge Computing Service,'' 2009.
    */
  class Deviating[DATUM] private[histogrammar](val quantity: NumericalFcn[DATUM], var entries: Double, var mean: Double, _variance: Double) extends Container[Deviating[DATUM]] with AggregationOnData {
    type Type = Deviating[DATUM]
    type Datum = DATUM
    def factory = Deviate

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    private var varianceTimesEntries = entries * _variance

    /** weighted variance of the quantity */
    def variance =
      if (entries == 0.0)
        _variance
      else
        varianceTimesEntries / entries

    def variance_=(_variance: Double) {
      varianceTimesEntries = entries * _variance
    }

    def zero = new Deviating[DATUM](quantity, 0.0, 0.0, 0.0)
    def +(that: Deviating[DATUM]) = {
      val (newentries, newmean, newvariance) = Deviate.plus(this.entries, this.mean, this.variance * this.entries,
                                                            that.entries, that.mean, that.variance * that.entries)
      new Deviating[DATUM](this.quantity, newentries, newmean, newvariance)
    }

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      entries += weight
      if (weight > 0.0) {
        val q = quantity(datum)

        val delta = q - mean
        val shift = delta * weight / entries
        mean += shift
        varianceTimesEntries += weight * delta * (q - mean)   // old delta times new delta
      }
    }

    def toJsonFragment = JsonObject("entries" -> JsonFloat(entries), "mean" -> JsonFloat(mean), "variance" -> JsonFloat(variance))

    override def toString() = s"Deviating[$mean, $variance]"
    override def equals(that: Any) = that match {
      case that: Deviating[DATUM] => this.quantity == that.quantity  &&  this.entries === that.entries  &&  this.mean === that.mean  &&  this.variance === that.variance
      case _ => false
    }
    override def hashCode() = (quantity, entries, mean, variance).hashCode
  }
}
