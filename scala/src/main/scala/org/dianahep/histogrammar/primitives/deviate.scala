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
    val detailedHelp = """Deviate(quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM])"""

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
      * @param selection Boolean or non-negative function that cuts or weights entries.
      */
    def apply[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = new Deviating(quantity, selection, 0.0, 0.0, 0.0)

    /** Synonym for `apply`. */
    def ing[DATUM](quantity: NumericalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM]) = apply(quantity, selection)

    /** Use [[org.dianahep.histogrammar.Deviated]] in Scala pattern-matching. */
    def unapply(x: Deviated) = Some((x.entries, x.mean, x.variance))
    /** Use [[org.dianahep.histogrammar.Deviating]] in Scala pattern-matching. */
    def unapply[DATUM](x: Deviating[DATUM]) = Some((x.entries, x.mean, x.variance))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("entries", "mean", "variance")) =>
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
    * @param entries Weighted number of entries (sum of all weights).
    * @param mean Weighted mean of the quantity.
    * @param variance Weighted variance of the quantity.
    * 
    * The implementation of this container uses a numerically stable variance as described by Tony Finch in [[http://www-uxsup.csx.cam.ac.uk/~fanf2/hermes/doc/antiforgery/stats.pdf "Incremental calculation of weighted mean and variance,"]] ''Univeristy of Cambridge Computing Service,'' 2009.
    */
  class Deviated(val entries: Double, val mean: Double, val variance: Double) extends Container[Deviated] {
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
    * @param quantity Numerical function to track.
    * @param selection Boolean or non-negative function that cuts or weights entries.
    * @param entries Weighted number of entries (sum of all weights).
    * @param mean Weighted mean of the quantity.
    * @param _variance Weighted variance of the quantity.
    * 
    * The implementation of this container uses a numerically stable variance as described by Tony Finch in [[http://www-uxsup.csx.cam.ac.uk/~fanf2/hermes/doc/antiforgery/stats.pdf "Incremental calculation of weighted mean and variance,"]] ''Univeristy of Cambridge Computing Service,'' 2009.
    */
  class Deviating[DATUM](val quantity: NumericalFcn[DATUM], val selection: Selection[DATUM], var entries: Double, var mean: Double, _variance: Double) extends Container[Deviating[DATUM]] with AggregationOnData {
    type Type = Deviating[DATUM]
    type Datum = DATUM
    def factory = Deviate

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    private var varianceTimesTotalWeight = entries * _variance

    /** weighted variance of the quantity */
    def variance =
      if (entries == 0.0)
        _variance
      else
        varianceTimesTotalWeight / entries

    def variance_=(_variance: Double) {
      varianceTimesTotalWeight = entries * _variance
    }

    def zero = new Deviating[DATUM](quantity, selection, 0.0, 0.0, 0.0)
    def +(that: Deviating[DATUM]) = {
      val (newentries, newmean, newvariance) = Deviate.plus(this.entries, this.mean, this.variance * this.entries,
                                                            that.entries, that.mean, that.variance * that.entries)
      new Deviating[DATUM](this.quantity, this.selection, newentries, newmean, newvariance)
    }

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)

        entries += w
        val delta = q - mean
        val shift = delta * w / entries
        mean += shift
        varianceTimesTotalWeight += w * delta * (q - mean)   // old delta times new delta
      }
    }

    def toJsonFragment = JsonObject("entries" -> JsonFloat(entries), "mean" -> JsonFloat(mean), "variance" -> JsonFloat(variance))

    override def toString() = s"Deviating[$mean, $variance]"
    override def equals(that: Any) = that match {
      case that: Deviating[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.entries === that.entries  &&  this.mean === that.mean  &&  this.variance === that.variance
      case _ => false
    }
    override def hashCode() = (quantity, selection, entries, mean, variance).hashCode
  }
}
