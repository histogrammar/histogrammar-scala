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
  //////////////////////////////////////////////////////////////// Deviate/Deviated/Deviating

  /** Accumulate the weighted mean and weighted variance of a given quantity.
    * 
    * The variance is computed around the mean, not zero.
    * 
    * Uses the numerically stable weighted mean and weighted variance algorithms described in Tony Finch, [[http://www-uxsup.csx.cam.ac.uk/~fanf2/hermes/doc/antiforgery/stats.pdf "Incremental calculation of weighted mean and variance"]] ''Univeristy of Cambridge Computing Service, 2009''.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Deviating]] and immutable [[org.dianahep.histogrammar.Deviated]] objects.
    */
  object Deviate extends Factory {
    val name = "Deviate"
    val help = "Accumulate the weighted mean and weighted variance of a given quantity."
    val detailedHelp = """The variance is computed around the mean, not zero.

Uses the numerically stable weighted mean and weighted variance algorithms described in Tony Finch, [[http://www-uxsup.csx.cam.ac.uk/~fanf2/hermes/doc/antiforgery/stats.pdf "Incremental calculation of weighted mean and variance"]] ''Univeristy of Cambridge Computing Service, 2009''."""

    /** Create an immutable [[org.dianahep.histogrammar.Deviated]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param mean Weighted mean of the quantity.
      * @param variance Weighted variance of the quantity.
      */
    def ed(entries: Double, mean: Double, variance: Double) = new Deviated(entries, None, mean, variance)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Deviating]].
      * 
      * @param quantity Numerical function to track.
      */
    def apply[DATUM](quantity: UserFcn[DATUM, Double]) = new Deviating(quantity, 0.0, java.lang.Double.NaN, java.lang.Double.NaN)

    /** Synonym for `apply`. */
    def ing[DATUM](quantity: UserFcn[DATUM, Double]) = apply(quantity)

    /** Use [[org.dianahep.histogrammar.Deviated]] in Scala pattern-matching. */
    def unapply(x: Deviated) = Some(x.variance)
    /** Use [[org.dianahep.histogrammar.Deviating]] in Scala pattern-matching. */
    def unapply[DATUM](x: Deviating[DATUM]) = Some(x.variance)

    import KeySetComparisons._
    def fromJsonFragment(json: Json, nameFromParent: Option[String]): Container[_] with NoAggregation = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("entries", "mean", "variance").maybe("name")) =>
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

        val mean = get("mean") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".mean")
        }

        val variance = get("variance") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".variance")
        }

        new Deviated(entries, (nameFromParent ++ quantityName).lastOption, mean, variance)

      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def plus(ca: Double, mua: Double, sa: Double, cb: Double, mub: Double, sb: Double) =
      if (ca == 0.0)
        (cb, mub, sb / cb)
      else if (cb == 0.0)
        (ca, mua, sa / ca)
      else {
        val muab = (ca*mua + cb*mub)/(ca + cb)
        val sab = sa + sb + ca*mua*mua + cb*mub*mub - 2.0*muab*(ca*mua + cb*mub) + muab*muab*(ca + cb)
        (ca + cb, muab, sab / (ca + cb))
      }
  }

  /** An accumulated weighted variance (and mean) of a given quantity.
    * 
    * Use the factory [[org.dianahep.histogrammar.Deviate]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param quantityName Optional name given to the quantity function, passed for bookkeeping.
    * @param mean Weighted mean of the quantity.
    * @param variance Weighted variance of the quantity.
    * 
    * The implementation of this container uses a numerically stable variance as described by Tony Finch in [[http://www-uxsup.csx.cam.ac.uk/~fanf2/hermes/doc/antiforgery/stats.pdf "Incremental calculation of weighted mean and variance,"]] ''Univeristy of Cambridge Computing Service,'' 2009.
    */
  class Deviated private[histogrammar](val entries: Double, val quantityName: Option[String], val mean: Double, val variance: Double) extends Container[Deviated] with NoAggregation with QuantityName {
    type Type = Deviated
    type EdType = Deviated
    def factory = Deviate

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def zero = new Deviated(0.0, quantityName, java.lang.Double.NaN, java.lang.Double.NaN)
    def +(that: Deviated) =
      if (this.quantityName != that.quantityName)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantityName differs (${this.quantityName} vs ${that.quantityName})")
      else {
        val (newentries, newmean, newvariance) = Deviate.plus(this.entries, this.mean, this.variance * this.entries,
                                                              that.entries, that.mean, that.variance * that.entries)
        new Deviated(newentries, this.quantityName, newmean, newvariance)
      }
    def *(factor: Double) =
      if (factor.isNaN  ||  factor <= 0.0)
        zero
      else
        new Deviated(factor * entries, quantityName, mean, variance)

    def children = Nil

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "mean" -> JsonFloat(mean),
      "variance" -> JsonFloat(variance)).
      maybe(JsonString("name") -> (if (suppressName) None else quantityName.map(JsonString(_))))

    override def toString() = s"<Deviated mean=$mean, variance=$variance>"
    override def equals(that: Any) = that match {
      case that: Deviated => this.entries === that.entries  &&  this.quantityName == that.quantityName  &&  this.mean === that.mean  &&  this.variance === that.variance
      case _ => false
    }
    override def hashCode() = (entries, quantityName, mean, variance).hashCode
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
  class Deviating[DATUM] private[histogrammar](val quantity: UserFcn[DATUM, Double], var entries: Double, var mean: Double, _variance: Double) extends Container[Deviating[DATUM]] with AggregationOnData with NumericalQuantity[DATUM] {
    type Type = Deviating[DATUM]
    type EdType = Deviated
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

    def variance_=(_variance: Double): Unit = {
      varianceTimesEntries = entries * _variance
    }

    def zero = new Deviating[DATUM](quantity, 0.0, java.lang.Double.NaN, java.lang.Double.NaN)
    def +(that: Deviating[DATUM]) =
      if (this.quantity.name != that.quantity.name)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantity name differs (${this.quantity.name} vs ${that.quantity.name})")
      else {
        val (newentries, newmean, newvariance) = Deviate.plus(this.entries, this.mean, this.variance * this.entries,
                                                              that.entries, that.mean, that.variance * that.entries)
        new Deviating[DATUM](this.quantity, newentries, newmean, newvariance)
      }
    def *(factor: Double) =
      if (factor.isNaN  ||  factor <= 0.0)
        zero
      else
        new Deviating[DATUM](quantity, factor * entries, mean, variance)

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0): Unit = {
      checkForCrossReferences()
      if (weight > 0.0) {
        val q = quantity(datum)

        // no possibility of exception from here on out (for rollback)
        if (entries == 0.0) {
          mean = q
          varianceTimesEntries = 0.0
        }
        entries += weight

        if (mean.isNaN  ||  q.isNaN) {
          mean = java.lang.Double.NaN
          varianceTimesEntries = java.lang.Double.NaN
        }

        else if (mean.isInfinite  ||  q.isInfinite) {
          if (mean.isInfinite  &&  q.isInfinite  &&  mean * q < 0.0)
            mean = java.lang.Double.NaN
          else if (q.isInfinite)
            mean = q
          else
            { }
          if (entries.isInfinite  ||  entries.isNaN)
            mean = java.lang.Double.NaN

          varianceTimesEntries = java.lang.Double.NaN
        }

        else {
          val delta = q - mean
          val shift = delta * weight / entries
          mean += shift
          varianceTimesEntries += weight * delta * (q - mean)   // old delta times new delta
        }
      }
    }

    def children = Nil

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "mean" -> JsonFloat(mean),
      "variance" -> JsonFloat(variance)).
      maybe(JsonString("name") -> (if (suppressName) None else quantity.name.map(JsonString(_))))

    override def toString() = s"<Deviating mean=$mean, variance=$variance>"
    override def equals(that: Any) = that match {
      case that: Deviating[DATUM] => this.quantity == that.quantity  &&  this.entries === that.entries  &&  this.mean === that.mean  &&  this.variance === that.variance
      case _ => false
    }
    override def hashCode() = (quantity, entries, mean, variance).hashCode
  }
}
