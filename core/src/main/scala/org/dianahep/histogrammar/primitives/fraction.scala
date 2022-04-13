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
  //////////////////////////////////////////////////////////////// Fraction/Fractioned/Fractioning

  /** Accumulate two aggregators, one containing only entries that pass a given selection (numerator) and another that contains all entries (denominator).
    * 
    * The aggregator may be a simple [[org.dianahep.histogrammar.Count]] to measure the efficiency of a cut, a [[org.dianahep.histogrammar.Bin]] to plot a turn-on curve, or anything else to be tested with and without a cut.
    * 
    * As a side effect of NaN values returning false for any comparison, a NaN return value from the selection is treated as a failed cut (the denominator is filled but the numerator is not).
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Fractioning]] and immutable [[org.dianahep.histogrammar.Fractioned]] objects.
    */
  object Fraction extends Factory {
    val name = "Fraction"
    val help = "Accumulate two aggregators, one containing only entries that pass a given selection (numerator) and another that contains all entries (denominator)."
    val detailedHelp = """The aggregator may be a simple [[org.dianahep.histogrammar.Count]] to measure the efficiency of a cut, a [[org.dianahep.histogrammar.Bin]] to plot a turn-on curve, or anything else to be tested with and without a cut.

As a side effect of NaN values returning false for any comparison, a NaN return value from the selection is treated as a failed cut (the denominator is filled but the numerator is not)."""

    /** Create an immutable [[org.dianahep.histogrammar.Fractioned]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights without the cut applied).
      * @param numerator Container for data that passed the given selection.
      * @param denominator Container for all data, regardless of whether it passed the given selection.
      */
    def ed[V <: Container[V] with NoAggregation](entries: Double, numerator: V, denominator: V) = new Fractioned(entries, None, numerator, denominator)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Fractioning]].
      * 
      * @param selection Boolean or non-negative function that cuts or weights entries.
      * @param value Template used to create zero values (by calling this `value`'s `zero` method).
      */
    def apply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](quantity: UserFcn[DATUM, Double], value: => V = Count()) =
      new Fractioning(quantity, 0.0, value.zero, value.zero)

    /** Synonym for `apply`. */
    def ing[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](quantity: UserFcn[DATUM, Double], value: => V = Count()) =
      apply(quantity, value)

    /** Alternate constructor for [[org.dianahep.histogrammar.Fractioned]] that builds from a pre-aggregated numerator and denominator.
      * 
      * This could be used for any bin-by-bin ratio (or even a difference or other reduction), such as a data/Monte Caro ratio. The purpose of binding the histograms together like this is to verify that they have compatible bins and to provide access to existing methods for creating ratio plots from Fractioned objects.
      */
    def build[N <: Container[N], D <: Container[D]](numerator: N, denominator: D): Fractioned[N, D] = {
      // check for compatibility
      val d2 = denominator.asInstanceOf[N]
      numerator + d2
      // return object
      new Fractioned(denominator.entries, None, numerator, denominator)
    }

    import KeySetComparisons._
    def fromJsonFragment(json: Json, nameFromParent: Option[String]): Container[_] with NoAggregation = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("entries", "sub:type", "numerator", "denominator").maybe("name").maybe("sub:name")) =>
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

        val factory = get("sub:type") match {
          case JsonString(name) => Factory(name)
          case x => throw new JsonFormatException(x, name + ".sub:type")
        }

        val subName = get.getOrElse("sub:name", JsonNull) match {
          case JsonString(x) => Some(x)
          case JsonNull => None
          case x => throw new JsonFormatException(x, name + ".sub:name")
        }

        val numerator = factory.fromJsonFragment(get("numerator"), subName)
        val denominator = factory.fromJsonFragment(get("denominator"), subName)

        new Fractioned(entries, (nameFromParent ++ quantityName).lastOption, numerator.asInstanceOf[C forSome {type C <: Container[C] with NoAggregation}], denominator.asInstanceOf[C forSome {type C <: Container[C] with NoAggregation}])

      case _ => throw new JsonFormatException(json, name)
    }

    // // Confidence interval formulas for fractions (e.g. bin-by-bin).
    // // For more information, see https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval
    // // and maybe R's binom package.
    // // http://www.sigmazone.com/binomial_confidence_interval.htm
    // // http://www.mwsug.org/proceedings/2008/pharma/MWSUG-2008-P08.pdf
    // // "Clopper-Pearson" == "exact"
    // // ATLAS http://www.pp.rhul.ac.uk/~cowan/atlas/ErrorBars.pdf

    // /** Compute the Wald (normal) confidence interval on the ratio of `numerator` over `denominator` (where `numerator` is a subset of `denominator`.
    //   * 
    //   * The Wald confidence interval is `p + z * sqrt(p * (1.0 - p) / denominator)` where `p = numerator / denominator`. It is symmetric in `z` and (unfortunately) limits to zero as `p` approaches zero or one. See `wilsonConfidenceInterval` for a better estimate at the extremes.
    //   * 
    //   * @param z Informally, the "number of sigmas." Formally, `z` is the `1 - alpha/2` quantile of the standard normal distribution, where `alpha` is the error quantile. For example, for a 95% confidence level, the error (`alpha`) is 5%, so z = 1.96. Note that `z` is signed. `z = 0` yields the central value, `z = -1` yields "one sigma" below the central value, and `z = 1` yields "one sigma" above the central value.
    //   */
    // def normalConfidenceInterval(numerator: Double, denominator: Double, z: Double): Double = normalConfidenceInterval(numerator, denominator, List(z): _*).head

    // /** Compute the normal confidence interval on the ratio of `numerator` over `denominator` (where `numerator` is a subset of `denominator`.
    //   * 
    //   * The normal confidence interval is `p + z * sqrt(p * (1.0 - p) / denominator)` where `p = numerator / denominator`. It is symmetric in `z` and (unfortunately) limits to zero as `p` approaches zero or one. See `wilsonConfidenceInterval` for a better estimate at the extremes.
    //   * 
    //   * @param z Informally, the "number of sigmas." Formally, `z` is the `1 - alpha/2` quantile of the standard normal distribution, where `alpha` is the error quantile. For example, for a 95% confidence level, the error (`alpha`) is 5%, so z = 1.96. Note that `z` is signed. `z = 0` yields the central value, `z = -1` yields "one sigma" below the central value, and `z = 1` yields "one sigma" above the central value.
    //   */
    // def normalConfidenceInterval(numerator: Double, denominator: Double, zs: Double*): Seq[Double] = {
    //   val p = numerator / denominator
    //   zs.map(p + _ * Math.sqrt(p * (1.0 - p) / denominator))
    // }

    // /** Compute the Wilson confidence interval on the ratio of `numerator` over `denominator` (where `numerator` is a subset of `denominator`.
    //   * 
    //   * The Wilson confidence interval is `(p + z**2/(2*n) + z*sqrt(p * (1.0 - p) / n + z**2/(4*n**2))) / (1.0 + z**2/n)` where `p = numerator / denominator` and `n = denominator`. It has good properties even for a small denominator and/or an extreme probability.
    //   * 
    //   * @param z Informally, the "number of sigmas." Formally, `z` is the `1 - alpha/2` quantile of the standard normal distribution, where `alpha` is the error quantile. For example, for a 95% confidence level, the error (`alpha`) is 5%, so z = 1.96. Note that `z` is signed. `z = 0` yields the central value, `z = -1` yields "one sigma" below the central value, and `z = 1` yields "one sigma" above the central value.
    //   */
    // def wilsonConfidenceInterval(numerator: Double, denominator: Double, z: Double): Double = wilsonConfidenceInterval(numerator, denominator, List(z): _*).head

    // /** Compute the Wilson confidence interval on the ratio of `numerator` over `denominator` (where `numerator` is a subset of `denominator`.
    //   * 
    //   * The Wilson confidence interval is `(p + z**2/(2*n) + z*sqrt(p * (1.0 - p) / n + z**2/(4*n**2))) / (1.0 + z**2/n)` where `p = numerator / denominator` and `n = denominator`. It has good properties even for a small denominator and/or an extreme probability.
    //   * 
    //   * @param z Informally, the "number of sigmas." Formally, `z` is the `1 - alpha/2` quantile of the standard normal distribution, where `alpha` is the error quantile. For example, for a 95% confidence level, the error (`alpha`) is 5%, so z = 1.96. Note that `z` is signed. `z = 0` yields the central value, `z = -1` yields "one sigma" below the central value, and `z = 1` yields "one sigma" above the central value.
    //   */
    // def wilsonConfidenceInterval(numerator: Double, denominator: Double, zs: Double*): Seq[Double] = {
    //   val p = numerator / denominator
    //   val n = denominator
    //   zs.map(z => (p + z*z/(2.0*n) + z*Math.sqrt(p * (1.0 - p) / n + z*z/(4.0*n*n))) / (1.0 + z*z/n))
    // }

    // def jeffreysConfidenceInterval = ???
    // def clopperPearsonConfidenceInterval = ???
    // def agrestiCoullConfidenceInterval = ???
  }

  /** An accumulated pair of containers, one with all data (denominator), and one with data that passed a given selection (numerator).
    * 
    * Use the factory [[org.dianahep.histogrammar.Fraction]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights without the cut applied).
    * @param quantityName Optional name given to the selection function, passed for bookkeeping.
    * @param numerator Container for data that passed the given selection.
    * @param denominator Container for all data, regardless of whether it passed the given selection.
    */
  class Fractioned[N <: Container[N], D <: Container[D]] private[histogrammar](val entries: Double, val quantityName: Option[String], val numerator: N, val denominator: D) extends Container[Fractioned[N, D]] with NoAggregation with QuantityName with Select.Methods {
    // NOTE: The type bounds ought to be N <: Container[N] with NoAggregation, but this constraint has
    //       been relaxed to allow the alternate constructor. The standard constructor applies this
    //       constraint, so normal Fractioned objects will have the correct types. HOWEVER, this class
    //       no longer "knows" that. I am not sure if this lack of knowledge will ever become a problem.

    type Type = Fractioned[N, D]
    type EdType = Fractioned[N, D]
    def factory = Fraction

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def fractionPassing = numerator.entries / entries

    def zero = new Fractioned[N, D](0.0, quantityName, numerator.zero, denominator.zero)
    def +(that: Fractioned[N, D]) =
      if (this.quantityName != that.quantityName)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantityName differs (${this.quantityName} vs ${that.quantityName})")
      else
        new Fractioned(this.entries + that.entries, this.quantityName, this.numerator + that.numerator, this.denominator + that.denominator)
    def *(factor: Double) =
      if (factor.isNaN  ||  factor <= 0.0)
        zero
      else
        new Fractioned[N, D](factor * entries, quantityName, numerator * factor, denominator * factor)

    def children = numerator :: denominator :: Nil

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "sub:type" -> JsonString(numerator.factory.name),
      "numerator" -> numerator.toJsonFragment(true),
      "denominator" -> denominator.toJsonFragment(true)).
      maybe(JsonString("name") -> (if (suppressName) None else quantityName.map(JsonString(_)))).
      maybe(JsonString("sub:name") -> (numerator match {case x: QuantityName => x.quantityName.map(JsonString(_)); case _ => None}))

    override def toString() = s"""<Fractioned values=${numerator.factory.name}>"""
    override def equals(that: Any) = that match {
      case that: Fractioned[N, D] => this.entries === that.entries  &&  this.quantityName == that.quantityName  &&  this.numerator == that.numerator  &&  this.denominator == that.denominator
      case _ => false
    }
    override def hashCode() = (entries, quantityName, numerator, denominator).hashCode
  }

  /** Accumulating a pair of containers, one with all data (denominator), and one with data that passed a given selection (numerator).
    * 
    * Use the factory [[org.dianahep.histogrammar.Fraction]] to construct an instance.
    * 
    * @param quantity Boolean or non-negative function that cuts or weights entries.
    * @param entries Weighted number of entries (sum of all observed weights without the cut applied).
    * @param numerator Container for data that passed the given selection.
    * @param denominator Container for all data, regardless of whether it passed the given selection.
    */
  class Fractioning[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}] private[histogrammar](val quantity: UserFcn[DATUM, Double], var entries: Double, val numerator: V, val denominator: V) extends Container[Fractioning[DATUM, V]] with AggregationOnData with NumericalQuantity[DATUM] with Select.Methods {
    type Type = Fractioning[DATUM, V]
    type EdType = Fractioned[numerator.EdType, numerator.EdType]
    type Datum = DATUM
    def factory = Fraction

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def fractionPassing = numerator.entries / entries

    def zero = new Fractioning[DATUM, V](quantity, 0.0, numerator.zero, denominator.zero)
    def +(that: Fractioning[DATUM, V]) =
      if (this.quantity.name != that.quantity.name)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantity name differs (${this.quantity.name} vs ${that.quantity.name})")
      else
        new Fractioning(this.quantity, this.entries + that.entries, this.numerator + that.numerator, this.denominator + that.denominator)
    def *(factor: Double) =
      if (factor.isNaN  ||  factor <= 0.0)
        zero
      else
        new Fractioning[DATUM, V](quantity, factor * entries, numerator * factor, denominator * factor)

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0): Unit = {
      checkForCrossReferences()
      if (weight > 0.0) {
        val w = weight * quantity(datum)

        denominator.fill(datum, weight)
        if (w > 0.0)
          numerator.fill(datum, w)

        // no possibility of exception from here on out (for rollback)
        entries += weight
      }
    }

    def children = numerator :: denominator :: Nil

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "sub:type" -> JsonString(numerator.factory.name),
      "numerator" -> numerator.toJsonFragment(true),
      "denominator" -> denominator.toJsonFragment(true)).
      maybe(JsonString("name") -> (if (suppressName) None else quantity.name.map(JsonString(_)))).
      maybe(JsonString("sub:name") -> (numerator match {case x: AnyQuantity[_, _] => x.quantity.name.map(JsonString(_)); case _ => None}))

    override def toString() = s"""<Fractioning values=${numerator.factory.name}>"""
    override def equals(that: Any) = that match {
      case that: Fractioning[DATUM, V] => this.quantity == that.quantity  &&  this.entries === that.entries  &&  this.numerator == that.numerator  &&  this.denominator == that.denominator
      case _ => false
    }
    override def hashCode() = (quantity, entries, numerator, denominator).hashCode
  }
}
