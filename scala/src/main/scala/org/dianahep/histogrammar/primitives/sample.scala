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

import scala.util.Random

import org.dianahep.histogrammar.json._
import org.dianahep.histogrammar.util._

package histogrammar {
  //////////////////////////////////////////////////////////////// Sample

  /** Accumulate raw numbers, vectors of numbers, or strings, randomly replacing them with Reservoir Sampling when the number of values exceeds a limit.

Sample collects raw values without attempting to group them by distinct value (as [[org.dianahep.histogrammar.Bag]] does), up to a given maximum ''number'' of entries (unlike [[org.dianahep.histogrammar.Limit]], which rolls over at a given total weight). The reason for the limit on Sample is purely to conserve memory.

The maximum number of entries and the data type together determine the size of the working set. If new values are added after this set is full, individual values will be randomly chosen for replacement. The probability of replacement is proportional to an entry's weight and it decreases with time, such that the final sample is a representative subset of all observed values, without preference for early values or late values.

This algorithm is known as weighted Reservoir Sampling, and it is non-deterministic. Each evaluation will likely result in a different final set.

Specifically, the algorithm implemented here was described in Pavlos S. Efraimidis and Paul G. Spirakis, [[http://www.sciencedirect.com/science/article/pii/S002001900500298X "Weighted random sampling with a reservoir,"]] ''Information Processing Letters 97 (5): 181–185, 2005'' (doi:10.1016/j.ipl.2005.11.003).

Although the user-defined function may return scalar numbers, fixed-dimension vectors of numbers, or categorical strings, it may not mix types. Different Sample primitives in an analysis tree may collect different types.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Sampling]] and immutable [[org.dianahep.histogrammar.Sampled]] objects.
    */
  object Sample extends Factory {
    val name = "Sample"
    val help = "Accumulate raw numbers, vectors of numbers, or strings, randomly replacing them with Reservoir Sampling when the number of values exceeds a limit."
    val detailedHelp = """Sample collects raw values without attempting to group them by distinct value (as [[org.dianahep.histogrammar.Bag]] does), up to a given maximum ''number'' of entries (unlike [[org.dianahep.histogrammar.Limit]], which rolls over at a given total weight). The reason for the limit on Sample is purely to conserve memory.

The maximum number of entries and the data type together determine the size of the working set. If new values are added after this set is full, individual values will be randomly chosen for replacement. The probability of replacement is proportional to an entry's weight and it decreases with time, such that the final sample is a representative subset of all observed values, without preference for early values or late values.

This algorithm is known as weighted Reservoir Sampling, and it is non-deterministic. Each evaluation will likely result in a different final set.

Specifically, the algorithm implemented here was described in Pavlos S. Efraimidis and Paul G. Spirakis, [[http://www.sciencedirect.com/science/article/pii/S002001900500298X "Weighted random sampling with a reservoir,"]] ''Information Processing Letters 97 (5): 181–185, 2005'' (doi:10.1016/j.ipl.2005.11.003).

Although the user-defined function may return scalar numbers, fixed-dimension vectors of numbers, or categorical strings, it may not mix types. Different Sample primitives in an analysis tree may collect different types."""

    /** Create an immutable [[org.dianahep.histogrammar.Sampled]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param limit Maximum number of data points in the sample.
      * @param values Distinct multidimensional vectors and their weights, sampled from the observed distribution.
      * @param randomSeed Provide a fixed seed for deterministic operation; None for random.
      */
    def ed[RANGE](entries: Double, limit: Int, values: Seq[(RANGE, Double)], randomSeed: Option[Long]) = new Sampled(entries, None, limit, values, randomSeed.map(new Random(_)))

    /** Create an empty, mutable [[org.dianahep.histogrammar.Sampling]].
      * 
      * @param limit Maximum number of data points in the sample.
      * @param quantity Function that produces numbers, vectors of numbers, or strings.
      * @param randomSeed Provide a fixed seed for deterministic operation; None for random.
      */
    def apply[DATUM, RANGE](limit: Int, quantity: UserFcn[DATUM, RANGE], randomSeed: Option[Long] = None) =
      new Sampling[DATUM, RANGE](quantity, 0.0, new mutable.Reservoir[RANGE](limit), randomSeed.map(new Random(_)))

    /** Synonym for `apply`. */
    def ing[DATUM, RANGE](limit: Int, quantity: UserFcn[DATUM, RANGE], randomSeed: Option[Long] = None) = apply(limit, quantity, randomSeed)

    /** Use [[org.dianahep.histogrammar.Sampled]] in Scala pattern-matching. */
    def unapply[RANGE](x: Sampled[RANGE]) = x.values
    /** Use [[org.dianahep.histogrammar.Sampling]] in Scala pattern-matching. */
    def unapply[DATUM, RANGE](x: Sampling[DATUM, RANGE]) = x.values

    import KeySetComparisons._
    def fromJsonFragment(json: Json, nameFromParent: Option[String]): Container[_] with NoAggregation = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("entries", "limit", "values").maybe("name").maybe("seed")) =>
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

        val limit = get("limit") match {
          case JsonInt(x) => x
          case x => throw new JsonFormatException(x, name + ".limit")
        }

        val values = get("values") match {
          case JsonArray(elems @ _*) => Seq[(Any, Double)](elems.zipWithIndex map {
            case (JsonObject(wv @ _*), i) if (wv.keySet has Set("w", "v")) =>
              val wvget = wv.toMap

              val n = wvget("w") match {
                case JsonNumber(x) => x
                case x => throw new JsonFormatException(x, name + s".values $i n")
              }

              val v: Any = wvget("v") match {
                case JsonString(x) => x
                case JsonNumber(x) => x
                case JsonArray(d @ _*) => Vector(d.zipWithIndex map {
                  case (JsonNumber(x), j) => x
                  case (x, j) => throw new JsonFormatException(x, name + s".values $i v $j")
                }: _*)
                case x => throw new JsonFormatException(x, name + s".values $i v")
              }

              (v -> n)

            case (x, i) => throw new JsonFormatException(x, name + s".values $i")
          }: _*)
          case x => throw new JsonFormatException(x, name + ".values")
        }

        val seed = get.getOrElse("seed", JsonNull) match {
          case JsonInt(x) => Some(x)
          case JsonNull => None
          case x => throw new JsonFormatException(x, name + ".seed")
        }

        new Sampled[Any](entries, (nameFromParent ++ quantityName).lastOption, limit.toInt, values, seed.map(new Random(_)))

      case _ => throw new JsonFormatException(json, name)
    }
  }

  /** An accumulated sample of numbers, vectors of numbers, or strings.
    * 
    * Use the factory [[org.dianahep.histogrammar.Sample]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param quantityName Optional name given to the quantity function, passed for bookkeeping.
    * @param limit Maximum number of data points in the sample.
    * @param randomGenerator Provide a generator for deterministic operation; None for random.
    * @param values Distinct multidimensional vectors and their weights, sampled from the observed distribution.
    */
  class Sampled[RANGE] private[histogrammar](val entries: Double, val quantityName: Option[String], val limit: Int, val values: Seq[(RANGE, Double)], val randomGenerator: Option[Random]) extends Container[Sampled[RANGE]] with NoAggregation with QuantityName {
    type Type = Sampled[RANGE]
    type EdType = Sampled[RANGE]
    def factory = Sample

    if (limit <= 0)
      throw new ContainerException(s"limit ($limit) must be positive")
    
    /** Number of data points in the sample (saturates at `limit`). */
    def size = values.size
    /** Determine if the sample is empty. */
    def isEmpty = values.isEmpty

    def zero = new Sampled(0.0, quantityName, limit, Seq[(RANGE, Double)](), randomGenerator.map(x => new Random(x.nextLong)))
    def +(that: Sampled[RANGE]) = {
      if (this.limit != that.limit)
        throw new ContainerException(s"cannot add ${getClass.getName} because limit differs (${this.limit} vs ${that.limit})")
      if (this.quantityName != that.quantityName)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantityName differs (${this.quantityName} vs ${that.quantityName})")

      val newGenerator = (this.randomGenerator, that.randomGenerator) match {
        case (Some(x), Some(y)) => Some(new Random(x.nextLong + y.nextLong))
        case (Some(x), None) => Some(new Random(x.nextLong))
        case (None, Some(y)) => Some(new Random(y.nextLong))
        case (None, None) => None
      }

      val reservoir = new mutable.Reservoir[RANGE](limit, values: _*)
      that.values foreach {case (y, weight) => reservoir.update(y, weight, newGenerator)}

      new Sampled[RANGE](this.entries + that.entries, this.quantityName, this.limit, reservoir.values, newGenerator)
    }

    def children = Nil

    def toJsonFragment(suppressName: Boolean) = {
      implicit val rangeOrdering = Bag.rangeOrdering[RANGE]
      JsonObject(
        "entries" -> JsonFloat(entries),
        "limit" -> JsonInt(limit),
        "values" -> JsonArray(values.toSeq.sortBy(_._1).map({
          case (v: String, n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonString(v))
          case (v: Double, n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonFloat(v))
          case (v: Vector[_], n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonArray(v.map({case vi: Double => JsonFloat(vi)}): _*))
        }): _*)).
      maybe(JsonString("name") -> (if (suppressName) None else quantityName.map(JsonString(_)))).
      maybe(JsonString("seed") -> randomGenerator.map(_.nextLong))
    }

    override def toString() = s"""<Sampled size=$size>"""
    override def equals(that: Any) = that match {
      case that: Sampled[RANGE] => this.entries === that.entries  &&  this.quantityName == that.quantityName  &&  this.limit == that.limit  &&  this.values == that.values  &&  this.randomGenerator.isEmpty == that.randomGenerator.isEmpty
      case _ => false
    }
    override def hashCode() = (entries, quantityName, limit, values, randomGenerator.isEmpty).hashCode()
  }

  /** An accumulated sample of numbers, vectors of numbers, or strings.
    * 
    * Use the factory [[org.dianahep.histogrammar.Sample]] to construct an instance.
    * 
    * @param quantity Function that produces numbers, vectors of numbers, or strings.
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param reservoir Data structure to perform weighted reservoir sampling.
    * @param randomGenerator Provide a generator for deterministic operation; None for random.
    */
  class Sampling[DATUM, RANGE] private[histogrammar](val quantity: UserFcn[DATUM, RANGE], var entries: Double, reservoir: mutable.Reservoir[RANGE], val randomGenerator: Option[Random]) extends Container[Sampling[DATUM, RANGE]] with AggregationOnData with AnyQuantity[DATUM, RANGE] {
    type Type = Sampling[DATUM, RANGE]
    type EdType = Sampled[RANGE]
    type Datum = DATUM
    def factory = Sample

    if (limit <= 0)
      throw new ContainerException(s"limit ($limit) must be positive")

    /** Maximum number of data points in the sample. */
    def limit = reservoir.limit
    /** Distinct multidimensional vectors and their weights, sampled from the observed distribution. */
    def values = reservoir.values
    /** Number of data points in the sample (saturates at `limit`). */
    def size = reservoir.size
    /** Determine if the sample is empty. */
    def isEmpty = reservoir.isEmpty

    def zero = new Sampling(quantity, 0.0, new mutable.Reservoir[RANGE](limit), randomGenerator.map(x => new Random(x.nextLong)))
    def +(that: Sampling[DATUM, RANGE]) = {
      if (this.limit != that.limit)
        throw new ContainerException(s"cannot add ${getClass.getName} because limit differs (${this.limit} vs ${that.limit})")
      if (this.quantity.name != that.quantity.name)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantity name differs (${this.quantity.name} vs ${that.quantity.name})")

      val newGenerator = (this.randomGenerator, that.randomGenerator) match {
        case (Some(x), Some(y)) => Some(new Random(x.nextLong + y.nextLong))
        case (Some(x), None) => Some(new Random(x.nextLong))
        case (None, Some(y)) => Some(new Random(y.nextLong))
        case (None, None) => None
      }

      val newreservoir = new mutable.Reservoir[RANGE](this.limit, this.values: _*)
      that.values foreach {case (y, weight) => newreservoir.update(y, weight, newGenerator)}

      new Sampling[DATUM, RANGE](quantity, this.entries + that.entries, newreservoir, newGenerator)
    }

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      checkForCrossReferences()
      if (weight > 0.0) {
        val q = quantity(datum)

        reservoir.update(q, weight, randomGenerator)

        // no possibility of exception from here on out (for rollback)
        entries += weight
      }
    }

    def children = Nil

    def toJsonFragment(suppressName: Boolean) = {
      implicit val rangeOrdering = Bag.rangeOrdering[RANGE]
      JsonObject(
        "entries" -> JsonFloat(entries),
        "limit" -> JsonInt(limit),
        "values" -> JsonArray(values.toSeq.sortBy(_._1).map({
          case (v: String, n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonString(v))
          case (v: Double, n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonFloat(v))
          case (v: Vector[_], n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonArray(v.map({case vi: Double => JsonFloat(vi)}): _*))
        }): _*)).
        maybe(JsonString("name") -> (if (suppressName) None else quantity.name.map(JsonString(_)))).
        maybe(JsonString("seed") -> randomGenerator.map(_.nextLong))
    }

    override def toString() = s"""<Sampling size=$size>"""
    override def equals(that: Any) = that match {
      case that: Sampling[DATUM, RANGE] => this.quantity == that.quantity  &&  this.entries === that.entries  &&  this.limit == that.limit  &&  this.values == that.values  &&  this.randomGenerator.isEmpty == that.randomGenerator.isEmpty
      case _ => false
    }
    override def hashCode() = (quantity, entries, limit, values, randomGenerator.isEmpty).hashCode()
  }
}
