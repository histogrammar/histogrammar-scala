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

import scala.reflect.classTag
import scala.reflect.ClassTag

import org.dianahep.histogrammar.json._
import org.dianahep.histogrammar.util._

package histogrammar {
  //////////////////////////////////////////////////////////////// Bag/Bagged/Bagging

  /** Accumulate raw numbers, vectors of numbers, or strings, with identical values merged.
    * 
    * A bag is the appropriate data type for scatter plots: a container that collects raw values, maintaining multiplicity but not order. (A "bag" is also known as a "multiset.") Conceptually, it is a mapping from distinct raw values to the number of observations: when two instances of the same raw value are observed, one key is stored and their weights add.
    * 
    * Although the user-defined function may return scalar numbers, fixed-dimension vectors of numbers, or categorical strings, it may not mix range types. For the purposes of Label and Index (which can only collect aggregators of a single type), bags with different ranges are different types.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Bagging]] and immutable [[org.dianahep.histogrammar.Bagged]] objects.
    */
  object Bag extends Factory {
    val name = "Bag"
    val help = "Accumulate raw numbers, vectors of numbers, or strings, with identical values merged."
    val detailedHelp = """A bag is the appropriate data type for scatter plots: a container that collects raw values, maintaining multiplicity but not order. (A "bag" is also known as a "multiset.") Conceptually, it is a mapping from distinct raw values to the number of observations: when two instances of the same raw value are observed, one key is stored and their weights add.

    Although the user-defined function may return scalar numbers, fixed-dimension vectors of numbers, or categorical strings, it may not mix range types. For the purposes of Label and Index (which can only collect aggregators of a single type), bags with different ranges are different types."""

    /** Create an immutable [[org.dianahep.histogrammar.Bagged]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param values Distinct multidimensional vectors and the (weighted) number of times they were observed or `None` if they were dropped.
      * @param range The data type: "N" for number, "N#" where "#" is a positive integer for vector of numbers, or "S" for string.
      */
    def ed[RANGE](entries: Double, values: Map[RANGE, Double], range: String) = new Bagged[RANGE](entries, None, values, range)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Bagging]].
      * 
      * @param quantity Function that produces numbers, vectors of numbers, or strings.
      * @param range The data type: "N" for number, "N#" where "#" is a positive integer for vector of numbers, or "S" for string.
      */
    def apply[DATUM, RANGE : ClassTag](quantity: UserFcn[DATUM, RANGE], range: String = "") = {
      val r =
        if (range == "") {
          val t = implicitly[ClassTag[RANGE]]
          if (t == classTag[Double])
            "N"
          else if (classTag[String].runtimeClass.isAssignableFrom(t.runtimeClass))
            "S"
          else
            throw new ContainerException("cannot infer Bag type without an explicit 'range' parameter because it is not Double or String")
        }
        else
          range
      new Bagging[DATUM, RANGE](quantity, 0.0, scala.collection.mutable.Map[RANGE, Double](), r)
    }

    /** Synonym for `apply`. */
    def ing[DATUM, RANGE : ClassTag](quantity: UserFcn[DATUM, RANGE], range: String = "") = apply(quantity, range)

    /** Use [[org.dianahep.histogrammar.Bagged]] in Scala pattern-matching. */
    def unapply[RANGE](x: Bagged[RANGE]) = x.values
    /** Use [[org.dianahep.histogrammar.Bagging]] in Scala pattern-matching. */
    def unapply[DATUM, RANGE](x: Bagging[DATUM, RANGE]) = x.values

    import KeySetComparisons._
    def fromJsonFragment(json: Json, nameFromParent: Option[String]): Container[_] with NoAggregation = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("entries", "values", "range").maybe("name")) =>
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

        val values = get("values") match {
          case JsonArray(elems @ _*) => Map[Any, Double](elems.zipWithIndex map {
            case (JsonObject(nv @ _*), i) if (nv.keySet has Set("w", "v")) =>
              val nvget = nv.toMap

              val n = nvget("w") match {
                case JsonNumber(x) => x
                case x => throw new JsonFormatException(x, name + s".values $i n")
              }

              val v: Any = nvget("v") match {
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

        val range = get("range") match {
          case JsonString(x) if (x == "N") => "N"
          case JsonString(x) if (x == "S") => "S"
          case JsonString(x) if (!("^N([0-9]+)$".r.findFirstIn(x).isEmpty)) => x
          case x => throw new JsonFormatException(x, name + ".range")
        }

        new Bagged[Any](entries, (nameFromParent ++ quantityName).lastOption, values, range)

      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def rangeOrdering[RANGE] = new Ordering[RANGE] {
      def compare(x: RANGE, y: RANGE) = (x, y) match {
        case (xx: String, yy: String) => xx compare yy
        case (xx: Double, yy: Double) => xx compare yy
        case (xx: Vector[_], yy: Vector[_]) if (!xx.isEmpty  &&  !yy.isEmpty  &&  xx.head == yy.head) => compare(xx.tail.asInstanceOf[RANGE], yy.tail.asInstanceOf[RANGE])
        case (xx: Vector[_], yy: Vector[_]) if (!xx.isEmpty  &&  !yy.isEmpty) => xx.head.asInstanceOf[Double] compare yy.head.asInstanceOf[Double]
        case (xx: Vector[_], yy: Vector[_]) if (xx.isEmpty  &&  yy.isEmpty) => 0
        case (xx: Vector[_], yy: Vector[_]) if (xx.isEmpty) => -1
        case (xx: Vector[_], yy: Vector[_]) if (yy.isEmpty) => 1
      }
    }
  }

  /** An accumulated bag of numbers, vectors of numbers, or strings.
    * 
    * Use the factory [[org.dianahep.histogrammar.Bag]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param quantityName Optional name given to the quantity function, passed for bookkeeping.
    * @param values Distinct values and the (weighted) number of times they were observed.
    * @param range The data type: "N" for number, "N#" where "#" is a positive integer for vector of numbers, or "S" for string.
    */
  class Bagged[RANGE] private[histogrammar](val entries: Double, val quantityName: Option[String], val values: Map[RANGE, Double], val range: String) extends Container[Bagged[RANGE]] with NoAggregation with QuantityName {
    type Type = Bagged[RANGE]
    type EdType = Bagged[RANGE]
    def factory = Bag

    def zero = new Bagged(0.0, this.quantityName, Map[RANGE, Double](), range)
    def +(that: Bagged[RANGE]) =
      if (this.quantityName != that.quantityName)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantityName differs (${this.quantityName} vs ${that.quantityName})")
      else if (this.range != that.range)
        throw new ContainerException(s"cannot add ${getClass.getName} because range differs (${this.range} vs ${that.range})")
      else {
        val newentries = this.entries + that.entries
        val newvalues = {
          val out = scala.collection.mutable.Map(this.values.toSeq: _*)
          that.values foreach {case (k, v) =>
            if (out contains k)
              out(k) += v
            else
              out(k) = v
          }
          out.toMap
        }

        new Bagged[RANGE](newentries, this.quantityName, newvalues, range)
      }

    def children = Nil

    def toJsonFragment(suppressName: Boolean) = {
      implicit val rangeOrdering = Bag.rangeOrdering[RANGE]
      JsonObject(
        "entries" -> JsonFloat(entries),
        "values" -> JsonArray(values.toSeq.sortBy(_._1).map({
          case (v: String, n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonString(v))
          case (v: Double, n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonFloat(v))
          case (v: Vector[_], n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonArray(v.map({case vi: Double => JsonFloat(vi)}): _*))
        }): _*),
        "range" -> JsonString(range)).maybe(JsonString("name") -> (if (suppressName) None else quantityName.map(JsonString(_))))
    }

    override def toString() = s"""<Bagged size=${values.size} range=${range}>"""
    override def equals(that: Any) = that match {
      case that: Bagged[RANGE] => this.entries === that.entries  &&  this.quantityName == that.quantityName  &&  this.values == that.values  &&  this.range == that.range
      case _ => false
    }
    override def hashCode() = (entries, quantityName, values, range).hashCode()
  }

  /** An accumulated bag of numbers, vectors of numbers, or strings.
    * 
    * Use the factory [[org.dianahep.histogrammar.Bag]] to construct an instance.
    * 
    * @param quantity Function that produces numbers, vectors of numbers, or strings.
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param values Distinct values and the (weighted) number of times they were observed.
    * @param range The data type: "N" for number, "N#" where "#" is a positive integer for vector of numbers, or "S" for string.
    */
  class Bagging[DATUM, RANGE] private[histogrammar](val quantity: UserFcn[DATUM, RANGE], var entries: Double, var values: scala.collection.mutable.Map[RANGE, Double], val range: String) extends Container[Bagging[DATUM, RANGE]] with AggregationOnData with AnyQuantity[DATUM, RANGE] {
    type Type = Bagging[DATUM, RANGE]
    type EdType = Bagged[RANGE]
    type Datum = DATUM
    def factory = Bag

    val dimension =
      if (range.head == 'N'  &&  range.size > 1)
        java.lang.Integer.parseInt(range.tail)
      else
        0

    def zero = new Bagging[DATUM, RANGE](quantity, 0.0, scala.collection.mutable.Map[RANGE, Double](), range)
    def +(that: Bagging[DATUM, RANGE]) =
      if (this.quantity.name != that.quantity.name)
        throw new ContainerException(s"cannot add ${getClass.getName} because quantity name differs (${this.quantity.name} vs ${that.quantity.name})")
      else if (this.range != that.range)
        throw new ContainerException(s"cannot add ${getClass.getName} because range differs (${this.range} vs ${that.range})")
      else {
        val newentries = this.entries + that.entries
        val newvalues = {
          val out = scala.collection.mutable.Map(this.values.toSeq: _*)
          that.values foreach {case (k, v) =>
            if (out contains k)
              out(k) += v
            else
              out(k) = v
          }
          out
        }

        new Bagging[DATUM, RANGE](quantity, newentries, newvalues, range)
      }

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      checkForCrossReferences()
      if (weight > 0.0) {
        val q = quantity(datum)

        if (dimension > 0) q match {
          case v: Vector[_] if (v.size == dimension) =>
          case _ => throw new ContainerException(s"Bag range declared as $range but encountered $q")
        }
        else if (range == "N") q match {
          case _: Double =>
          case _ => throw new ContainerException(s"Bag range declared as $range but encountered $q")
        }
        else q match {
          case _: String =>
          case _ => throw new ContainerException(s"Bag range declared as $range but encountered $q")
        }

        // no possibility of exception from here on out (for rollback)
        entries += weight
        if (values contains q)
          values(q) += weight
        else
          values(q) = weight
      }
    }

    def children = Nil

    def toJsonFragment(suppressName: Boolean) = {
      implicit val rangeOrdering = Bag.rangeOrdering[RANGE]
      JsonObject(
        "entries" -> JsonFloat(entries),
        "values" -> JsonArray(values.toSeq.sortBy(_._1).map({
          case (v: String, n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonString(v))
          case (v: Double, n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonFloat(v))
          case (v: Vector[_], n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonArray(v.map({case vi: Double => JsonFloat(vi)}): _*))
        }): _*),
        "range" -> JsonString(range)).maybe(JsonString("name") -> (if (suppressName) None else quantity.name.map(JsonString(_))))
    }

    override def toString() = s"""<Bagging size=${values.size} range=$range>"""
    override def equals(that: Any) = that match {
      case that: Bagging[DATUM, RANGE] => this.quantity == that.quantity  &&  this.entries === that.entries  &&  this.values == that.values  &&  this.range == that.range
      case _ => false
    }
    override def hashCode() = (quantity, entries, values, range).hashCode()
  }
}
