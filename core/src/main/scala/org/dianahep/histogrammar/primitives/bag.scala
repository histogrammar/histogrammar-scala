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
    def ed[RANGE](entries: Double, values: Map[RANGE, Double], range: String) = new Bagged[RANGE](entries, None, values map {case (v, n) => (toHandleNaN(v), n)}, range)

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
      new Bagging[DATUM, RANGE](quantity, 0.0, scala.collection.mutable.Map[HandleNaN[RANGE], Double](), r)
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
          case JsonArray(elems @ _*) => Map[Bag.HandleNaN[Any], Double](elems.zipWithIndex map {
            case (JsonObject(nv @ _*), i) if (nv.keySet has Set("w", "v")) =>
              val nvget = nv.toMap

              val n = nvget("w") match {
                case JsonNumber(x) => x
                case x => throw new JsonFormatException(x, name + s".values $i n")
              }

              val v: Bag.HandleNaN[Any] = nvget("v") match {
                case JsonString(x) => Bag.IgnoreNaN(x).asInstanceOf[Bag.HandleNaN[Any]]
                case JsonNumber(x) => Bag.DoubleNaN(x).asInstanceOf[Bag.HandleNaN[Any]]
                case JsonArray(d @ _*) => Bag.SeqNaN[Vector[Double]](d.zipWithIndex map {
                  case (JsonNumber(x), j) => x
                  case (x, j) => throw new JsonFormatException(x, name + s".values $i v $j")
                }: _*).asInstanceOf[Bag.HandleNaN[Any]]
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
          case JsonString(x) if (!("^N([1-9][0-9]*)$".r.findFirstIn(x).isEmpty)) => x
          case x => throw new JsonFormatException(x, name + ".range")
        }

        new Bagged[Any](entries, (nameFromParent ++ quantityName).lastOption, values, range)

      case _ => throw new JsonFormatException(json, name)
    }

    trait HandleNaN[RANGE] extends Ordered[HandleNaN[RANGE]] {
      override def equals(that: Any): Boolean
      def compare(that: HandleNaN[RANGE]): Int
    }

    case class IgnoreNaN(x: String) extends HandleNaN[String] {
      override def equals(that: Any) = that match {
        case that: IgnoreNaN => this.x == that.x
        case _ => false
      }
      def compare(that: HandleNaN[String]) = that match {
        case that: IgnoreNaN => this.x compare that.x
        case _ => throw new Exception
      }
    }

    case class DoubleNaN(x: Double) extends HandleNaN[Double] {
      override def equals(that: Any) = that match {
        case that: DoubleNaN => (this.x.isNaN  &&  that.x.isNaN)  ||  (this.x == that.x)
        case _ => false
      }
      def compare(that: HandleNaN[Double]) = that match {
        case that: DoubleNaN =>
          if (this.x.isNaN  &&  that.x.isNaN)
            0
          else if (this.x.isNaN  ||  this.x > that.x)
            1
          else if (that.x.isNaN  ||  this.x < that.x)
            -1
          else
            0
        case _ => throw new Exception
      }
    }

    case class SeqNaN[RANGE](components: Double*) extends HandleNaN[RANGE] with Seq[Double] {
      def apply(index: Int) = components(index)
      def iterator = components.iterator
      def length = components.length
      override def equals(that: Any) = that match {
        case that: SeqNaN[_] => this.length == that.length  &&  (this zip that).forall({case (x, y) => (x.isNaN  &&  y.isNaN)  ||  (x == y)})
        case _ => false
      }
      def compare(that: HandleNaN[RANGE]): Int = that match {
        case that: SeqNaN[_] =>
          var index = 0
          while (index < length) {
            if (this(index).isNaN  &&  that(index).isNaN)
            { }
            else if (this(index).isNaN  ||  this(index) > that(index))
              return 1
            else if (that(index).isNaN  ||  this(index) < that(index))
              return -1
            index += 1
          }
          0
        case _ => throw new Exception
      }
    }

    private def toHandleNaN[RANGE](x: RANGE): Bag.HandleNaN[RANGE] = x match {
      case v: String => Bag.IgnoreNaN(v).asInstanceOf[Bag.HandleNaN[RANGE]]
      case v: Double => Bag.DoubleNaN(v).asInstanceOf[Bag.HandleNaN[RANGE]]
      case v: Seq[_] => Bag.SeqNaN(v.map({
        case vi: Double => vi
        case vi: Float => vi.toDouble
        case vi: Long => vi.toDouble
        case vi: Int => vi.toDouble
        case vi: Short => vi.toDouble
        case vi: Byte => vi.toDouble
        case _ => throw new Exception
      }): _*)
      case _ => throw new Exception
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
  class Bagged[RANGE] private[histogrammar](val entries: Double, val quantityName: Option[String], val values: Map[Bag.HandleNaN[RANGE], Double], val range: String) extends Container[Bagged[RANGE]] with NoAggregation with QuantityName {
    type Type = Bagged[RANGE]
    type EdType = Bagged[RANGE]
    def factory = Bag

    def zero = new Bagged(0.0, this.quantityName, Map[Bag.HandleNaN[RANGE], Double](), range)
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

    def toJsonFragment(suppressName: Boolean) =
      JsonObject(
        "entries" -> JsonFloat(entries),
        "values" -> JsonArray(values.toSeq.sortBy(_._1).map({
          case (v: Bag.IgnoreNaN, n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonString(v.x))
          case (v: Bag.DoubleNaN, n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonFloat(v.x))
          case (v: Bag.SeqNaN[_], n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonArray(v.iterator.map({case vi: Double => JsonFloat(vi)}).toSeq: _*))
        }): _*),
        "range" -> JsonString(range)).maybe(JsonString("name") -> (if (suppressName) None else quantityName.map(JsonString(_))))

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
  class Bagging[DATUM, RANGE] private[histogrammar](val quantity: UserFcn[DATUM, RANGE], var entries: Double, var values: scala.collection.mutable.Map[Bag.HandleNaN[RANGE], Double], val range: String) extends Container[Bagging[DATUM, RANGE]] with AggregationOnData with AnyQuantity[DATUM, RANGE] {
    type Type = Bagging[DATUM, RANGE]
    type EdType = Bagged[RANGE]
    type Datum = DATUM
    def factory = Bag

    val dimension =
      if (range.head == 'N'  &&  range.size > 1) {
        val out = java.lang.Integer.parseInt(range.tail)
        if (out <= 0)
          throw new ContainerException(s"vector-valued range (${range}) must have positive dimension")
        out
      }
      else
        0

    def zero = new Bagging[DATUM, RANGE](quantity, 0.0, scala.collection.mutable.Map[Bag.HandleNaN[RANGE], Double](), range)
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

        val qnan: Bag.HandleNaN[RANGE] =
          if (dimension > 0) q match {
            case v: Seq[_] if (v.size == dimension) => Bag.SeqNaN(v.map({
              case vi: Double => vi
              case vi: Float => vi.toDouble
              case vi: Long => vi.toDouble
              case vi: Int => vi.toDouble
              case vi: Short => vi.toDouble
              case vi: Byte => vi.toDouble
              case _ => throw new ContainerException(s"Bag range declared as $range but encountered $q")
            }): _*)
            case _ => throw new ContainerException(s"Bag range declared as $range but encountered $q")
          }
          else if (range == "N") q match {
            case v: Double => Bag.DoubleNaN(v).asInstanceOf[Bag.HandleNaN[RANGE]]
            case _ => throw new ContainerException(s"Bag range declared as $range but encountered $q")
          }
          else q match {
            case v: String => Bag.IgnoreNaN(v).asInstanceOf[Bag.HandleNaN[RANGE]]
            case _ => throw new ContainerException(s"Bag range declared as $range but encountered $q")
          }

        // no possibility of exception from here on out (for rollback)
        entries += weight
        if (values contains qnan)
          values(qnan) += weight
        else
          values(qnan) = weight
      }
    }

    def children = Nil

    def toJsonFragment(suppressName: Boolean) =
      JsonObject(
        "entries" -> JsonFloat(entries),
        "values" -> JsonArray(values.toSeq.sortBy(_._1).map({
          case (v: Bag.IgnoreNaN, n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonString(v.x))
          case (v: Bag.DoubleNaN, n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonFloat(v.x))
          case (v: Bag.SeqNaN[_], n) => JsonObject("w" -> JsonFloat(n), "v" -> JsonArray(v.iterator.map({case vi: Double => JsonFloat(vi)}).toSeq: _*))
        }): _*),
        "range" -> JsonString(range)).maybe(JsonString("name") -> (if (suppressName) None else quantity.name.map(JsonString(_))))

    override def toString() = s"""<Bagging size=${values.size} range=$range>"""
    override def equals(that: Any) = that match {
      case that: Bagging[DATUM, RANGE] => this.quantity == that.quantity  &&  this.entries === that.entries  &&  this.values == that.values  &&  this.range == that.range
      case _ => false
    }
    override def hashCode() = (quantity, entries, values, range).hashCode()
  }
}
