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
  //////////////////////////////////////////////////////////////// Bag/Bagged/Bagging

  /**Accumulate raw data up to an optional limit, at which point only the total number are preserved.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Bagging]] and immutable [[org.dianahep.histogrammar.Bagged]] objects.
    */
  object Bag extends Factory {
    val name = "Bag"
    val help = "Accumulate raw data up to an optional limit, at which point only the total number are preserved."
    val detailedHelp = """Bag(quantity: MultivariateFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM], limit: Option[Double] = None)"""

    /** Create an immutable [[org.dianahep.histogrammar.Bagged]] from arguments (instead of JSON).
      * 
      * @param entries weighted number of entries (sum of all observed weights)
      * @param limit if not `None` and `entries > limit`, the `values` are dropped, leaving only `entries` to count data
      * @param values distinct multidimensional vectors and the (weighted) number of times they were observed or `None` if they were dropped
      */
    def ed(entries: Double, limit: Option[Double], values: Option[Map[Vector[Double], Double]]) =
      new Bagged(entries, limit, values)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Bagging]].
      * 
      * @param quantity multivariate function to track
      * @param selection boolean or non-negative function that cuts or weights entries
      * @param limit if not `None` and `entries > limit`, the `values` are dropped, leaving only `entries` to count data
      */
    def apply[DATUM](quantity: MultivariateFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM], limit: Option[Double] = None) =
      new Bagging[DATUM](quantity, selection, limit, 0.0, Some(scala.collection.mutable.Map[Vector[Double], Double]()))

    /** Synonym for `apply`. */
    def ing[DATUM](quantity: MultivariateFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM], limit: Option[Double] = None) =
      apply(quantity, selection, limit)

    /** Use [[org.dianahep.histogrammar.Bagged]] in Scala pattern-matching. */
    def unapply(x: Bagged) = Some((x.entries, x.limit, x.values))
    /** Use [[org.dianahep.histogrammar.Bagging]] in Scala pattern-matching. */
    def unapply[DATUM](x: Bagging[DATUM])  = Some((x.entries, x.limit, x.values))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("entries", "limit", "values")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val limit = get("limit") match {
          case JsonNumber(x) => Some(x)
          case JsonNull => None
          case x => throw new JsonFormatException(x, name + ".limit")
        }

        val values = get("values") match {
          case JsonArray(elems @ _*) => Some(Map[Vector[Double], Double](elems.zipWithIndex map {
            case (JsonObject(nv @ _*), i) if (nv.keySet == Set("n", "v")) =>
              val nvget = nv.toMap

              val n = nvget("n") match {
                case JsonNumber(x) => x
                case x => throw new JsonFormatException(x, name + s".values $i n")
              }

              val v = nvget("v") match {
                case JsonArray(d @ _*) => Vector(d.zipWithIndex map {
                  case (JsonNumber(x), j) => x
                  case (x, j) => throw new JsonFormatException(x, name + s".values $i v $j")
                }: _*)
                case x => throw new JsonFormatException(x, name + s".values $i v")
              }

              (v -> n)

            case (x, i) => throw new JsonFormatException(x, name + s".values $i")
          }: _*))
          case JsonNull => None
          case x => throw new JsonFormatException(x, name + ".values")
        }

        new Bagged(entries, limit, values)

      case _ => throw new JsonFormatException(json, name)
    }
  }

  /** An accumulated set of raw data or just the number of entries if it exceeded its limit.
    * @param entries weighted number of entries (sum of all observed weights)
    * @param limit if not `None` and `entries > limit`, the `values` are dropped, leaving only `entries` to count data
    * @param values distinct multidimensional vectors and the (weighted) number of times they were observed or `None` if they were dropped
    */
  class Bagged(val entries: Double, val limit: Option[Double], val values: Option[Map[Vector[Double], Double]]) extends Container[Bagged] {
    type Type = Bagged
    def factory = Bag

    def zero = new Bagged(0.0, limit, Some(Map[Vector[Double], Double]()))
    def +(that: Bagged) = {
      val newentries = this.entries + that.entries
      val newvalues =
        if (!limit.isEmpty  &&  limit.get < newentries)
          None
        else if (that.values.isEmpty)
          this.values
        else if (this.values.isEmpty)
          that.values
        else {
          val out = scala.collection.mutable.Map(this.values.get.toSeq: _*)
          that.values.get foreach {case (k, v) =>
            if (out contains k)
              out(k) += v
            else
              out(k) = v
          }
          Some(out.toMap)
        }

      new Bagged(newentries, limit, newvalues)
    }

    def toJsonFragment = {
      import Ordering.Implicits._
      JsonObject(
        "entries" -> JsonFloat(entries),
        "limit" -> (limit match {
          case Some(x) => JsonFloat(x)
          case None => JsonNull
        }),
        "values" -> (values match {
          case Some(m) => JsonArray(m.toSeq.sortBy(_._1).map({case (v, n) => JsonObject("n" -> JsonFloat(n), "v" -> JsonArray(v.map(JsonFloat(_)): _*))}): _*)
          case None => JsonNull
        }))
    }

    override def toString() = s"""Bagged(entries=$entries, ${if (values.isEmpty) "empty" else "non-empty"})"""
    override def equals(that: Any) = that match {
      case that: Bagged => this.entries === that.entries  &&  this.limit == that.limit  &&  this.values == that.values
      case _ => false
    }
    override def hashCode() = (entries, limit, values).hashCode()
  }

  /** Accumulating a quantity as raw data up to an optional limit, at which point only the total number are preserved.
    * 
    * @param quantity multivariate function to track
    * @param selection boolean or non-negative function that cuts or weights entries
    * @param limit if not `None` and `entries > limit`, the `values` are dropped, leaving only `entries` to count data
    * @param entries weighted number of entries (sum of all observed weights)
    * @param values distinct multidimensional vectors and the (weighted) number of times they were observed or `None` if they were dropped
    */
  class Bagging[DATUM](val quantity: MultivariateFcn[DATUM], val selection: Selection[DATUM], val limit: Option[Double], var entries: Double, var values: Option[scala.collection.mutable.Map[Vector[Double], Double]]) extends Container[Bagging[DATUM]] with AggregationOnData {
    type Type = Bagging[DATUM]
    type Datum = DATUM
    def factory = Bag

    def zero = new Bagging[DATUM](quantity, selection, limit, 0.0, Some(scala.collection.mutable.Map[Vector[Double], Double]()))
    def +(that: Bagging[DATUM]) = {
      val newentries = this.entries + that.entries
      val newvalues =
        if (!limit.isEmpty  &&  limit.get < newentries)
          None
        else if (that.values.isEmpty)
          this.values
        else if (this.values.isEmpty)
          that.values
        else {
          val out = scala.collection.mutable.Map(this.values.get.toSeq: _*)
          that.values.get foreach {case (k, v) =>
            if (out contains k)
              out(k) += v
            else
              out(k) = v
          }
          Some(out)
        }

      new Bagging[DATUM](quantity, selection, limit, newentries, newvalues)
    }

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)
        entries += w
        if (!limit.isEmpty  &&  limit.get < entries)
          values = None
        else
          if (!values.isEmpty) {
            if (values.get contains q)
              values.get(q) += w
            else
              values.get(q) = w
          }
      }
    }

    def toJsonFragment = {
      import Ordering.Implicits._
      JsonObject(
        "entries" -> JsonFloat(entries),
        "limit" -> (limit match {
          case Some(x) => JsonFloat(x)
          case None => JsonNull
        }),
        "values" -> (values match {
          case Some(m) => JsonArray(m.toSeq.sortBy(_._1).map({case (v, n) => JsonObject("n" -> JsonFloat(n), "v" -> JsonArray(v.map(JsonFloat(_)): _*))}): _*)
          case None => JsonNull
        }))
    }

    override def toString() = s"""Bagging(entries=$entries, ${if (values.isEmpty) "empty" else "non-empty"})"""
    override def equals(that: Any) = that match {
      case that: Bagging[DATUM] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.entries === that.entries  &&  this.limit == that.limit  &&  this.values == that.values
      case _ => false
    }
    override def hashCode() = (quantity, selection, entries, limit, values).hashCode()
  }

}
