package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Bag/Bagged/Bagging

  object Bag extends Factory {
    val name = "Bag"
    val help = "Accumulate raw data up to a limit."
    val detailedHelp = """Bag(quantity: MultivariateFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM], limit: Option[Double] = None)"""

    def ed(entries: Double, limit: Option[Double], values: Option[Map[Vector[Double], Double]]) =
      new Bagged(entries, limit, values)

    def apply[DATUM](quantity: MultivariateFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM], limit: Option[Double] = None) =
      new Bagging[DATUM](quantity, selection, limit, 0.0, Some(scala.collection.mutable.Map[Vector[Double], Double]()))

    def unapply(x: Bagged) = Some((x.entries, x.limit, x.values))
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
