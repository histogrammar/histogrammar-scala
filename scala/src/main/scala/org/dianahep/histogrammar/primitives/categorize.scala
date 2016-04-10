package org.dianahep

import scala.collection.mutable

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Categorize/Categorized/Categorizing

  object Categorize extends Factory {
    val name = "Categorize"
    val help = "Split a given quantity by its categorical (string-based) value and fill only one category per datum."
    val detailedHelp = """Categorize(quantity: CategoricalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM], value: => V = Count())"""

    def fixed[V <: Container[V]](contentType: String, pairs: (String, V)*) = new Categorized(contentType, pairs: _*)

    def apply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](quantity: CategoricalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM], value: => V = Count()) =
      new Categorizing(quantity, selection, value, mutable.HashMap[String, V]())

    def unapplySeq[V <: Container[V]](x: Categorized[V]) = Some(x.pairs)
    def unapplySeq[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](x: Categorizing[DATUM, V]) = Some(x.pairs)

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("type", "data")) =>
        val get = pairs.toMap

        val (contentType, factory) = get("type") match {
          case JsonString(name) => (name, Factory(name))
          case x => throw new JsonFormatException(x, name + ".type")
        }

        get("data") match {
          case JsonObject(categoryPairs @ _*) =>
            new Categorized[Container[_]](contentType, categoryPairs map {
              case (JsonString(category), value) =>
                category -> factory.fromJsonFragment(value)
            }: _*)

          case x => throw new JsonFormatException(x, name + ".data")
        }

      case _ => throw new JsonFormatException(json, name)
    }
  }

  class Categorized[V <: Container[V]](contentType: String, val pairs: (String, V)*) extends Container[Categorized[V]] {
    type Type = Categorized[V]
    type FixedType = Categorized[V]
    def factory = Categorize

    val pairsMap = pairs.toMap
    def size = pairs.size
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    def values: Iterable[Container[V]] = pairs.toIterable.map(_._2)
    def keySet: Set[String] = keys.toSet
    def apply(x: String) = pairsMap(x)
    def get(x: String) = pairsMap.get(x)
    def getOrElse(x: String, default: => V) = pairsMap.getOrElse(x, default)

    def +(that: Categorized[V]) = new Categorized(contentType, (this.keySet union that.keySet).toSeq map {key =>
      if ((this.pairsMap contains key)  &&  (that.pairsMap contains key))
        (key, this.pairsMap(key) + that.pairsMap(key))
      else if (this.pairsMap contains key)
        (key, this.pairsMap(key))
      else
        (key, that.pairsMap(key))
    }: _*)

    def fix = this
    def toJsonFragment = JsonObject(
      "type" -> JsonString(contentType),
      "data" -> JsonObject(pairs map {case (k, v) => (JsonString(k), v.toJsonFragment)}: _*))

    override def toString() = s"""Categorized[${if (pairs.isEmpty) "" else pairs.head._2.toString + ", "}size=${pairs.size}]"""
    override def equals(that: Any) = that match {
      case that: Categorized[V] => this.pairs == that.pairs
      case _ => false
    }
  }

  class Categorizing[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](val quantity: CategoricalFcn[DATUM], val selection: Selection[DATUM], value: => V, val pairs: mutable.HashMap[String, V]) extends Container[Categorizing[DATUM, V]] with AggregationOnData {
    type Type = Categorizing[DATUM, V]
    type FixedType = Categorized[V#FixedType]
    type Datum = DATUM
    def factory = Categorize

    def pairsMap = pairs.toMap
    def size = pairs.size
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    def values: Iterable[V] = pairs.toIterable.map(_._2)
    def keySet: Set[String] = keys.toSet
    def apply(x: String) = pairsMap(x)
    def get(x: String) = pairsMap.get(x)
    def getOrElse(x: String, default: => V) = pairsMap.getOrElse(x, default)

    def +(that: Categorizing[DATUM, V]) = new Categorizing[DATUM, V](
      this.quantity,
      this.selection,
      this.value,
      mutable.HashMap[String, V]((this.keySet union that.keySet).toSeq map {key =>
        if ((this.pairsMap contains key)  &&  (that.pairsMap contains key))
          (key, this.pairsMap(key) + that.pairsMap(key))
        else if (this.pairsMap contains key)
          (key, this.pairsMap(key))
        else
          (key, that.pairsMap(key))
      }: _*))
    
    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)

        if (!(pairs contains q))
          pairs(q) = value
        pairs(q).fillWeighted(datum, w)
      }
    }

    def fix = new Categorized(value.factory.name, pairs.toSeq map {case (k, v) => (k, v.fix)}: _*)
    def toJsonFragment = fix.toJsonFragment

    override def toString() = s"Categorizing[$value, size=${pairs.size}]"
    override def equals(that: Any) = that match {
      case that: Categorizing[DATUM, V] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.pairs == that.pairs
      case _ => false
    }
  }
}
