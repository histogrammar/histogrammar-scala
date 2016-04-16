package org.dianahep

import scala.collection.mutable

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Categorize/Categorized/Categorizing

  object Categorize extends Factory {
    val name = "Categorize"
    val help = "Split a given quantity by its categorical (string-based) value and fill only one category per datum."
    val detailedHelp = """Categorize(quantity: CategoricalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM], value: => V = Count())"""

    def fixed[V <: Container[V]](entries: Double, contentType: String, pairs: (String, V)*) = new Categorized(entries, contentType, pairs: _*)

    def apply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](quantity: CategoricalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM], value: => V = Count()) =
      new Categorizing(quantity, selection, 0.0, value, mutable.HashMap[String, V]())

    def unapply[V <: Container[V]](x: Categorized[V]) = Some((x.entries, x.pairsMap))
    def unapply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](x: Categorizing[DATUM, V]) = Some((x.entries, x.pairsMap))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("entries", "type", "data")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val (contentType, factory) = get("type") match {
          case JsonString(name) => (name, Factory(name))
          case x => throw new JsonFormatException(x, name + ".type")
        }

        get("data") match {
          case JsonObject(categoryPairs @ _*) =>
            new Categorized[Container[_]](entries, contentType, categoryPairs map {
              case (JsonString(category), value) =>
                category -> factory.fromJsonFragment(value)
            }: _*)

          case x => throw new JsonFormatException(x, name + ".data")
        }

      case _ => throw new JsonFormatException(json, name)
    }
  }

  class Categorized[V <: Container[V]](val entries: Double, contentType: String, val pairs: (String, V)*) extends Container[Categorized[V]] {
    type Type = Categorized[V]
    def factory = Categorize

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    val pairsMap = pairs.toMap
    def size = pairs.size
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    def values: Iterable[Container[V]] = pairs.toIterable.map(_._2)
    def keySet: Set[String] = keys.toSet
    def apply(x: String) = pairsMap(x)
    def get(x: String) = pairsMap.get(x)
    def getOrElse(x: String, default: => V) = pairsMap.getOrElse(x, default)

    def zero = new Categorized[V](0.0, contentType)
    def +(that: Categorized[V]) = new Categorized(
      this.entries + that.entries,
      contentType,
      (this.keySet union that.keySet).toSeq map {key =>
        if ((this.pairsMap contains key)  &&  (that.pairsMap contains key))
          (key, this.pairsMap(key) + that.pairsMap(key))
        else if (this.pairsMap contains key)
          (key, this.pairsMap(key))
        else
          (key, that.pairsMap(key))
      }: _*)

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "type" -> JsonString(contentType),
      "data" -> JsonObject(pairs map {case (k, v) => (JsonString(k), v.toJsonFragment)}: _*))

    override def toString() = s"""Categorized[entries=$entries, [${if (pairs.isEmpty) contentType else pairs.head._2.toString}..., size=${pairs.size}]]"""
    override def equals(that: Any) = that match {
      case that: Categorized[V] => this.entries === that.entries  &&  this.pairs == that.pairs
      case _ => false
    }
    override def hashCode() = (entries, pairs).hashCode()
  }

  class Categorizing[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](val quantity: CategoricalFcn[DATUM], val selection: Selection[DATUM], var entries: Double, value: => V, val pairs: mutable.HashMap[String, V]) extends Container[Categorizing[DATUM, V]] with AggregationOnData {
    type Type = Categorizing[DATUM, V]
    type Datum = DATUM
    def factory = Categorize

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def pairsMap = pairs.toMap
    def size = pairs.size
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    def values: Iterable[V] = pairs.toIterable.map(_._2)
    def keySet: Set[String] = keys.toSet
    def apply(x: String) = pairsMap(x)
    def get(x: String) = pairsMap.get(x)
    def getOrElse(x: String, default: => V) = pairsMap.getOrElse(x, default)

    def zero = new Categorizing[DATUM, V](quantity, selection, 0.0, value, mutable.HashMap(pairs.toSeq map {case (c, v) => (c, v.zero)}: _*))
    def +(that: Categorizing[DATUM, V]) = new Categorizing[DATUM, V](
      this.quantity,
      this.selection,
      this.entries + that.entries,
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

        entries += w
        if (!(pairs contains q))
          pairs(q) = value
        pairs(q).fillWeighted(datum, w)
      }
    }

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "type" -> JsonString(value.factory.name),
      "data" -> JsonObject(pairs.toSeq map {case (k, v) => (JsonString(k), v.toJsonFragment)}: _*))

    override def toString() = s"Categorizing[entries=$entries, [${if (values.isEmpty) value.factory.name else values.head.toString}..., size=${pairs.size}]]"
    override def equals(that: Any) = that match {
      case that: Categorizing[DATUM, V] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.entries === that.entries  &&  this.pairs == that.pairs
      case _ => false
    }
    override def hashCode() = (quantity, selection, entries, pairs).hashCode()
  }
}
