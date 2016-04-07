package org.dianahep

import scala.collection.mutable

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Categorize/Categorized/Categorizing

  object Categorize extends Factory {
    val name = "Categorize"

    def container[V <: Container[V]](pairs: (String, V)*) = new Categorized(pairs: _*)

    def apply[DATUM, V <: Container[V]](quantity: CategoricalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM], value: => V = Count()) =
      new Categorizing(quantity, selection, value, mutable.HashMap[String, V]())

    def unapplySeq[V <: Container[V]](x: Categorized[V]) = Some(x.pairs)
    def unapplySeq[DATUM, V <: Container[V]](x: Categorizing[DATUM, V]) = Some(x.pairs)

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("type", "data")) =>
        val get = pairs.toMap

        val factoryName = get("type") match {
          case JsonString(name) => name
          case x => throw new JsonFormatException(x, name + ".type")
        }

        get("data") match {
          case JsonObject(categoryPairs @ _*) =>
            new Categorized[Container[_]](categoryPairs map {
              case (JsonString(category), value) =>
                category -> Factory(factoryName).fromJsonFragment(value)
            }: _*)

          case x => throw new JsonFormatException(x, name + ".data")
        }

      case _ => throw new JsonFormatException(json, name)
    }
  }

  class Categorized[V <: Container[V]](val pairs: (String, V)*) extends Container[Categorized[V]] {
    def factory = Categorize

    val pairsMap = pairs.toMap
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    def values: Iterable[Container[_]] = pairs.toIterable.map(_._2)
    def keySet: Set[String] = keys.toSet
    def apply(x: String) = pairsMap(x)
    def get(x: String) = pairsMap.get(x)
    def getOrElse(x: String, default: => V) = pairsMap.getOrElse(x, default)

    def +(that: Categorized[V]) = new Categorized((this.keySet union that.keySet).toSeq map {key =>
      if ((this.pairsMap contains key)  &&  (that.pairsMap contains key))
        (key, this.pairsMap(key) + that.pairsMap(key))
      else if (this.pairsMap contains key)
        (key, this.pairsMap(key))
      else
        (key, that.pairsMap(key))
    }: _*)

    def toJsonFragment = JsonObject(
      "type" -> JsonString(if (pairs.isEmpty) "?" else pairs.head._2.factory.name),
      "data" -> JsonObject(pairs map {case (k, v) => (JsonString(k), v.toJsonFragment)}: _*))

    override def toString() = s"""Categorized[${if (pairs.isEmpty) "" else pairs.head._2.toString}, size=${pairs.size}]"""
    override def equals(that: Any) = that match {
      case that: Categorized[V] => this.pairs == that.pairs
      case _ => false
    }
  }

  class Categorizing[DATUM, V <: Container[V]](val quantity: CategoricalFcn[DATUM], val selection: Selection[DATUM], value: => V, val pairs: mutable.HashMap[String, V]) extends Container[Categorizing[DATUM, V]] with Aggregation[DATUM] {
    def factory = Categorize

    if (!value.isInstanceOf[Aggregation[DATUM]])
      throw new AggregatorException(s"Categorizing should be built with Aggregation-enabled value (ending in -ing)")
    if (!pairs.values.forall(_.isInstanceOf[Aggregation[DATUM]]))
      throw new AggregatorException(s"Categorizing should be built with Aggregation-enabled values (ending in -ing)")

    def pairsMap = pairs.toMap
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
    
    def fillWeighted(x: Weighted[DATUM]) {
      val Weighted(datum, weight) = x

      val w = weight * selection(datum)
      if (w > 0.0) {
        val q = quantity(datum)

        if (!(pairs contains q))
          pairs(q) = value
        pairs(q).asInstanceOf[Aggregation[DATUM]].fillWeighted(Weighted(datum, w))
      }
    }

    def toJsonFragment = JsonObject(
      "type" -> JsonString(if (pairs.isEmpty) "?" else pairs.head._2.factory.name),
      "data" -> JsonObject(pairs.toSeq map {case (k, v) => (JsonString(k), v.toJsonFragment)}: _*))

    override def toString() = s"Categorizing[$value, size=${pairs.size}]"
    override def equals(that: Any) = that match {
      case that: Categorizing[DATUM, V] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.pairs == that.pairs
      case _ => false
    }
  }
}
