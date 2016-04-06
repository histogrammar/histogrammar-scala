package org.dianahep

import scala.collection.mutable

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Categorize/Categorized/Categorizing

  object Categorize extends Factory {
    val name = "Categorize"

    def default[DATUM] = Count[DATUM]()

    def ed[V <: Container[V]](pairs: (String, V)*) = new Categorized(pairs: _*)

    def ing[DATUM, V <: Container[V]](quantity: CategoricalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM], value: => Aggregator[DATUM, V] = default[DATUM]) =
      new Categorizing(quantity, selection, value, mutable.HashMap[String, Aggregator[DATUM, V]]())

    def apply[DATUM, V <: Container[V]](quantity: CategoricalFcn[DATUM], selection: Selection[DATUM] = unweighted[DATUM], value: => Aggregator[DATUM, V] = default[DATUM]) =
      ing(quantity, selection, value)

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
    def apply[CONTAINER <: Container[CONTAINER]](x: String) = pairsMap(x).asInstanceOf[CONTAINER]
    def get[CONTAINER <: Container[CONTAINER]](x: String) = pairsMap.get(x).asInstanceOf[CONTAINER]
    def getOrElse[CONTAINER <: Container[CONTAINER]](x: String, default: => CONTAINER) = pairsMap.getOrElse(x, default).asInstanceOf[CONTAINER]

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

  class Categorizing[DATUM, V <: Container[V]](val quantity: CategoricalFcn[DATUM], val selection: Selection[DATUM], value: => Aggregator[DATUM, V], val pairs: mutable.HashMap[String, Aggregator[DATUM, V]]) extends Aggregator[DATUM, Categorized[V]] {
    def factory = Categorize

    def pairsMap = pairs.toMap
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    def values: Iterable[Aggregator[DATUM, _]] = pairs.toIterable.map(_._2)
    def keySet: Set[String] = keys.toSet
    def apply[AGGREGATOR <: Aggregator[DATUM, _]](x: String) = pairsMap(x).asInstanceOf[AGGREGATOR]
    def get[AGGREGATOR <: Aggregator[DATUM, _]](x: String) = pairsMap.get(x).asInstanceOf[AGGREGATOR]
    def getOrElse[AGGREGATOR <: Aggregator[DATUM, _]](x: String, default: => AGGREGATOR) = pairsMap.getOrElse(x, default).asInstanceOf[AGGREGATOR]
    
    def fill(x: Weighted[DATUM]) {
      val k = quantity(x)
      val y = x reweight selection(x)
      if (y.contributes) {
        if (!(pairs contains k))
          pairs(k) = value
        pairs(k).fill(y)
      }
    }

    def fix = new Categorized(pairs.toSeq map {case (k, v) => (k, v.fix)}: _*)
    override def toString() = s"Categorizing[$value, size=${pairs.size}]"
    override def equals(that: Any) = that match {
      case that: Categorizing[DATUM, V] => this.quantity == that.quantity  &&  this.selection == that.selection  &&  this.pairs == that.pairs
      case _ => false
    }
  }
}
