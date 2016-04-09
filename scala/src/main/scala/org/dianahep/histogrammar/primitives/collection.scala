package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Label/Labeled/Labeling
  object Label extends Factory {
    val name = "Label"
    val help = "Accumulate any number of aggregators of the SAME type and label them with strings. Every one is filled with every input datum."
    val detailedHelp = """Label(pairs: (String, V)*)"""

    def container[V <: Container[V]](pairs: (String, V)*) = new Labeled[V](pairs: _*)
    // def apply[DATUM, V <: Aggregator[DATUM, V]](pairs: (String, V)*) = new Labeling[V](pairs: _*)

    def unapplySeq[V <: Container[V]](x: Labeled[V]) = Some(x.pairs)
    // def unapplySeq[DATUM, V <: Aggregator[DATUM, V]](x: Labeling[V]) = Some(x.pairs)

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("type", "data")) =>
        val get = pairs.toMap

        val factory =
          get("type") match {
            case JsonString(factory) => Factory(factory)
            case _ => throw new JsonFormatException(json, name + ".type")
          }

        get("data") match {
          case JsonObject(labelPairs @ _*) if (labelPairs.size >= 1) =>
            new Labeled[Container[_]](labelPairs map {case (JsonString(label), sub) => label -> factory.fromJsonFragment(sub)}: _*)
          case _ =>
            throw new JsonFormatException(json, name + ".data")
        }

      case _ => throw new JsonFormatException(json, name)
    }
  }

  class Labeled[V <: Container[V]](val pairs: (String, V)*) extends Container[Labeled[V]] {
    def factory = Label

    val pairsMap = pairs.toMap
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    def values: Iterable[Container[_]] = pairs.toIterable.map(_._2)
    def keySet: Set[String] = keys.toSet
    def apply(x: String) = pairsMap(x)
    def get(x: String) = pairsMap.get(x)
    def getOrElse(x: String, default: => V) = pairsMap.getOrElse(x, default)

    def +(that: Labeled[V]) =
      if (this.keySet != that.keySet)
        throw new AggregatorException(s"""cannot add Labeled because they have different keys:\n    ${this.keys.toArray.sorted.mkString(" ")}\nvs\n    ${that.keys.toArray.sorted.mkString(" ")}""")
      else
        new Labeled[V](this.pairs map {case (label, mysub) =>
          val yoursub = that.pairsMap(label)
          label -> (mysub + yoursub)
        }: _*)

    def toJsonFragment =
      JsonObject("type" -> JsonString(factory.name), "data" -> JsonObject(
        pairs map {case (label, sub) => label -> sub.toJsonFragment}: _*))

    override def toString() = s"Labeled[size=${pairs.size}]"
    override def equals(that: Any) = that match {
      case that: Labeled[V] => this.pairsMap == that.pairsMap
      case _ => false
    }
    override def hashCode() = pairsMap.hashCode
  }

  //////////////////////////////////////////////////////////////// MultiLabel/MultiLabeled/MultiLabeling

  //////////////////////////////////////////////////////////////// Index/Indexed/Indexing

  //////////////////////////////////////////////////////////////// MultiIndex/MultiIndexed/MultiIndexing

}
