package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// NameMap/NameMapped/NameMapping

  // this is a *heterogeneous* map, so some runtime casting is necessary; also note that data is broadcast to all members
  object NameMap extends Factory {
    val name = "NameMap"

    def ed(pairs: (String, Container[_])*) = new NameMapped(pairs: _*)
    def apply[DATUM](pairs: (String, Aggregator[DATUM, _])*) = new NameMapping(pairs: _*)

    def unapplySeq(x: NameMapped) = Some(x.pairs)
    def unapplySeq[DATUM](x: NameMapping[DATUM]) = Some(x.pairs)

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) =>
        new NameMapped(pairs map {
          case (JsonString(key), JsonObject(typedata @ _*)) if (typedata.keySet == Set("type", "data")) =>
            val get = typedata.toMap
            (get("type"), get("data")) match {
              case (JsonString(factory), sub) => (key.toString, Factory(factory).fromJsonFragment(sub))
              case _ => throw new JsonFormatException(json, name + s""" key "$key"""")
            }
          case _ => throw new JsonFormatException(json, name + s" key")
        }: _*)

      case _ => throw new JsonFormatException(json, name)
    }
  }

  class NameMapped(val pairs: (String, Container[_])*) extends Container[NameMapped] {
    def factory = NameMap

    val pairsMap = pairs.toMap
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    def values: Iterable[Container[_]] = pairs.toIterable.map(_._2)
    def keySet: Set[String] = keys.toSet
    def apply[CONTAINER <: Container[CONTAINER]](x: String) = pairsMap(x).asInstanceOf[CONTAINER]
    def get[CONTAINER <: Container[CONTAINER]](x: String) = pairsMap.get(x).asInstanceOf[CONTAINER]
    def getOrElse[CONTAINER <: Container[CONTAINER]](x: String, default: => CONTAINER) = pairsMap.getOrElse(x, default).asInstanceOf[CONTAINER]

    private def combine[CONTAINER <: Container[CONTAINER]](one: Container[_], two: Container[_]) =
      one.asInstanceOf[CONTAINER] + two.asInstanceOf[CONTAINER]

    def +(that: NameMapped) =
      if (this.keySet != that.keySet)
        throw new AggregatorException(s"""cannot add NameMapped because they have different keys:\n    ${this.keys.toArray.sorted.mkString(" ")}\nvs\n    ${that.keys.toArray.sorted.mkString(" ")}""")
      else
        new NameMapped(this.pairs.map({case (key, mysub) =>
          val yoursub = that.pairsMap(key)
          if (mysub.factory != yoursub.factory)
            throw new AggregatorException(s"""cannot add NameMapped because key "$key" has a different type in the two maps: ${mysub.factory.name} vs ${yoursub.factory.name}""")
          (key, combine(mysub, yoursub))
        }): _*)

    def toJsonFragment = JsonObject(pairs map {case (key, sub) =>
      key -> JsonObject("type" -> JsonString(sub.factory.name), "data" -> sub.toJsonFragment)
    }: _*)

    override def toString() = s"NameMapped[size=${pairs.size}]"
    override def equals(that: Any) = that match {
      case that: NameMapped => this.pairsMap == that.pairsMap
      case _ => false
    }
    override def hashCode() = pairsMap.hashCode
  }

  class NameMapping[DATUM](val pairs: (String, Aggregator[DATUM, _])*) extends Aggregator[DATUM, NameMapping[DATUM]] {
    def factory = NameMap

    val pairsMap = pairs.toMap
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    def values[AGGREGATOR <: Aggregator[DATUM, _]]: Iterable[AGGREGATOR] = pairs.toIterable.map(_._2.asInstanceOf[AGGREGATOR])
    def keySet: Set[String] = keys.toSet
    def apply[AGGREGATOR <: Aggregator[DATUM, _]](x: String) = pairsMap(x).asInstanceOf[AGGREGATOR]
    def get[AGGREGATOR <: Aggregator[DATUM, _]](x: String) = pairsMap.get(x).asInstanceOf[AGGREGATOR]
    def getOrElse[AGGREGATOR <: Aggregator[DATUM, _]](x: String, default: => AGGREGATOR) = pairsMap.getOrElse(x, default).asInstanceOf[AGGREGATOR]

    private def combine[DATUM, AGGREGATOR <: Aggregator[DATUM, AGGREGATOR]](one: Aggregator[_, _], two: Aggregator[_, _]) =
      one.asInstanceOf[AGGREGATOR] + two.asInstanceOf[AGGREGATOR]

    def +(that: NameMapping[DATUM]) =
      if (this.keySet != that.keySet)
        throw new AggregatorException(s"""cannot add NameMapping because they have different keys:\n    ${this.keys.toArray.sorted.mkString(" ")}\nvs\n    ${that.keys.toArray.sorted.mkString(" ")}""")
      else
        new NameMapping[DATUM](this.pairs.map({case (key, mysub) =>
          val yoursub = that.pairsMap(key)
          if (mysub.factory != yoursub.factory)
            throw new AggregatorException(s"""cannot add NameMapping because key "$key" has a different type in the two maps: ${mysub.factory.name} vs ${yoursub.factory.name}""")
          (key, combine(mysub, yoursub))
        }): _*)

    def fill(x: Weighted[DATUM]) {
      if (x.contributes)
        values foreach {v: Aggregator[DATUM, _] => v.fill(x)}
    }

    def toJsonFragment = JsonObject(pairs map {case (key, sub) =>
      key -> JsonObject("type" -> JsonString(sub.factory.name), "data" -> sub.toJsonFragment)
    }: _*)

    override def toString() = s"NameMapping[size=${pairs.size}]"
    override def equals(that: Any) = that match {
      case that: NameMapping[DATUM] => this.pairsMap == that.pairsMap
      case _ => false
    }
    override def hashCode() = pairsMap.hashCode
  }
}
