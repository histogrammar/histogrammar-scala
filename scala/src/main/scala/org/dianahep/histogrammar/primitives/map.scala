package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// NameMap/NameMapped/NameMapping

  // this is a *heterogeneous* map, so some runtime casting is necessary; also note that data is broadcast to all members
  object NameMap extends Factory {
    val name = "NameMap"

    def container(pairs: (String, Container[_])*) = new NameMapped(pairs: _*)
    def apply[DATUM](pairs: (String, Container[_])*) = new NameMapping[DATUM](pairs: _*)

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

    private[histogrammar] def combine[CONTAINER <: Container[CONTAINER]](one: Container[_], two: Container[_]) =
      one.asInstanceOf[CONTAINER] + two.asInstanceOf[CONTAINER]
  }

  class NameMapped(val pairs: (String, Container[_])*) extends Container[NameMapped] {
    def factory = NameMap

    val pairsMap = pairs.toMap
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    def values[CONTAINER <: Container[_]]: Iterable[CONTAINER] = pairs.toIterable.map(_._2.asInstanceOf[CONTAINER])
    def keySet: Set[String] = keys.toSet
    def apply[CONTAINER <: Container[CONTAINER]](x: String) = pairsMap(x).asInstanceOf[CONTAINER]
    def get[CONTAINER <: Container[CONTAINER]](x: String) = pairsMap.get(x).asInstanceOf[CONTAINER]
    def getOrElse[CONTAINER <: Container[CONTAINER]](x: String, default: => CONTAINER) = pairsMap.getOrElse(x, default).asInstanceOf[CONTAINER]

    def +(that: NameMapped) =
      if (this.keySet != that.keySet)
        throw new AggregatorException(s"""cannot add NameMapped because they have different keys:\n    ${this.keys.toArray.sorted.mkString(" ")}\nvs\n    ${that.keys.toArray.sorted.mkString(" ")}""")
      else
        new NameMapped(this.pairs.map({case (key, mysub) =>
          val yoursub = that.pairsMap(key)
          if (mysub.factory != yoursub.factory)
            throw new AggregatorException(s"""cannot add NameMapped because key "$key" has a different type in the two maps: ${mysub.factory.name} vs ${yoursub.factory.name}""")
          (key, NameMap.combine(mysub, yoursub))
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

  class NameMapping[DATUM](val pairs: (String, Container[_])*) extends Container[NameMapping[DATUM]] with Aggregation[DATUM] {
    def factory = NameMap

    if (!pairs.forall(_._2.isInstanceOf[Aggregation[DATUM]]))
      throw new AggregatorException(s"NameMapping should be built with Aggregation-enabled pairs (ending in -ing)")

    val pairsMap = pairs.toMap
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    def values[CONTAINER <: Container[_]]: Iterable[CONTAINER] = pairs.toIterable.map(_._2.asInstanceOf[CONTAINER])
    def keySet: Set[String] = keys.toSet
    def apply[CONTAINER <: Container[_]](x: String) = pairsMap(x).asInstanceOf[CONTAINER]
    def get[CONTAINER <: Container[_]](x: String) = pairsMap.get(x).asInstanceOf[CONTAINER]
    def getOrElse[CONTAINER <: Container[_]](x: String, default: => CONTAINER) = pairsMap.getOrElse(x, default).asInstanceOf[CONTAINER]

    def +(that: NameMapping[DATUM]) =
      if (this.keySet != that.keySet)
        throw new AggregatorException(s"""cannot add NameMapping because they have different keys:\n    ${this.keys.toArray.sorted.mkString(" ")}\nvs\n    ${that.keys.toArray.sorted.mkString(" ")}""")
      else
        new NameMapping[DATUM](this.pairs.map({case (key, mysub) =>
          val yoursub = that.pairsMap(key)
          if (mysub.factory != yoursub.factory)
            throw new AggregatorException(s"""cannot add NameMapping because key "$key" has a different type in the two maps: ${mysub.factory.name} vs ${yoursub.factory.name}""")
          (key, NameMap.combine(mysub, yoursub))
        }): _*)

    def fillWeighted(x: Weighted[DATUM]) {
      if (x.weight > 0.0)
        values foreach {v: Container[_] => v.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)}
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
