package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  // //////////////////////////////////////////////////////////////// Label/Labeled/Labeling
  // object Label extends Factory {
  //   val name = "Label"
  //   val help = "Accumulate any number of containers of the SAME type and label them with strings. Every one is filled with every input datum."
  //   val detailedHelp = """Label(pairs: (String, V)*)"""

  //   def container[V <: Container[V]](pairs: (String, V)*) = new Labeled[V](pairs: _*)
  //   def apply[DATUM, V <: Container[DATUM, V]](pairs: (String, V)*) = new Labeling[DATUM, V](pairs: _*)

  //   def unapplySeq[V <: Container[V]](x: Labeled[V]) = Some(x.pairs)
  //   def unapplySeq[DATUM, V <: Container[DATUM, V]](x: Labeling[DATUM, V]) = Some(x.pairs)

  //   def fromJsonFragment(json: Json): Container[_] = json match {
  //     case JsonObject(pairs @ _*) if (pairs.keySet == Set("type", "data")) =>
  //       val get = pairs.toMap

  //       val factory =
  //         get("type") match {
  //           case JsonString(factory) => Factory(factory)
  //           case _ => throw new JsonFormatException(json, name + ".type")
  //         }

  //       get("data") match {
  //         case JsonObject(labelPairs @ _*) if (labelPairs.size >= 1) =>
  //           new Labeled[Container[_]](labelPairs map {case (JsonString(label), sub) => label -> factory.fromJsonFragment(sub)}: _*)
  //         case _ =>
  //           throw new JsonFormatException(json, name + ".data")
  //       }

  //     case _ => throw new JsonFormatException(json, name)
  //   }
  // }

  // class Labeled[V <: Container[V]](val pairs: (String, V)*) extends Container[Labeled[V]] {
  //   def factory = Label

  //   if (pairs.isEmpty)
  //     throw new ContainerException("Labeled needs at least one element")

  //   val pairsMap = pairs.toMap
  //   def size = pairs.size
  //   def keys: Iterable[String] = pairs.toIterable.map(_._1)
  //   def values: Iterable[Container[V]] = pairs.toIterable.map(_._2)
  //   def keySet: Set[String] = keys.toSet
  //   def apply(x: String) = pairsMap(x)
  //   def get(x: String) = pairsMap.get(x)
  //   def getOrElse(x: String, default: => V) = pairsMap.getOrElse(x, default)

  //   def +(that: Labeled[V]) =
  //     if (this.keySet != that.keySet)
  //       throw new ContainerException(s"""cannot add Labeled because they have different keys:\n    ${this.keys.toArray.sorted.mkString(" ")}\nvs\n    ${that.keys.toArray.sorted.mkString(" ")}""")
  //     else
  //       new Labeled[V](this.pairs map {case (label, mysub) =>
  //         val yoursub = that.pairsMap(label)
  //         label -> (mysub + yoursub)
  //       }: _*)

  //   def toJsonFragment =
  //     JsonObject("type" -> JsonString(factory.name), "data" -> JsonObject(
  //       pairs map {case (label, sub) => label -> sub.toJsonFragment}: _*))

  //   override def toString() = s"Labeled[${pairs.head.toString}, size=${pairs.size}]"
  //   override def equals(that: Any) = that match {
  //     case that: Labeled[V] => this.pairsMap == that.pairsMap
  //     case _ => false
  //   }
  //   override def hashCode() = pairsMap.hashCode
  // }

  // class Labeling[DATUM, V <: Container[DATUM, V]](val pairs: (String, V)*) extends Container[DATUM, Labeling[DATUM, V]] {
  //   def factory = Label

  //   if (pairs.isEmpty)
  //     throw new ContainerException("Labeling needs at least one element")

  //   val pairsMap = pairs.toMap
  //   def size = pairs.size
  //   def keys: Iterable[String] = pairs.toIterable.map(_._1)
  //   def values: Iterable[V] = pairs.toIterable.map(_._2)
  //   def keySet: Set[String] = keys.toSet
  //   def apply(x: String) = pairsMap(x)
  //   def get(x: String) = pairsMap.get(x)
  //   def getOrElse(x: String, default: => V) = pairsMap.getOrElse(x, default)

  //   def +(that: Labeling[DATUM, V]) =
  //     if (this.keySet != that.keySet)
  //       throw new ContainerException(s"""cannot add Labeling because they have different keys:\n    ${this.keys.toArray.sorted.mkString(" ")}\nvs\n    ${that.keys.toArray.sorted.mkString(" ")}""")
  //     else
  //       new Labeling[DATUM, V](this.pairs map {case (label, mysub) =>
  //         val yoursub = that.pairsMap(label)
  //         label -> (mysub + yoursub)
  //       }: _*)

  //   def fillWeighted[SUB <: DATUM](datum: SUB, weight: Double) {
  //     values.foreach(_.fillWeighted(datum, weight))
  //   }

  //   def toJsonFragment =
  //     JsonObject("type" -> JsonString(factory.name), "data" -> JsonObject(
  //       pairs map {case (label, sub) => label -> sub.toJsonFragment}: _*))

  //   override def toString() = s"Labeling[${pairs.head.toString}, size=${pairs.size}]"
  //   override def equals(that: Any) = that match {
  //     case that: Labeled[V] => this.pairsMap == that.pairsMap
  //     case _ => false
  //   }
  //   override def hashCode() = pairsMap.hashCode
  // }

  //////////////////////////////////////////////////////////////// MixedTypeLabel/MixedTypeLabeled/MixedTypeLabeling

  //////////////////////////////////////////////////////////////// Index/Indexed/Indexing

  object Index extends Factory {
    val name = "Index"
    val help = "Accumulate any number of containers of the SAME type anonymously in a list. Every one is filled with every input datum."
    val detailedHelp = """"""

    def container[V <: Container[V]](values: V*) = new Indexed[V](values: _*)
    def apply[V <: Container[V] with Aggregation](values: V*) = new Indexing[V](values: _*)

    def unapplySeq[V <: Container[V]](x: Indexed[V]) = Some(x.values)
    def unapplySeq[V <: Container[V] with Aggregation](x: Indexing[V]) = Some(x.values)

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("type", "data")) =>
        val get = pairs.toMap

        val factory =
          get("type") match {
            case JsonString(factory) => Factory(factory)
            case _ => throw new JsonFormatException(json, name + ".type")
          }

        get("data") match {
          case JsonArray(values @ _*) if (values.size >= 1) =>
            new Indexed[Container[_]](values.map(factory.fromJsonFragment(_)): _*)
          case _ =>
            throw new JsonFormatException(json, name + ".data")
        }

      case _ => throw new JsonFormatException(json, name)
    }
  }

  class Indexed[V <: Container[V]](val values: V*) extends Container[Indexed[V]] {
    type Type = Indexed[V]
    def factory = Index

    if (values.isEmpty)
      throw new ContainerException("Indexed needs at least one element")

    def size = values.size
    def apply(i: Int) = values(i)
    def get(i: Int) =
      if (i < 0  ||  i >= size)
        None
      else
        Some(apply(i))
    def getOrElse(i: Int, default: => V) =
      if (i < 0  ||  i >= size)
        default
      else
        apply(i)

    def +(that: Indexed[V]) =
      if (this.size != that.size)
        throw new ContainerException(s"""cannot add Indexed because they have different sizes: (${this.size} vs ${that.size})""")
      else
        new Indexed[V](this.values zip that.values map {case(me, you) => me + you}: _*)

    def toJsonFragment =
      JsonObject("type" -> JsonString(factory.name), "data" -> JsonArray(values.map(_.toJsonFragment): _*))

    override def toString() = s"Indexed[${values.head.toString}, size=${size}]"
    override def equals(that: Any) = that match {
      case that: Indexed[V] => this.values == that.values
      case _ => false
    }
    override def hashCode() = values.hashCode
  }

  class Indexing[V <: Container[V] with Aggregation](val values: V*) extends Container[Indexing[V]] with Aggregation {
    type Type = Indexing[V]
    type Datum = V#Datum
    def factory = Index

    if (values.isEmpty)
      throw new ContainerException("Indexing needs at least one element")

    def size = values.size
    def apply(i: Int) = values(i)
    def get(i: Int) =
      if (i < 0  ||  i >= size)
        None
      else
        Some(apply(i))
    def getOrElse(i: Int, default: => V) =
      if (i < 0  ||  i >= size)
        default
      else
        apply(i)

    def +(that: Indexing[V]) =
      if (this.size != that.size)
        throw new ContainerException(s"""cannot add Indexing because they have different sizes: (${this.size} vs ${that.size})""")
      else
        new Indexing[V](this.values zip that.values map {case (me, you) => me + you}: _*)

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      var i = 0
      while (i < values.size) {
        val v = values(i)
        v.fillWeighted(datum.asInstanceOf[v.Datum], weight)   // This type is ensured, but Scala doesn't recognize it.
        i += 1                                                // Also, Scala undergoes infinite recursion in a
      }                                                       // "foreach" version of this loop--- that's weird!
    }

    def toJsonFragment =
      JsonObject("type" -> JsonString(factory.name), "data" -> JsonArray(values.map(_.toJsonFragment): _*))

    override def toString() = s"Indexing[${values.head.toString}, size=${size}]"
    override def equals(that: Any) = that match {
      case that: Indexing[V] => this.values == that.values
      case _ => false
    }
    override def hashCode() = values.hashCode
  }

  //////////////////////////////////////////////////////////////// MixedTypeIndex/MixedTypeIndexed/MixedTypeIndexing

}
