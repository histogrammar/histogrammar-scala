package org.dianahep

import scala.language.existentials

import org.dianahep.histogrammar.json._

package histogrammar {
  trait Compatible[-X, -Y]
  object Compatible {
    implicit object BothAreCounting extends Compatible[Counting, Counting]
    implicit object XIsCounting extends Compatible[Counting, AggregationOnData]
    implicit object YIsCounting extends Compatible[AggregationOnData, Counting]
    implicit def dataAreCompatible[X <: AggregationOnData, Y <: AggregationOnData](implicit evidence: X#Datum =:= Y#Datum) = new Compatible[X, Y] {}
  }

  //////////////////////////////////////////////////////////////// Label/Labeled/Labeling
  object Label extends Factory {
    val name = "Label"
    val help = "Accumulate containers of any type except Count and label them with strings. Every one is filled with every input datum."
    val detailedHelp = """Label(pairs: (String -> Container)*)"""

    def fixed(pairs: (String, Container[_])*) = new Labeled(pairs: _*)
    def apply[DATUM](pairs: (String, Container[_] with AggregationOnData {type Datum = DATUM})*) = new Labeling(pairs: _*)

    def unapplySeq(x: Labeled) = Some(x.pairs)
    def unapplySeq[DATUM](x: Labeling[DATUM]) = Some(x.pairs)

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) =>
        new Labeled(pairs map {
          case (JsonString(label), JsonObject(typedata @ _*)) if (typedata.keySet == Set("type", "data")) =>
            val get = typedata.toMap
            (get("type"), get("data")) match {
              case (JsonString(factory), sub) => (label.toString, Factory(factory).fromJsonFragment(sub))
              case _ => throw new JsonFormatException(json, name + s""" label "$label"""")
            }
          case _ => throw new JsonFormatException(json, name + s" label")
        }: _*)

      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def combine[CONTAINER <: Container[CONTAINER]](one: Container[_], two: Container[_]) =
      one.asInstanceOf[CONTAINER] + two.asInstanceOf[CONTAINER]
  }

  class Labeled(val pairs: (String, Container[_])*) extends Container[Labeled] {
    def factory = Label

    val pairsMap = pairs.toMap
    def size = pairs.size
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    def values: Iterable[Container[_]] = pairs.toIterable.map(_._2)
    def keySet: Set[String] = keys.toSet
    def apply(x: String) = pairsMap(x)
    def get(x: String) = pairsMap.get(x)
    def getOrElse(x: String, default: => Container[_]) = pairsMap.getOrElse(x, default)

    def +(that: Labeled) =
      if (this.keySet != that.keySet)
        throw new ContainerException(s"""cannot add Labeled because they have different keys:\n    ${this.keys.toArray.sorted.mkString(" ")}\nvs\n    ${that.keys.toArray.sorted.mkString(" ")}""")
      else
        new Labeled(this.pairs.map({case (key, mysub) =>
          val yoursub = that.pairsMap(key)
          if (mysub.factory != yoursub.factory)
            throw new ContainerException(s"""cannot add Labeled because key "$key" has a different type in the two maps: ${mysub.factory.name} vs ${yoursub.factory.name}""")
          (key, Label.combine(mysub, yoursub))
        }): _*)

    def toJsonFragment = JsonObject(pairs map {case (key, sub) =>
      key -> JsonObject("type" -> JsonString(sub.factory.name), "data" -> sub.toJsonFragment)
    }: _*)

    override def toString() = s"Labeled[size=${pairs.size}]"
    override def equals(that: Any) = that match {
      case that: Labeled => this.pairsMap == that.pairsMap
      case _ => false
    }
    override def hashCode() = pairsMap.hashCode
  }

  class Labeling[DATUM](val pairs: (String, Container[_] with AggregationOnData {type Datum = DATUM})*) extends Container[Labeling[DATUM]] with AggregationOnData {
    type Datum = DATUM
    def factory = Label

    val pairsMap = pairs.toMap
    def size = pairs.size
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    def values: Iterable[Container[_]] = pairs.toIterable.map(_._2)
    def keySet: Set[String] = keys.toSet
    def apply(x: String) = pairsMap(x)
    def get(x: String) = pairsMap.get(x)
    def getOrElse(x: String, default: => Container[_]) = pairsMap.getOrElse(x, default)

    def +(that: Labeling[DATUM]) =
      if (this.keySet != that.keySet)
        throw new ContainerException(s"""cannot add Labeling because they have different keys:\n    ${this.keys.toArray.sorted.mkString(" ")}\nvs\n    ${that.keys.toArray.sorted.mkString(" ")}""")
      else
        new Labeling[DATUM](this.pairs.map({case (key, mysub) =>
          val yoursub = that.pairsMap(key)
          if (mysub.factory != yoursub.factory)
            throw new ContainerException(s"""cannot add Labeling because key "$key" has a different type in the two maps: ${mysub.factory.name} vs ${yoursub.factory.name}""")
          (key, Label.combine(mysub, yoursub))
        }): _*)

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      var i = 0
      while (i < size) {
        val (_, v) = pairs(i)
        v.fillWeighted(datum.asInstanceOf[v.Datum], weight)      // see notes in Indexing[V]
        i += 1
      }
    }

    def toJsonFragment = JsonObject(pairs map {case (key, sub) =>
      key -> JsonObject("type" -> JsonString(sub.factory.name), "data" -> sub.toJsonFragment)
    }: _*)

    override def toString() = s"Labeling[size=${pairs.size}]"
    override def equals(that: Any) = that match {
      case that: Labeling[DATUM] => this.pairsMap == that.pairsMap
      case _ => false
    }
    override def hashCode() = pairsMap.hashCode
  }

  //////////////////////////////////////////////////////////////// Index/Indexed/Indexing

  object Index extends Factory {
    val name = "Index"
    val help = "Accumulate any number of containers of the SAME type anonymously in a list. Every one is filled with every input datum."
    val detailedHelp = """"""

    def fixed[V <: Container[V]](values: V*) = new Indexed[V](values: _*)
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

  class Indexing[V <: Container[V] with Aggregation](val values: V*) extends Container[Indexing[V]] with AggregationOnData {
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
      while (i < size) {
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

  //////////////////////////////////////////////////////////////// Branch/Branched/Branching

  object Branch extends Factory {
    def name = "Branch"
    def help = "Accumulate containers of DIFFERENT types, indexed by i0 through i9. Every one is filled with every input datum."
    def detailedHelp = "Branch(container0, container1, ...)"

    def fixed[C0 <: Container[C0]](i0: C0) = new Branched(i0, BranchedNil)
    def fixed[C0 <: Container[C0], C1 <: Container[C1]](i0: C0, i1: C1) = new Branched(i0, new Branched(i1, BranchedNil))
    def fixed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2]](i0: C0, i1: C1, i2: C2) = new Branched(i0, new Branched(i1, new Branched(i2, BranchedNil)))
    def fixed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](i0: C0, i1: C1, i2: C2, i3: C3) = new Branched(i0, new Branched(i1, new Branched(i2, new Branched(i3, BranchedNil))))
    def fixed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4) = new Branched(i0, new Branched(i1, new Branched(i2, new Branched(i3, new Branched(i4, BranchedNil)))))
    def fixed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5) = new Branched(i0, new Branched(i1, new Branched(i2, new Branched(i3, new Branched(i4, new Branched(i5, BranchedNil))))))
    def fixed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6) = new Branched(i0, new Branched(i1, new Branched(i2, new Branched(i3, new Branched(i4, new Branched(i5, new Branched(i6, BranchedNil)))))))
    def fixed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7) = new Branched(i0, new Branched(i1, new Branched(i2, new Branched(i3, new Branched(i4, new Branched(i5, new Branched(i6, new Branched(i7, BranchedNil))))))))
    def fixed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7, i8: C8) = new Branched(i0, new Branched(i1, new Branched(i2, new Branched(i3, new Branched(i4, new Branched(i5, new Branched(i6, new Branched(i7, new Branched(i8, BranchedNil)))))))))
    def fixed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7, i8: C8, i9: C9) = new Branched(i0, new Branched(i1, new Branched(i2, new Branched(i3, new Branched(i4, new Branched(i5, new Branched(i6, new Branched(i7, new Branched(i8, new Branched(i9, BranchedNil))))))))))

    def apply[C0 <: Container[C0] with Aggregation](i0: C0) = new Branching(i0, BranchingNil)
    def apply[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation](i0: C0, i1: C1)(implicit e01: C0 Compatible C1) = new Branching(i0, new Branching(i1, BranchingNil))
    def apply[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation](i0: C0, i1: C1, i2: C2)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2) = new Branching(i0, new Branching(i1, new Branching(i2, BranchingNil)))
    def apply[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3) = new Branching(i0, new Branching(i1, new Branching(i2, new Branching(i3, BranchingNil))))
    def apply[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4) = new Branching(i0, new Branching(i1, new Branching(i2, new Branching(i3, new Branching(i4, BranchingNil)))))
    def apply[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4, e05: C0 Compatible C5) = new Branching(i0, new Branching(i1, new Branching(i2, new Branching(i3, new Branching(i4, new Branching(i5, BranchingNil))))))
    def apply[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4, e05: C0 Compatible C5, e06: C0 Compatible C6) = new Branching(i0, new Branching(i1, new Branching(i2, new Branching(i3, new Branching(i4, new Branching(i5, new Branching(i6, BranchingNil)))))))
    def apply[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation, C7 <: Container[C7] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4, e05: C0 Compatible C5, e06: C0 Compatible C6, e07: C0 Compatible C7) = new Branching(i0, new Branching(i1, new Branching(i2, new Branching(i3, new Branching(i4, new Branching(i5, new Branching(i6, new Branching(i7, BranchingNil))))))))
    def apply[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation, C7 <: Container[C7] with Aggregation, C8 <: Container[C8] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7, i8: C8)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4, e05: C0 Compatible C5, e06: C0 Compatible C6, e07: C0 Compatible C7, e08: C0 Compatible C8) = new Branching(i0, new Branching(i1, new Branching(i2, new Branching(i3, new Branching(i4, new Branching(i5, new Branching(i6, new Branching(i7, new Branching(i8, BranchingNil)))))))))
    def apply[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation, C7 <: Container[C7] with Aggregation, C8 <: Container[C8] with Aggregation, C9 <: Container[C9] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7, i8: C8, i9: C9)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4, e05: C0 Compatible C5, e06: C0 Compatible C6, e07: C0 Compatible C7, e08: C0 Compatible C8, e09: C0 Compatible C9) = new Branching(i0, new Branching(i1, new Branching(i2, new Branching(i3, new Branching(i4, new Branching(i5, new Branching(i6, new Branching(i7, new Branching(i8, new Branching(i9, BranchingNil))))))))))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonArray(values @ _*) if (values.size >= 1) =>
        var backwards: BranchedList = BranchedNil

        values.zipWithIndex.toList foreach {
          case (JsonObject((JsonString(factory), sub)), _) =>
            val item = Factory(factory).fromJsonFragment(sub).asInstanceOf[C forSome {type C <: Container[C]}]
            backwards = new Branched(item, backwards)

          case (_, i) => throw new ContainerException(s"Branched $i")
        }

        var out: BranchedList = BranchedNil
        while (!backwards.isEmpty) {
          val list = backwards.asInstanceOf[Branched[C forSome {type C <: Container[C]}, BranchedList]]
          out = new Branched(list.head, out)
          backwards = list.tail
        }
        out.asInstanceOf[Container[_]]

      case _ => throw new ContainerException("Branched")
    }
  }

  sealed trait BranchedList {
    def values: List[Container[_]]
    def isEmpty: Boolean
    def size: Int
  }

  object BranchedNil extends BranchedList {
    def values: List[Container[_]] = Nil
    def isEmpty: Boolean = true
    def size: Int = 0
  }

  class Branched[HEAD <: Container[HEAD], TAIL <: BranchedList](val head: HEAD, val tail: TAIL) extends Container[Branched[HEAD, TAIL]] with BranchedList {
    type Type = Branched[HEAD, TAIL]
    def factory = Branch

    def values: List[Container[_]] = head :: tail.values
    def isEmpty: Boolean = false
    def size: Int = 1 + tail.size

    def +(that: Branched[HEAD, TAIL]) = new Branched[HEAD, TAIL](this.head + that.head, this.tail)

    def toJsonFragment = JsonArray(values.map(x => JsonObject(JsonString(x.factory.name) -> x.toJsonFragment)): _*)

    override def toString() = "Branched[" + values.mkString(", ") + "]"

    override def equals(that: Any) = that match {
      case other: Branched[_, _] => this.head == other.head  &&  this.tail == other.tail
      case _ => false
    }
    override def hashCode() = values.hashCode
  }

  sealed trait BranchingList {
    def values: List[Container[_]]
    def isEmpty: Boolean
    def size: Int
  }

  object BranchingNil extends BranchingList {
    def values: List[Container[_]] = Nil
    def isEmpty: Boolean = true
    def size: Int = 0
  }

  class Branching[HEAD <: Container[HEAD] with Aggregation, TAIL <: BranchingList](val head: HEAD, val tail: TAIL) extends Container[Branching[HEAD, TAIL]] with AggregationOnData with BranchingList {
    type Type = Branching[HEAD, TAIL]
    type Datum = head.Datum
    def factory = Branch

    def values: List[Container[_]] = head :: tail.values
    def isEmpty: Boolean = false
    def size: Int = 1 + tail.size

    def +(that: Branching[HEAD, TAIL]) = new Branching[HEAD, TAIL](this.head + that.head, this.tail)

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      head.fillWeighted(datum, weight)
      tail match {
        case x: Aggregation => x.fillWeighted(datum.asInstanceOf[x.Datum], weight)
        case _ =>
      }
    }

    def toJsonFragment = JsonArray(values.map(x => JsonObject(JsonString(x.factory.name) -> x.toJsonFragment)): _*)

    override def toString() = "Branching[" + values.mkString(", ") + "]"

    override def equals(that: Any) = that match {
      case other: Branching[_, _] => this.head == other.head  &&  this.tail == other.tail
      case _ => false
    }
    override def hashCode() = values.hashCode
  }

}
