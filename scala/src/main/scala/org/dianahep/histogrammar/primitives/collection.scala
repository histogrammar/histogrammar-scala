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
    val help = "Accumulate any number of containers of the SAME type and label them with strings. Every one is filled with every input datum."
    val detailedHelp = """Label(pairs: (String, V)*)"""

    def fixed[V <: Container[V]](entries: Double, pairs: (String, V)*) = new Labeled[V](entries, pairs: _*)
    def apply[V <: Container[V] with Aggregation](pairs: (String, V)*) = new Labeling[V](0.0, pairs: _*)

    def unapply[V <: Container[V]](x: Labeled[V]) = Some((x.entries, x.pairs))
    def unapply[V <: Container[V] with Aggregation](x: Labeling[V]) = Some((x.entries, x.pairs))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("entries", "type", "data")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val factory =
          get("type") match {
            case JsonString(factory) => Factory(factory)
            case x => throw new JsonFormatException(x, name + ".type")
          }

        get("data") match {
          case JsonObject(labelPairs @ _*) if (labelPairs.size >= 1) =>
            new Labeled[Container[_]](entries, labelPairs map {case (JsonString(label), sub) => label -> factory.fromJsonFragment(sub)}: _*)
          case x => throw new JsonFormatException(x, name + ".data")
        }

      case _ => throw new JsonFormatException(json, name)
    }
  }

  class Labeled[V <: Container[V]](val entries: Double, val pairs: (String, V)*) extends Container[Labeled[V]] {
    type Type = Labeled[V]
    def factory = Label

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (pairs.isEmpty)
      throw new ContainerException("at least one pair required")

    val pairsMap = pairs.toMap
    def size = pairs.size
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    def values: Iterable[Container[V]] = pairs.toIterable.map(_._2)
    def keySet: Set[String] = keys.toSet
    def apply(x: String) = pairsMap(x)
    def get(x: String) = pairsMap.get(x)
    def getOrElse(x: String, default: => V) = pairsMap.getOrElse(x, default)

    def zero = new Labeled[V](0.0, pairs map {case (k, v) => (k, v.zero)}: _*)
    def +(that: Labeled[V]) =
      if (this.keySet != that.keySet)
        throw new ContainerException(s"""cannot add Labeled because they have different keys:\n    ${this.keys.toArray.sorted.mkString(" ")}\nvs\n    ${that.keys.toArray.sorted.mkString(" ")}""")
      else
        new Labeled[V](
          this.entries + that.entries,
          this.pairs map {case (label, mysub) =>
            val yoursub = that.pairsMap(label)
            label -> (mysub + yoursub)
          }: _*)

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "type" -> JsonString(factory.name),
      "data" -> JsonObject(pairs map {case (label, sub) => label -> sub.toJsonFragment}: _*))

    override def toString() = s"Labeled[entries=$entries, ${pairs.head.toString}, size=${pairs.size}]"
    override def equals(that: Any) = that match {
      case that: Labeled[V] => this.entries === that.entries  &&  this.pairsMap == that.pairsMap
      case _ => false
    }
    override def hashCode() = (entries, pairsMap).hashCode
  }

  class Labeling[V <: Container[V] with Aggregation](var entries: Double, val pairs: (String, V)*) extends Container[Labeling[V]] with AggregationOnData {
    type Type = Labeling[V]
    type Datum = V#Datum
    def factory = Label

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (pairs.isEmpty)
      throw new ContainerException("at least one pair required")

    val pairsMap = pairs.toMap
    def size = pairs.size
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    def values: Iterable[V] = pairs.toIterable.map(_._2)
    def keySet: Set[String] = keys.toSet
    def apply(x: String) = pairsMap(x)
    def get(x: String) = pairsMap.get(x)
    def getOrElse(x: String, default: => V) = pairsMap.getOrElse(x, default)

    def zero = new Labeling[V](0.0, pairs map {case (k, v) => (k, v.zero)}: _*)
    def +(that: Labeling[V]) =
      if (this.keySet != that.keySet)
        throw new ContainerException(s"""cannot add Labeling because they have different keys:\n    ${this.keys.toArray.sorted.mkString(" ")}\nvs\n    ${that.keys.toArray.sorted.mkString(" ")}""")
      else
        new Labeling[V](
          this.entries + that.entries,
          this.pairs map {case (label, mysub) =>
            val yoursub = that.pairsMap(label)
            label -> (mysub + yoursub)
          }: _*)

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      entries += weight
      var i = 0
      while (i < size) {
        val (_, v) = pairs(i)
        v.fillWeighted(datum.asInstanceOf[v.Datum], weight)      // see notes in Indexing[V]
        i += 1
      }
    }

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "type" -> JsonString(factory.name),
      "data" -> JsonObject(pairs map {case (label, sub) => label -> sub.toJsonFragment}: _*))

    override def toString() = s"Labeling[entries=$entries, ${pairs.head.toString}, size=${pairs.size}]"
    override def equals(that: Any) = that match {
      case that: Labeled[V] => this.entries === that.entries  &&  this.pairsMap == that.pairsMap
      case _ => false
    }
    override def hashCode() = (entries, pairsMap).hashCode
  }

  //////////////////////////////////////////////////////////////// UntypedLabel/UntypedLabeled/UntypedLabeling
  object UntypedLabel extends Factory {
    val name = "UntypedLabel"
    val help = "Accumulate containers of any type except Count and label them with strings. Every one is filled with every input datum."
    val detailedHelp = """UntypedLabel(pairs: (String -> Container)*)"""

    def fixed(entries: Double, pairs: (String, Container[_])*) = new UntypedLabeled(entries, pairs: _*)
    def apply[DATUM](pairs: (String, Container[_] with AggregationOnData {type Datum = DATUM})*) = new UntypedLabeling(0.0, pairs: _*)

    def unapply(x: UntypedLabeled) = Some((x.entries, x.pairs))
    def unapply[DATUM](x: UntypedLabeling[DATUM]) = Some((x.entries, x.pairs))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("entries", "data")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        get("data") match {
          case JsonObject(subpairs @ _*) =>
            new UntypedLabeled(entries, subpairs map {
              case (JsonString(label), JsonObject(typedata @ _*)) if (typedata.keySet == Set("type", "data")) =>
                val subget = typedata.toMap
                  (subget("type"), subget("data")) match {
                  case (JsonString(factory), sub) => (label.toString, Factory(factory).fromJsonFragment(sub))
                  case (_, x) => throw new JsonFormatException(x, name + s""".data "$label"""")
                }
              case (_, x) => throw new JsonFormatException(x, name + s".data")
            }: _*)

            case x => throw new JsonFormatException(x, name)
        }

      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def combine[CONTAINER <: Container[CONTAINER]](one: Container[_], two: Container[_]) =
      one.asInstanceOf[CONTAINER] + two.asInstanceOf[CONTAINER]
  }

  class UntypedLabeled(val entries: Double, val pairs: (String, Container[_])*) extends Container[UntypedLabeled] {
    type Type = UntypedLabeled
    def factory = UntypedLabel

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    val pairsMap = pairs.toMap
    def size = pairs.size
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    def values: Iterable[Container[_]] = pairs.toIterable.map(_._2)
    def keySet: Set[String] = keys.toSet
    def apply(x: String) = pairsMap(x)
    def get(x: String) = pairsMap.get(x)
    def getOrElse(x: String, default: => Container[_]) = pairsMap.getOrElse(x, default)

    def zero = new UntypedLabeled(0.0, pairs map {case (k, v) => (k, v.zero.asInstanceOf[Container[_]])}: _*)
    def +(that: UntypedLabeled) =
      if (this.keySet != that.keySet)
        throw new ContainerException(s"""cannot add UntypedLabeled because they have different keys:\n    ${this.keys.toArray.sorted.mkString(" ")}\nvs\n    ${that.keys.toArray.sorted.mkString(" ")}""")
      else
        new UntypedLabeled(
          this.entries + that.entries,
          this.pairs.map({case (key, mysub) =>
            val yoursub = that.pairsMap(key)
            if (mysub.factory != yoursub.factory)
              throw new ContainerException(s"""cannot add UntypedLabeled because key "$key" has a different type in the two maps: ${mysub.factory.name} vs ${yoursub.factory.name}""")
            (key, UntypedLabel.combine(mysub, yoursub))
          }): _*)

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "data" -> JsonObject(pairs map {case (key, sub) =>
        key -> JsonObject("type" -> JsonString(sub.factory.name), "data" -> sub.toJsonFragment)
      }: _*))

    override def toString() = s"UntypedLabeled[entries=$entries, size=${pairs.size}]"
    override def equals(that: Any) = that match {
      case that: UntypedLabeled => this.entries === that.entries  &&  this.pairsMap == that.pairsMap
      case _ => false
    }
    override def hashCode() = (entries, pairsMap).hashCode
  }

  class UntypedLabeling[DATUM](var entries: Double, val pairs: (String, Container[_] with AggregationOnData {type Datum = DATUM})*) extends Container[UntypedLabeling[DATUM]] with AggregationOnData {
    type Type = UntypedLabeled
    type Datum = DATUM
    def factory = UntypedLabel

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    val pairsMap = pairs.toMap
    def size = pairs.size
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    def values: Iterable[Container[_]] = pairs.toIterable.map(_._2)
    def keySet: Set[String] = keys.toSet
    def apply(x: String) = pairsMap(x)
    def get(x: String) = pairsMap.get(x)
    def getOrElse(x: String, default: => Container[_]) = pairsMap.getOrElse(x, default)

    def zero = new UntypedLabeling[DATUM](0.0, pairs map {case (k, v) => (k, v.zero.asInstanceOf[Container[_] with AggregationOnData {type Datum = DATUM}])}: _*)
    def +(that: UntypedLabeling[DATUM]) =
      if (this.keySet != that.keySet)
        throw new ContainerException(s"""cannot add UntypedLabeling because they have different keys:\n    ${this.keys.toArray.sorted.mkString(" ")}\nvs\n    ${that.keys.toArray.sorted.mkString(" ")}""")
      else
        new UntypedLabeling[DATUM](
          this.entries + that.entries,
          this.pairs.map({case (key, mysub) =>
            val yoursub = that.pairsMap(key)
            if (mysub.factory != yoursub.factory)
              throw new ContainerException(s"""cannot add UntypedLabeling because key "$key" has a different type in the two maps: ${mysub.factory.name} vs ${yoursub.factory.name}""")
            (key, UntypedLabel.combine(mysub, yoursub))
          }): _*)

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      entries += weight
      var i = 0
      while (i < size) {
        val (_, v) = pairs(i)
        v.fillWeighted(datum.asInstanceOf[v.Datum], weight)      // see notes in Indexing[V]
        i += 1
      }
    }

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "data" -> JsonObject(pairs map {case (key, sub) =>
        key -> JsonObject("type" -> JsonString(sub.factory.name), "data" -> sub.toJsonFragment)
      }: _*))

    override def toString() = s"UntypedLabeling[entries=$entries, size=${pairs.size}]"
    override def equals(that: Any) = that match {
      case that: UntypedLabeling[DATUM] => this.entries === that.entries  &&  this.pairsMap == that.pairsMap
      case _ => false
    }
    override def hashCode() = (entries, pairsMap).hashCode
  }

  //////////////////////////////////////////////////////////////// Index/Indexed/Indexing

  object Index extends Factory {
    val name = "Index"
    val help = "Accumulate any number of containers of the SAME type anonymously in a list. Every one is filled with every input datum."
    val detailedHelp = """"""

    def fixed[V <: Container[V]](entries: Double, values: V*) = new Indexed[V](entries, values: _*)
    def apply[V <: Container[V] with Aggregation](values: V*) = new Indexing[V](0.0, values: _*)

    def unapply[V <: Container[V]](x: Indexed[V]) = Some((x.entries, x.values))
    def unapply[V <: Container[V] with Aggregation](x: Indexing[V]) = Some((x.entries, x.values))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("entries", "type", "data")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val factory =
          get("type") match {
            case JsonString(factory) => Factory(factory)
            case x => throw new JsonFormatException(x, name + ".type")
          }

        get("data") match {
          case JsonArray(values @ _*) if (values.size >= 1) =>
            new Indexed[Container[_]](entries, values.map(factory.fromJsonFragment(_)): _*)
          case x =>
            throw new JsonFormatException(x, name + ".data")
        }

      case _ => throw new JsonFormatException(json, name)
    }
  }

  class Indexed[V <: Container[V]](val entries: Double, val values: V*) extends Container[Indexed[V]] {
    type Type = Indexed[V]
    def factory = Index

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (values.isEmpty)
      throw new ContainerException("at least one element required")

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

    def zero = new Indexed[V](0.0, values.map(_.zero): _*)
    def +(that: Indexed[V]) =
      if (this.size != that.size)
        throw new ContainerException(s"""cannot add Indexed because they have different sizes: (${this.size} vs ${that.size})""")
      else
        new Indexed[V](this.entries + that.entries, this.values zip that.values map {case(me, you) => me + you}: _*)

    // def fix = this
    def toJsonFragment = JsonObject("entries" -> JsonFloat(entries), "type" -> JsonString(factory.name), "data" -> JsonArray(values.map(_.toJsonFragment): _*))

    override def toString() = s"Indexed[entries=$entries, ${values.head.toString}, size=${size}]"
    override def equals(that: Any) = that match {
      case that: Indexed[V] => this.entries === that.entries  &&  this.values == that.values
      case _ => false
    }
    override def hashCode() = (entries, values).hashCode
  }

  class Indexing[V <: Container[V] with Aggregation](var entries: Double, val values: V*) extends Container[Indexing[V]] with AggregationOnData {
    type Type = Indexing[V]
    type Datum = V#Datum
    def factory = Index

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (values.isEmpty)
      throw new ContainerException("at least one element required")

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

    def zero = new Indexing[V](0.0, values.map(_.zero): _*)
    def +(that: Indexing[V]) =
      if (this.size != that.size)
        throw new ContainerException(s"""cannot add Indexing because they have different sizes: (${this.size} vs ${that.size})""")
      else
        new Indexing[V](this.entries + that.entries, this.values zip that.values map {case (me, you) => me + you}: _*)

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      entries += weight
      var i = 0
      while (i < size) {
        val v = values(i)
        v.fillWeighted(datum.asInstanceOf[v.Datum], weight)   // This type is ensured, but Scala doesn't recognize it.
        i += 1                                                // Also, Scala undergoes infinite recursion in a
      }                                                       // "foreach" version of this loop--- that's weird!
    }

    def toJsonFragment = JsonObject("entries" -> JsonFloat(entries), "type" -> JsonString(factory.name), "data" -> JsonArray(values.map(_.toJsonFragment): _*))

    override def toString() = s"Indexing[entries=$entries, ${values.head.toString}, size=${size}]"
    override def equals(that: Any) = that match {
      case that: Indexing[V] => this.entries === that.entries  &&  this.values == that.values
      case _ => false
    }
    override def hashCode() = (entries, values).hashCode
  }

  //////////////////////////////////////////////////////////////// Branch/Branched/Branching

  object Branch extends Factory {
    def name = "Branch"
    def help = "Accumulate containers of DIFFERENT types, indexed by i0 through i9. Every one is filled with every input datum."
    def detailedHelp = "Branch(container0, container1, ...)"

    def fixed[C0 <: Container[C0]](entries: Double, i0: C0) = new Branched(entries, i0, BranchedNil)
    def fixed[C0 <: Container[C0], C1 <: Container[C1]](entries: Double, i0: C0, i1: C1) = new Branched(entries, i0, new Branched(entries, i1, BranchedNil))
    def fixed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2]](entries: Double, i0: C0, i1: C1, i2: C2) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, BranchedNil)))
    def fixed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](entries: Double, i0: C0, i1: C1, i2: C2, i3: C3) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, new Branched(entries, i3, BranchedNil))))
    def fixed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](entries: Double, i0: C0, i1: C1, i2: C2, i3: C3, i4: C4) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, new Branched(entries, i3, new Branched(entries, i4, BranchedNil)))))
    def fixed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](entries: Double, i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, new Branched(entries, i3, new Branched(entries, i4, new Branched(entries, i5, BranchedNil))))))
    def fixed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](entries: Double, i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, new Branched(entries, i3, new Branched(entries, i4, new Branched(entries, i5, new Branched(entries, i6, BranchedNil)))))))
    def fixed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](entries: Double, i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, new Branched(entries, i3, new Branched(entries, i4, new Branched(entries, i5, new Branched(entries, i6, new Branched(entries, i7, BranchedNil))))))))
    def fixed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](entries: Double, i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7, i8: C8) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, new Branched(entries, i3, new Branched(entries, i4, new Branched(entries, i5, new Branched(entries, i6, new Branched(entries, i7, new Branched(entries, i8, BranchedNil)))))))))
    def fixed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](entries: Double, i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7, i8: C8, i9: C9) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, new Branched(entries, i3, new Branched(entries, i4, new Branched(entries, i5, new Branched(entries, i6, new Branched(entries, i7, new Branched(entries, i8, new Branched(entries, i9, BranchedNil))))))))))

    def apply[C0 <: Container[C0] with Aggregation](i0: C0) = new Branching(0.0, i0, BranchingNil)
    def apply[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation](i0: C0, i1: C1)(implicit e01: C0 Compatible C1) = new Branching(0.0, i0, new Branching(0.0, i1, BranchingNil))
    def apply[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation](i0: C0, i1: C1, i2: C2)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, BranchingNil)))
    def apply[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, BranchingNil))))
    def apply[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, BranchingNil)))))
    def apply[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4, e05: C0 Compatible C5) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, new Branching(0.0, i5, BranchingNil))))))
    def apply[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4, e05: C0 Compatible C5, e06: C0 Compatible C6) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, new Branching(0.0, i5, new Branching(0.0, i6, BranchingNil)))))))
    def apply[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation, C7 <: Container[C7] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4, e05: C0 Compatible C5, e06: C0 Compatible C6, e07: C0 Compatible C7) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, new Branching(0.0, i5, new Branching(0.0, i6, new Branching(0.0, i7, BranchingNil))))))))
    def apply[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation, C7 <: Container[C7] with Aggregation, C8 <: Container[C8] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7, i8: C8)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4, e05: C0 Compatible C5, e06: C0 Compatible C6, e07: C0 Compatible C7, e08: C0 Compatible C8) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, new Branching(0.0, i5, new Branching(0.0, i6, new Branching(0.0, i7, new Branching(0.0, i8, BranchingNil)))))))))
    def apply[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation, C7 <: Container[C7] with Aggregation, C8 <: Container[C8] with Aggregation, C9 <: Container[C9] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7, i8: C8, i9: C9)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4, e05: C0 Compatible C5, e06: C0 Compatible C6, e07: C0 Compatible C7, e08: C0 Compatible C8, e09: C0 Compatible C9) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, new Branching(0.0, i5, new Branching(0.0, i6, new Branching(0.0, i7, new Branching(0.0, i8, new Branching(0.0, i9, BranchingNil))))))))))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("entries", "data")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        get("data") match {
          case JsonArray(values @ _*) if (values.size >= 1) =>
            var backwards: BranchedList = BranchedNil

            values.zipWithIndex.toList foreach {
              case (JsonObject((JsonString(factory), sub)), _) =>
                val item = Factory(factory).fromJsonFragment(sub).asInstanceOf[C forSome {type C <: Container[C]}]
                backwards = new Branched(entries, item, backwards)

              case (x, i) => throw new JsonFormatException(x, name + s".data $i")
            }

            // we've loaded it backwards, so reverse the order before returning it
            var out: BranchedList = BranchedNil
            while (!backwards.isEmpty) {
              val list = backwards.asInstanceOf[Branched[C forSome {type C <: Container[C]}, BranchedList]]
              out = new Branched(list.entries, list.head, out)
              backwards = list.tail
            }
            out.asInstanceOf[Container[_]]

          case x => throw new JsonFormatException(x, name + ".data")
        }

      case _ => throw new JsonFormatException(json, name)
    }
  }

  sealed trait BranchedList {
    def values: List[Container[_]]
    def isEmpty: Boolean
    def size: Int
    def zero: BranchedList
  }

  object BranchedNil extends BranchedList {
    def values: List[Container[_]] = Nil
    def isEmpty: Boolean = true
    def size: Int = 0
    def zero = this
  }

  class Branched[HEAD <: Container[HEAD], TAIL <: BranchedList](val entries: Double, val head: HEAD, val tail: TAIL) extends Container[Branched[HEAD, TAIL]] with BranchedList {
    type Type = Branched[HEAD, TAIL]
    def factory = Branch

    def values: List[Container[_]] = head :: tail.values
    def isEmpty: Boolean = false
    def size: Int = 1 + tail.size

    def zero = new Branched[HEAD, TAIL](0.0, head.zero, tail.zero.asInstanceOf[TAIL])
    def +(that: Branched[HEAD, TAIL]) = new Branched[HEAD, TAIL](this.entries + that.entries, this.head + that.head, this.tail)

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "data" -> JsonArray(values.map(x => JsonObject(JsonString(x.factory.name) -> x.toJsonFragment)): _*))

    override def toString() = "Branched[entries=$entries, " + values.mkString(", ") + "]"

    override def equals(that: Any) = that match {
      case that: Branched[_, _] => this.entries === that.entries  &&  this.head == that.head  &&  this.tail == that.tail
      case _ => false
    }
    override def hashCode() = (entries, values).hashCode
  }

  sealed trait BranchingList {
    def values: List[Container[_]]
    def isEmpty: Boolean
    def size: Int
    def zero: BranchingList
  }

  object BranchingNil extends BranchingList {
    def values: List[Container[_]] = Nil
    def isEmpty: Boolean = true
    def size: Int = 0
    def zero = this
  }

  class Branching[HEAD <: Container[HEAD] with Aggregation, TAIL <: BranchingList](var entries: Double, val head: HEAD, val tail: TAIL) extends Container[Branching[HEAD, TAIL]] with AggregationOnData with BranchingList {
    type Type = Branching[HEAD, TAIL]
    type Datum = head.Datum
    def factory = Branch

    def values: List[Container[_]] = head :: tail.values
    def isEmpty: Boolean = false
    def size: Int = 1 + tail.size

    def zero = new Branching[HEAD, TAIL](0.0, head.zero, tail.zero.asInstanceOf[TAIL])
    def +(that: Branching[HEAD, TAIL]) = new Branching[HEAD, TAIL](this.entries + that.entries, this.head + that.head, this.tail)

    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double) {
      entries += weight
      head.fillWeighted(datum, weight)
      tail match {
        case x: Aggregation => x.fillWeighted(datum.asInstanceOf[x.Datum], weight)
        case _ =>
      }
    }

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "data" -> JsonArray(values.map(x => JsonObject(JsonString(x.factory.name) -> x.toJsonFragment)): _*))

    override def toString() = "Branching[entries=$entries, " + values.mkString(", ") + "]"

    override def equals(that: Any) = that match {
      case that: Branching[_, _] => this.entries === that.entries  &&  this.head == that.head  &&  this.tail == that.tail
      case _ => false
    }
    override def hashCode() = (entries, values).hashCode
  }

}
