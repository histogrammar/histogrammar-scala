// Copyright 2016 Jim Pivarski
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// 
//     http://www.apache.org/licenses/LICENSE-2.0
// 
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package org.dianahep

import scala.language.existentials

import org.dianahep.histogrammar.json._

package histogrammar {
  private[histogrammar] trait Compatible[-X, -Y]
  private[histogrammar] object Compatible {
    implicit object BothAreCounting extends Compatible[Counting, Counting]
    implicit object XIsCounting extends Compatible[Counting, AggregationOnData]
    implicit object YIsCounting extends Compatible[AggregationOnData, Counting]
    implicit def dataAreCompatible[X <: AggregationOnData, Y <: AggregationOnData](implicit evidence: X#Datum =:= Y#Datum) = new Compatible[X, Y] {}
  }

  //////////////////////////////////////////////////////////////// Cut/Cutted/Cutting

  /** Accumulate an aggregator for data that satisfy a cut (or more generally, a weighting).
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Cutting]] and immutable [[org.dianahep.histogrammar.Cutted]] (sic) objects.
    */
  object Cut extends Factory {
    val name = "Cut"
    val help = "Accumulate an aggregator for data that satisfy a cut (or more generally, a weighting)."
    val detailedHelp = """Cut(selection: Selection[DATUM], value: V, name: Option[String] = None)"""

    /** Create an immutable [[org.dianahep.histogrammar.Cutted]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights without the cut applied).
      * @param value Aggregator that accumulated values that passed the cut.
      * @param name Optional name for bookkeeping.
      */
    def ed[V <: Container[V]](entries: Double, value: V, name: Option[String] = None) = new Cutted[V](entries, value, name)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Limiting]].
      * 
      * @param selection Boolean or non-negative function that cuts or weights entries.
      * @param value Aggregator to accumulate for values that pass `selection`.
      * @param name Optional name for bookkeeping.
      */
    def apply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](selection: Selection[DATUM], value: V, name: Option[String] = None) = new Cutting[DATUM, V](0.0, selection, value, name)

    /** Synonym for `apply`. */
    def ing[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](selection: Selection[DATUM], value: V, name: Option[String] = None) = apply(selection, value, name)

    /** Use [[org.dianahep.histogrammar.Cutted]] in Scala pattern-matching. */
    def unapply[V <: Container[V]](x: Cutted[V]) = Some(x.value)
    /** Use [[org.dianahep.histogrammar.Cutting]] in Scala pattern-matching. */
    def unapply[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}](x: Cutting[DATUM, V]) = Some(x.value)

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("entries", "type", "data")  ||  pairs.keySet == Set("entries", "type", "data", "name")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val factory = get("type") match {
          case JsonString(x) => Factory(x)
          case x => throw new JsonFormatException(x, name + ".type")
        }

        val value = factory.fromJsonFragment(get("data"))

        val n = get.getOrElse("name", JsonNull) match {
          case JsonNull => None
          case JsonString(x) => Some(x)
          case x => throw new JsonFormatException(x, name + ".name")
        }

        new Cutted[Container[_]](entries, value, n)

      case _ => throw new JsonFormatException(json, name)
    }
  }

  /** An accumulated aggregator of data that passed the cut.
    * 
    * @param entries Weighted number of entries (sum of all observed weights without the cut applied).
    * @param value Aggregator that accumulated values that passed the cut.
    * @param name Optional name for bookkeeping.
    */
  class Cutted[V <: Container[V]] private[histogrammar](val entries: Double, val value: V, val name: Option[String]) extends Container[Cutted[V]] {
    type Type = Cutted[V]
    def factory = Cut

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def zero = new Cutted[V](0.0, value.zero, name)
    def +(that: Cutted[V]) = {
      if (this.name != that.name)
        throw new ContainerException(s"cannot add Cutted because they have different names (${this.name} vs ${that.name})")
      new Cutted[V](this.entries + that.entries, this.value + that.value, name)
    }

    def toJsonFragment = JsonObject(
      (Seq("entries" -> JsonFloat(entries), "type" -> JsonString(value.factory.name), "data" -> value.toJsonFragment) ++ name.map("name" -> JsonString(_))): _*)

    override def toString() = s"""Cutted[$value${if (name.isEmpty) "" else ", name=\"" + name.get + "\""}]"""
    override def equals(that: Any) = that match {
      case that: Cutted[V] => this.entries === that.entries  &&  this.value == that.value  &&  this.name == that.name
      case _ => false
    }
    override def hashCode() = (entries, value, name).hashCode
  }

  /** Accumulating an aggregator of data that passes a cut.
    * 
    * @param entries Weighted number of entries (sum of all observed weights without the cut applied).
    * @param selection Boolean or non-negative function that cuts or weights entries.
    * @param value Aggregator to accumulate values that pass the cut.
    * @param name Optional name for bookkeeping.
    */
  class Cutting[DATUM, V <: Container[V] with Aggregation{type Datum >: DATUM}] private[histogrammar](var entries: Double, val selection: Selection[DATUM], val value: V, val name: Option[String]) extends Container[Cutting[DATUM, V]] with AggregationOnData {
    type Type = Cutting[DATUM, V]
    type Datum = DATUM
    def factory = Cut

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def zero = new Cutting[DATUM, V](0.0, selection, value.zero, name)
    def +(that: Cutting[DATUM, V]) = {
      if (this.name != that.name)
        throw new ContainerException(s"cannot add Cutting because they have different names (${this.name} vs ${that.name})")
      new Cutting[DATUM, V](this.entries + that.entries, this.selection, this.value + that.value, this.name)
    }

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      entries += weight
      val w = weight * selection(datum)
      if (w > 0.0) {
        value.fill(datum, w)
      }
    }

    def toJsonFragment = JsonObject(
      (Seq("entries" -> JsonFloat(entries), "type" -> JsonString(value.factory.name), "data" -> value.toJsonFragment) ++ name.map("name" -> JsonString(_))): _*)

    override def toString() = s"""Cutting[$value${if (name.isEmpty) "" else ", name=\"" + name.get + "\""}]"""
    override def equals(that: Any) = that match {
      case that: Cutting[DATUM, V] => this.entries === that.entries  &&  this.selection == that.selection  &&  this.value == that.value  &&  this.name == that.name
      case _ => false
    }
    override def hashCode() = (entries, selection, value, name).hashCode
  }

  //////////////////////////////////////////////////////////////// Limit/Limited/Limiting

  /** Accumulate an aggregator until its number of entries reaches a predefined limit.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Limiting]] and immutable [[org.dianahep.histogrammar.Limited]] objects.
    */
  object Limit extends Factory {
    val name = "Limit"
    val help = "Accumulate an aggregator until its number of entries reaches a predefined limit."
    val detailedHelp = """Limit(limit: Double, value: V)"""

    /** Create an immutable [[org.dianahep.histogrammar.Limited]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param limit Maximum sum of weights to keep; above this, `value` goes to `None`.
      * @param contentType Name of the Factory for `value`.
      * @param value Some aggregator or `None`.
      */
    def ed[V <: Container[V]](entries: Double, limit: Double, contentType: String, value: Option[V]) = new Limited[V](entries, limit, contentType, value)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Limiting]].
      * 
      * @param value Aggregator to apply a limit to.
      * @param limit Maximum sum of weights to keep; above this, `value` goes to `None`.
      */
    def apply[DATUM, V <: Container[V] with Aggregation](limit: Double, value: V) = new Limiting[DATUM, V](0.0, limit, value.factory.name, Some(value))

    /** Synonym for `apply`. */
    def ing[DATUM, V <: Container[V] with Aggregation](limit: Double, value: V) = apply[DATUM, V](limit, value)

    /** Use [[org.dianahep.histogrammar.Limited]] in Scala pattern-matching. */
    def unapply[V <: Container[V]](x: Limited[V]) = x.value
    /** Use [[org.dianahep.histogrammar.Limiting]] in Scala pattern-matching. */
    def unapply[DATUM, V <: Container[V] with Aggregation](x: Limiting[DATUM, V]) = x.value

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("entries", "limit", "type", "data")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        val limit = get("limit") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".limit")
        }

        val contentType = get("type") match {
          case JsonString(x) => x
          case x => throw new JsonFormatException(x, name + ".type")
        }
        val factory = Factory(contentType)

        get("data") match {
          case JsonNull => new Limited[Container[_]](entries, limit, contentType, None)
          case x => new Limited[Container[_]](entries, limit, contentType, Some(factory.fromJsonFragment(x)))
        }

      case _ => throw new JsonFormatException(json, name)
    }
  }

  /** An accumulated aggregator or `None` if the number of entries exceeded the limit.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param limit Maximum sum of weights to keep; above this, `value` goes to `None`.
    * @param contentType Name of the Factory for `value`.
    * @param value Some aggregator or `None`.
    */
  class Limited[V <: Container[V]] private[histogrammar](val entries: Double, val limit: Double, val contentType: String, val value: Option[V]) extends Container[Limited[V]] {
    type Type = Limited[V]
    def factory = Limit

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    /** True if `entries` exceeds `limit` and `value` is `None`. */
    def saturated = value.isEmpty
    /** Get the value of `value` or raise an error if it is `None`. */
    def get = value.get
    /** Get the value of `value` or return a default if it is `None`. */
    def getOrElse(default: => V) = value.getOrElse(default)

    def zero = new Limited[V](0.0, limit, contentType, value.map(_.zero))
    def +(that: Limited[V]) =
      if (this.limit != that.limit)
        throw new ContainerException(s"""cannot add Limited because they have different limits (${this.limit} vs ${that.limit}))""")
      else {
        val newentries = this.entries + that.entries
        val newvalue =
          if (newentries > limit)
            None
          else
            Some(this.value.get + that.value.get)
        new Limited[V](newentries, limit, contentType, newvalue)
      }

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "limit" -> JsonFloat(limit),
      "type" -> JsonString(contentType),
      "data" -> (value match {
        case None => JsonNull
        case Some(x) => x.toJsonFragment
      }))

    override def toString() = s"""Limited[${if (saturated) "saturated" else value.get}]"""
    override def equals(that: Any) = that match {
      case that: Limited[V] => this.entries === that.entries  &&  this.limit === that.limit  &&  this.contentType == that.contentType  &&  this.value == that.value
      case _ => false
    }
    override def hashCode() = (entries, limit, contentType, value).hashCode
  }

  /** Accumulating an aggregator or `None` if the number of entries exceeds the limit.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param limit Maximum sum of weights to keep; above this, `value` goes to `None`.
    * @param contentType Name of the Factory for `value`.
    * @param value Some aggregator or `None`.
    */
  class Limiting[DATUM, V <: Container[V] with Aggregation] private[histogrammar](var entries: Double, val limit: Double, val contentType: String, var value: Option[V]) extends Container[Limiting[DATUM, V]] with AggregationOnData {
    type Type = Limiting[DATUM, V]
    type Datum = V#Datum
    def factory = Limit

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    /** True if `entries` exceeds `limit` and `value` is `None`. */
    def saturated = value.isEmpty
    /** Get the value of `value` or raise an error if it is `None`. */
    def get = value.get
    /** Get the value of `value` or return a default if it is `None`. */
    def getOrElse(default: => V) = value.getOrElse(default)

    def zero = new Limiting[DATUM, V](0.0, limit, contentType, value.map(_.zero))
    def +(that: Limiting[DATUM, V]) =
      if (this.limit != that.limit)
        throw new ContainerException(s"""cannot add Limiting because they have different limits (${this.limit} vs ${that.limit}))""")
      else {
        val newentries = this.entries + that.entries
        val newvalue =
          if (newentries > limit)
            None
          else
            Some(this.value.get + that.value.get)
        new Limiting[DATUM, V](newentries, limit, contentType, newvalue)
      }

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      entries += weight
      if (entries > limit)
        value = None
      else
        value.foreach(v => v.fill(datum.asInstanceOf[v.Datum], weight))
    }

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "limit" -> JsonFloat(limit),
      "type" -> JsonString(contentType),
      "data" -> (value match {
        case None => JsonNull
        case Some(x) => x.toJsonFragment
      }))

    override def toString() = s"""Limiting[${if (saturated) "saturated" else value.get}]"""
    override def equals(that: Any) = that match {
      case that: Limited[V] => this.entries === that.entries  &&  this.limit === that.limit  &&  this.contentType == that.contentType  &&  this.value == that.value
      case _ => false
    }
    override def hashCode() = (entries, limit, contentType, value).hashCode
  }

  //////////////////////////////////////////////////////////////// Label/Labeled/Labeling

  /** Accumulate any number of containers of the SAME type and label them with strings. Every one is filled with every input datum.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Labeling]] and immutable [[org.dianahep.histogrammar.Labeled]] objects.
    */
  object Label extends Factory {
    val name = "Label"
    val help = "Accumulate any number of containers of the SAME type and label them with strings. Every one is filled with every input datum."
    val detailedHelp = """Label(pairs: (String, V)*)"""

    /** Create an immutable [[org.dianahep.histogrammar.Labeled]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param pairs Names (strings) associated with containers of the SAME type.
      */
    def ed[V <: Container[V]](entries: Double, pairs: (String, V)*) = new Labeled[V](entries, pairs: _*)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Labeling]].
      * 
      * @param pairs Names (strings) associated with containers of the SAME type.
      */
    def apply[V <: Container[V] with Aggregation](pairs: (String, V)*) = new Labeling[V](0.0, pairs: _*)

    /** Synonym for `apply`. */
    def ing[V <: Container[V] with Aggregation](pairs: (String, V)*) = apply(pairs: _*)

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

  /** An accumulated collection of containers of the SAME type, labeled by strings.
    * 
    * Use the factory [[org.dianahep.histogrammar.Label]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param pairs Names (strings) associated with containers of the SAME type.
    */
  class Labeled[V <: Container[V]] private[histogrammar](val entries: Double, val pairs: (String, V)*) extends Container[Labeled[V]] {
    type Type = Labeled[V]
    def factory = Label

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (pairs.isEmpty)
      throw new ContainerException("at least one pair required")

    /** Input `pairs` as a key-value map. */
    val pairsMap = pairs.toMap
    /** Number of `pairs`. */
    def size = pairs.size
    /** Iterable over the keys of the `pairs`. */
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    /** Iterable over the values of the `pairs`. */
    def values: Iterable[Container[V]] = pairs.toIterable.map(_._2)
    /** Set of keys among the `pairs`. */
    def keySet: Set[String] = keys.toSet
    /** Attempt to get key `x`, throwing an exception if it does not exist. */
    def apply(x: String) = pairsMap(x)
    /** Attempt to get key `x`, returning `None` if it does not exist. */
    def get(x: String) = pairsMap.get(x)
    /** Attempt to get key `x`, returning an alternative if it does not exist. */
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
      "type" -> JsonString(pairs.head._2.factory.name),
      "data" -> JsonObject(pairs map {case (label, sub) => label -> sub.toJsonFragment}: _*))

    override def toString() = s"Labeled[[${pairs.head.toString}..., size=${pairs.size}]]"
    override def equals(that: Any) = that match {
      case that: Labeled[V] => this.entries === that.entries  &&  this.pairsMap == that.pairsMap
      case _ => false
    }
    override def hashCode() = (entries, pairsMap).hashCode
  }

  /** Accumulating a collection of containers of the SAME type, labeled by strings.
    * 
    * Use the factory [[org.dianahep.histogrammar.Label]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param pairs Names (strings) associated with containers of the SAME type.
    */
  class Labeling[V <: Container[V] with Aggregation] private[histogrammar](var entries: Double, val pairs: (String, V)*) extends Container[Labeling[V]] with AggregationOnData {
    type Type = Labeling[V]
    type Datum = V#Datum
    def factory = Label

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (pairs.isEmpty)
      throw new ContainerException("at least one pair required")

    /** Input `pairs` as a key-value map. */
    val pairsMap = pairs.toMap
    /** Number of `pairs`. */
    def size = pairs.size
    /** Iterable over the keys of the `pairs`. */
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    /** Iterable over the values of the `pairs`. */
    def values: Iterable[V] = pairs.toIterable.map(_._2)
    /** Set of keys among the `pairs`. */
    def keySet: Set[String] = keys.toSet
    /** Attempt to get key `x`, throwing an exception if it does not exist. */
    def apply(x: String) = pairsMap(x)
    /** Attempt to get key `x`, returning `None` if it does not exist. */
    def get(x: String) = pairsMap.get(x)
    /** Attempt to get key `x`, returning an alternative if it does not exist. */
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

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      entries += weight
      var i = 0
      while (i < size) {
        val (_, v) = pairs(i)
        v.fill(datum.asInstanceOf[v.Datum], weight)      // see notes in Indexing[V]
        i += 1
      }
    }

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "type" -> JsonString(pairs.head._2.factory.name),
      "data" -> JsonObject(pairs map {case (label, sub) => label -> sub.toJsonFragment}: _*))

    override def toString() = s"Labeling[[${pairs.head.toString}..., size=${pairs.size}]]"
    override def equals(that: Any) = that match {
      case that: Labeled[V] => this.entries === that.entries  &&  this.pairsMap == that.pairsMap
      case _ => false
    }
    override def hashCode() = (entries, pairsMap).hashCode
  }

  //////////////////////////////////////////////////////////////// UntypedLabel/UntypedLabeled/UntypedLabeling

  /** Accumulate containers of any type except [[org.dianahep.histogrammar.Count]] and label them with strings. Every one is filled with every input datum.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.UntypedLabeling]] and immutable [[org.dianahep.histogrammar.UntypedLabeled]] objects.
    * 
    * '''Note:''' the compiler cannot predict the type of data that is drawn from this collection, so it must be cast with `as`.
    */
  object UntypedLabel extends Factory {
    val name = "UntypedLabel"
    val help = "Accumulate containers of any type except Count and label them with strings. Every one is filled with every input datum."
    val detailedHelp = """UntypedLabel(pairs: (String -> Container)*)"""

    /** Create an immutable [[org.dianahep.histogrammar.UntypedLabeled]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param pairs Names (strings) associated with containers of any type except [[org.dianahep.histogrammar.Counted]].
      */
    def ed(entries: Double, pairs: (String, Container[_])*) = new UntypedLabeled(entries, pairs: _*)

    /** Create an empty, mutable [[org.dianahep.histogrammar.UntypedLabeling]].
      * 
      * @param pairs Names (strings) associated with containers of any type except [[org.dianahep.histogrammar.Counting]].
      */
    def apply[DATUM](pairs: (String, Container[_] with AggregationOnData {type Datum = DATUM})*) = new UntypedLabeling(0.0, pairs: _*)

    /** Synonym for `apply`. */
    def ing[DATUM](pairs: (String, Container[_] with AggregationOnData {type Datum = DATUM})*) = apply(pairs: _*)

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

  /** An accumulated collection of containers of any type except [[org.dianahep.histogrammar.Counted]], labeled by strings.
    * 
    * Use the factory [[org.dianahep.histogrammar.UntypedLabel]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param pairs Names (strings) associated with containers.
    * 
    * '''Note:''' the compiler cannot predict the type of data that is drawn from this collection, so it must be cast with `as`.
    */
  class UntypedLabeled private[histogrammar](val entries: Double, val pairs: (String, Container[_])*) extends Container[UntypedLabeled] {
    type Type = UntypedLabeled
    def factory = UntypedLabel

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    /** Input `pairs` as a key-value map. */
    val pairsMap = pairs.toMap
    /** Number of `pairs`. */
    def size = pairs.size
    /** Iterable over the keys of the `pairs`. */
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    /** Iterable over the values of the `pairs`. */
    def values: Iterable[Container[_]] = pairs.toIterable.map(_._2)
    /** Set of keys among the `pairs`. */
    def keySet: Set[String] = keys.toSet
    /** Attempt to get key `x`, throwing an exception if it does not exist. */
    def apply(x: String) = pairsMap(x)
    /** Attempt to get key `x`, returning `None` if it does not exist. */
    def get(x: String) = pairsMap.get(x)
    /** Attempt to get key `x`, returning an alternative if it does not exist. */
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

    override def toString() =
      if (pairs.isEmpty)
        s"UntypedLabeled[[size=${pairs.size}]]"
      else
        s"UntypedLabeled[[${pairs.head.toString}..., size=${pairs.size}]]"
    override def equals(that: Any) = that match {
      case that: UntypedLabeled => this.entries === that.entries  &&  this.pairsMap == that.pairsMap
      case _ => false
    }
    override def hashCode() = (entries, pairsMap).hashCode
  }

  /** Accumulating a collection of containers of any type except [[org.dianahep.histogrammar.Counting]], labeled by strings.
    * 
    * Use the factory [[org.dianahep.histogrammar.UntypedLabel]] to construct an instance.
    * 
    * @param pairs Names (strings) associated with containers.
    * 
    * '''Note:''' the compiler cannot predict the type of data that is drawn from this collection, so it must be cast with `as`.
    */
  class UntypedLabeling[DATUM] private[histogrammar](var entries: Double, val pairs: (String, Container[_] with AggregationOnData {type Datum = DATUM})*) extends Container[UntypedLabeling[DATUM]] with AggregationOnData {
    type Type = UntypedLabeled
    type Datum = DATUM
    def factory = UntypedLabel

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    /** Input `pairs` as a key-value map. */
    val pairsMap = pairs.toMap
    /** Number of `pairs`. */
    def size = pairs.size
    /** Iterable over the keys of the `pairs`. */
    def keys: Iterable[String] = pairs.toIterable.map(_._1)
    /** Iterable over the values of the `pairs`. */
    def values: Iterable[Container[_]] = pairs.toIterable.map(_._2)
    /** Set of keys among the `pairs`. */
    def keySet: Set[String] = keys.toSet
    /** Attempt to get key `x`, throwing an exception if it does not exist. */
    def apply(x: String) = pairsMap(x)
    /** Attempt to get key `x`, returning `None` if it does not exist. */
    def get(x: String) = pairsMap.get(x)
    /** Attempt to get key `x`, returning an alternative if it does not exist. */
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

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      entries += weight
      var i = 0
      while (i < size) {
        val (_, v) = pairs(i)
        v.fill(datum.asInstanceOf[v.Datum], weight)      // see notes in Indexing[V]
        i += 1
      }
    }

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "data" -> JsonObject(pairs map {case (key, sub) =>
        key -> JsonObject("type" -> JsonString(sub.factory.name), "data" -> sub.toJsonFragment)
      }: _*))

    override def toString() = s"UntypedLabeling[[${pairs.head.toString}..., size=${pairs.size}]]"
    override def equals(that: Any) = that match {
      case that: UntypedLabeling[DATUM] => this.entries === that.entries  &&  this.pairsMap == that.pairsMap
      case _ => false
    }
    override def hashCode() = (entries, pairsMap).hashCode
  }

  //////////////////////////////////////////////////////////////// Index/Indexed/Indexing

  /** Accumulate any number of containers of the SAME type anonymously in a list. Every one is filled with every input datum.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Indexing]] and immutable [[org.dianahep.histogrammar.Indexed]] objects.
    */
  object Index extends Factory {
    val name = "Index"
    val help = "Accumulate any number of containers of the SAME type anonymously in a list. Every one is filled with every input datum."
    val detailedHelp = """"""

    /** Create an immutable [[org.dianahep.histogrammar.Indexed]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param values Ordered list of containers that can be retrieved by index number.
      */
    def ed[V <: Container[V]](entries: Double, values: V*) = new Indexed[V](entries, values: _*)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Indexing]].
      * 
      * @param values Ordered list of containers that can be retrieved by index number.
      */
    def apply[V <: Container[V] with Aggregation](values: V*) = new Indexing[V](0.0, values: _*)

    /** Synonym for `apply`. */
    def ing[V <: Container[V] with Aggregation](values: V*) = apply(values: _*)

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

  /** An accumulated collection of containers of the SAME type, indexed by number.
    * 
    * Use the factory [[org.dianahep.histogrammar.Index]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param values Ordered list of containers that can be retrieved by index number.
    */
  class Indexed[V <: Container[V]] private[histogrammar](val entries: Double, val values: V*) extends Container[Indexed[V]] {
    type Type = Indexed[V]
    def factory = Index

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (values.isEmpty)
      throw new ContainerException("at least one element required")

    /** Number of `values`. */
    def size = values.size
    /** Attempt to get index `i`, throwing an exception if it does not exist. */
    def apply(i: Int) = values(i)
    /** Attempt to get index `i`, returning `None` if it does not exist. */
    def get(i: Int) =
      if (i < 0  ||  i >= size)
        None
      else
        Some(apply(i))
    /** Attempt to get index `i`, returning an alternative if it does not exist. */
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

    def toJsonFragment = JsonObject("entries" -> JsonFloat(entries), "type" -> JsonString(values.head.factory.name), "data" -> JsonArray(values.map(_.toJsonFragment): _*))

    override def toString() = s"Indexed[[${values.head.toString}..., size=${size}]]"
    override def equals(that: Any) = that match {
      case that: Indexed[V] => this.entries === that.entries  &&  this.values == that.values
      case _ => false
    }
    override def hashCode() = (entries, values).hashCode
  }

  /** Accumulating a collection of containers of the SAME type, indexed by number.
    * 
    * Use the factory [[org.dianahep.histogrammar.Index]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param values Ordered list of containers that can be retrieved by index number.
    */
  class Indexing[V <: Container[V] with Aggregation] private[histogrammar](var entries: Double, val values: V*) extends Container[Indexing[V]] with AggregationOnData {
    type Type = Indexing[V]
    type Datum = V#Datum
    def factory = Index

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (values.isEmpty)
      throw new ContainerException("at least one element required")

    /** Number of `values`. */
    def size = values.size
    /** Attempt to get index `i`, throwing an exception if it does not exist. */
    def apply(i: Int) = values(i)
    /** Attempt to get index `i`, returning `None` if it does not exist. */
    def get(i: Int) =
      if (i < 0  ||  i >= size)
        None
      else
        Some(apply(i))
    /** Attempt to get index `i`, returning an alternative if it does not exist. */
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

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      entries += weight
      var i = 0
      while (i < size) {
        val v = values(i)
        v.fill(datum.asInstanceOf[v.Datum], weight)   // This type is ensured, but Scala doesn't recognize it.
        i += 1                                        // Also, Scala undergoes infinite recursion in a
      }                                               // "foreach" version of this loop--- that's weird!
    }

    def toJsonFragment = JsonObject("entries" -> JsonFloat(entries), "type" -> JsonString(values.head.factory.name), "data" -> JsonArray(values.map(_.toJsonFragment): _*))

    override def toString() = s"Indexing[[${values.head.toString}..., size=${size}]]"
    override def equals(that: Any) = that match {
      case that: Indexing[V] => this.entries === that.entries  &&  this.values == that.values
      case _ => false
    }
    override def hashCode() = (entries, values).hashCode
  }

  //////////////////////////////////////////////////////////////// Branch/Branched/Branching

  /** Accumulate containers of DIFFERENT types, indexed by `i0` through `i9`. Every one is filled with every input datum.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Branching]] and immutable [[org.dianahep.histogrammar.Branched]] objects.
    * 
    * '''Note:''' there is nothing intrinsic about the limit of 10 items. The data themselves are stored in a linked list (in value space and type space) and index fields `i0` through `i9` are added implicitly to lists of type-length 2 through 10, respectively. Longer lists can be created by adding more implicit methods.
    * 
    * To create a `Branched`, do `Branch.ed(entries, h1, h2, h3, ...)`.
    * 
    * To create a `Branching`, do `Branch(h1, h2, h3, ...)`.
    */
  object Branch extends Factory {
    def name = "Branch"
    def help = "Accumulate containers of DIFFERENT types, indexed by i0 through i9. Every one is filled with every input datum."
    def detailedHelp = "Branch(container0, container1, ...)"

    def ed[C0 <: Container[C0]](entries: Double, i0: C0) = new Branched(entries, i0, BranchedNil)
    def ed[C0 <: Container[C0], C1 <: Container[C1]](entries: Double, i0: C0, i1: C1) = new Branched(entries, i0, new Branched(entries, i1, BranchedNil))
    def ed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2]](entries: Double, i0: C0, i1: C1, i2: C2) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, BranchedNil)))
    def ed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](entries: Double, i0: C0, i1: C1, i2: C2, i3: C3) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, new Branched(entries, i3, BranchedNil))))
    def ed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](entries: Double, i0: C0, i1: C1, i2: C2, i3: C3, i4: C4) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, new Branched(entries, i3, new Branched(entries, i4, BranchedNil)))))
    def ed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](entries: Double, i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, new Branched(entries, i3, new Branched(entries, i4, new Branched(entries, i5, BranchedNil))))))
    def ed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](entries: Double, i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, new Branched(entries, i3, new Branched(entries, i4, new Branched(entries, i5, new Branched(entries, i6, BranchedNil)))))))
    def ed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](entries: Double, i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, new Branched(entries, i3, new Branched(entries, i4, new Branched(entries, i5, new Branched(entries, i6, new Branched(entries, i7, BranchedNil))))))))
    def ed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](entries: Double, i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7, i8: C8) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, new Branched(entries, i3, new Branched(entries, i4, new Branched(entries, i5, new Branched(entries, i6, new Branched(entries, i7, new Branched(entries, i8, BranchedNil)))))))))
    def ed[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](entries: Double, i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7, i8: C8, i9: C9) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, new Branched(entries, i3, new Branched(entries, i4, new Branched(entries, i5, new Branched(entries, i6, new Branched(entries, i7, new Branched(entries, i8, new Branched(entries, i9, BranchedNil))))))))))

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

    def ing[C0 <: Container[C0] with Aggregation](i0: C0) = apply(i0)
    def ing[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation](i0: C0, i1: C1)(implicit e01: C0 Compatible C1) = apply(i0, i1)
    def ing[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation](i0: C0, i1: C1, i2: C2)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2) = apply(i0, i1, i2)
    def ing[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3) = apply(i0, i1, i2, i3)
    def ing[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4) = apply(i0, i1, i2, i3, i4)
    def ing[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4, e05: C0 Compatible C5) = apply(i0, i1, i2, i3, i4, i5)
    def ing[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4, e05: C0 Compatible C5, e06: C0 Compatible C6) = apply(i0, i1, i2, i3, i4, i5, i6)
    def ing[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation, C7 <: Container[C7] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4, e05: C0 Compatible C5, e06: C0 Compatible C6, e07: C0 Compatible C7) = apply(i0, i1, i2, i3, i4, i5, i6, i7)
    def ing[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation, C7 <: Container[C7] with Aggregation, C8 <: Container[C8] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7, i8: C8)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4, e05: C0 Compatible C5, e06: C0 Compatible C6, e07: C0 Compatible C7, e08: C0 Compatible C8) = apply(i0, i1, i2, i3, i4, i5, i6, i7, i8)
    def ing[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation, C7 <: Container[C7] with Aggregation, C8 <: Container[C8] with Aggregation, C9 <: Container[C9] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7, i8: C8, i9: C9)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4, e05: C0 Compatible C5, e06: C0 Compatible C6, e07: C0 Compatible C7, e08: C0 Compatible C8, e09: C0 Compatible C9) = apply(i0, i1, i2, i3, i4, i5, i6, i7, i8, i9)

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
            while (backwards != BranchedNil) {
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
    def size: Int
    def get(i: Int): Option[Container[_]]
    def zero: BranchedList
  }

  object BranchedNil extends BranchedList {
    def values: List[Container[_]] = Nil
    def size: Int = 0
    def get(i: Int) = None
    def zero = this
  }

  /** An accumulated collection of containers of the ANY type, indexed by number.
    * 
    * Use the factory [[org.dianahep.histogrammar.Branch]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param head Container associated with the first item in the list.
    * @param tail List of all other `Branched` objects (or `BranchedNil`, the end of the list).
    * 
    * Note that concrete instances of `Branched` implicitly have fields `i0` through `i9`, which are shortcuts to the first ten items.
    */
  class Branched[HEAD <: Container[HEAD], TAIL <: BranchedList] private[histogrammar](val entries: Double, val head: HEAD, val tail: TAIL) extends Container[Branched[HEAD, TAIL]] with BranchedList {
    type Type = Branched[HEAD, TAIL]
    def factory = Branch

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    /** List the containers (dropping type information). */
    def values: List[Container[_]] = head :: tail.values
    /** Return the number of containers. */
    def size: Int = 1 + tail.size
    /** Attempt to get index `i`, throwing an exception if it does not exist. */
    def apply(i: Int) = get(i).get
    /** Attempt to get index `i`, returning `None` if it does not exist. */
    def get(i: Int) =
      if (i == 0)
        Some(head)
      else
        tail.get(i - 1)
    /** Attempt to get index `i`, returning an alternative if it does not exist. */
    def getOrElse(i: Int, default: => Container[_]) = get(i).getOrElse(default)

    def zero = new Branched[HEAD, TAIL](0.0, head.zero, tail.zero.asInstanceOf[TAIL])
    def +(that: Branched[HEAD, TAIL]) = new Branched[HEAD, TAIL](this.entries + that.entries, this.head + that.head, this.tail)

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "data" -> JsonArray(values.map(x => JsonObject(JsonString(x.factory.name) -> x.toJsonFragment)): _*))

    override def toString() = "Branched[" + values.mkString(", ") + "]"

    override def equals(that: Any) = that match {
      case that: Branched[_, _] => this.entries === that.entries  &&  this.head == that.head  &&  this.tail == that.tail
      case _ => false
    }
    override def hashCode() = (entries, values).hashCode
  }

  sealed trait BranchingList {
    def values: List[Container[_]]
    def size: Int
    def get(i: Int): Option[Container[_]]
    def zero: BranchingList
  }

  object BranchingNil extends BranchingList {
    def values: List[Container[_]] = Nil
    def size: Int = 0
    def get(i: Int) = None
    def zero = this
  }

  /** Accumulating a collection of containers of the ANY type, indexed by number.
    * 
    * Use the factory [[org.dianahep.histogrammar.Branch]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    * @param head Container associated with the first item in the list.
    * @param tail List of all other `Branching` objects (or `BranchingNil`, the end of the list).
    * 
    * Note that concrete instances of `Branching` implicitly have fields `i0` through `i9`, which are shortcuts to the first ten items.
    */
  class Branching[HEAD <: Container[HEAD] with Aggregation, TAIL <: BranchingList] private[histogrammar](var entries: Double, val head: HEAD, val tail: TAIL) extends Container[Branching[HEAD, TAIL]] with AggregationOnData with BranchingList {
    type Type = Branching[HEAD, TAIL]
    type Datum = head.Datum
    def factory = Branch

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    /** List the containers (dropping type information). */
    def values: List[Container[_]] = head :: tail.values
    /** Return the number of containers. */
    def size: Int = 1 + tail.size
    /** Attempt to get index `i`, throwing an exception if it does not exist. */
    def apply(i: Int) = get(i).get
    /** Attempt to get index `i`, returning `None` if it does not exist. */
    def get(i: Int) =
      if (i == 0)
        Some(head)
      else
        tail.get(i - 1)
    /** Attempt to get index `i`, returning an alternative if it does not exist. */
    def getOrElse(i: Int, default: => Container[_]) = get(i).getOrElse(default)

    def zero = new Branching[HEAD, TAIL](0.0, head.zero, tail.zero.asInstanceOf[TAIL])
    def +(that: Branching[HEAD, TAIL]) = new Branching[HEAD, TAIL](this.entries + that.entries, this.head + that.head, this.tail)

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      entries += weight
      head.fill(datum, weight)
      tail match {
        case x: Aggregation => x.fill(datum.asInstanceOf[x.Datum], weight)
        case _ =>
      }
    }

    def toJsonFragment = JsonObject(
      "entries" -> JsonFloat(entries),
      "data" -> JsonArray(values.map(x => JsonObject(JsonString(x.factory.name) -> x.toJsonFragment)): _*))

    override def toString() = "Branching[" + values.mkString(", ") + "]"

    override def equals(that: Any) = that match {
      case that: Branching[_, _] => this.entries === that.entries  &&  this.head == that.head  &&  this.tail == that.tail
      case _ => false
    }
    override def hashCode() = (entries, values).hashCode
  }

}
