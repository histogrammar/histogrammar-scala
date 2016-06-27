// Copyright 2016 DIANA-HEP
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
import org.dianahep.histogrammar.util._

package histogrammar {
  //////////////////////////////////////////////////////////////// Label/Labeled/Labeling

  /** Accumulate any number of aggregators of the same type and label them with strings. Every sub-aggregator is filled with every input datum.
    * 
    * This primitive simulates a directory of aggregators. For sub-directories, nest collections within the Label collection.
    * 
    * Note that all sub-aggregators within a Label must have the ''same type'' (e.g. histograms of different binnings, but all histograms). To collect objects of ''different types'' with string-based look-up keys, use [[org.dianahep.histogrammar.UntypedLabel]].
    * 
    * To collect aggregators of the ''same type'' without naming them, use [[org.dianahep.histogrammar.Index]]. To collect aggregators of ''different types'' without naming them, use [[org.dianahep.histogrammar.Branch]].
    * 
    * In strongly typed languages, the restriction to a single type allows nested objects to be extracted without casting.
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Labeling]] and immutable [[org.dianahep.histogrammar.Labeled]] objects.
    */
  object Label extends Factory {
    val name = "Label"
    val help = "Accumulate any number of aggregators of the same type and label them with strings. Every sub-aggregator is filled with every input datum."
    val detailedHelp = """This primitive simulates a directory of aggregators. For sub-directories, nest collections within the Label collection.

Note that all sub-aggregators within a Label must have the ''same type'' (e.g. histograms of different binnings, but all histograms). To collect objects of ''different types'' with string-based look-up keys, use [[org.dianahep.histogrammar.UntypedLabel]].

To collect aggregators of the ''same type'' without naming them, use [[org.dianahep.histogrammar.Index]]. To collect aggregators of ''different types'' without naming them, use [[org.dianahep.histogrammar.Branch]].

In strongly typed languages, the restriction to a single type allows nested objects to be extracted without casting.
"""

    /** Create an immutable [[org.dianahep.histogrammar.Labeled]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param pairs Names (strings) associated with containers of the SAME type.
      */
    def ed[V <: Container[V] with NoAggregation](entries: Double, pairs: (String, V)*) = new Labeled[V](entries, pairs: _*)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Labeling]].
      * 
      * @param pairs Names (strings) associated with containers of the SAME type.
      */
    def apply[V <: Container[V] with Aggregation](pairs: (String, V)*) = new Labeling[V](0.0, pairs: _*)

    /** Synonym for `apply`. */
    def ing[V <: Container[V] with Aggregation](pairs: (String, V)*) = apply(pairs: _*)

    import KeySetComparisons._
    def fromJsonFragment(json: Json, nameFromParent: Option[String]): Container[_] with NoAggregation = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("entries", "type", "data")) =>
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
            new Labeled[Container[_]](entries, labelPairs map {case (JsonString(label), sub) => label -> factory.fromJsonFragment(sub, None)}: _*)
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
  class Labeled[V <: Container[V] with NoAggregation] private[histogrammar](val entries: Double, val pairs: (String, V)*) extends Container[Labeled[V]] with NoAggregation with Collection {
    type Type = Labeled[V]
    type EdType = Labeled[V]
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
    /** Attempt to get key `x`, returning `None` if it does not exist. */
    def get(x: String) = pairsMap.get(x)
    /** Attempt to get key `x`, returning an alternative if it does not exist. */
    def getOrElse(x: String, default: => V) = pairsMap.getOrElse(x, default)

    /** Attempt to get key `index`, throwing an exception if it does not exist. */
    def apply(index: CollectionIndex): V = index match {
      case StringIndex(i) if (pairsMap contains i) => pairsMap(i)
      case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Labeled: $index""")
    }
    /** Attempt to get keys `indexes`, throwing an exception if it does not exist. */
    def apply(indexes: CollectionIndex*) = indexes.toList match {
      case StringIndex(i) :: Nil if (pairsMap contains i) => pairsMap(i)
      case StringIndex(i) :: rest if (pairsMap contains i) => pairsMap(i) match {
        case sub: Collection => sub(rest: _*)
        case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Labeled: ${indexes.mkString(", ")}""")
      }
      case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Labeled: ${indexes.mkString(", ")}""")
    }

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

    def children = values.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "type" -> JsonString(pairs.head._2.factory.name),
      "data" -> JsonObject(pairs map {case (label, sub) => label -> sub.toJsonFragment(false)}: _*))

    override def toString() = s"""<Labeled values=${pairs.head._2.factory.name} size=${pairs.size}>"""
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
  class Labeling[V <: Container[V] with Aggregation] private[histogrammar](var entries: Double, val pairs: (String, V)*) extends Container[Labeling[V]] with AggregationOnData with Collection {
    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (pairs.isEmpty)
      throw new ContainerException("at least one pair required")

    protected val v = pairs.head._2
    type Type = Labeling[V]
    type EdType = Labeled[v.EdType]
    type Datum = V#Datum
    def factory = Label

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
    /** Attempt to get key `x`, returning `None` if it does not exist. */
    def get(x: String) = pairsMap.get(x)
    /** Attempt to get key `x`, returning an alternative if it does not exist. */
    def getOrElse(x: String, default: => V) = pairsMap.getOrElse(x, default)

    /** Attempt to get key `index`, throwing an exception if it does not exist. */
    def apply(index: CollectionIndex): V = index match {
      case StringIndex(i) if (pairsMap contains i) => pairsMap(i)
      case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Labeling: $index""")
    }
    /** Attempt to get key `indexes`, throwing an exception if it does not exist. */
    def apply(indexes: CollectionIndex*) = indexes.toList match {
      case StringIndex(i) :: Nil if (pairsMap contains i) => pairsMap(i)
      case StringIndex(i) :: rest if (pairsMap contains i) => pairsMap(i) match {
        case sub: Collection => sub(rest: _*)
        case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Labeling: ${indexes.mkString(", ")}""")
      }
      case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Labeling: ${indexes.mkString(", ")}""")
    }

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
      checkForCrossReferences()
      var i = 0
      while (i < size) {
        val (_, v) = pairs(i)
        v.fill(datum.asInstanceOf[v.Datum], weight)      // see notes in Indexing[V]
        i += 1
      }
      // no possibility of exception from here on out (for rollback)
      entries += weight
    }

    def children = values.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "type" -> JsonString(pairs.head._2.factory.name),
      "data" -> JsonObject(pairs map {case (label, sub) => label -> sub.toJsonFragment(false)}: _*))

    override def toString() = s"""<Labeling values=${pairs.head._2.factory.name} size=${pairs.size}>"""
    override def equals(that: Any) = that match {
      case that: Labeled[V] => this.entries === that.entries  &&  this.pairsMap == that.pairsMap
      case _ => false
    }
    override def hashCode() = (entries, pairsMap).hashCode
  }

  //////////////////////////////////////////////////////////////// UntypedLabel/UntypedLabeled/UntypedLabeling

  /** Accumulate any number of aggregators of any type and label them with strings. Every sub-aggregator is filled with every input datum.
    * 
    * This primitive simulates a directory of aggregators. For sub-directories, nest collections within the UntypedLabel.
    * 
    * Note that sub-aggregators within an UntypedLabel may have ''different types''. In strongly typed languages, this flexibility poses a problem: nested objects must be type-cast before they can be used. To collect objects of the ''same type'' with string-based look-up keys, use [[org.dianahep.histogrammar.Label]].
    * 
    * To collect aggregators of the ''same type'' without naming them, use [[org.dianahep.histogrammar.Index]]. To collect aggregators of ''different types'' without naming them, use [[org.dianahep.histogrammar.Branch]].
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.UntypedLabeling]] and immutable [[org.dianahep.histogrammar.UntypedLabeled]] objects.
    * 
    * '''Note:''' the compiler cannot predict the type of data that is drawn from this collection, so it must be cast with `as`.
    */
  object UntypedLabel extends Factory {
    val name = "UntypedLabel"
    val help = "Accumulate any number of aggregators of any type and label them with strings. Every sub-aggregator is filled with every input datum."
    val detailedHelp = """This primitive simulates a directory of aggregators. For sub-directories, nest collections within the UntypedLabel.

Note that sub-aggregators within an UntypedLabel may have ''different types''. In strongly typed languages, this flexibility poses a problem: nested objects must be type-cast before they can be used. To collect objects of the ''same type'' with string-based look-up keys, use [[org.dianahep.histogrammar.Label]].

To collect aggregators of the ''same type'' without naming them, use [[org.dianahep.histogrammar.Index]]. To collect aggregators of ''different types'' without naming them, use [[org.dianahep.histogrammar.Branch]]."""

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

    import KeySetComparisons._
    def fromJsonFragment(json: Json, nameFromParent: Option[String]): Container[_] with NoAggregation = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("entries", "data")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        get("data") match {
          case JsonObject(subpairs @ _*) =>
            new UntypedLabeled(entries, subpairs map {
              case (JsonString(label), JsonObject(typedata @ _*)) if (typedata.keySet has Set("type", "data")) =>
                val subget = typedata.toMap

                (subget("type"), subget("data")) match {
                  case (JsonString(factory), sub) => (label.toString, Factory(factory).fromJsonFragment(sub, None))
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
  class UntypedLabeled private[histogrammar](val entries: Double, val pairs: (String, Container[_])*) extends Container[UntypedLabeled] with NoAggregation with Collection {
    type Type = UntypedLabeled
    type EdType = UntypedLabeled
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
    /** Attempt to get key `x`, returning `None` if it does not exist. */
    def get(x: String) = pairsMap.get(x)
    /** Attempt to get key `x`, returning an alternative if it does not exist. */
    def getOrElse(x: String, default: => Container[_]) = pairsMap.getOrElse(x, default)

    /** Attempt to get key `index`, throwing an exception if it does not exist. */
    def apply(index: CollectionIndex): Container[_] = index match {
      case StringIndex(i) if (pairsMap contains i) => pairsMap(i)
      case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for UntypedLabeled: $index""")
    }
    /** Attempt to get key `indexes`, throwing an exception if it does not exist. */
    def apply(indexes: CollectionIndex*) = indexes.toList match {
      case StringIndex(i) :: Nil if (pairsMap contains i) => pairsMap(i)
      case StringIndex(i) :: rest if (pairsMap contains i) => pairsMap(i) match {
        case sub: Collection => sub(rest: _*)
        case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for UntypedLabeled: ${indexes.mkString(", ")}""")
      }
      case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for UntypedLabeled: ${indexes.mkString(", ")}""")
    }

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

    def children = values.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "data" -> JsonObject(pairs map {case (key, sub) =>
        key -> JsonObject("type" -> JsonString(sub.factory.name), "data" -> sub.toJsonFragment(false))
      }: _*))

    override def toString() = s"""<UntypedLabeled size=${pairs.size}>"""
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
  class UntypedLabeling[DATUM] private[histogrammar](var entries: Double, val pairs: (String, Container[_] with AggregationOnData {type Datum = DATUM})*) extends Container[UntypedLabeling[DATUM]] with AggregationOnData with Collection {
    type Type = UntypedLabeled
    type EdType = UntypedLabeled
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
    /** Attempt to get key `x`, returning `None` if it does not exist. */
    def get(x: String) = pairsMap.get(x)
    /** Attempt to get key `x`, returning an alternative if it does not exist. */
    def getOrElse(x: String, default: => Container[_]) = pairsMap.getOrElse(x, default)

    /** Attempt to get key `index`, throwing an exception if it does not exist. */
    def apply(index: CollectionIndex): Container[_] = index match {
      case StringIndex(i) if (pairsMap contains i) => pairsMap(i)
      case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for UntypedLabeling: $index""")
    }
    /** Attempt to get key `indexes`, throwing an exception if it does not exist. */
    def apply(indexes: CollectionIndex*) = indexes.toList match {
      case StringIndex(i) :: Nil if (pairsMap contains i) => pairsMap(i)
      case StringIndex(i) :: rest if (pairsMap contains i) => pairsMap(i) match {
        case sub: Collection => sub(rest: _*)
        case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for UntypedLabeling: ${indexes.mkString(", ")}""")
      }
      case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for UntypedLabeling: ${indexes.mkString(", ")}""")
    }

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
      checkForCrossReferences()
      var i = 0
      while (i < size) {
        val (_, v) = pairs(i)
        v.fill(datum.asInstanceOf[v.Datum], weight)      // see notes in Indexing[V]
        i += 1
      }
      // no possibility of exception from here on out (for rollback)
      entries += weight
    }

    def children = values.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "data" -> JsonObject(pairs map {case (key, sub) =>
        key -> JsonObject("type" -> JsonString(sub.factory.name), "data" -> sub.toJsonFragment(false))
      }: _*))

    override def toString() = s"""<UntypedLabeling size=${pairs.size}>"""
    override def equals(that: Any) = that match {
      case that: UntypedLabeling[DATUM] => this.entries === that.entries  &&  this.pairsMap == that.pairsMap
      case _ => false
    }
    override def hashCode() = (entries, pairsMap).hashCode
  }

  //////////////////////////////////////////////////////////////// Index/Indexed/Indexing

  /** Accumulate any number of aggregators of the same type in a list. Every sub-aggregator is filled with every input datum.
    * 
    * This primitive provides an anonymous collection of aggregators (unless the integer index is taken to have special meaning, but generally such bookkeeping should be encoded in strings). Indexes can be nested to create two-dimensional ordinal grids of aggregators. (Use [[org.dianahep.histogrammar.Bin]] if the space is to have a metric interpretation.)
    * 
    * Note that all sub-aggregators within an Index must have the ''same type'' (e.g. histograms of different binnings, but all histograms). To collect objects of ''different types,'' still indexed by integer, use [[org.dianahep.histogrammar.Branch]].
    * 
    * To collect aggregators of the ''same type'' with string-based labels, use [[org.dianahep.histogrammar.Label]]. To collect aggregators of ''different types'' with string-based labels, use [[org.dianahep.histogrammar.UntypedLabel]].
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Indexing]] and immutable [[org.dianahep.histogrammar.Indexed]] objects.
    */
  object Index extends Factory {
    val name = "Index"
    val help = "Accumulate any number of aggregators of the same type in a list. Every sub-aggregator is filled with every input datum."
    val detailedHelp = """This primitive provides an anonymous collection of aggregators (unless the integer index is taken to have special meaning, but generally such bookkeeping should be encoded in strings). Indexes can be nested to create two-dimensional ordinal grids of aggregators. (Use [[org.dianahep.histogrammar.Bin]] if the space is to have a metric interpretation.)

Note that all sub-aggregators within an Index must have the ''same type'' (e.g. histograms of different binnings, but all histograms). To collect objects of ''different types,'' still indexed by integer, use [[org.dianahep.histogrammar.Branch]].

To collect aggregators of the ''same type'' with string-based labels, use [[org.dianahep.histogrammar.Label]]. To collect aggregators of ''different types'' with string-based labels, use [[org.dianahep.histogrammar.UntypedLabel]]."""

    /** Create an immutable [[org.dianahep.histogrammar.Indexed]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      * @param values Ordered list of containers that can be retrieved by index number.
      */
    def ed[V <: Container[V] with NoAggregation](entries: Double, values: V*) = new Indexed[V](entries, values: _*)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Indexing]].
      * 
      * @param values Ordered list of containers that can be retrieved by index number.
      */
    def apply[V <: Container[V] with Aggregation](values: V*) = new Indexing[V](0.0, values: _*)

    /** Synonym for `apply`. */
    def ing[V <: Container[V] with Aggregation](values: V*) = apply(values: _*)

    import KeySetComparisons._
    def fromJsonFragment(json: Json, nameFromParent: Option[String]): Container[_] with NoAggregation = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("entries", "type", "data")) =>
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
            new Indexed[Container[_]](entries, values.map(factory.fromJsonFragment(_, None)): _*)
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
  class Indexed[V <: Container[V] with NoAggregation] private[histogrammar](val entries: Double, val values: V*) extends Container[Indexed[V]] with NoAggregation with Collection {
    type Type = Indexed[V]
    type EdType = Indexed[V]
    def factory = Index

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (values.isEmpty)
      throw new ContainerException("at least one element required")

    /** Number of `values`. */
    def size = values.size
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

    /** Attempt to get key `index`, throwing an exception if it does not exist. */
    def apply(index: CollectionIndex): V = index match {
      case IntegerIndex(i) if (i >= 0  &&  i < size) => values(i)
      case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Indexed: $index""")
    }
    /** Attempt to get key `indexes`, throwing an exception if it does not exist. */
    def apply(indexes: CollectionIndex*) = indexes.toList match {
      case IntegerIndex(i) :: Nil if (i >= 0  &&  i < size) => values(i)
      case IntegerIndex(i) :: rest if (i >= 0  &&  i < size) => values(i) match {
        case sub: Collection => sub(rest: _*)
        case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Indexed: ${indexes.mkString(", ")}""")
      }
      case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Indexed: ${indexes.mkString(", ")}""")
    }

    def zero = new Indexed[V](0.0, values.map(_.zero): _*)
    def +(that: Indexed[V]) =
      if (this.size != that.size)
        throw new ContainerException(s"""cannot add Indexed because they have different sizes: (${this.size} vs ${that.size})""")
      else
        new Indexed[V](this.entries + that.entries, this.values zip that.values map {case(me, you) => me + you}: _*)

    def children = values.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject("entries" -> JsonFloat(entries), "type" -> JsonString(values.head.factory.name), "data" -> JsonArray(values.map(_.toJsonFragment(false)): _*))

    override def toString() = s"""<Indexed values=${values.head.factory.name} size=${size}>"""
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
  class Indexing[V <: Container[V] with Aggregation] private[histogrammar](var entries: Double, val values: V*) extends Container[Indexing[V]] with AggregationOnData with Collection {
    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")
    if (values.isEmpty)
      throw new ContainerException("at least one element required")

    protected val v = values.head
    type Type = Indexing[V]
    type EdType = Indexed[v.EdType]
    type Datum = V#Datum
    def factory = Index

    /** Number of `values`. */
    def size = values.size
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

    /** Attempt to get key `index`, throwing an exception if it does not exist. */
    def apply(index: CollectionIndex): V = index match {
      case IntegerIndex(i) if (i >= 0  &&  i < size) => values(i)
      case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Indexing: $index""")
    }
    /** Attempt to get key `indexes`, throwing an exception if it does not exist. */
    def apply(indexes: CollectionIndex*) = indexes.toList match {
      case IntegerIndex(i) :: Nil if (i >= 0  &&  i < size) => values(i)
      case IntegerIndex(i) :: rest if (i >= 0  &&  i < size) => values(i) match {
        case sub: Collection => sub(rest: _*)
        case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Indexing: ${indexes.mkString(", ")}""")
      }
      case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Indexing: ${indexes.mkString(", ")}""")
    }

    def zero = new Indexing[V](0.0, values.map(_.zero): _*)
    def +(that: Indexing[V]) =
      if (this.size != that.size)
        throw new ContainerException(s"""cannot add Indexing because they have different sizes: (${this.size} vs ${that.size})""")
      else
        new Indexing[V](this.entries + that.entries, this.values zip that.values map {case (me, you) => me + you}: _*)

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      checkForCrossReferences()
      var i = 0
      while (i < size) {
        val v = values(i)
        v.fill(datum.asInstanceOf[v.Datum], weight)   // This type is ensured, but Scala doesn't recognize it.
        i += 1                                        // Also, Scala undergoes infinite recursion in a
      }                                               // "foreach" version of this loop--- that's weird!
      // no possibility of exception from here on out (for rollback)
      entries += weight
    }

    def children = values.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject("entries" -> JsonFloat(entries), "type" -> JsonString(values.head.factory.name), "data" -> JsonArray(values.map(_.toJsonFragment(false)): _*))

    override def toString() = s"""<Indexing values=${values.head.factory.name} size=${size}>"""
    override def equals(that: Any) = that match {
      case that: Indexing[V] => this.entries === that.entries  &&  this.values == that.values
      case _ => false
    }
    override def hashCode() = (entries, values).hashCode
  }

  //////////////////////////////////////////////////////////////// Branch/Branched/Branching

  /** Accumulate aggregators of different types, indexed by i0 through i9. Every sub-aggregator is filled with every input datum.
    * 
    * This primitive provides an anonymous collection of aggregators of ''different types,'' usually for gluing together various statistics. For instance, if the following associates a sum of weights to every bin in a histogram,
    * 
    * {{{Bin.ing(100, 0, 1, {d: Datum => d.x},
    *   Sum.ing({d: Datum => d.weight}))}}}
    * 
    * the following would associate the sum of weights and the sum of squared weights to every bin:
    * 
    * {{{Bin.ing(100, 0, 1, {d: Datum => d.x},
    *   Branch.ing(Sum.ing({d: Datum => d.weight}),
    *              Sum.ing({d: Datum => d.weight*d.weight})))}}}
    * 
    * Branch is a basic building block for complex aggregators. The limitation to ten branches, indexed from i0 to i9, is a concession to type inference in statically typed languages. It is not a fundamental limit, but the type-metaprogramming becomes increasingly complex as branches are added. Error messages may be convoluted as the compiler presents internals of the type-metaprogramming in response to a user's simple mistake.
    * 
    * Therefore, individual implementations may allow more than ten branches, but the Histogrammar standard only requires ten.
    * 
    * To collect an unlimited number of aggregators of the ''same type'' without naming them, use [[org.dianahep.histogrammar.Index]]. To collect aggregators of the ''same type'' with string-based labels, use [[org.dianahep.histogrammar.Label]]. To collect aggregators of ''different types'' with string-based labels, use [[org.dianahep.histogrammar.UntypedLabel]].
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
    def help = "Accumulate aggregators of different types, indexed by i0 through i9. Every sub-aggregator is filled with every input datum."
    def detailedHelp = """This primitive provides an anonymous collection of aggregators of ''different types,'' usually for gluing together various statistics. For instance, if the following associates a sum of weights to every bin in a histogram,

{{{Bin.ing(100, 0, 1, {d: Datum => d.x},
  Sum.ing({d: Datum => d.weight}))}}}

the following would associate the sum of weights and the sum of squared weights to every bin:

{{{Bin.ing(100, 0, 1, {d: Datum => d.x},
  Branch.ing(Sum.ing({d: Datum => d.weight}),
             Sum.ing({d: Datum => d.weight*d.weight})))}}}

Branch is a basic building block for complex aggregators. The limitation to ten branches, indexed from i0 to i9, is a concession to type inference in statically typed languages. It is not a fundamental limit, but the type-metaprogramming becomes increasingly complex as branches are added. Error messages may be convoluted as the compiler presents internals of the type-metaprogramming in response to a user's simple mistake.

Therefore, individual implementations may allow more than ten branches, but the Histogrammar standard only requires ten.

To collect an unlimited number of aggregators of the ''same type'' without naming them, use [[org.dianahep.histogrammar.Index]]. To collect aggregators of the ''same type'' with string-based labels, use [[org.dianahep.histogrammar.Label]]. To collect aggregators of ''different types'' with string-based labels, use [[org.dianahep.histogrammar.UntypedLabel]]."""

    def ed[C0 <: Container[C0] with NoAggregation](entries: Double, i0: C0) = new Branched(entries, i0, BranchedNil)
    def ed[C0 <: Container[C0] with NoAggregation, C1 <: Container[C1] with NoAggregation](entries: Double, i0: C0, i1: C1) = new Branched(entries, i0, new Branched(entries, i1, BranchedNil))
    def ed[C0 <: Container[C0] with NoAggregation, C1 <: Container[C1] with NoAggregation, C2 <: Container[C2] with NoAggregation](entries: Double, i0: C0, i1: C1, i2: C2) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, BranchedNil)))
    def ed[C0 <: Container[C0] with NoAggregation, C1 <: Container[C1] with NoAggregation, C2 <: Container[C2] with NoAggregation, C3 <: Container[C3] with NoAggregation](entries: Double, i0: C0, i1: C1, i2: C2, i3: C3) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, new Branched(entries, i3, BranchedNil))))
    def ed[C0 <: Container[C0] with NoAggregation, C1 <: Container[C1] with NoAggregation, C2 <: Container[C2] with NoAggregation, C3 <: Container[C3] with NoAggregation, C4 <: Container[C4] with NoAggregation](entries: Double, i0: C0, i1: C1, i2: C2, i3: C3, i4: C4) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, new Branched(entries, i3, new Branched(entries, i4, BranchedNil)))))
    def ed[C0 <: Container[C0] with NoAggregation, C1 <: Container[C1] with NoAggregation, C2 <: Container[C2] with NoAggregation, C3 <: Container[C3] with NoAggregation, C4 <: Container[C4] with NoAggregation, C5 <: Container[C5] with NoAggregation](entries: Double, i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, new Branched(entries, i3, new Branched(entries, i4, new Branched(entries, i5, BranchedNil))))))
    def ed[C0 <: Container[C0] with NoAggregation, C1 <: Container[C1] with NoAggregation, C2 <: Container[C2] with NoAggregation, C3 <: Container[C3] with NoAggregation, C4 <: Container[C4] with NoAggregation, C5 <: Container[C5] with NoAggregation, C6 <: Container[C6] with NoAggregation](entries: Double, i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, new Branched(entries, i3, new Branched(entries, i4, new Branched(entries, i5, new Branched(entries, i6, BranchedNil)))))))
    def ed[C0 <: Container[C0] with NoAggregation, C1 <: Container[C1] with NoAggregation, C2 <: Container[C2] with NoAggregation, C3 <: Container[C3] with NoAggregation, C4 <: Container[C4] with NoAggregation, C5 <: Container[C5] with NoAggregation, C6 <: Container[C6] with NoAggregation, C7 <: Container[C7] with NoAggregation](entries: Double, i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, new Branched(entries, i3, new Branched(entries, i4, new Branched(entries, i5, new Branched(entries, i6, new Branched(entries, i7, BranchedNil))))))))
    def ed[C0 <: Container[C0] with NoAggregation, C1 <: Container[C1] with NoAggregation, C2 <: Container[C2] with NoAggregation, C3 <: Container[C3] with NoAggregation, C4 <: Container[C4] with NoAggregation, C5 <: Container[C5] with NoAggregation, C6 <: Container[C6] with NoAggregation, C7 <: Container[C7] with NoAggregation, C8 <: Container[C8] with NoAggregation](entries: Double, i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7, i8: C8) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, new Branched(entries, i3, new Branched(entries, i4, new Branched(entries, i5, new Branched(entries, i6, new Branched(entries, i7, new Branched(entries, i8, BranchedNil)))))))))
    def ed[C0 <: Container[C0] with NoAggregation, C1 <: Container[C1] with NoAggregation, C2 <: Container[C2] with NoAggregation, C3 <: Container[C3] with NoAggregation, C4 <: Container[C4] with NoAggregation, C5 <: Container[C5] with NoAggregation, C6 <: Container[C6] with NoAggregation, C7 <: Container[C7] with NoAggregation, C8 <: Container[C8] with NoAggregation, C9 <: Container[C9] with NoAggregation](entries: Double, i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7, i8: C8, i9: C9) = new Branched(entries, i0, new Branched(entries, i1, new Branched(entries, i2, new Branched(entries, i3, new Branched(entries, i4, new Branched(entries, i5, new Branched(entries, i6, new Branched(entries, i7, new Branched(entries, i8, new Branched(entries, i9, BranchedNil))))))))))

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

    import KeySetComparisons._
    def fromJsonFragment(json: Json, nameFromParent: Option[String]): Container[_] with NoAggregation = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet has Set("entries", "data")) =>
        val get = pairs.toMap

        val entries = get("entries") match {
          case JsonNumber(x) => x
          case x => throw new JsonFormatException(x, name + ".entries")
        }

        get("data") match {
          case JsonArray(values @ _*) if (values.size >= 1) =>
            var backwards: BranchedList = BranchedNil

            values.zipWithIndex.toList foreach {
              case (JsonObject(typedata @ _*), i) if (typedata.keySet has Set("type", "data")) =>
                val subget = typedata.toMap

                (subget("type"), subget("data")) match {
                  case (JsonString(factory), sub) =>
                    val item = Factory(factory).fromJsonFragment(sub, None).asInstanceOf[C forSome {type C <: Container[C] with NoAggregation}]
                    backwards = new Branched(entries, item, backwards)
                  case (_, x) => throw new JsonFormatException(x, name + s".data")
                }

              case (x, i) => throw new JsonFormatException(x, name + s".data $i")
            }

            // we've loaded it backwards, so reverse the order before returning it
            var out: BranchedList = BranchedNil
            while (backwards != BranchedNil) {
              val list = backwards.asInstanceOf[Branched[C forSome {type C <: Container[C] with NoAggregation}, BranchedList]]
              out = new Branched(list.entries, list.head, out)
              backwards = list.tail
            }
            out.asInstanceOf[Container[_] with NoAggregation]

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

  object BranchedNil extends BranchedList with Serializable {
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
  class Branched[HEAD <: Container[HEAD] with NoAggregation, TAIL <: BranchedList] private[histogrammar](val entries: Double, val head: HEAD, val tail: TAIL) extends Container[Branched[HEAD, TAIL]] with NoAggregation with Collection with BranchedList {
    type Type = Branched[HEAD, TAIL]
    type EdType = Branched[HEAD, TAIL]
    def factory = Branch

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    /** List the containers (dropping type information). */
    def values: List[Container[_]] = head :: tail.values
    /** Return the number of containers. */
    def size: Int = 1 + tail.size
    /** Attempt to get index `i`, returning `None` if it does not exist. */
    def get(i: Int) =
      if (i == 0)
        Some(head)
      else
        tail.get(i - 1)
    /** Attempt to get index `i`, returning an alternative if it does not exist. */
    def getOrElse(i: Int, default: => Container[_]) = get(i).getOrElse(default)

    /** Attempt to get key `index`, throwing an exception if it does not exist. */
    def apply(index: CollectionIndex) = index match {
      case IntegerIndex(i) if (i >= 0  &&  i < size) => get(i).get
      case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Branched: $index""")
    }
    /** Attempt to get key `indexes`, throwing an exception if it does not exist. */
    def apply(indexes: CollectionIndex*) = indexes.toList match {
      case IntegerIndex(i) :: Nil if (i >= 0  &&  i < size) => get(i).get
      case IntegerIndex(i) :: rest if (i >= 0  &&  i < size) => get(i).get match {
        case sub: Collection => sub(rest: _*)
        case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Branched: ${indexes.mkString(", ")}""")
      }
      case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Branched: ${indexes.mkString(", ")}""")
    }

    def zero = new Branched[HEAD, TAIL](0.0, head.zero, tail.zero.asInstanceOf[TAIL])
    def +(that: Branched[HEAD, TAIL]) = tail match {
      case t: Branched[_, _] => new Branched[HEAD, TAIL](this.entries + that.entries, this.head + that.head, (t + that.tail.asInstanceOf[t.Type]).asInstanceOf[TAIL])
      case _: BranchedNil.type => new Branched[HEAD, TAIL](this.entries + that.entries, this.head + that.head, BranchedNil.asInstanceOf[TAIL])
    }

    def children = values.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "data" -> JsonArray(values.map(x => JsonObject("type" -> JsonString(x.factory.name), "data" -> x.toJsonFragment(false))): _*))

    override def toString() = s"""<Branched ${values.zipWithIndex.map({case (v, i) => "i" + i.toString + "=" + v.factory.name}).mkString(" ")}>"""
    override def equals(that: Any) = that match {
      case that: Branched[_, _] => this.entries === that.entries  &&  this.head == that.head  &&  this.tail == that.tail
      case _ => false
    }
    override def hashCode() = (entries, values).hashCode
  }

  sealed trait BranchingList {
    type EdType <: BranchedList
    def values: List[Container[_]]
    def size: Int
    def get(i: Int): Option[Container[_]]
    def zero: BranchingList
  }

  object BranchingNil extends BranchingList with Serializable {
    type EdType = BranchedNil.type
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
  class Branching[HEAD <: Container[HEAD] with Aggregation, TAIL <: BranchingList] private[histogrammar](var entries: Double, val head: HEAD, val tail: TAIL) extends Container[Branching[HEAD, TAIL]] with AggregationOnData with Collection with BranchingList {
    type Type = Branching[HEAD, TAIL]
    type EdType = Branched[head.EdType, tail.EdType]
    type Datum = head.Datum
    def factory = Branch

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    /** List the containers (dropping type information). */
    def values: List[Container[_]] = head :: tail.values
    /** Return the number of containers. */
    def size: Int = 1 + tail.size
    /** Attempt to get index `i`, returning `None` if it does not exist. */
    def get(i: Int) =
      if (i == 0)
        Some(head)
      else
        tail.get(i - 1)
    /** Attempt to get index `i`, returning an alternative if it does not exist. */
    def getOrElse(i: Int, default: => Container[_]) = get(i).getOrElse(default)

    /** Attempt to get key `index`, throwing an exception if it does not exist. */
    def apply(index: CollectionIndex) = index match {
      case SymbolIndex('i0) if (0 < size) => get(0).get
      case SymbolIndex('i1) if (1 < size) => get(1).get
      case SymbolIndex('i2) if (2 < size) => get(2).get
      case SymbolIndex('i3) if (3 < size) => get(3).get
      case SymbolIndex('i4) if (4 < size) => get(4).get
      case SymbolIndex('i5) if (5 < size) => get(5).get
      case SymbolIndex('i6) if (6 < size) => get(6).get
      case SymbolIndex('i7) if (7 < size) => get(7).get
      case SymbolIndex('i8) if (8 < size) => get(8).get
      case SymbolIndex('i9) if (9 < size) => get(9).get
      case IntegerIndex(i) if (i >= 0  &&  i < size) => get(i).get
      case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Branching: $index""")
    }

    /** Attempt to get key `indexes`, throwing an exception if it does not exist. */
    def apply(indexes: CollectionIndex*) = indexes.toList match {
      case SymbolIndex('i0) :: Nil if (0 < size) => get(0).get
      case SymbolIndex('i1) :: Nil if (1 < size) => get(1).get
      case SymbolIndex('i2) :: Nil if (2 < size) => get(2).get
      case SymbolIndex('i3) :: Nil if (3 < size) => get(3).get
      case SymbolIndex('i4) :: Nil if (4 < size) => get(4).get
      case SymbolIndex('i5) :: Nil if (5 < size) => get(5).get
      case SymbolIndex('i6) :: Nil if (6 < size) => get(6).get
      case SymbolIndex('i7) :: Nil if (7 < size) => get(7).get
      case SymbolIndex('i8) :: Nil if (8 < size) => get(8).get
      case SymbolIndex('i9) :: Nil if (9 < size) => get(9).get
      case IntegerIndex(i) :: Nil if (i >= 0  &&  i < size) => get(i).get

      case SymbolIndex('i0) :: rest if (0 < size) => get(0).get match {
        case sub: Collection => sub(rest: _*)
        case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Branching: ${indexes.mkString(", ")}""")
      }
      case SymbolIndex('i1) :: rest if (1 < size) => get(1).get match {
        case sub: Collection => sub(rest: _*)
        case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Branching: ${indexes.mkString(", ")}""")
      }
      case SymbolIndex('i2) :: rest if (2 < size) => get(2).get match {
        case sub: Collection => sub(rest: _*)
        case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Branching: ${indexes.mkString(", ")}""")
      }
      case SymbolIndex('i3) :: rest if (3 < size) => get(3).get match {
        case sub: Collection => sub(rest: _*)
        case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Branching: ${indexes.mkString(", ")}""")
      }
      case SymbolIndex('i4) :: rest if (4 < size) => get(4).get match {
        case sub: Collection => sub(rest: _*)
        case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Branching: ${indexes.mkString(", ")}""")
      }
      case SymbolIndex('i5) :: rest if (5 < size) => get(5).get match {
        case sub: Collection => sub(rest: _*)
        case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Branching: ${indexes.mkString(", ")}""")
      }
      case SymbolIndex('i6) :: rest if (6 < size) => get(6).get match {
        case sub: Collection => sub(rest: _*)
        case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Branching: ${indexes.mkString(", ")}""")
      }
      case SymbolIndex('i7) :: rest if (7 < size) => get(7).get match {
        case sub: Collection => sub(rest: _*)
        case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Branching: ${indexes.mkString(", ")}""")
      }
      case SymbolIndex('i8) :: rest if (8 < size) => get(8).get match {
        case sub: Collection => sub(rest: _*)
        case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Branching: ${indexes.mkString(", ")}""")
      }
      case SymbolIndex('i9) :: rest if (9 < size) => get(9).get match {
        case sub: Collection => sub(rest: _*)
        case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Branching: ${indexes.mkString(", ")}""")
      }
      case IntegerIndex(i) :: rest if (i >= 0  &&  i < size) => get(i).get match {
        case sub: Collection => sub(rest: _*)
        case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Branching: ${indexes.mkString(", ")}""")
      }

      case _ => throw new IllegalArgumentException(s"""wrong type or out of bounds index for Branching: ${indexes.mkString(", ")}""")
    }

    def zero = new Branching[HEAD, TAIL](0.0, head.zero, tail.zero.asInstanceOf[TAIL])
    def +(that: Branching[HEAD, TAIL]) = tail match {
      case t: Branching[_, _] => new Branching[HEAD, TAIL](this.entries + that.entries, this.head + that.head, (t + that.tail.asInstanceOf[t.Type]).asInstanceOf[TAIL])
      case _: BranchingNil.type => new Branching[HEAD, TAIL](this.entries + that.entries, this.head + that.head, BranchingNil.asInstanceOf[TAIL])
    }

    def fill[SUB <: Datum](datum: SUB, weight: Double = 1.0) {
      checkForCrossReferences()
      head.fill(datum, weight)
      tail match {
        case x: Aggregation => x.fill(datum.asInstanceOf[x.Datum], weight)
        case _ =>
      }
      // no possibility of exception from here on out (for rollback)
      entries += weight
    }

    def children = values.toList

    def toJsonFragment(suppressName: Boolean) = JsonObject(
      "entries" -> JsonFloat(entries),
      "data" -> JsonArray(values.map(x => JsonObject("type" -> JsonString(x.factory.name), "data" -> x.toJsonFragment(false))): _*))

    override def toString() = s"""<Branching ${values.zipWithIndex.map({case (v, i) => "i" + i.toString + "=" + v.factory.name}).mkString(" ")}>"""
    override def equals(that: Any) = that match {
      case that: Branching[_, _] => this.entries === that.entries  &&  this.head == that.head  &&  this.tail == that.tail
      case _ => false
    }
    override def hashCode() = (entries, values).hashCode
  }
}
