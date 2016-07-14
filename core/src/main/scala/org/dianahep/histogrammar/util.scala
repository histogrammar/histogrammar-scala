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

package org.dianahep.histogrammar

import scala.util.Random

import scala.collection.SortedMap
import scala.collection.SortedSet

import org.dianahep.histogrammar._

/** Supporting functions, mostly called by those in [[org.dianahep.histogrammar]]. */
package object util {
  /** The natural [[org.dianahep.histogrammar.util.MetricOrdering]] for `Double` precision numbers. */
  implicit val doubleOrdering: MetricOrdering[Double] = new MetricOrdering[Double] {
    def difference(x: Double, y: Double) = x - y
  }

  /** Relative tolerance for numerical equality. Only affects `equals` checks, not `fill` or `+`. */
  var relativeTolerance: Double = 0.0
  /** Absolute tolerance for numerical equality. Only affects `equals` checks, not `fill` or `+`. */
  var absoluteTolerance: Double = 0.0
}

package util {
  //////////////////////////////////////////////////////////////// type-level programming to test compatibility of user-defined functions

  private[histogrammar] trait Compatible[-X, -Y]
  private[histogrammar] object Compatible {
    implicit object BothAreCounting extends Compatible[Counting, Counting]
    implicit object XIsCounting extends Compatible[Counting, AggregationOnData]
    implicit object YIsCounting extends Compatible[AggregationOnData, Counting]
    implicit def dataAreCompatible[X <: AggregationOnData, Y <: AggregationOnData](implicit evidence: X#Datum =:= Y#Datum) = new Compatible[X, Y] {}
  }

  //////////////////////////////////////////////////////////////// handling key set comparisons with optional keys

  object KeySetComparisons {
    trait KeySet {
      def required: Set[String]
      def optional: Set[String]

      def maybe(string: String) = {
        val t = this
        new KeySet {
          def required = t.required
          def optional = t.optional ++ Set(string)
        }
      }
    }

    implicit class KeySetFromSet(test: Set[String]) extends KeySet {
      def required = test
      def optional = Set[String]()
      def has(that: KeySet) = (that.required subsetOf test)  &&  (test subsetOf (that.required ++ that.optional))
    }
  }

  //////////////////////////////////////////////////////////////// the metric equivalent of a SortedMap, used in a few containers

  /** Extension of Scala's `Ordering` trait to include distance. Used by [[org.dianahep.histogrammar.util.MetricSortedMap]]. */
  trait MetricOrdering[T] extends Ordering[T] {
    /** Extension of `compare` with distance. Sign has the same meaning for ordering as in `compare`. */
    def difference(x: T, y: T): Double
    /** Absolute value of `difference`. */
    def distance(x: T, y: T) = Math.abs(difference(x, y))
    /** Comparison function defined by `Ordering`. */
    def compare(x: T, y: T) = {
      val d = difference(x, y)
      if (d > 0.0) 1
      else if (d < 0.0) -1
      else 0
    }
  }

  /** Result of [[org.dianahep.histogrammar.util.MetricSortedMap]]'s `closest` method.
    * 
    * @param difference Signed difference between the requested point and the closest match. Absolute value of this is a the distance.
    * @param key Closest matching key in the [[org.dianahep.histogrammar.util.MetricSortedMap]].
    * @param value Value associated with the closest matching key.
    */
  case class Closest[A, B](difference: Double, key: A, value: B) {
    /** Convenience function for the absolute value of difference. */
    def distance = Math.abs(difference)
  }

  /** Extension of Scala's `SortedMap` with a [[org.dianahep.histogrammar.util.MetricOrdering]]. */
  abstract class MetricSortedMap[A, B](elems: (A, B)*)(ordering: MetricOrdering[A]) extends SortedMap[A, B] {
    // when the TreeSet searches for an element, keep track of the best distance it finds
    private val best = new java.lang.ThreadLocal[Tuple2[Option[Closest[A, B]], Option[Function2[A, B, Boolean]]]]

    protected val ord = new MetricOrdering[(A, B)] {
      def difference(x: (A, B), y: (A, B)) = {
        val diff = ordering.difference(x._1, y._1)

        if (best.get != null) {
          val (pos, obj) = (x, y) match {
            case ((to, null), (pos, obj)) => (pos, obj)
            case ((pos, obj), (to, null)) => (pos, obj)
          }

          val (current, constraint) = best.get

          if (current.isEmpty  ||  Math.abs(diff) < Math.abs(current.get.difference)) {
            val check = constraint match {
              case Some(f) => f(pos, obj)
              case None => true
            }
            if (check)
              best.set((Some(Closest(diff, pos, obj)), constraint))
          }
        }

        diff
      }
    }

    /** Used to provide a logarithmic-complexity lookup of key-value pairs. */
    def treeSet: SortedSet[(A, B)]

    /** Find the closest key to a given value, optionally with a constraint. */
    def closest(to: A, constraint: Option[Function2[A, B, Boolean]] = None): Option[Closest[A, B]] = {
      best.set((None, constraint))

      treeSet((to, null.asInstanceOf[B]))  // called for its side effects on "best"

      val out = best.get._1
      best.set(null.asInstanceOf[Tuple2[Option[Closest[A, B]], Option[Function2[A, B, Boolean]]]])
      out
    }
  }

  package immutable {
    /** Concrete immutable `MetricSortedMap`. */
    class MetricSortedMap[A, B](elems: (A, B)*)(implicit val ordering: MetricOrdering[A]) extends org.dianahep.histogrammar.util.MetricSortedMap[A, B](elems: _*)(ordering) {
      val treeSet = scala.collection.immutable.TreeSet[(A, B)](elems: _*)(ord)

      def +[B1 >: B](kv: (A, B1)): SortedMap[A, B1] = new MetricSortedMap[A, B](elems :+ (kv._1, kv._2.asInstanceOf[B]): _*)
      def -(key: A): SortedMap[A, B] = new MetricSortedMap[A, B](elems.filter(_._1 != key): _*)
      def get(key: A): Option[B] = treeSet.find(_._1 == key).map(_._2)
      def iterator: Iterator[(A, B)] = treeSet.iterator
      override def size: Int = treeSet.size
      override def contains(x: A) = treeSet contains (x, null.asInstanceOf[B])

      /** Not implemented: raises `UnsupportedOperationException`. */
      def rangeImpl(from: Option[A], until: Option[A]): SortedMap[A, B] = throw new UnsupportedOperationException
    }
    object MetricSortedMap {
      def apply[A, B](elems: (A, B)*)(implicit ordering: MetricOrdering[A]) = new MetricSortedMap[A, B](elems: _*)(ordering)
    }
  }

  package mutable {
    /** Concrete mutable `MetricSortedMap`. */
    class MetricSortedMap[A, B](elems: (A, B)*)(implicit val ordering: MetricOrdering[A]) extends org.dianahep.histogrammar.util.MetricSortedMap[A, B](elems: _*)(ordering) {
      val treeSet = scala.collection.mutable.TreeSet[(A, B)](elems: _*)(ord)

      def +[B1 >: B](kv: (A, B1)): SortedMap[A, B1] = new MetricSortedMap[A, B](elems :+ (kv._1, kv._2.asInstanceOf[B]): _*)
      def -(key: A): SortedMap[A, B] = new MetricSortedMap[A, B](elems.filter(_._1 != key): _*)
      def get(key: A): Option[B] = treeSet.find(_._1 == key).map(_._2)
      def iterator: Iterator[(A, B)] = treeSet.iterator
      override def size: Int = treeSet.size
      override def contains(x: A) = treeSet contains (x, null.asInstanceOf[B])

      def +=(kv: (A, B)): MetricSortedMap[A, B] = {treeSet += kv; this}
      def -=(elem: A): MetricSortedMap[A, B] = {treeSet -= Tuple2(elem, null.asInstanceOf[B]); this}

      /** Not implemented: raises `UnsupportedOperationException`. */
      def rangeImpl(from: Option[A], until: Option[A]): SortedMap[A, B] = throw new UnsupportedOperationException
    }
    object MetricSortedMap {
      def apply[A, B](elems: (A, B)*)(implicit ordering: MetricOrdering[A]) = new MetricSortedMap[A, B](elems: _*)(ordering)
    }
  }
}
