package org.dianahep.histogrammar

import scala.collection.SortedMap
import scala.collection.SortedSet

import org.dianahep.histogrammar._

// package object util {
//   implicit val doubleOrdering: MetricOrdering[Double] = new MetricOrdering[Double] {
//     def distance(x: Double, y: Double) = x - y
//   }
// }

package util {
  //////////////////////////////////////////////////////////////// avoid recomputing a function that appears in many histograms

  case class Cache[DOMAIN, RANGE](f: DOMAIN => RANGE) extends Function1[DOMAIN, RANGE] {
    private var last: Option[(DOMAIN, RANGE)] = None
    def apply(x: DOMAIN): RANGE = (x, last) match {
      case (xref: AnyRef, Some((oldx: AnyRef, oldy))) if (xref eq oldx) => oldy
      case (_,            Some((oldx, oldy)))         if (x == oldx)    => oldy
      case _ =>
        val y = f(x)
        last = Some(x -> y)
        y
    }
    def clear() { last = None }
  }

  //////////////////////////////////////////////////////////////// the metric equivalent of a SortedMap, used in a few containers

  trait MetricOrdering[T] extends Ordering[T] {
    def distance(x: T, y: T): Double
    def compare(x: T, y: T) = {
      val d = distance(x, y)
      if (d > 0.0) 1
      else if (d < 0.0) -1
      else 0
    }
  }

  abstract class MetricSortedMap[A, B](elems: (A, B)*)(ordering: MetricOrdering[A]) extends SortedMap[A, B] {
    // when the TreeSet searches for an element, keep track of the best distance it finds
    private val best = new java.lang.ThreadLocal[(Double, A, B)]
    best.set((-1.0, null.asInstanceOf[A], null.asInstanceOf[B]))

    protected val ord = new MetricOrdering[(A, B)] {
      def distance(x: (A, B), y: (A, B)) = {
        val diff = ordering.distance(x._1, y._1)
        val absdiff = Math.abs(diff)

        if (absdiff < best.get._1)
          (x, y) match {
            case ((to, null), (pos, obj)) =>
              best.set((absdiff, pos, obj))

            case ((pos, obj), (to, null)) =>
              best.set((absdiff, pos, obj))

            case _ =>
          }

        diff
      }
    }

    // use a TreeSet as a backing (not TreeMap because we need to get the whole pair back when we query it)
    def treeSet: SortedSet[(A, B)]

    // find the closest key and return: (distance to key, the key, its associated value)
    def closest(to: A): (Double, A, B) = {
      treeSet.headOption match {
        case Some((pos, obj)) => best.set((ordering.distance(to, pos), pos, obj))
        case None =>
          throw new java.util.NoSuchElementException("SortedMap has no elements, and hence no closest element")
      }

      treeSet((to, null.asInstanceOf[B]))  // called for its side effect on "best"

      best.get
    }
  }

  package immutable {
    class MetricSortedMap[A, B](elems: (A, B)*)(implicit val ordering: MetricOrdering[A]) extends org.dianahep.histogrammar.util.MetricSortedMap[A, B](elems: _*)(ordering) {
      val treeSet = scala.collection.immutable.TreeSet[(A, B)](elems: _*)(ord)

      def +[B1 >: B](kv: (A, B1)): SortedMap[A, B1] = new MetricSortedMap[A, B](elems :+ (kv._1, kv._2.asInstanceOf[B]): _*)
      def -(key: A): SortedMap[A, B] = new MetricSortedMap[A, B](elems.filter(_._1 != key): _*)
      def get(key: A): Option[B] = treeSet.find(_._1 == key).map(_._2)
      def iterator: Iterator[(A, B)] = treeSet.iterator
      override def size: Int = treeSet.size
      override def contains(x: A) = treeSet contains (x, null.asInstanceOf[B])

      def rangeImpl(from: Option[A], until: Option[A]): SortedMap[A, B] = throw new UnsupportedOperationException
    }
  }

  package mutable {
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

      def rangeImpl(from: Option[A], until: Option[A]): SortedMap[A, B] = throw new UnsupportedOperationException
    }
  }

  //////////////////////////////////////////////////////////////// 1D clustering algorithm (used by AdaptivelyBin, Median, Percentile)
  // Yael Ben-Haim and Elad Tom-Tov, "A streaming parallel decision tree algorithm",
  // J. Machine Learning Research 11 (2010)
  // http://www.jmlr.org/papers/volume11/ben-haim10a/ben-haim10a.pdf

  class Clustering1D[DATUM, CONTAINER <: Container[CONTAINER] with Aggregation{type Datum = DATUM}](val num: Int, value: => CONTAINER)
      extends mutable.MetricSortedMap[Double, CONTAINER]()(new MetricOrdering[Double] {def distance(x: Double, y: Double) = x - y}) {
    def cluster(n: Int) {
      while (size > n) {
        val bins = iterator.toList
        val neighbors = bins.init zip bins.tail
        val nearestNeighbors = neighbors minBy {case ((x1, v1), (x2, v2)) => x2 - x1}

        val ((x1, v1), (x2, v2)) = nearestNeighbors
        val replacement = ((x1 * v1.entries + x2 * v2.entries) / (v1.entries + v2.entries), v1 + v2)

        this -= x1
        this -= x2
        this += replacement
      }
    }

    def update(x: Double, datum: DATUM, weight: Double) {  // "Algorithm 1"
      get(x) match {
        case Some(v) => v.fillWeighted(datum, weight)
        case None =>
          val v = value   // create a new one
          v.fillWeighted(datum, weight)

          this += (x, v)
          cluster(num)
      }
    }


  }

}
