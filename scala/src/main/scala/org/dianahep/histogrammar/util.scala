package org.dianahep.histogrammar

import scala.collection.SortedMap
import scala.collection.SortedSet

import org.dianahep.histogrammar._

// package object util {
//   implicit val doubleOrdering: MetricOrdering[Double] = new MetricOrdering[Double] {
//     def difference(x: Double, y: Double) = x - y
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
    def difference(x: T, y: T): Double
    def distance(x: T, y: T) = Math.abs(difference(x, y))
    def compare(x: T, y: T) = {
      val d = difference(x, y)
      if (d > 0.0) 1
      else if (d < 0.0) -1
      else 0
    }
  }

  abstract class MetricSortedMap[A, B](elems: (A, B)*)(ordering: MetricOrdering[A]) extends SortedMap[A, B] {
    // when the TreeSet searches for an element, keep track of the best distance it finds
    private val best1 = new java.lang.ThreadLocal[(Double, A, B)]
    private val best2 = new java.lang.ThreadLocal[(Double, A, B)]
    best1.set((-1.0, null.asInstanceOf[A], null.asInstanceOf[B]))
    best2.set((-1.0, null.asInstanceOf[A], null.asInstanceOf[B]))

    protected val ord = new MetricOrdering[(A, B)] {
      def difference(x: (A, B), y: (A, B)) = {
        val diff = ordering.difference(x._1, y._1)
        val absdiff = Math.abs(diff)

        if (absdiff < best1.get._1)
          (x, y) match {
            case ((to, null), (pos, obj)) =>
              best2.set(best1.get)
              best1.set((absdiff, pos, obj))

            case ((pos, obj), (to, null)) =>
              best2.set(best1.get)
              best1.set((absdiff, pos, obj))

            case _ =>
          }
        else if (absdiff < best2.get._1)
          (x, y) match {
            case ((to, null), (pos, obj)) =>
              best2.set((absdiff, pos, obj))

            case ((pos, obj), (to, null)) =>
              best2.set((absdiff, pos, obj))

            case _ =>
          }

        diff
      }
    }

    // use a TreeSet as a backing (not TreeMap because we need to get the whole pair back when we query it)
    def treeSet: SortedSet[(A, B)]

    // find the closest key and return: (difference from key, the key, its associated value)
    def closest(to: A): (Double, A, B) = {
      treeSet.headOption match {
        case Some((pos, obj)) =>
          best1.set((ordering.difference(to, pos), pos, obj))
          best2.set((ordering.difference(to, pos), pos, obj))
        case None =>
          throw new java.util.NoSuchElementException("SortedMap has no elements, and hence no closest element")
      }

      treeSet((to, null.asInstanceOf[B]))  // called for its side effects on "best1"

      best1.get
    }

    // find the closest two keys and return: ((difference from closest key1, key1, value1), (difference from second-closest key2, key2, value2))
    def closest2(to: A): ((Double, A, B), (Double, A, B)) = {
      val iter = treeSet.iterator
      if (!iter.hasNext)
        throw new java.util.NoSuchElementException("SortedMap has no elements, and hence no closest element")
      val first = iter.next()
      if (!iter.hasNext)
        throw new java.util.NoSuchElementException("SortedMap has fewer than two elements, and hence no two closest elements")
      val second = iter.next()

      (first, second) match {
        case ((pos1, obj1), (pos2, obj2)) if (ordering.distance(to, pos1) < ordering.distance(to, pos2)) =>
          best1.set((ordering.difference(to, pos1), pos1, obj1))
          best2.set((ordering.difference(to, pos2), pos2, obj2))

        case ((pos2, obj2), (pos1, obj1)) if (ordering.distance(to, pos1) < ordering.distance(to, pos2)) =>
          best1.set((ordering.difference(to, pos1), pos1, obj1))
          best2.set((ordering.difference(to, pos2), pos2, obj2))
      }

      treeSet((to, null.asInstanceOf[B]))  // called for its side effects on "best1" and "best2"

      (best1.get, best2.get)
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

  class Clustering1D[CONTAINER <: Container[CONTAINER]](val num: Int, value: => CONTAINER, elems: (Double, CONTAINER)*)
      extends mutable.MetricSortedMap[Double, CONTAINER](elems: _*)(new MetricOrdering[Double] {def difference(x: Double, y: Double) = x - y}) {

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

    // Ben-Haim and Tom-Tov's "Algorithm 1"
    def update[DATUM](x: Double, datum: DATUM, weight: Double) {
      // Here we subtly assume this CONTAINER has Aggregation so we can call fillWeighted.
      // You wouldn't use it on a non-aggregatable container, would you?
      get(x) match {
        case Some(v) =>
          v.asInstanceOf[CONTAINER with Aggregation{type Datum >: DATUM}].fillWeighted(datum, weight)

        case None =>
          val v = value   // create a new one
          v.asInstanceOf[CONTAINER with Aggregation{type Datum >: DATUM}].fillWeighted(datum, weight)

          this += (x, v)
          cluster(num)
      }
    }

    // Ben-Haim and Tom-Tov's "Algorithm 2"
    def merge(that: Clustering1D[CONTAINER]): Clustering1D[CONTAINER] = {
      val bins = scala.collection.mutable.Map(this.iterator.toSeq: _*)
      that.iterator foreach {case (x, v) =>
        if (bins contains x)
          bins(x) = bins(x) + v   // replace them; don't update them in-place
        else
          bins(x) = v
      }

      val out = new Clustering1D[CONTAINER](num, value, bins.toSeq: _*)
      out.cluster(num)
      out
    }

    // Ben-Haim and Tom-Tov's "Algorithm 3" modified so that bin ps are interpreted as centers, rather than low edges (for consistency with the above)
    def sum(bs: Double*): Seq[Double] =
      if (isEmpty)
        Array.fill(bs.size)(0.0)
      else if (size == 1)
        bs.toArray.map(b => if (treeSet.head._1 < b) 0.0 else if (treeSet.head._1 == b) treeSet.head._2.entries/2.0 else treeSet.head._2.entries).toSeq
      else {
        val out = Array.fill(bs.size)(0.0)
        var total = 0.0

        val bins = iterator.toList
        val neighbors = bins.init zip bins.tail

        // TODO: think about this more
        // make sure that the ps are the CENTERS of bins, and HALF of the corresponding m.entries are counted at that line
        // below the first point and above the last, fit an exponential with slope to match the closest two points

        for (((p1, m1), (p2, m2)) <- neighbors) {
          for ((b, i) <- bs.zipWithIndex) {
            if (p1 <= b  &&  b < p2) {
              val mb = m1.entries + (m2.entries - m1.entries)*(b - p1)/(p2 - p1)
              val s = (m1.entries + mb)*(b - p1)/(p2 - p1)/2.0
              out(i) = total + m1.entries/2.0 + s
            }
          }
          total += m1.entries
        }
        val (pn, mn) = bins.last
        total += mn.entries

        for ((b, i) <- bs.zipWithIndex) {
          if (pn <= b)
            out(i) = total
        }

        out.toSeq
      }
  }

}
