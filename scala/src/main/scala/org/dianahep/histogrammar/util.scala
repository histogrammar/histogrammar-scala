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
    // private val best2 = new java.lang.ThreadLocal[(Double, A, B)]
    best1.set((-1.0, null.asInstanceOf[A], null.asInstanceOf[B]))
    // best2.set((-1.0, null.asInstanceOf[A], null.asInstanceOf[B]))

    protected val ord = new MetricOrdering[(A, B)] {
      def difference(x: (A, B), y: (A, B)) = {
        val diff = ordering.difference(x._1, y._1)
        val absdiff = Math.abs(diff)

        if (absdiff < best1.get._1)
          (x, y) match {
            case ((to, null), (pos, obj)) =>
              // best2.set(best1.get)
              best1.set((absdiff, pos, obj))

            case ((pos, obj), (to, null)) =>
              // best2.set(best1.get)
              best1.set((absdiff, pos, obj))

            case _ =>
          }
        // else if (absdiff < best2.get._1)
        //   (x, y) match {
        //     case ((to, null), (pos, obj)) =>
        //       best2.set((absdiff, pos, obj))

        //     case ((pos, obj), (to, null)) =>
        //       best2.set((absdiff, pos, obj))

        //     case _ =>
        //   }

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
          // best2.set((ordering.difference(to, pos), pos, obj))
        case None =>
          throw new java.util.NoSuchElementException("SortedMap has no elements, and hence no closest element")
      }

      treeSet((to, null.asInstanceOf[B]))  // called for its side effects on "best1"

      best1.get
    }

    // // find the closest two keys and return: ((difference from closest key1, key1, value1), (difference from second-closest key2, key2, value2))
    // def closest2(to: A): ((Double, A, B), (Double, A, B)) = {
    //   val iter = treeSet.iterator
    //   if (!iter.hasNext)
    //     throw new java.util.NoSuchElementException("SortedMap has no elements, and hence no closest element")
    //   val first = iter.next()
    //   if (!iter.hasNext)
    //     throw new java.util.NoSuchElementException("SortedMap has fewer than two elements, and hence no two closest elements")
    //   val second = iter.next()

    //   (first, second) match {
    //     case ((pos1, obj1), (pos2, obj2)) if (ordering.distance(to, pos1) < ordering.distance(to, pos2)) =>
    //       best1.set((ordering.difference(to, pos1), pos1, obj1))
    //       best2.set((ordering.difference(to, pos2), pos2, obj2))

    //     case ((pos2, obj2), (pos1, obj1)) if (ordering.distance(to, pos1) < ordering.distance(to, pos2)) =>
    //       best1.set((ordering.difference(to, pos1), pos1, obj1))
    //       best2.set((ordering.difference(to, pos2), pos2, obj2))
    //   }

    //   treeSet((to, null.asInstanceOf[B]))  // called for its side effects on "best1" and "best2"

    //   (best1.get, best2.get)
    // }
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

  //////////////////////////////////////////////////////////////// interpretation of central bins as a distribution

  trait CentralBinsDistribution[CONTAINER <: Container[CONTAINER]] {
    def bins: MetricSortedMap[Double, CONTAINER]
    def min: Double
    def max: Double

    def pdf(x: Double): Double = pdf(x, List[Double](): _*).head
    def cdf(x: Double): Double = cdf(x, List[Double](): _*).head

    def pdf(first: Double, rest: Double*): Seq[Double] =
      (first :: rest.toList) map {x =>
        if (bins.isEmpty)
          0.0
        if (bins.size == 1  &&  x == bins.head._1)
          java.lang.Double.POSITIVE_INFINITY
        else if (x < min  ||  x >= max)
          0.0
        else {
          val (diff, key, value) = bins.closest(x)
          value.entries
        }
      }

    def cdf(first: Double, rest: Double*): Seq[Double] =
      if (bins.isEmpty)
        Seq.fill(rest.size + 1)(0.0)
      else if (bins.size == 1)
        (first :: rest.toList) map {case x =>
          if (x < bins.head._1)
            0.0
          else if (x == bins.head._1)
            bins.head._2.entries / 2.0
          else
            bins.head._2.entries
        }
      else {
        var binsArray = bins.toArray
        val in = (first :: rest.toList).zipWithIndex.toArray
        val out = Array.fill(in.size)(0.0)

        var i = 0
        var left = min
        var cumulative = 0.0
        while (i < binsArray.size) {
          val right =
            if (i < binsArray.size - 1)
              (binsArray(i)._1 + binsArray(i + 1)._1) / 2.0
            else
              max
          val entries = binsArray(i)._2.entries

          in foreach {case (x, j) =>
            if (left <= x  &&  x < right)
              out(j) = entries * (2.0 * x - left - right) / (right - left)
          }

          left = right
          cumulative += entries
          i += 1
        }

        in foreach {case (x, j) => if (x >= max) out(j) = cumulative}

        out.toSeq
      }
  }

  //////////////////////////////////////////////////////////////// 1D clustering algorithm (used by AdaptivelyBin, Median, Percentile)
  // Yael Ben-Haim and Elad Tom-Tov, "A streaming parallel decision tree algorithm",
  // J. Machine Learning Research 11 (2010)
  // http://www.jmlr.org/papers/volume11/ben-haim10a/ben-haim10a.pdf

  class Clustering1D[CONTAINER <: Container[CONTAINER]](val num: Int, value: => CONTAINER, centers: (Double, CONTAINER)*)
      extends mutable.MetricSortedMap[Double, CONTAINER](centers: _*)(new MetricOrdering[Double] {def difference(x: Double, y: Double) = x - y}) {

    var min = java.lang.Double.NaN
    var max = java.lang.Double.NaN

    centers foreach {case (x, _) =>
      if (min.isNaN  ||  x < min)
        min = x
      if (max.isNaN  ||  x > max)
        max = x
    }

    def cluster(n: Int) {
      while (size > n) {
        val bins = iterator.toSeq
        val neighbors = bins.init zip bins.tail
        val nearestNeighbors = neighbors minBy {case ((x1, v1), (x2, v2)) => x2 - x1}

        val ((x1, v1), (x2, v2)) = nearestNeighbors
        val replacement = ((x1 * v1.entries + x2 * v2.entries) / (v1.entries + v2.entries), v1 + v2)

        this -= x1
        this -= x2
        this += replacement
      }
    }

    // Ben-Haim and Tom-Tov's "Algorithm 1" with additional min/max tracking
    def update[DATUM](x: Double, datum: DATUM, weight: Double) {
      if (weight > 0.0) {
        if (min.isNaN  ||  x < min)
          min = x
        if (max.isNaN  ||  x > max)
          max = x

        // assumes that CONTAINER has Aggregation (can call fillWeighted)
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
    }

    // Ben-Haim and Tom-Tov's "Algorithm 2" with additional min/max tracking
    def merge(that: Clustering1D[CONTAINER]): Clustering1D[CONTAINER] = {
      val bins = scala.collection.mutable.Map[Double, CONTAINER]()

      this.iterator foreach {case (x, v) =>
        bins(x) = v.copy        // replace them; don't update them in-place
      }

      that.iterator foreach {case (x, v) =>
        if (bins contains x)
          bins(x) = bins(x) + v   // replace them; don't update them in-place
        else
          bins(x) = v.copy        // replace them; don't update them in-place
      }

      val out = new Clustering1D[CONTAINER](num, value, bins.toSeq: _*)
      out.cluster(num)

      out.min =
        if (this.min.isNaN  &&  that.min.isNaN)
          java.lang.Double.NaN
        else if (this.min.isNaN  ||  that.min < this.min)
          that.min
        else
          this.min

      out.max =
        if (this.max.isNaN  &&  that.max.isNaN)
          java.lang.Double.NaN
        else if (this.max.isNaN  ||  that.max > this.max)
          that.max
        else
          this.max

      out
    }
  }

}
