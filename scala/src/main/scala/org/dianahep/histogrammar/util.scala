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

package org.dianahep.histogrammar

import scala.collection.SortedMap
import scala.collection.SortedSet
import scala.util.Random

import org.dianahep.histogrammar._

/** Supporting functions, mostly called by those in [[org.dianahep.histogrammar]]. */
package object util {
  /** The natural [[org.dianahep.histogrammar.util.MetricOrdering]] for `Double` precision numbers. */
  implicit val doubleOrdering: MetricOrdering[Double] = new MetricOrdering[Double] {
    def difference(x: Double, y: Double) = x - y
  }
}

package util {
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

    implicit class KeySetFromSet(s: Set[String]) extends KeySet {
      def required = s
      def optional = Set[String]()
      def has(that: KeySet) = (that.required subsetOf s)  &&  (s subsetOf (that.required ++ that.optional))
    }
  }

  //////////////////////////////////////////////////////////////// random sampling

  package mutable {
    /** Utility for reservoir sampling on data of type RANGE.
      * 
      * Collects up to `limit` data points (based on count, not sum of weights), including their weights. Once the `limit` is reached, points are deweighted such that the final sample is a statistically unbiased representative of the full sample.
      * 
      * Merge step assumes that RANGE is immutable.
      */
    class Reservoir[RANGE](val limit: Int, initial: (RANGE, Double)*) {
      var numObserved = 0
      var sortedMap = SortedMap[Double, (RANGE, Double)]()
      initial foreach {case (y, weight) => update(y, weight)}

      /** Add a point to the reservoir.
        * 
        * @param y data point to insert or possibly insert in the sample
        * @param weight weight of the point; data in the final sample are represented in proportion to these weights (probability of a point with weight `w` to be in the final sample: `w/W` where `W` is the sum of all weights)
        */
      def update(y: RANGE, weight: Double = 1.0) {
        numObserved += 1

        val r = Math.pow(Random.nextDouble, 1.0/weight)

        // exception in updated WILL leave old sortedMap in previous state, since it's immutable (for rollback)
        if (numObserved <= limit)
          sortedMap = sortedMap.updated(r, (y, weight))
        else if (sortedMap.head._1 < r) {
          sortedMap = sortedMap.tail
          sortedMap = sortedMap.updated(r, (y, weight))
        }
      }

      /** Get a snapshot of the sample. */
      def values: Seq[(RANGE, Double)] = sortedMap.values.toVector

      /** Get some element of the sample. */
      def some: (RANGE, Double) = sortedMap.values.head

      /** Get the number of elements in the sample (saturates at `limit`). */
      def size = sortedMap.size

      /** Determine if the sample is empty. */
      def isEmpty = sortedMap.isEmpty
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

    //////////////////////////////////////////////////////////////// 1D clustering algorithm (used by AdaptivelyBin)

    /** Clusters data in one dimension for adaptive histogramming and approximating quantiles (such as the median) in one pass over the data.
      * 
      * Adapted from Yael Ben-Haim and Elad Tom-Tov, [[http://www.jmlr.org/papers/volume11/ben-haim10a/ben-haim10a.pdf "A streaming parallel decision tree algorithm"]] ''J. Machine Learning Research 11 (2010)''.
      * 
      * In the original paper, when the cluster-set needs to merge clusters (bins), it does so in increasing distance between neighboring bins. This algorithm also considers the content of the bins: the least-filled bins are merged first.
      * 
      * The `tailDetail` parameter scales between extremes: `tailDetail = 0` ''only'' considers the content of the bins and `tailDetail = 1` ''only'' considers the distance between bins (pure Ben-Haim/Tom-Tov). Specifically, the first bins to be merged are the ones that minimize
      * 
      * {{{tailDetail*(x2 - x1)/(max - min) + (1.0 - tailDetail)*(v1 + v2)/entries}}}
      * 
      * where `x1` and `x2` are the positions of the neighboring bins, `min` and `max` are the most extreme data positions observed so far, `v1` and `v2` are the (weighted) number of entries in the neighboring bins, and `entries` is the total (weighted) number of entries. The corresponding objective function for pure Ben-Haim/Tom-Tov is just `x2 - x1`.
      * 
      * @param num Maximum number of bins (used as a constraint when merging).
      * @param tailDetail Between 0 and 1 inclusive: use 0 to focus on the bulk of the distribution and 1 to focus on the tails; see above for details.
      * @param value New value (note the `=>`: expression is reevaluated every time a new value is needed).
      * @param values Containers for the surviving bins.
      * @param min Lowest observed value; used to interpret the first bin as a finite PDF (since the first bin technically extends to minus infinity).
      * @param max Highest observed value; used to interpret the last bin as a finite PDF (since the last bin technically extends to plus infinity).
      * @param entries Weighted number of entries (sum of all observed weights).
      */
    class Clustering1D[CONTAINER <: Container[CONTAINER]](val num: Int, val tailDetail: Double, value: => CONTAINER, val values: MetricSortedMap[Double, CONTAINER], var min: Double, var max: Double, var entries: Double) {
      private def mergeClusters() {
        // assuming this function cannot throw an exception (for rollback)
        while (values.size > num) {
          val bins = values.iterator.toSeq
          val neighbors = bins.init zip bins.tail

          val nearestNeighbors = neighbors.minBy({case ((x1, v1), (x2, v2)) => tailDetail*(x2 - x1)/(max - min) + (1.0 - tailDetail)*(v1.entries + v2.entries)/entries})(doubleOrdering)

          val ((x1, v1), (x2, v2)) = nearestNeighbors
          val replacement = ((x1 * v1.entries + x2 * v2.entries) / (v1.entries + v2.entries), v1 + v2)

          values -= x1
          values -= x2
          values += replacement
        }
      }

      mergeClusters()

      /** Ben-Haim and Tom-Tov's "Algorithm 1" with min/max/entries tracking.
        * 
        * This method assumes that `CONTAINER` (mutable or immutable) is actually a `CONTAINER with Aggregation{type Datum >: DATUM}` (mutable only). If it is used on an immutable container, it will raise a runtime exception.
        */
      def update[DATUM](x: Double, datum: DATUM, weight: Double) {
        if (weight > 0.0) {
          // assumes that CONTAINER has Aggregation (can call fillWeighted)
          values.get(x) match {
            case Some(v) =>
              v.asInstanceOf[CONTAINER with Aggregation{type Datum >: DATUM}].fill(datum, weight)
            case None =>
              val v = value.zero
              v.asInstanceOf[CONTAINER with Aggregation{type Datum >: DATUM}].fill(datum, weight)

              // assuming that += cannot throw an exception (for rollback)
              values += (x, v)
              mergeClusters()
          }

          entries += weight

          if (min.isNaN  ||  x < min)
            min = x

          if (max.isNaN  ||  x > max)
            max = x
        }
      }

      /** Ben-Haim and Tom-Tov's "Algorithm 2" with min/max/entries tracking. */
      def merge(that: Clustering1D[CONTAINER]): Clustering1D[CONTAINER] = {
        val bins = scala.collection.mutable.Map[Double, CONTAINER]()

        this.values.iterator foreach {case (x, v) =>
          bins(x) = v.copy        // replace them; don't update them in-place
        }

        that.values.iterator foreach {case (x, v) =>
          if (bins contains x)
            bins(x) = bins(x) + v   // replace them; don't update them in-place
          else
            bins(x) = v.copy        // replace them; don't update them in-place
        }

        Clustering1D[CONTAINER](num, tailDetail, value, Clustering1D.values(bins.toSeq: _*), Minimize.plus(this.min, that.min), Maximize.plus(this.max, that.max), this.entries + that.entries)
      }

      override def equals(that: Any) = that match {
        case that: Clustering1D[CONTAINER] => this.num == that.num  &&  this.tailDetail === that.tailDetail  &&  this.values == that.values  &&  this.min === that.min  &&  this.max === that.max  &&  this.entries === that.entries
        case _ => false
      }
      override def hashCode() = (num, tailDetail, values, min, max, entries).hashCode()
    }
    object Clustering1D {
      def values[CONTAINER <: Container[CONTAINER]](elems: (Double, CONTAINER)*) = MetricSortedMap[Double, CONTAINER](elems: _*)(new MetricOrdering[Double] {def difference(x: Double, y: Double) = x - y})

      def apply[CONTAINER <: Container[CONTAINER]](num: Int, tailDetail: Double, value: => CONTAINER, values: MetricSortedMap[Double, CONTAINER], min: Double, max: Double, entries: Double) =
        new Clustering1D[CONTAINER](num, tailDetail, value, values, min, max, entries)
    }
  }

  //////////////////////////////////////////////////////////////// interpretation of central bins as a distribution

  /** Mix-in for containers with non-uniform bins defined by centers (such as [[org.dianahep.histogrammar.CentrallyBinned]]/[[org.dianahep.histogrammar.CentrallyBinning]] and [[org.dianahep.histogrammar.AdaptivelyBinned]]/[[org.dianahep.histogrammar.AdaptivelyBinning]]). */
  trait CentralBinsDistribution[CONTAINER <: Container[CONTAINER]] {
    /**Weighted number of entries (sum of all observed weights). */
    def entries: Double
    /** Bin centers and their contents. */
    def bins: MetricSortedMap[Double, CONTAINER]
    /** Minimium data value observed so far or `NaN` if no data have been observed so far. */
    def min: Double
    /** Maximum data value observed so far or `NaN` if no data have been observed so far. */
    def max: Double

    /** Probability distribution function (PDF) of one sample point.
      * 
      * Computed as the `entries` of the corresponding bin divided by total number of entries divided by bin width.
      */
    def pdf(x: Double): Double = pdf(List(x): _*).head
    /** Cumulative distribution function (CDF, or "accumulation function") of one sample point.
      * 
      * Computed by adding bin contents from minus infinity to the point in question. This is a continuous, piecewise linear function.
      */
    def cdf(x: Double): Double = cdf(List(x): _*).head
    /** Quantile function (QF, or "inverse of the accumulation function") of one sample point.
      * 
      * Computed like the CDF, but solving for the point in question, rather than integrating up to it. This is a continuous, piecewise linear function.
      */
    def qf(x: Double): Double = qf(List(x): _*).head

    /** Probability distribution function (PDF) of many sample points.
      * 
      * Computed as the `entries` of the corresponding bins divided by total number of entries divided by bin width.
      */
    def pdf(xs: Double*): Seq[Double] = pdfTimesEntries(xs: _*).map(_ / entries)
    /** Cumulative distribution function (CDF, or "accumulation function") of many sample points.
      * 
      * Computed by adding bin contents from minus infinity to the points in question. This is a continuous, piecewise linear function.
      */
    def cdf(xs: Double*): Seq[Double] = cdfTimesEntries(xs: _*).map(_ / entries)
    /** Quantile function (QF, or "inverse of the accumulation function") of many sample points.
      * 
      * Computed like the CDF, but solving for the points in question, rather than integrating up to them. This is a continuous, piecewise linear function.
      */
    def qf(xs: Double*): Seq[Double] = qfTimesEntries(xs.map(_ * entries): _*)

    /** PDF without the non-unity number of entries removed (no division by zero when `entries` is zero). */
    def pdfTimesEntries(x: Double): Double = pdfTimesEntries(List(x): _*).head
    /** CDF without the non-unity number of entries removed (no division by zero when `entries` is zero). */
    def cdfTimesEntries(x: Double): Double = cdfTimesEntries(List(x): _*).head
    /** QF without the non-unity number of entries removed (no division by zero when `entries` is zero). */
    def qfTimesEntries(x: Double): Double = qfTimesEntries(List(x): _*).head

    /** PDF without the non-unity number of entries removed (no division by zero when `entries` is zero). */
    def pdfTimesEntries(xs: Double*): Seq[Double] =
      if (bins.isEmpty  ||  min.isNaN  ||  max.isNaN)
        Seq.fill(xs.size)(0.0)
      else if (bins.size == 1)
        xs map {case x =>
          if (x == bins.head._1)
            java.lang.Double.POSITIVE_INFINITY
          else
            0.0
        }
      else {
        val binsArray = bins.toArray
        val in = xs.zipWithIndex.toArray
        val out = Array.fill(in.size)(0.0)

        var i = 0
        var left = min
        while (i < binsArray.size) {
          val right =
            if (i < binsArray.size - 1)
              (binsArray(i)._1 + binsArray(i + 1)._1) / 2.0
            else
              max
          val entries = binsArray(i)._2.entries

          in foreach {case (x, j) =>
            if (left <= x  &&  x < right)
              out(j) = entries / (right - left)
          }

          left = right
          i += 1
        }
        out.toSeq
      }

    /** CDF without the non-unity number of entries removed (no division by zero when `entries` is zero). */
    def cdfTimesEntries(xs: Double*): Seq[Double] =
      if (bins.isEmpty  ||  min.isNaN  ||  max.isNaN)
        Seq.fill(xs.size)(0.0)
      else if (bins.size == 1)
        xs map {case x =>
          if (x < bins.head._1)
            0.0
          else if (x == bins.head._1)
            bins.head._2.entries / 2.0
          else
            bins.head._2.entries
        }
      else {
        val binsArray = bins.toArray
        val in = xs.zipWithIndex.toArray
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
              out(j) = cumulative + entries * (x - left)/(right - left)
          }

          left = right
          cumulative += entries
          i += 1
        }

        in foreach {case (x, j) => if (x >= max) out(j) = cumulative}

        out.toSeq
      }

    /** QF without the non-unity number of entries removed (no division by zero when `entries` is zero). */
    def qfTimesEntries(ys: Double*): Seq[Double] =
      if (bins.isEmpty  ||  min.isNaN  ||  max.isNaN)
        Seq.fill(ys.size)(java.lang.Double.NaN)
      else if (bins.size == 1)
        Seq.fill(ys.size)(bins.head._1)
      else {
        val binsArray = bins.toArray
        val in = ys.zipWithIndex.toArray
        val out = Array.fill(in.size)(min)

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

          val low = cumulative
          val high = cumulative + entries

          in foreach {case (y, j) =>
            if (low <= y  &&  y < high)
              out(j) = left + (right - left)*(y - low)/(high - low)
          }

          left = right
          cumulative += entries
          i += 1
        }

        in foreach {case (y, j) => if (y >= cumulative) out(j) = max}

        out.toSeq
      }
  }
}
