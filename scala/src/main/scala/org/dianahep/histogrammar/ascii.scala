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

import scala.collection.mutable
import scala.language.implicitConversions

import org.dianahep.histogrammar._

/** Methods for drawing familiar combinations of containers, such as histograms, in ASCII art. */
package object ascii {
  //////////////////////////////////////////////////////////////// methods for Histogram and SparselyHistogram

  implicit def binnedToHistogramMethodsAscii(hist: Binned[Counted, Counted, Counted, Counted]): HistogramMethodsAscii =
    new HistogramMethodsAscii(binnedToHistogramMethods(hist).selected)
  implicit def binningToHistogramMethodsAscii[DATUM](hist: Binning[DATUM, Counting, Counting, Counting, Counting]): HistogramMethodsAscii =
    new HistogramMethodsAscii(binningToHistogramMethods(hist).selected)
  implicit def selectedBinnedToHistogramMethodsAscii(hist: Selected[Binned[Counted, Counted, Counted, Counted]]): HistogramMethodsAscii =
    new HistogramMethodsAscii(selectedBinnedToHistogramMethods(hist).selected)
  implicit def selectingBinningToHistogramMethodsAscii[DATUM](hist: Selecting[DATUM, Binning[DATUM, Counting, Counting, Counting, Counting]]): HistogramMethodsAscii =
    new HistogramMethodsAscii(selectingBinningToHistogramMethods(hist).selected)
  implicit def sparselyBinnedToHistogramMethodsAscii(hist: SparselyBinned[Counted, Counted]): HistogramMethodsAscii =
    new HistogramMethodsAscii(sparselyBinnedToHistogramMethods(hist).selected)
  implicit def sparselyBinningToHistogramMethodsAscii[DATUM](hist: SparselyBinning[DATUM, Counting, Counting]): HistogramMethodsAscii =
    new HistogramMethodsAscii(sparselyBinningToHistogramMethods(hist).selected)
  implicit def selectedSparselyBinnedToHistogramMethodsAscii(hist: Selected[SparselyBinned[Counted, Counted]]): HistogramMethodsAscii =
    new HistogramMethodsAscii(selectedSparselyBinnedToHistogramMethods(hist).selected)
  implicit def selectingSparselyBinningToHistogramMethodsAscii[DATUM](hist: Selecting[DATUM, SparselyBinning[DATUM, Counting, Counting]]): HistogramMethodsAscii =
    new HistogramMethodsAscii(selectingSparselyBinningToHistogramMethods(hist).selected)

  class HistogramMethodsAscii(val selected: Selected[Binned[Counted, Counted, Counted, Counted]]) {
    /** Print an ASCII representation of a histogram for debugging on headless systems. Limited to 80 columns. */
    def println {
      System.out.println(ascii(80))
    }
    /** Print an ASCII representation of a histogram for debugging on headless systems. Limited to `width` columns. */
    def println(width: Int = 80) {
      System.out.println(ascii(width))
    }

    /** ASCII representation of a histogram for debugging on headless systems. Limited to 80 columns. */
    def ascii: String = ascii(80)
    /** ASCII representation of a histogram for debugging on headless systems. Limited to `width` columns. */
    def ascii(width: Int): String = {
      val binned = selected.cut

      val minCount = Math.min(Math.min(Math.min(binned.values.map(_.entries).min, binned.overflow.entries), binned.underflow.entries), binned.nanflow.entries)
      val maxCount = Math.max(Math.max(Math.max(binned.values.map(_.entries).max, binned.overflow.entries), binned.underflow.entries), binned.nanflow.entries)
      val range = maxCount - minCount
      val minEdge = if (minCount < 0.0) minCount - 0.1*range else 0.0
      val maxEdge = maxCount + 0.1*range

      val binWidth = (binned.high - binned.low) / binned.values.size
      def sigfigs(x: Double, n: Int) = new java.math.BigDecimal(x).round(new java.math.MathContext(n)).toString

      val prefixValues = binned.values.zipWithIndex map {case (v, i) =>
        (i * binWidth + binned.low, (i + 1) * binWidth + binned.low, v.entries)
      }
      val prefixValuesStr = prefixValues map {case (binlow, binhigh, entries) => (sigfigs(Math.abs(binlow), 3), sigfigs(Math.abs(binhigh), 3), sigfigs(Math.abs(entries), 4))}

      val widestBinlow = Math.max(prefixValuesStr.map(_._1.size).max, 2)
      val widestBinhigh = Math.max(prefixValuesStr.map(_._2.size).max, 2)
      val widestValue = Math.max(prefixValuesStr.map(_._3.size).max, 2)
      val formatter = s"[ %s%-${widestBinlow}s, %s%-${widestBinhigh}s) %s%-${widestValue}s "
      val prefixWidth = widestBinlow + widestBinhigh + widestValue + 9

      val reducedWidth = width - prefixWidth
      val zeroIndex = Math.round(reducedWidth * (0.0 - minEdge) / (maxEdge - minEdge)).toInt
      val zeroLine1 = " " * prefixWidth + " " + (if (zeroIndex > 0) " " else "") + " " * zeroIndex + "0" + " " * (reducedWidth - zeroIndex - 10) + " " + f"$maxEdge%10g"
      val zeroLine2 = " " * prefixWidth + " " + (if (zeroIndex > 0) "+" else "") + "-" * zeroIndex + "+" + "-" * (reducedWidth - zeroIndex - 1) + "-" + "+"

      val lines = binned.values zip prefixValues zip prefixValuesStr map {case ((v, (binlow, binhigh, value)), (binlowAbs, binhighAbs, valueAbs)) =>
        val binlowSign = if (binlow < 0) "-" else " "
        val binhighSign = if (binhigh < 0) "-" else " "
        val valueSign = if (value < 0) "-" else " "
        val peakIndex = Math.round(reducedWidth * (v.entries - minEdge) / (maxEdge - minEdge)).toInt
        if (peakIndex < zeroIndex)
          formatter.format(binlowSign, binlowAbs, binhighSign, binhighAbs, valueSign, valueAbs) + (if (zeroIndex > 0) "|" else "") + " " * peakIndex + "*" * (zeroIndex - peakIndex) + "|" + " " * (reducedWidth - zeroIndex) + "|"
        else
          formatter.format(binlowSign, binlowAbs, binhighSign, binhighAbs, valueSign, valueAbs) + (if (zeroIndex > 0) "|" else "") + " " * zeroIndex + "|" + "*" * (peakIndex - zeroIndex) + " " * (reducedWidth - peakIndex) + "|"
      }

      val underflowIndex = Math.round(reducedWidth * (binned.underflow.entries - minEdge) / (maxEdge - minEdge)).toInt
      val underflowFormatter = s"%-${widestBinlow + widestBinhigh + 5}s    %-${widestValue}s "
      val underflowLine =
        if (underflowIndex < zeroIndex)
          underflowFormatter.format("underflow", sigfigs(binned.underflow.entries, 4)) + (if (zeroIndex > 0) "|" else "") + " " * underflowIndex + "*" * (zeroIndex - underflowIndex) + "|" + " " * (reducedWidth - zeroIndex) + "|"
        else
          underflowFormatter.format("underflow", sigfigs(binned.underflow.entries, 4)) + (if (zeroIndex > 0) "|" else "") + " " * zeroIndex + "|" + "*" * (underflowIndex - zeroIndex) + " " * (reducedWidth - underflowIndex) + "|"

      val overflowIndex = Math.round(reducedWidth * (binned.overflow.entries - minEdge) / (maxEdge - minEdge)).toInt
      val overflowFormatter = s"%-${widestBinlow + widestBinhigh + 5}s    %-${widestValue}s "
      val overflowLine =
        if (overflowIndex < zeroIndex)
          overflowFormatter.format("overflow", sigfigs(binned.overflow.entries, 4)) + (if (zeroIndex > 0) "|" else "") + " " * overflowIndex + "*" * (zeroIndex - overflowIndex) + "|" + " " * (reducedWidth - zeroIndex) + "|"
        else
          overflowFormatter.format("overflow", sigfigs(binned.overflow.entries, 4)) + (if (zeroIndex > 0) "|" else "") + " " * zeroIndex + "|" + "*" * (overflowIndex - zeroIndex) + " " * (reducedWidth - overflowIndex) + "|"

      val nanflowIndex = Math.round(reducedWidth * (binned.nanflow.entries - minEdge) / (maxEdge - minEdge)).toInt
      val nanflowFormatter = s"%-${widestBinlow + widestBinhigh + 5}s    %-${widestValue}s "
      val nanflowLine =
        if (nanflowIndex < zeroIndex)
          nanflowFormatter.format("nanflow", sigfigs(binned.nanflow.entries, 4)) + (if (zeroIndex > 0) "|" else "") + " " * nanflowIndex + "*" * (zeroIndex - nanflowIndex) + "|" + " " * (reducedWidth - zeroIndex) + "|"
        else
          nanflowFormatter.format("nanflow", sigfigs(binned.nanflow.entries, 4)) + (if (zeroIndex > 0) "|" else "") + " " * zeroIndex + "|" + "*" * (nanflowIndex - zeroIndex) + " " * (reducedWidth - nanflowIndex) + "|"

      (List(zeroLine1, zeroLine2, underflowLine) ++ lines ++ List(overflowLine, nanflowLine, zeroLine2)).mkString("\n")      
    }
  }

  //////////////////////////////////////////////////////////////// methods for HistogramAverage and SparselyHistogramAverage

  implicit def binnedToHistogramAverageMethodsAscii(hist: Binned[Averaged, Counted, Counted, Counted]): HistogramAverageMethodsAscii =
    new HistogramAverageMethodsAscii(binnedToHistogramAverageMethods(hist).selected)
  implicit def binningToHistogramAverageMethodsAscii[DATUM](hist: Binning[DATUM, Averaging[DATUM], Counting, Counting, Counting]): HistogramAverageMethodsAscii =
    new HistogramAverageMethodsAscii(binningToHistogramAverageMethods(hist).selected)
  implicit def selectedBinnedToHistogramAverageMethodsAscii(hist: Selected[Binned[Averaged, Counted, Counted, Counted]]): HistogramAverageMethodsAscii =
    new HistogramAverageMethodsAscii(selectedBinnedToHistogramAverageMethods(hist).selected)
  implicit def selectingBinningToHistogramAverageMethodsAscii[DATUM](hist: Selecting[DATUM, Binning[DATUM, Averaging[DATUM], Counting, Counting, Counting]]): HistogramAverageMethodsAscii =
    new HistogramAverageMethodsAscii(selectingBinningToHistogramAverageMethods(hist).selected)
  implicit def sparselyBinnedToHistogramAverageMethodsAscii(hist: SparselyBinned[Averaged, Counted]): HistogramAverageMethodsAscii =
    new HistogramAverageMethodsAscii(sparselyBinnedToHistogramAverageMethods(hist).selected)
  implicit def sparselyBinningToHistogramAverageMethodsAscii[DATUM](hist: SparselyBinning[DATUM, Averaging[DATUM], Counting]): HistogramAverageMethodsAscii =
    new HistogramAverageMethodsAscii(sparselyBinningToHistogramAverageMethods(hist).selected)
  implicit def selectedSparselyBinnedToHistogramAverageMethodsAscii(hist: Selected[SparselyBinned[Averaged, Counted]]): HistogramAverageMethodsAscii =
    new HistogramAverageMethodsAscii(selectedSparselyBinnedToHistogramAverageMethods(hist).selected)
  implicit def selectingSparselyBinningToHistogramAverageMethodsAscii[DATUM](hist: Selecting[DATUM, SparselyBinning[DATUM, Averaging[DATUM], Counting]]): HistogramAverageMethodsAscii =
    new HistogramAverageMethodsAscii(selectingSparselyBinningToHistogramAverageMethods(hist).selected)

  class HistogramAverageMethodsAscii(val selected: Selected[Binned[Averaged, Counted, Counted, Counted]]) {
    /** Print an ASCII representation of a histogram for debugging on headless systems. Limited to 80 columns. */
    def println {
      System.out.println(ascii(80))
    }
    /** Print an ASCII representation of a histogram for debugging on headless systems. Limited to `width` columns. */
    def println(width: Int = 80) {
      System.out.println(ascii(width))
    }

    /** ASCII representation of a histogram for debugging on headless systems. Limited to 80 columns. */
    def ascii: String = ascii(80)
    /** ASCII representation of a histogram for debugging on headless systems. Limited to `width` columns. */
    def ascii(width: Int): String = {
      val binned = selected.binned

      val minValue = binned.values.map(dev => dev.mean).min
      val maxValue = binned.values.map(dev => dev.mean).max
      val range = maxValue - minValue
      val minEdge = minValue - 0.1*range
      val maxEdge = maxValue + 0.1*range

      val binWidth = (binned.high - binned.low) / binned.values.size
      def sigfigs(x: Double, n: Int) = new java.math.BigDecimal(x).round(new java.math.MathContext(n)).toString

      val prefixValues = binned.values.zipWithIndex map {case (v, i) =>
        (i * binWidth + binned.low, (i + 1) * binWidth + binned.low, v.mean)
      }
      val prefixValuesStr = prefixValues map {case (binlow, binhigh, mean) => (sigfigs(Math.abs(binlow), 3), sigfigs(Math.abs(binhigh), 3), sigfigs(Math.abs(mean), 4))}

      val widestBinlow = Math.max(prefixValuesStr.map(_._1.size).max, 2)
      val widestBinhigh = Math.max(prefixValuesStr.map(_._2.size).max, 2)
      val widestMean = Math.max(prefixValuesStr.map(_._3.size).max, 2)
      val formatter = s"[ %s%-${widestBinlow}s, %s%-${widestBinhigh}s) %s%-${widestMean}s "
      val prefixWidth = widestBinlow + widestBinhigh + widestMean + 9

      val reducedWidth = width - prefixWidth
      val zeroIndex = Math.round(reducedWidth * (0.0 - minEdge) / (maxEdge - minEdge)).toInt
      val zeroLine1 = " " * prefixWidth + " " + f"$minEdge%-10g" + " " + (0 until (reducedWidth - 20)).map({i =>
        if (i + 10 == zeroIndex)
          "0"
        else
          " "
      }).mkString + " " + f"$maxEdge%10g"
      val zeroLine2 = " " * prefixWidth + " " + "+" + (0 until (reducedWidth - 1)).map({i =>
        if (i == zeroIndex)
          "+"
        else
          "-"
      }).mkString + "-" + "+"

      val lines = binned.values zip prefixValues zip prefixValuesStr map {case ((v, (binlow, binhigh, mean)), (binlowAbs, binhighAbs, meanAbs)) =>
        val binlowSign = if (binlow < 0) "-" else " "
        val binhighSign = if (binhigh < 0) "-" else " "
        val meanSign = if (mean < 0) "-" else " "

        val midIndex = Math.round(reducedWidth * (v.mean                         - minEdge) / (maxEdge - minEdge)).toInt

        formatter.format(binlowSign, binlowAbs, binhighSign, binhighAbs, meanSign, meanAbs) + "|" + (0 until reducedWidth).map({i =>
          if (i == zeroIndex)
            "|"
          else if (i == midIndex)
            "+"
          else
            " "
        }).mkString + "|"
      }

      (List(zeroLine1, zeroLine2) ++ lines ++ List(zeroLine2)).mkString("\n")      
    }
  }

  //////////////////////////////////////////////////////////////// methods for Profile and SparselyProfile

  implicit def binnedToProfileMethodsAscii(hist: Binned[Deviated, Counted, Counted, Counted]): ProfileMethodsAscii =
    new ProfileMethodsAscii(binnedToProfileMethods(hist).selected)
  implicit def binningToProfileMethodsAscii[DATUM](hist: Binning[DATUM, Deviating[DATUM], Counting, Counting, Counting]): ProfileMethodsAscii =
    new ProfileMethodsAscii(binningToProfileMethods(hist).selected)
  implicit def selectedBinnedToProfileMethodsAscii(hist: Selected[Binned[Deviated, Counted, Counted, Counted]]): ProfileMethodsAscii =
    new ProfileMethodsAscii(selectedBinnedToProfileMethods(hist).selected)
  implicit def selectingBinningToProfileMethodsAscii[DATUM](hist: Selecting[DATUM, Binning[DATUM, Deviating[DATUM], Counting, Counting, Counting]]): ProfileMethodsAscii =
    new ProfileMethodsAscii(selectingBinningToProfileMethods(hist).selected)
  implicit def sparselyBinnedToProfileMethodsAscii(hist: SparselyBinned[Deviated, Counted]): ProfileMethodsAscii =
    new ProfileMethodsAscii(sparselyBinnedToProfileMethods(hist).selected)
  implicit def sparselyBinningToProfileMethodsAscii[DATUM](hist: SparselyBinning[DATUM, Deviating[DATUM], Counting]): ProfileMethodsAscii =
    new ProfileMethodsAscii(sparselyBinningToProfileMethods(hist).selected)
  implicit def selectedSparselyBinnedToProfileMethodsAscii(hist: Selected[SparselyBinned[Deviated, Counted]]): ProfileMethodsAscii =
    new ProfileMethodsAscii(selectedSparselyBinnedToProfileMethods(hist).selected)
  implicit def selectingSparselyBinningToProfileMethodsAscii[DATUM](hist: Selecting[DATUM, SparselyBinning[DATUM, Deviating[DATUM], Counting]]): ProfileMethodsAscii =
    new ProfileMethodsAscii(selectingSparselyBinningToProfileMethods(hist).selected)

  class ProfileMethodsAscii(val selected: Selected[Binned[Deviated, Counted, Counted, Counted]]) {
    /** Print an ASCII representation of a histogram for debugging on headless systems. Limited to 80 columns. */
    def println {
      System.out.println(ascii(80))
    }
    /** Print an ASCII representation of a histogram for debugging on headless systems. Limited to `width` columns. */
    def println(width: Int = 80) {
      System.out.println(ascii(width))
    }

    /** ASCII representation of a histogram for debugging on headless systems. Limited to 80 columns. */
    def ascii: String = ascii(80)
    /** ASCII representation of a histogram for debugging on headless systems. Limited to `width` columns. */
    def ascii(width: Int): String = {
      val binned = selected.binned

      val binWidth = (binned.high - binned.low) / binned.values.size
      def sigfigs(x: Double, n: Int) = new java.math.BigDecimal(x).round(new java.math.MathContext(n)).toString
      val prefixValues = binned.values.zipWithIndex map {case (v, i) =>
        (i * binWidth + binned.low, (i + 1) * binWidth + binned.low, v.mean, if (v.entries > 0.0) Math.sqrt(v.variance / v.entries) else 0.0)
      }

      val minValue = prefixValues.map({case (binlow, binhigh, mean, stdev) => mean - 3.0*stdev}).min
      val maxValue = prefixValues.map({case (binlow, binhigh, mean, stdev) => mean + 3.0*stdev}).max
      val range = maxValue - minValue
      val minEdge = minValue - 0.1*range
      val maxEdge = maxValue + 0.1*range

      val prefixValuesStr = prefixValues map {case (binlow, binhigh, mean, stdev) => (sigfigs(Math.abs(binlow), 3), sigfigs(Math.abs(binhigh), 3), sigfigs(Math.abs(mean), 4), sigfigs(Math.abs(stdev), 4))}

      val widestBinlow = Math.max(prefixValuesStr.map(_._1.size).max, 2)
      val widestBinhigh = Math.max(prefixValuesStr.map(_._2.size).max, 2)
      val widestMean = Math.max(prefixValuesStr.map(_._3.size).max, 2)
      val widestStdev = Math.max(prefixValuesStr.map(_._4.size).max, 2)
      val formatter = s"[ %s%-${widestBinlow}s, %s%-${widestBinhigh}s) %s%-${widestMean}s +- %s%-${widestStdev}s "
      val prefixWidth = widestBinlow + widestBinhigh + widestMean + widestStdev + 14

      val reducedWidth = width - prefixWidth
      val zeroIndex = Math.round(reducedWidth * (0.0 - minEdge) / (maxEdge - minEdge)).toInt
      val zeroLine1 = " " * prefixWidth + " " + f"$minEdge%-10g" + " " + (0 until (reducedWidth - 20)).map({i =>
        if (i + 10 == zeroIndex)
          "0"
        else
          " "
      }).mkString + " " + f"$maxEdge%10g"
      val zeroLine2 = " " * prefixWidth + " " + "+" + (0 until (reducedWidth - 1)).map({i =>
        if (i == zeroIndex)
          "+"
        else
          "-"
      }).mkString + "-" + "+"

      val lines = binned.values zip prefixValues zip prefixValuesStr map {case ((v, (binlow, binhigh, mean, stdev)), (binlowAbs, binhighAbs, meanAbs, stdevAbs)) =>
        val binlowSign = if (binlow < 0) "-" else " "
        val binhighSign = if (binhigh < 0) "-" else " "
        val meanSign = if (mean < 0) "-" else " "
        val stdevSign = if (stdev < 0) "-" else " "

        val botIndex = Math.round(reducedWidth * (mean - stdev - minEdge) / (maxEdge - minEdge)).toInt
        val midIndex = Math.round(reducedWidth * (mean         - minEdge) / (maxEdge - minEdge)).toInt
        val topIndex = Math.round(reducedWidth * (mean + stdev - minEdge) / (maxEdge - minEdge)).toInt

        formatter.format(binlowSign, binlowAbs, binhighSign, binhighAbs, meanSign, meanAbs, stdevSign, stdevAbs) + "|" + (0 until reducedWidth).map({i =>
          if (i == zeroIndex)
            "|"
          else if (i < botIndex  ||  i > topIndex)
            " "
          else if (i == midIndex)
            "+"
          else if (i == botIndex  ||  i == topIndex)
            "|"
          else
            "-"
        }).mkString + "|"
      }

      (List(zeroLine1, zeroLine2) ++ lines ++ List(zeroLine2)).mkString("\n")      
    }
  }

  //////////////////////////////////////////////////////////////// methods for StackedHistogram, including cases for mixed tenses

  implicit def binnedToStackedHistogramMethodsAscii(hist: Stacked[Binned[Counted, Counted, Counted, Counted], Counted]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(binnedToStackedHistogramMethods(hist).stacked)
  implicit def binningToStackedHistogramMethodsAscii[DATUM](hist: Stacking[DATUM, Binning[DATUM, Counting, Counting, Counting, Counting], Counting]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(binningToStackedHistogramMethods(hist).stacked)
  implicit def selectedBinnedToStackedHistogramMethodsAscii(hist: Stacked[Selected[Binned[Counted, Counted, Counted, Counted]], Counted]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(selectedBinnedToStackedHistogramMethods(hist).stacked)
  implicit def selectingBinningToStackedHistogramMethodsAscii[DATUM](hist: Stacking[DATUM, Selecting[DATUM, Binning[DATUM, Counting, Counting, Counting, Counting]], Counting]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(selectingBinningToStackedHistogramMethods(hist).stacked)
  implicit def sparselyBinnedToStackedHistogramMethodsAscii(hist: Stacked[SparselyBinned[Counted, Counted], Counted]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(sparselyBinnedToStackedHistogramMethods(hist).stacked)
  implicit def sparselyBinningToStackingHistogramMethodsAscii[DATUM](hist: Stacking[DATUM, SparselyBinning[DATUM, Counting, Counting], Counting]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(sparselyBinningToStackingHistogramMethods(hist).stacked)
  implicit def selectedSparselyBinnedToStackedHistogramMethodsAscii(hist: Stacked[Selected[SparselyBinned[Counted, Counted]], Counted]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(selectedSparselyBinnedToStackedHistogramMethods(hist).stacked)
  implicit def selectingSparselyBinningToStackedHistogramMethodsAscii[DATUM](hist: Stacking[DATUM, Selecting[DATUM, SparselyBinning[DATUM, Counting, Counting]], Counting]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(selectingSparselyBinningToStackedHistogramMethods(hist).stacked)
  implicit def binnedMixedToStackedHistogramMethodsAscii[DATUM](hist: Stacked[Binning[DATUM, Counting, Counting, Counting, Counting], Counted]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(binnedMixedToStackedHistogramMethods(hist).stacked)
  implicit def selectedBinnedMixedToStackedHistogramMethodsAscii[DATUM](hist: Stacked[Selecting[DATUM, Binning[DATUM, Counting, Counting, Counting, Counting]], Counted]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(selectedBinnedMixedToStackedHistogramMethods(hist).stacked)
  implicit def sparselyBinnedMixedToStackedHistogramMethodsAscii[DATUM](hist: Stacked[SparselyBinning[DATUM, Counting, Counting], Counted]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(sparselyBinnedMixedToStackedHistogramMethods(hist).stacked)
  implicit def selectedSparselyBinnedMixedToStackedHistogramMethodsAscii[DATUM](hist: Stacked[Selecting[DATUM, SparselyBinning[DATUM, Counting, Counting]], Counted]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(selectedSparselyBinnedMixedToStackedHistogramMethods(hist).stacked)

  class StackedHistogramMethodsAscii(val stacked: Stacked[Selected[Binned[Counted, Counted, Counted, Counted]], Counted])

  //////////////////////////////////////////////////////////////// methods for PartitionedHistogram

  implicit def binnedToPartitionedHistogramMethodsAscii(hist: Partitioned[Binned[Counted, Counted, Counted, Counted], Counted]): PartitionedHistogramMethodsAscii =
    new PartitionedHistogramMethodsAscii(binnedToPartitionedHistogramMethods(hist).partitioned)
  implicit def binningToPartitionedHistogramMethodsAscii[DATUM](hist: Partitioning[DATUM, Binning[DATUM, Counting, Counting, Counting, Counting], Counting]): PartitionedHistogramMethodsAscii =
    new PartitionedHistogramMethodsAscii(binningToPartitionedHistogramMethods(hist).partitioned)
  implicit def selectedBinnedToPartitionedHistogramMethodsAscii(hist: Partitioned[Selected[Binned[Counted, Counted, Counted, Counted]], Counted]): PartitionedHistogramMethodsAscii =
    new PartitionedHistogramMethodsAscii(selectedBinnedToPartitionedHistogramMethods(hist).partitioned)
  implicit def selectingBinningToPartitionedHistogramMethodsAscii[DATUM](hist: Partitioning[DATUM, Selecting[DATUM, Binning[DATUM, Counting, Counting, Counting, Counting]], Counting]): PartitionedHistogramMethodsAscii =
    new PartitionedHistogramMethodsAscii(selectingBinningToPartitionedHistogramMethods(hist).partitioned)
  implicit def sparselyBinnedToPartitionedHistogramMethodsAscii(hist: Partitioned[SparselyBinned[Counted, Counted], Counted]): PartitionedHistogramMethodsAscii =
    new PartitionedHistogramMethodsAscii(sparselyBinnedToPartitionedHistogramMethods(hist).partitioned)
  implicit def sparselyBinningToPartitioningHistogramMethodsAscii[DATUM](hist: Partitioning[DATUM, SparselyBinning[DATUM, Counting, Counting], Counting]): PartitionedHistogramMethodsAscii =
    new PartitionedHistogramMethodsAscii(sparselyBinningToPartitioningHistogramMethods(hist).partitioned)
  implicit def selectedSparselyBinnedToPartitionedHistogramMethodsAscii(hist: Partitioned[Selected[SparselyBinned[Counted, Counted]], Counted]): PartitionedHistogramMethodsAscii =
    new PartitionedHistogramMethodsAscii(selectedSparselyBinnedToPartitionedHistogramMethods(hist).partitioned)
  implicit def selectingSparselyBinningToPartitionedHistogramMethodsAscii[DATUM](hist: Partitioning[DATUM, Selecting[DATUM, SparselyBinning[DATUM, Counting, Counting]], Counting]): PartitionedHistogramMethodsAscii =
    new PartitionedHistogramMethodsAscii(selectingSparselyBinningToPartitionedHistogramMethods(hist).partitioned)

  class PartitionedHistogramMethodsAscii(val partitioned: Partitioned[Selected[Binned[Counted, Counted, Counted, Counted]], Counted])

  //////////////////////////////////////////////////////////////// methods for FractionedHistogram

  implicit def binnedToFractionedHistogramMethodsAscii(hist: Fractioned[Binned[Counted, Counted, Counted, Counted]]): FractionedHistogramMethodsAscii =
    new FractionedHistogramMethodsAscii(binnedToFractionedHistogramMethods(hist).fractioned)
  implicit def binningToFractionedHistogramMethodsAscii[DATUM](hist: Fractioning[DATUM, Binning[DATUM, Counting, Counting, Counting, Counting]]): FractionedHistogramMethodsAscii =
    new FractionedHistogramMethodsAscii(binningToFractionedHistogramMethods(hist).fractioned)
  implicit def selectedBinnedToFractionedHistogramMethodsAscii(hist: Fractioned[Selected[Binned[Counted, Counted, Counted, Counted]]]): FractionedHistogramMethodsAscii =
    new FractionedHistogramMethodsAscii(selectedBinnedToFractionedHistogramMethods(hist).fractioned)
  implicit def selectingBinningToFractionedHistogramMethodsAscii[DATUM](hist: Fractioning[DATUM, Selecting[DATUM, Binning[DATUM, Counting, Counting, Counting, Counting]]]): FractionedHistogramMethodsAscii =
    new FractionedHistogramMethodsAscii(selectingBinningToFractionedHistogramMethods(hist).fractioned)
  implicit def sparselyBinnedToFractionedHistogramMethodsAscii(hist: Fractioned[SparselyBinned[Counted, Counted]]): FractionedHistogramMethodsAscii =
    new FractionedHistogramMethodsAscii(sparselyBinnedToFractionedHistogramMethods(hist).fractioned)
  implicit def sparselyBinningToFractioningHistogramMethodsAscii[DATUM](hist: Fractioning[DATUM, SparselyBinning[DATUM, Counting, Counting]]): FractionedHistogramMethodsAscii =
    new FractionedHistogramMethodsAscii(sparselyBinningToFractioningHistogramMethods(hist).fractioned)
  implicit def selectedSparselyBinnedToFractionedHistogramMethodsAscii(hist: Fractioned[Selected[SparselyBinned[Counted, Counted]]]): FractionedHistogramMethodsAscii =
    new FractionedHistogramMethodsAscii(selectedSparselyBinnedToFractionedHistogramMethods(hist).fractioned)
  implicit def selectingSparselyBinningToFractionedHistogramMethodsAscii[DATUM](hist: Fractioning[DATUM, Selecting[DATUM, SparselyBinning[DATUM, Counting, Counting]]]): FractionedHistogramMethodsAscii =
    new FractionedHistogramMethodsAscii(selectingSparselyBinningToFractionedHistogramMethods(hist).fractioned)

  class FractionedHistogramMethodsAscii(val fractioned: Fractioned[Selected[Binned[Counted, Counted, Counted, Counted]]]) {
    /** Print an ASCII representation of a histogram for debugging on headless systems. Limited to `width` columns.
      * 
      * @param confidenceInterval confidence interval function, which takes (numerator entries, denominator entries, `z`) as arguments, where `z` is the "number of sigmas:" `z = 0` is the central value, `z = -1` is the 68% confidence level below the central value, and `z = 1` is the 68% confidence level above the central value.
      */
    def println(confidenceInterval: (Double, Double, Double) => Double, width: Int = 80) {
      System.out.println(ascii(confidenceInterval, width))
    }

    /** ASCII representation of a histogram for debugging on headless systems. Limited to `width` columns.
      * 
      * @param confidenceInterval confidence interval function, which takes (numerator entries, denominator entries, `z`) as arguments, where `z` is the "number of sigmas:" `z = 0` is the central value, `z = -1` is the 68% confidence level below the central value, and `z = 1` is the 68% confidence level above the central value.
      */
    def ascii(confidenceInterval: (Double, Double, Double) => Double, width: Int = 80): String = {
      val methods = new FractionedHistogramMethods(fractioned)
      val numer = methods.numeratorBinned
      val denom = methods.denominatorBinned

      val ciValues = methods.confidenceIntervalValues(confidenceInterval, 1.0)
      val ciOverflow = methods.confidenceIntervalOverflow(confidenceInterval, 1.0)
      val ciUnderflow = methods.confidenceIntervalUnderflow(confidenceInterval, 1.0)
      val ciNanflow = methods.confidenceIntervalNanflow(confidenceInterval, 1.0)

      val minValue = (ciOverflow._1 :: ciUnderflow._1 :: ciNanflow._1 :: ciValues.toList.map(_._1)).filter(x => !x.isNaN  &&  !x.isInfinite).min
      val maxValue = (ciOverflow._3 :: ciUnderflow._3 :: ciNanflow._3 :: ciValues.toList.map(_._3)).filter(x => !x.isNaN  &&  !x.isInfinite).max
      val range = maxValue - minValue
      val minEdge = Math.max(0.0, minValue - 0.1*range)
      val maxEdge = Math.min(1.0, maxValue + 0.1*range)

      val binWidth = (numer.high - numer.low) / numer.values.size
      def sigfigs(x: Double, n: Int) =
        if (x.isNaN)
          "nan"
        else if (x.isInfinite)
          "inf"
        else
          new java.math.BigDecimal(x).round(new java.math.MathContext(n)).toString

      val prefixValues = 0 until ciValues.size map {i =>
        (i * binWidth + numer.low, (i + 1) * binWidth + numer.low, ciValues(i)._2)
      }
      val prefixValuesStr = prefixValues map {case (binlow, binhigh, frac) => (sigfigs(Math.abs(binlow), 3), sigfigs(Math.abs(binhigh), 3), sigfigs(Math.abs(frac), 4))}

      val widestBinlow = Math.max(prefixValuesStr.map(_._1.size).max, 2)
      val widestBinhigh = Math.max(prefixValuesStr.map(_._2.size).max, 2)
      val widestValue = Math.max(prefixValuesStr.map(_._3.size).max, 2)
      val formatter = s"[ %s%-${widestBinlow}s, %s%-${widestBinhigh}s) %s%-${widestValue}s "
      val prefixWidth = widestBinlow + widestBinhigh + widestValue + 9

      val reducedWidth = width - prefixWidth
      val zeroLine1 = " " * prefixWidth + " " + f"$minEdge%-10g" + " " + " " * (reducedWidth - 20) + " " + f"$maxEdge%10g"
      val zeroLine2 = " " * prefixWidth + " " + "+" + "-" * (reducedWidth - 1) + "-" + "+"

      val lines = prefixValues zip prefixValuesStr zip ciValues map {case (((binlow, binhigh, frac), (binlowAbs, binhighAbs, fracAbs)), (ciLow, ciMid, ciHigh)) =>
        val binlowSign = if (binlow < 0) "-" else " "
        val binhighSign = if (binhigh < 0) "-" else " "
        val fracSign = if (frac < 0) "-" else " "

        val (botIndex, midIndex, topIndex) =
          if (!ciLow.isNaN  &&  !ciLow.isInfinite  &&  !ciMid.isNaN  &&  !ciMid.isInfinite  &&  !ciHigh.isNaN  &&  !ciHigh.isInfinite)
            (Math.round(reducedWidth * (ciLow  - minEdge) / (maxEdge - minEdge)).toInt,
             Math.round(reducedWidth * (ciMid  - minEdge) / (maxEdge - minEdge)).toInt,
             Math.round(reducedWidth * (ciHigh - minEdge) / (maxEdge - minEdge)).toInt)
          else
            (-1, -1, -1)

        formatter.format(binlowSign, binlowAbs, binhighSign, binhighAbs, fracSign, fracAbs) + "|" + (0 until reducedWidth).map({i =>
          if (i < botIndex  ||  i > topIndex)
            " "
          else if (i == midIndex)
            "+"
          else if (i == botIndex  ||  i == topIndex)
            "|"
          else
            "-"
        }).mkString + "|"
      }

      val (underflowBotIndex, underflowMidIndex, underflowTopIndex) =
        if (!ciUnderflow._1.isNaN  &&  !ciUnderflow._1.isInfinite  &&  !ciUnderflow._2.isNaN  &&  !ciUnderflow._2.isInfinite  &&  !ciUnderflow._3.isNaN  &&  !ciUnderflow._3.isInfinite)
          (Math.round(reducedWidth * (ciUnderflow._1 - minEdge) / (maxEdge - minEdge)).toInt,
           Math.round(reducedWidth * (ciUnderflow._2 - minEdge) / (maxEdge - minEdge)).toInt,
           Math.round(reducedWidth * (ciUnderflow._3 - minEdge) / (maxEdge - minEdge)).toInt)
        else
          (-1, -1, -1)
      val underflowFormatter = s"%-${widestBinlow + widestBinhigh + 5}s    %-${widestValue}s "
      val underflowLine =
        underflowFormatter.format("underflow", sigfigs(ciUnderflow._2, 4)) + "|" + (0 until reducedWidth).map({i =>
          if (i < underflowBotIndex  ||  i > underflowTopIndex)
            " "
          else if (i == underflowMidIndex)
            "+"
          else if (i == underflowBotIndex  ||  i == underflowTopIndex)
            "|"
          else
            "-"
        }).mkString + "|"

      val (overflowBotIndex, overflowMidIndex, overflowTopIndex) =
        if (!ciOverflow._1.isNaN  &&  !ciOverflow._1.isInfinite  &&  !ciOverflow._2.isNaN  &&  !ciOverflow._2.isInfinite  &&  !ciOverflow._3.isNaN  &&  !ciOverflow._3.isInfinite)
          (Math.round(reducedWidth * (ciOverflow._1 - minEdge) / (maxEdge - minEdge)).toInt,
           Math.round(reducedWidth * (ciOverflow._2 - minEdge) / (maxEdge - minEdge)).toInt,
           Math.round(reducedWidth * (ciOverflow._3 - minEdge) / (maxEdge - minEdge)).toInt)
        else
          (-1, -1, -1)
      val overflowFormatter = s"%-${widestBinlow + widestBinhigh + 5}s    %-${widestValue}s "
      val overflowLine =
        overflowFormatter.format("overflow", sigfigs(ciOverflow._2, 4)) + "|" + (0 until reducedWidth).map({i =>
          if (i < overflowBotIndex  ||  i > overflowTopIndex)
            " "
          else if (i == overflowMidIndex)
            "+"
          else if (i == overflowBotIndex  ||  i == overflowTopIndex)
            "|"
          else
            "-"
        }).mkString + "|"

      val (nanflowBotIndex, nanflowMidIndex, nanflowTopIndex) =
        if (!ciNanflow._1.isNaN  &&  !ciNanflow._1.isInfinite  &&  !ciNanflow._2.isNaN  &&  !ciNanflow._2.isInfinite  &&  !ciNanflow._3.isNaN  &&  !ciNanflow._3.isInfinite)
          (Math.round(reducedWidth * (ciNanflow._1 - minEdge) / (maxEdge - minEdge)).toInt,
           Math.round(reducedWidth * (ciNanflow._2 - minEdge) / (maxEdge - minEdge)).toInt,
           Math.round(reducedWidth * (ciNanflow._3 - minEdge) / (maxEdge - minEdge)).toInt)
        else
          (-1, -1, -1)
      val nanflowFormatter = s"%-${widestBinlow + widestBinhigh + 5}s    %-${widestValue}s "
      val nanflowLine =
        nanflowFormatter.format("nanflow", sigfigs(ciNanflow._2, 4)) + "|" + (0 until reducedWidth).map({i =>
          if (i < nanflowBotIndex  ||  i > nanflowTopIndex)
            " "
          else if (i == nanflowMidIndex)
            "+"
          else if (i == nanflowBotIndex  ||  i == nanflowTopIndex)
            "|"
          else
            "-"
        }).mkString + "|"

      (List(zeroLine1, zeroLine2, underflowLine) ++ lines ++ List(overflowLine, nanflowLine, zeroLine2)).mkString("\n")      
    }
  }

  //////////////////////////////////////////////////////////////// methods for (nested) collections

  implicit def labeledToCollectionMethodsAscii(labeled: Labeled[_]) = new CollectionMethodsAscii(labeled)
  implicit def labelingToCollectionMethodsAscii(labeling: Labeling[_]) = new CollectionMethodsAscii(labeling)
  implicit def untypedLabeledToCollectionMethodsAscii(untypedLabeled: UntypedLabeled) = new CollectionMethodsAscii(untypedLabeled)
  implicit def untypedLabelingToCollectionMethodsAscii(untypedLabeling: UntypedLabeling[_]) = new CollectionMethodsAscii(untypedLabeling)
  implicit def indexedToCollectionMethodsAscii(indexed: Indexed[_]) = new CollectionMethodsAscii(indexed)
  implicit def indexingToCollectionMethodsAscii(indexing: Indexing[_]) = new CollectionMethodsAscii(indexing)
  implicit def branchedToCollectionMethodsAscii(branched: Branched[_, _]) = new CollectionMethodsAscii(branched)
  implicit def branchingToCollectionMethodsAscii(branching: Branching[_, _]) = new CollectionMethodsAscii(branching)

  class CollectionMethodsAscii(collection: Collection) {
    /** Print an ASCII representation of a histogram for debugging on headless systems. Limited to 80 columns with 40 reserved for indexes. */
    def println {
      System.out.println(ascii(40, 80))
    }
    /** Print an ASCII representation of a histogram for debugging on headless systems. Limited to `width` columns with `indexWidth` reserved for indexes. */
    def println(indexWidth: Int = 40, width: Int = 80) {
      System.out.println(ascii(indexWidth, width))
    }

    /** ASCII representation of a histogram for debugging on headless systems. Limited to 80 columns with 40 reserved for indexes. */
    def ascii: String = ascii(40, 80)

    /** ASCII representation of a histogram for debugging on headless systems. Limited to `width` columns with `indexWidth` reserved for indexes. */
    def ascii(indexWidth: Int = 40, width: Int = 80): String = collection.walk({index: Seq[CollectionIndex] =>
      val indexWords = index.map(_.toString)

      val indexLines = List.newBuilder[String]
      indexLines += "("
      var charCounter = 1
      indexWords.zipWithIndex foreach {case (word, i) =>
        if (charCounter + word.size + 1 > indexWidth) {
          indexLines += "\n    "
          charCounter = 4
        }
        indexLines += word
        charCounter += word.size
        if (i != indexWords.size - 1) {
          indexLines += ", "
          charCounter += 2
        }
      }
      indexLines += ")"
      val indexLinesResult = indexLines.result.mkString

      val reprWidth = width - indexLinesResult.size - 4
      val reprString = collection(index: _*).toString
      val reprStringTruncated = if (reprString.size < reprWidth) reprString else reprString.substring(0, reprWidth - 3) + "..."

      indexLinesResult + " -> " + reprStringTruncated
    }).mkString("\n")
  }

}
