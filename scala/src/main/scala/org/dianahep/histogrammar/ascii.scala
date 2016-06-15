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

  implicit def anyBinnedToHistogramMethodsAscii[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Binned[Counted, U, O, N]): HistogramMethodsAscii =
    new HistogramMethodsAscii(anyBinnedToHistogramMethods(hist).selected)
  implicit def anyBinningToHistogramMethodsAscii[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Binning[DATUM, Counting, U, O, N]): HistogramMethodsAscii =
    new HistogramMethodsAscii(anyBinningToHistogramMethods(hist).selected)
  implicit def anySelectedBinnedToHistogramMethodsAscii[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Selected[Binned[Counted, U, O, N]]): HistogramMethodsAscii =
    new HistogramMethodsAscii(anySelectedBinnedToHistogramMethods(hist).selected)
  implicit def anySelectingBinningToHistogramMethodsAscii[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, Binning[DATUM, Counting, U, O, N]]): HistogramMethodsAscii =
    new HistogramMethodsAscii(anySelectingBinningToHistogramMethods(hist).selected)
  implicit def anySparselyBinnedToHistogramMethodsAscii[N <: Container[N] with NoAggregation](hist: SparselyBinned[Counted, N]): HistogramMethodsAscii =
    new HistogramMethodsAscii(anySparselyBinnedToHistogramMethods(hist).selected)
  implicit def anySparselyBinningToHistogramMethodsAscii[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: SparselyBinning[DATUM, Counting, N]): HistogramMethodsAscii =
    new HistogramMethodsAscii(anySparselyBinningToHistogramMethods(hist).selected)
  implicit def anySelectedSparselyBinnedToHistogramMethodsAscii[N <: Container[N] with NoAggregation](hist: Selected[SparselyBinned[Counted, N]]): HistogramMethodsAscii =
    new HistogramMethodsAscii(anySelectedSparselyBinnedToHistogramMethods(hist).selected)
  implicit def anySelectingSparselyBinningToHistogramMethodsAscii[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, SparselyBinning[DATUM, Counting, N]]): HistogramMethodsAscii =
    new HistogramMethodsAscii(anySelectingSparselyBinningToHistogramMethods(hist).selected)

  class HistogramMethodsAscii(val selected: Selected[Binned[Counted, Counted, Counted, Counted]]) {
    /** Print an ASCII representation of a histogram for debugging on headless systems. Limited to 80 columns. */
    def println {
      System.out.println(ascii(80))
    }
    /** Print an ASCII representation of a histogram for debugging on headless systems. Limited to `width` columns. */
    def println(width: Int) {
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

  //////////////////////////////////////////////////////////////// methods for Profile and SparselyProfile

  implicit def anyBinnedToProfileMethodsAscii[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Binned[Averaged, U, O, N]): ProfileMethodsAscii =
    new ProfileMethodsAscii(anyBinnedToProfileMethods(hist).selected)
  implicit def anyBinningToProfileMethodsAscii[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Binning[DATUM, Averaging[DATUM], U, O, N]): ProfileMethodsAscii =
    new ProfileMethodsAscii(anyBinningToProfileMethods(hist).selected)
  implicit def anySelectedBinnedToProfileMethodsAscii[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Selected[Binned[Averaged, U, O, N]]): ProfileMethodsAscii =
    new ProfileMethodsAscii(anySelectedBinnedToProfileMethods(hist).selected)
  implicit def anySelectingBinningToProfileMethodsAscii[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, Binning[DATUM, Averaging[DATUM], U, O, N]]): ProfileMethodsAscii =
    new ProfileMethodsAscii(anySelectingBinningToProfileMethods(hist).selected)
  implicit def anySparselyBinnedToProfileMethodsAscii[N <: Container[N] with NoAggregation](hist: SparselyBinned[Averaged, N]): ProfileMethodsAscii =
    new ProfileMethodsAscii(anySparselyBinnedToProfileMethods(hist).selected)
  implicit def anySparselyBinningToProfileMethodsAscii[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: SparselyBinning[DATUM, Averaging[DATUM], N]): ProfileMethodsAscii =
    new ProfileMethodsAscii(anySparselyBinningToProfileMethods(hist).selected)
  implicit def anySelectedSparselyBinnedToProfileMethodsAscii[N <: Container[N] with NoAggregation](hist: Selected[SparselyBinned[Averaged, N]]): ProfileMethodsAscii =
    new ProfileMethodsAscii(anySelectedSparselyBinnedToProfileMethods(hist).selected)
  implicit def anySelectingSparselyBinningToProfileMethodsAscii[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, SparselyBinning[DATUM, Averaging[DATUM], N]]): ProfileMethodsAscii =
    new ProfileMethodsAscii(anySelectingSparselyBinningToProfileMethods(hist).selected)

  class ProfileMethodsAscii(val selected: Selected[Binned[Averaged, Counted, Counted, Counted]]) {
    /** Print an ASCII representation of a histogram for debugging on headless systems. Limited to 80 columns. */
    def println {
      System.out.println(ascii(80))
    }
    /** Print an ASCII representation of a histogram for debugging on headless systems. Limited to `width` columns. */
    def println(width: Int) {
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

  //////////////////////////////////////////////////////////////// methods for ProfileErr and SparselyProfileErr

  implicit def anyBinnedToProfileErrMethodsAscii[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Binned[Deviated, U, O, N]): ProfileErrMethodsAscii =
    new ProfileErrMethodsAscii(anyBinnedToProfileErrMethods(hist).selected)
  implicit def anyBinningToProfileErrMethodsAscii[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Binning[DATUM, Deviating[DATUM], U, O, N]): ProfileErrMethodsAscii =
    new ProfileErrMethodsAscii(anyBinningToProfileErrMethods(hist).selected)
  implicit def anySelectedBinnedToProfileErrMethodsAscii[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Selected[Binned[Deviated, U, O, N]]): ProfileErrMethodsAscii =
    new ProfileErrMethodsAscii(anySelectedBinnedToProfileErrMethods(hist).selected)
  implicit def anySelectingBinningToProfileErrMethodsAscii[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, Binning[DATUM, Deviating[DATUM], U, O, N]]): ProfileErrMethodsAscii =
    new ProfileErrMethodsAscii(anySelectingBinningToProfileErrMethods(hist).selected)
  implicit def anySparselyBinnedToProfileErrMethodsAscii[N <: Container[N] with NoAggregation](hist: SparselyBinned[Deviated, N]): ProfileErrMethodsAscii =
    new ProfileErrMethodsAscii(anySparselyBinnedToProfileErrMethods(hist).selected)
  implicit def anySparselyBinningToProfileErrMethodsAscii[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: SparselyBinning[DATUM, Deviating[DATUM], N]): ProfileErrMethodsAscii =
    new ProfileErrMethodsAscii(anySparselyBinningToProfileErrMethods(hist).selected)
  implicit def anySelectedSparselyBinnedToProfileErrMethodsAscii[N <: Container[N] with NoAggregation](hist: Selected[SparselyBinned[Deviated, N]]): ProfileErrMethodsAscii =
    new ProfileErrMethodsAscii(anySelectedSparselyBinnedToProfileErrMethods(hist).selected)
  implicit def anySelectingSparselyBinningToProfileErrMethodsAscii[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, SparselyBinning[DATUM, Deviating[DATUM], N]]): ProfileErrMethodsAscii =
    new ProfileErrMethodsAscii(anySelectingSparselyBinningToProfileErrMethods(hist).selected)

  class ProfileErrMethodsAscii(val selected: Selected[Binned[Deviated, Counted, Counted, Counted]]) {
    /** Print an ASCII representation of a histogram for debugging on headless systems. Limited to 80 columns. */
    def println {
      System.out.println(ascii(80))
    }
    /** Print an ASCII representation of a histogram for debugging on headless systems. Limited to `width` columns. */
    def println(width: Int) {
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

  implicit def anyBinnedToStackedHistogramMethodsAscii[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: Stacked[Binned[Counted, U, O, N], SN]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(anyBinnedToStackedHistogramMethods(hist).stacked)
  implicit def anyBinningToStackedHistogramMethodsAscii[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: Stacking[DATUM, Binning[DATUM, Counting, U, O, N], SN]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(anyBinningToStackedHistogramMethods(hist).stacked)
  implicit def anySelectedBinnedToStackedHistogramMethodsAscii[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: Stacked[Selected[Binned[Counted, U, O, N]], SN]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(anySelectedBinnedToStackedHistogramMethods(hist).stacked)
  implicit def anySelectingBinningToStackedHistogramMethodsAscii[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: Stacking[DATUM, Selecting[DATUM, Binning[DATUM, Counting, U, O, N]], SN]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(anySelectingBinningToStackedHistogramMethods(hist).stacked)
  implicit def anySparselyBinnedToStackedHistogramMethodsAscii[N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: Stacked[SparselyBinned[Counted, N], SN]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(anySparselyBinnedToStackedHistogramMethods(hist).stacked)
  implicit def anySparselyBinningToStackedHistogramMethodsAscii[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: Stacking[DATUM, SparselyBinning[DATUM, Counting, N], SN]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(anySparselyBinningToStackedHistogramMethods(hist).stacked)
  implicit def anySelectedSparselyBinnedToStackedHistogramMethodsAscii[N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: Stacked[Selected[SparselyBinned[Counted, N]], SN]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(anySelectedSparselyBinnedToStackedHistogramMethods(hist).stacked)
  implicit def anySelectingSparselyBinningToStackedHistogramMethodsAscii[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: Stacking[DATUM, Selecting[DATUM, SparselyBinning[DATUM, Counting, N]], SN]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(anySelectingSparselyBinningToStackedHistogramMethods(hist).stacked)
  implicit def anyBinnedMixedToStackedHistogramMethodsAscii[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with NoAggregation](hist: Stacked[Binning[DATUM, Counting, U, O, N], SN]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(anyBinnedMixedToStackedHistogramMethods(hist).stacked)
  implicit def anySelectedBinnedMixedToStackedHistogramMethodsAscii[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with NoAggregation](hist: Stacked[Selecting[DATUM, Binning[DATUM, Counting, U, O, N]], SN]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(anySelectedBinnedMixedToStackedHistogramMethods(hist).stacked)
  implicit def anySparselyBinnedMixedToStackedHistogramMethodsAscii[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with NoAggregation](hist: Stacked[SparselyBinning[DATUM, Counting, N], SN]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(anySparselyBinnedMixedToStackedHistogramMethods(hist).stacked)
  implicit def anySelectedSparselyBinnedMixedToStackedHistogramMethodsAscii[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with NoAggregation](hist: Stacked[Selecting[DATUM, SparselyBinning[DATUM, Counting, N]], SN]): StackedHistogramMethodsAscii =
    new StackedHistogramMethodsAscii(anySelectedSparselyBinnedMixedToStackedHistogramMethods(hist).stacked)

  class StackedHistogramMethodsAscii(val stacked: Stacked[Selected[Binned[Counted, Counted, Counted, Counted]], Counted])

  //////////////////////////////////////////////////////////////// methods for PartitionedHistogram

  implicit def anyBinnedToPartitionedHistogramMethodsAscii[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: Partitioned[Binned[Counted, U, O, N], SN]): PartitionedHistogramMethodsAscii =
    new PartitionedHistogramMethodsAscii(anyBinnedToPartitionedHistogramMethods(hist).partitioned)
  implicit def anyBinningToPartitionedHistogramMethodsAscii[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: Partitioning[DATUM, Binning[DATUM, Counting, U, O, N], SN]): PartitionedHistogramMethodsAscii =
    new PartitionedHistogramMethodsAscii(anyBinningToPartitionedHistogramMethods(hist).partitioned)
  implicit def anySelectedBinnedToPartitionedHistogramMethodsAscii[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: Partitioned[Selected[Binned[Counted, U, O, N]], SN]): PartitionedHistogramMethodsAscii =
    new PartitionedHistogramMethodsAscii(anySelectedBinnedToPartitionedHistogramMethods(hist).partitioned)
  implicit def anySelectingBinningToPartitionedHistogramMethodsAscii[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: Partitioning[DATUM, Selecting[DATUM, Binning[DATUM, Counting, U, O, N]], SN]): PartitionedHistogramMethodsAscii =
    new PartitionedHistogramMethodsAscii(anySelectingBinningToPartitionedHistogramMethods(hist).partitioned)
  implicit def anySparselyBinnedToPartitionedHistogramMethodsAscii[N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: Partitioned[SparselyBinned[Counted, N], SN]): PartitionedHistogramMethodsAscii =
    new PartitionedHistogramMethodsAscii(anySparselyBinnedToPartitionedHistogramMethods(hist).partitioned)
  implicit def anySparselyBinningToPartitionedHistogramMethodsAscii[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: Partitioning[DATUM, SparselyBinning[DATUM, Counting, N], SN]): PartitionedHistogramMethodsAscii =
    new PartitionedHistogramMethodsAscii(anySparselyBinningToPartitionedHistogramMethods(hist).partitioned)
  implicit def anySelectedSparselyBinnedToPartitionedHistogramMethodsAscii[N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: Partitioned[Selected[SparselyBinned[Counted, N]], SN]): PartitionedHistogramMethodsAscii =
    new PartitionedHistogramMethodsAscii(anySelectedSparselyBinnedToPartitionedHistogramMethods(hist).partitioned)
  implicit def anySelectingSparselyBinningToPartitionedHistogramMethodsAscii[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: Partitioning[DATUM, Selecting[DATUM, SparselyBinning[DATUM, Counting, N]], SN]): PartitionedHistogramMethodsAscii =
    new PartitionedHistogramMethodsAscii(anySelectingSparselyBinningToPartitionedHistogramMethods(hist).partitioned)

  class PartitionedHistogramMethodsAscii(val partitioned: Partitioned[Selected[Binned[Counted, Counted, Counted, Counted]], Counted])

  //////////////////////////////////////////////////////////////// methods for FractionedHistogram

  implicit def anyBinnedToFractionedHistogramMethodsAscii[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Fractioned[Binned[Counted, U, O, N]]): FractionedHistogramMethodsAscii =
    new FractionedHistogramMethodsAscii(anyBinnedToFractionedHistogramMethods(hist).fractioned)
  implicit def anyBinningToFractionedHistogramMethodsAscii[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Fractioning[DATUM, Binning[DATUM, Counting, U, O, N]]): FractionedHistogramMethodsAscii =
    new FractionedHistogramMethodsAscii(anyBinningToFractionedHistogramMethods(hist).fractioned)
  implicit def anySelectedBinnedToFractionedHistogramMethodsAscii[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Fractioned[Selected[Binned[Counted, U, O, N]]]): FractionedHistogramMethodsAscii =
    new FractionedHistogramMethodsAscii(anySelectedBinnedToFractionedHistogramMethods(hist).fractioned)
  implicit def anySelectingBinningToFractionedHistogramMethodsAscii[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Fractioning[DATUM, Selecting[DATUM, Binning[DATUM, Counting, U, O, N]]]): FractionedHistogramMethodsAscii =
    new FractionedHistogramMethodsAscii(anySelectingBinningToFractionedHistogramMethods(hist).fractioned)
  implicit def anySparselyBinnedToFractionedHistogramMethodsAscii[N <: Container[N] with NoAggregation](hist: Fractioned[SparselyBinned[Counted, N]]): FractionedHistogramMethodsAscii =
    new FractionedHistogramMethodsAscii(anySparselyBinnedToFractionedHistogramMethods(hist).fractioned)
  implicit def anySparselyBinningToFractionedHistogramMethodsAscii[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Fractioning[DATUM, SparselyBinning[DATUM, Counting, N]]): FractionedHistogramMethodsAscii =
    new FractionedHistogramMethodsAscii(anySparselyBinningToFractionedHistogramMethods(hist).fractioned)
  implicit def anySelectedSparselyBinnedToFractionedHistogramMethodsAscii[N <: Container[N] with NoAggregation](hist: Fractioned[Selected[SparselyBinned[Counted, N]]]): FractionedHistogramMethodsAscii =
    new FractionedHistogramMethodsAscii(anySelectedSparselyBinnedToFractionedHistogramMethods(hist).fractioned)
  implicit def anySelectingSparselyBinningToFractionedHistogramMethodsAscii[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Fractioning[DATUM, Selecting[DATUM, SparselyBinning[DATUM, Counting, N]]]): FractionedHistogramMethodsAscii =
    new FractionedHistogramMethodsAscii(anySelectingSparselyBinningToFractionedHistogramMethods(hist).fractioned)

  class FractionedHistogramMethodsAscii(val fractioned: Fractioned[Selected[Binned[Counted, Counted, Counted, Counted]]]) {
    /** Print an ASCII representation of a histogram for debugging on headless systems. Limited to 80 columns. */
    def println {
      println({(n: Double, d: Double, z: Double) => n/d}, 80)
    }

    /** Print an ASCII representation of a histogram for debugging on headless systems. Limited to `width` columns. */
    def println(width: Int) {
      println({(n: Double, d: Double, z: Double) => n/d}, width)
    }

    /** Print an ASCII representation of a histogram for debugging on headless systems. Limited to `width` columns.
      * 
      * @param confidenceInterval confidence interval function, which takes (numerator entries, denominator entries, `z`) as arguments, where `z` is the "number of sigmas:" `z = 0` is the central value, `z = -1` is the 68% confidence level below the central value, and `z = 1` is the 68% confidence level above the central value.
      */
    def println(confidenceInterval: (Double, Double, Double) => Double, width: Int = 80) {
      System.out.println(ascii(confidenceInterval, width))
    }

    /** ASCII representation of a histogram for debugging on headless systems. Limited to 80 columns. */
    def ascii: String = ascii({(n: Double, d: Double, z: Double) => n/d}, 80)

    /** ASCII representation of a histogram for debugging on headless systems. Limited to `width` columns. */
    def ascii(width: Int): String = ascii({(n: Double, d: Double, z: Double) => n/d}, width)

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

  //////////////////////////////////////////////////////////////// methods for TwoDimensionallyHistogram and TwoDimensionallySparselyHistogram

  implicit def binnedToTwoDimensionallyHistogramMethodsAscii[UX <: Container[UX] with NoAggregation, OX <: Container[OX] with NoAggregation, NX <: Container[NX] with NoAggregation, UY <: Container[UY] with NoAggregation, OY <: Container[OY] with NoAggregation, NY <: Container[NY] with NoAggregation](hist: Binned[Binned[Counted, UY, OY, NY], UX, OX, NX]): TwoDimensionallyHistogramMethodsAscii =
    new TwoDimensionallyHistogramMethodsAscii(binnedToTwoDimensionallyHistogramMethods(hist).selected)
  implicit def binningToTwoDimensionallyHistogramMethodsAscii[DATUM, UX <: Container[UX] with Aggregation{type Datum >: DATUM}, OX <: Container[OX] with Aggregation{type Datum >: DATUM}, NX <: Container[NX] with Aggregation{type Datum >: DATUM}, UY <: Container[UY] with Aggregation{type Datum >: DATUM}, OY <: Container[OY] with Aggregation{type Datum >: DATUM}, NY <: Container[NY] with Aggregation{type Datum >: DATUM}](hist: Binning[DATUM, Binning[DATUM, Counting, UY, OY, NY], UX, OX, NX]): TwoDimensionallyHistogramMethodsAscii =
    new TwoDimensionallyHistogramMethodsAscii(binningToTwoDimensionallyHistogramMethods(hist).selected)
  implicit def selectedBinnedToTwoDimensionallyHistogramMethodsAscii[UX <: Container[UX] with NoAggregation, OX <: Container[OX] with NoAggregation, NX <: Container[NX] with NoAggregation, UY <: Container[UY] with NoAggregation, OY <: Container[OY] with NoAggregation, NY <: Container[NY] with NoAggregation](hist: Selected[Binned[Binned[Counted, UY, OY, NY], UX, OX, NX]]): TwoDimensionallyHistogramMethodsAscii =
    new TwoDimensionallyHistogramMethodsAscii(selectedBinnedToTwoDimensionallyHistogramMethods(hist).selected)
  implicit def selectingBinningToTwoDimensionallyHistogramMethodsAscii[DATUM, UX <: Container[UX] with Aggregation{type Datum >: DATUM}, OX <: Container[OX] with Aggregation{type Datum >: DATUM}, NX <: Container[NX] with Aggregation{type Datum >: DATUM}, UY <: Container[UY] with Aggregation{type Datum >: DATUM}, OY <: Container[OY] with Aggregation{type Datum >: DATUM}, NY <: Container[NY] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, Binning[DATUM, Binning[DATUM, Counting, UY, OY, NY], UX, OX, NX]]): TwoDimensionallyHistogramMethodsAscii =
    new TwoDimensionallyHistogramMethodsAscii(selectingBinningToTwoDimensionallyHistogramMethods(hist).selected)
  implicit def sparselyBinnedToTwoDimensionallyHistogramMethodsAscii[NX <: Container[NX] with NoAggregation, NY <: Container[NY] with NoAggregation](hist: SparselyBinned[SparselyBinned[Counted, NY], NX]): TwoDimensionallyHistogramMethodsAscii =
    new TwoDimensionallyHistogramMethodsAscii(sparselyBinnedToTwoDimensionallyHistogramMethods(hist).selected)
  implicit def sparselyBinningToTwoDimensionallyHistogramMethodsAscii[DATUM, NX <: Container[NX] with Aggregation{type Datum >: DATUM}, NY <: Container[NY] with Aggregation{type Datum >: DATUM}](hist: SparselyBinning[DATUM, SparselyBinning[DATUM, Counting, NY], NX]): TwoDimensionallyHistogramMethodsAscii =
    new TwoDimensionallyHistogramMethodsAscii(sparselyBinningToTwoDimensionallyHistogramMethods(hist).selected)
  implicit def selectedSparselyBinnedToTwoDimensionallyHistogramMethodsAscii[NX <: Container[NX] with NoAggregation, NY <: Container[NY] with NoAggregation](hist: Selected[SparselyBinned[SparselyBinned[Counted, NY], NX]]): TwoDimensionallyHistogramMethodsAscii =
    new TwoDimensionallyHistogramMethodsAscii(selectedSparselyBinnedToTwoDimensionallyHistogramMethods(hist).selected)
  implicit def selectingSparselyBinningToTwoDimensionallyHistogramMethodsAscii[DATUM, NX <: Container[NX] with Aggregation{type Datum >: DATUM}, NY <: Container[NY] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, SparselyBinning[DATUM, SparselyBinning[DATUM, Counting, NY], NX]]): TwoDimensionallyHistogramMethodsAscii =
    new TwoDimensionallyHistogramMethodsAscii(selectingSparselyBinningToTwoDimensionallyHistogramMethods(hist).selected)

  class TwoDimensionallyHistogramMethodsAscii(val selected: Selected[Binned[Binned[Counted, Counted, Counted, Counted], Counted, Counted, Counted]])

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
