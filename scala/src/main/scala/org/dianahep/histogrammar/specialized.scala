package org.dianahep.histogrammar

import scala.language.implicitConversions

package object specialized {
  implicit def binnedToHistogram(hist: Binned[Counted, Counted, Counted, Counted]): HistogramMethods = new HistogramMethods(hist)
  implicit def binningToHistogram[DEBUG](hist: Binning[DEBUG, Counted, Counted, Counted, Counted]): HistogramMethods = new HistogramMethods(hist.fix)
}

package specialized {
  class HistogramMethods(hist: Binned[Counted, Counted, Counted, Counted]) {
    def show = {
      val minCount = hist.values.map(_.value).min
      val maxCount = hist.values.map(_.value).max
      val range = maxCount - minCount
      val minEdge = if (minCount < 0.0) minCount - 0.1*range else 0.0
      val maxEdge = maxCount + 0.1*range

      val binWidth = (hist.high - hist.low) / hist.values.size
      def sigfigs(x: Double, n: Int) = new java.math.BigDecimal(x).round(new java.math.MathContext(n)).toString

      val prefixValues = hist.values.zipWithIndex map {case (v, i) =>
        val binlow = sigfigs(i * binWidth + hist.low, 3)
        val binhigh = sigfigs((i + 1) * binWidth + hist.low, 3)
        val value = sigfigs(v.value, 4)
        (binlow, binhigh, value)
      }
      val widestBinlow = prefixValues.map(_._1.size).max
      val widestBinhigh = prefixValues.map(_._2.size).max
      val widestValue = prefixValues.map(_._3.size).max
      val formatter = s"[%-${widestBinlow}s, %-${widestBinhigh}s) %-${widestValue}s "
      val prefixWidth = widestBinlow + widestBinhigh + widestValue + 6

      val width = 80 - prefixWidth
      val zeroIndex = Math.round(width * (0.0 - minEdge) / (maxEdge - minEdge)).toInt
      val zeroLine1 = " " * prefixWidth + (if (zeroIndex > 0) " " else "") + " " * zeroIndex + "0" + " " * (width - zeroIndex - 10) + " " + f"$maxEdge%10g"
      val zeroLine2 = " " * prefixWidth + (if (zeroIndex > 0) "+" else "") + "-" * zeroIndex + "+" + "-" * (width - zeroIndex - 1) + "-" + "+"

      val underflowIndex = Math.round(width * (hist.underflow.value - minEdge) / (maxEdge - minEdge)).toInt
      val underflowFormatter = s"%-${widestBinlow + widestBinhigh + 4}s %-${widestValue}s "
      val underflowLine =
        if (underflowIndex < zeroIndex)
          underflowFormatter.format("underflow", sigfigs(hist.underflow.value, 4)) + (if (zeroIndex > 0) "|" else "") + " " * underflowIndex + "*" * (zeroIndex - underflowIndex) + "|" + " " * (width - zeroIndex) + "|"
        else
          underflowFormatter.format("underflow", sigfigs(hist.underflow.value, 4)) + (if (zeroIndex > 0) "|" else "") + " " * zeroIndex + "|" + "*" * (underflowIndex - zeroIndex) + " " * (width - underflowIndex) + "|"

      val overflowIndex = Math.round(width * (hist.overflow.value - minEdge) / (maxEdge - minEdge)).toInt
      val overflowFormatter = s"%-${widestBinlow + widestBinhigh + 4}s %-${widestValue}s "
      val overflowLine =
        if (overflowIndex < zeroIndex)
          overflowFormatter.format("overflow", sigfigs(hist.overflow.value, 4)) + (if (zeroIndex > 0) "|" else "") + " " * overflowIndex + "*" * (zeroIndex - overflowIndex) + "|" + " " * (width - zeroIndex) + "|"
        else
          overflowFormatter.format("overflow", sigfigs(hist.overflow.value, 4)) + (if (zeroIndex > 0) "|" else "") + " " * zeroIndex + "|" + "*" * (overflowIndex - zeroIndex) + " " * (width - overflowIndex) + "|"

      val nanflowIndex = Math.round(width * (hist.nanflow.value - minEdge) / (maxEdge - minEdge)).toInt
      val nanflowFormatter = s"%-${widestBinlow + widestBinhigh + 4}s %-${widestValue}s "
      val nanflowLine =
        if (nanflowIndex < zeroIndex)
          nanflowFormatter.format("nanflow", sigfigs(hist.nanflow.value, 4)) + (if (zeroIndex > 0) "|" else "") + " " * nanflowIndex + "*" * (zeroIndex - nanflowIndex) + "|" + " " * (width - zeroIndex) + "|"
        else
          nanflowFormatter.format("nanflow", sigfigs(hist.nanflow.value, 4)) + (if (zeroIndex > 0) "|" else "") + " " * zeroIndex + "|" + "*" * (nanflowIndex - zeroIndex) + " " * (width - nanflowIndex) + "|"

      val lines = hist.values zip prefixValues map {case (v, (binlow, binhigh, value)) =>
        val peakIndex = Math.round(width * (v.value - minEdge) / (maxEdge - minEdge)).toInt
        if (peakIndex < zeroIndex)
          formatter.format(binlow, binhigh, value) + (if (zeroIndex > 0) "|" else "") + " " * peakIndex + "*" * (zeroIndex - peakIndex) + "|" + " " * (width - zeroIndex) + "|"
        else
          formatter.format(binlow, binhigh, value) + (if (zeroIndex > 0) "|" else "") + " " * zeroIndex + "|" + "*" * (peakIndex - zeroIndex) + " " * (width - peakIndex) + "|"
      }
      (List(zeroLine1, zeroLine2, underflowLine) ++ lines ++ List(overflowLine, nanflowLine, zeroLine2)).mkString("\n")      
    }
  }
}
