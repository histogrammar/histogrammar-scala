package org.dianahep.histogrammar.specialized

import scala.collection.mutable
import scala.language.implicitConversions

import org.dianahep.histogrammar._

package object histogram {
  type Histogrammed = Binned[Counted, Counted, Counted, Counted]
  type Histogramming[DATUM] = Binning[DATUM, Counting, Counting, Counting, Counting]
  def Histogram[DATUM]
    (num: Int,
      low: Double,
      high: Double,
      quantity: NumericalFcn[DATUM],
      selection: Selection[DATUM] = unweighted[DATUM]) =
    new Binning[DATUM, Counting, Counting, Counting, Counting](low, high, quantity, selection, 0.0, Array.fill(num)(Count()).toSeq, Count(), Count(), Count())

  type SparselyHistogrammed = SparselyBinned[Counted, Counted]
  type SparselyHistogramming[DATUM] = SparselyBinning[DATUM, Counting, Counting]
  def SparselyHistogram[DATUM]
    (binWidth: Double,
      quantity: NumericalFcn[DATUM],
      selection: Selection[DATUM] = unweighted[DATUM],
      origin: Double = 0.0) =
    new SparselyBinning[DATUM, Counting, Counting](binWidth, quantity, selection, Count(), mutable.HashMap[Long, Counting](), Count(), origin)

  implicit def binnedToHistogramMethods(hist: Binned[Counted, Counted, Counted, Counted]): HistogramMethods =
    new HistogramMethods(hist)

  implicit def binningToHistogramMethods(hist: Binning[_, Counting, Counting, Counting, Counting]): HistogramMethods =
    new HistogramMethods(Factory.fromJson(hist.toJson).as[Binned[Counted, Counted, Counted, Counted]])

  implicit def sparselyBinnedToHistogramMethods(hist: SparselyBinned[Counted, Counted]): HistogramMethods =
    new HistogramMethods(
      new Binned(hist.low, hist.high, 0.0, hist.minBin to hist.maxBin map {i => Count.fixed(hist.at(i).flatMap(x => Some(x.entries)).getOrElse(0L))}, Count.fixed(0L), Count.fixed(0L), hist.nanflow)
    )

  implicit def sparselyBinningToHistogramMethods(hist: SparselyBinning[_, _, _]): HistogramMethods =
    sparselyBinnedToHistogramMethods(Factory.fromJson(hist.toJson).as[SparselyBinned[Counted, Counted]])
}

package histogram {
  class HistogramMethods(hist: Binned[Counted, Counted, Counted, Counted]) {
    def numericalValues: Seq[Double] = hist.values.map(_.entries)
    def numericalOverflow: Double = hist.overflow.entries
    def numericalUnderflow: Double = hist.underflow.entries
    def numericalNanflow: Double = hist.nanflow.entries

    def ascii: String = ascii(80)
    def ascii(width: Int): String = {
      val minCount = Math.min(Math.min(Math.min(hist.values.map(_.entries).min, hist.overflow.entries), hist.underflow.entries), hist.nanflow.entries)
      val maxCount = Math.max(Math.max(Math.max(hist.values.map(_.entries).max, hist.overflow.entries), hist.underflow.entries), hist.nanflow.entries)
      val range = maxCount - minCount
      val minEdge = if (minCount < 0.0) minCount - 0.1*range else 0.0
      val maxEdge = maxCount + 0.1*range

      val binWidth = (hist.high - hist.low) / hist.values.size
      def sigfigs(x: Double, n: Int) = new java.math.BigDecimal(x).round(new java.math.MathContext(n)).toString

      val prefixValues = hist.values.zipWithIndex map {case (v, i) =>
        (i * binWidth + hist.low, (i + 1) * binWidth + hist.low, v.entries)
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

      val lines = hist.values zip prefixValues zip prefixValuesStr map {case ((v, (binlow, binhigh, value)), (binlowAbs, binhighAbs, valueAbs)) =>
        val binlowSign = if (binlow < 0) "-" else " "
        val binhighSign = if (binhigh < 0) "-" else " "
        val valueSign = if (value < 0) "-" else " "
        val peakIndex = Math.round(reducedWidth * (v.entries - minEdge) / (maxEdge - minEdge)).toInt
        if (peakIndex < zeroIndex)
          formatter.format(binlowSign, binlowAbs, binhighSign, binhighAbs, valueSign, valueAbs) + (if (zeroIndex > 0) "|" else "") + " " * peakIndex + "*" * (zeroIndex - peakIndex) + "|" + " " * (reducedWidth - zeroIndex) + "|"
        else
          formatter.format(binlowSign, binlowAbs, binhighSign, binhighAbs, valueSign, valueAbs) + (if (zeroIndex > 0) "|" else "") + " " * zeroIndex + "|" + "*" * (peakIndex - zeroIndex) + " " * (reducedWidth - peakIndex) + "|"
      }

      val underflowIndex = Math.round(reducedWidth * (hist.underflow.entries - minEdge) / (maxEdge - minEdge)).toInt
      val underflowFormatter = s"%-${widestBinlow + widestBinhigh + 5}s    %-${widestValue}s "
      val underflowLine =
        if (underflowIndex < zeroIndex)
          underflowFormatter.format("underflow", sigfigs(hist.underflow.entries, 4)) + (if (zeroIndex > 0) "|" else "") + " " * underflowIndex + "*" * (zeroIndex - underflowIndex) + "|" + " " * (reducedWidth - zeroIndex) + "|"
        else
          underflowFormatter.format("underflow", sigfigs(hist.underflow.entries, 4)) + (if (zeroIndex > 0) "|" else "") + " " * zeroIndex + "|" + "*" * (underflowIndex - zeroIndex) + " " * (reducedWidth - underflowIndex) + "|"

      val overflowIndex = Math.round(reducedWidth * (hist.overflow.entries - minEdge) / (maxEdge - minEdge)).toInt
      val overflowFormatter = s"%-${widestBinlow + widestBinhigh + 5}s    %-${widestValue}s "
      val overflowLine =
        if (overflowIndex < zeroIndex)
          overflowFormatter.format("overflow", sigfigs(hist.overflow.entries, 4)) + (if (zeroIndex > 0) "|" else "") + " " * overflowIndex + "*" * (zeroIndex - overflowIndex) + "|" + " " * (reducedWidth - zeroIndex) + "|"
        else
          overflowFormatter.format("overflow", sigfigs(hist.overflow.entries, 4)) + (if (zeroIndex > 0) "|" else "") + " " * zeroIndex + "|" + "*" * (overflowIndex - zeroIndex) + " " * (reducedWidth - overflowIndex) + "|"

      val nanflowIndex = Math.round(reducedWidth * (hist.nanflow.entries - minEdge) / (maxEdge - minEdge)).toInt
      val nanflowFormatter = s"%-${widestBinlow + widestBinhigh + 5}s    %-${widestValue}s "
      val nanflowLine =
        if (nanflowIndex < zeroIndex)
          nanflowFormatter.format("nanflow", sigfigs(hist.nanflow.entries, 4)) + (if (zeroIndex > 0) "|" else "") + " " * nanflowIndex + "*" * (zeroIndex - nanflowIndex) + "|" + " " * (reducedWidth - zeroIndex) + "|"
        else
          nanflowFormatter.format("nanflow", sigfigs(hist.nanflow.entries, 4)) + (if (zeroIndex > 0) "|" else "") + " " * zeroIndex + "|" + "*" * (nanflowIndex - zeroIndex) + " " * (reducedWidth - nanflowIndex) + "|"

      (List(zeroLine1, zeroLine2, underflowLine) ++ lines ++ List(overflowLine, nanflowLine, zeroLine2)).mkString("\n")      
    }
  }
}
