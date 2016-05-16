package org.dianahep.histogrammar

import scala.language.implicitConversions

import io.continuum.bokeh._

package object bokeh {
  implicit def binnedToHistogramMethods(hist: Cutted[Binned[Counted, Counted, Counted, Counted]]): HistogramMethods =
    new HistogramMethods(hist)

  implicit def binningToHistogramMethods[DATUM](hist: Cutting[DATUM, Binning[DATUM, Counting, Counting, Counting, Counting]]): HistogramMethods =
    new HistogramMethods(Factory.fromJson(hist.toJson).as[Cutted[Binned[Counted, Counted, Counted, Counted]]])

  implicit def sparselyBinnedToHistogramMethods(hist: Cutted[SparselyBinned[Counted, Counted]]): HistogramMethods =
    if (hist.value.numFilled > 0)
      new HistogramMethods(
        new Cutted(hist.entries, hist.selectionName, new Binned(hist.value.low.get, hist.value.high.get, 0.0, hist.value.quantityName, hist.value.minBin.get to hist.value.maxBin.get map {i => new Counted(hist.value.at(i).flatMap(x => Some(x.entries)).getOrElse(0L))}, new Counted(0L), new Counted(0L), hist.value.nanflow))
      )
    else
      throw new RuntimeException("sparsely binned histogram has no entries")

  implicit def sparselyBinningToHistogramMethods[DATUM](hist: Cutting[DATUM, SparselyBinning[DATUM, Counting, Counting]]): HistogramMethods =
    sparselyBinnedToHistogramMethods(Factory.fromJson(hist.toJson).as[Cutted[SparselyBinned[Counted, Counted]]])
}

package bokeh {
  class HistogramMethods(hist: Cutted[Binned[Counted, Counted, Counted, Counted]]) {
    def bokeh() = None  // FIXME

  }
}
