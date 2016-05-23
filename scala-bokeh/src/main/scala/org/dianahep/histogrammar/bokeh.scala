package org.dianahep.histogrammar

import scala.language.implicitConversions

import io.continuum.bokeh._

package object bokeh extends Tools {

  implicit def binnedToHistogramMethods(hist: Binned[Counted, Counted, Counted, Counted]): HistogramMethods =
    new HistogramMethods(new Selected(hist.entries, None, hist))

  implicit def binningToHistogramMethods[DATUM](hist: Binning[DATUM, Counting, Counting, Counting, Counting]): HistogramMethods =
    new HistogramMethods(new Selected(hist.entries, None, Factory.fromJson(hist.toJson).as[Binned[Counted, Counted, Counted, Counted]]))

  implicit def selectedBinnedToHistogramMethods(hist: Selected[Binned[Counted, Counted, Counted, Counted]]): HistogramMethods =
    new HistogramMethods(hist)

  implicit def selectingBinningToHistogramMethods[DATUM](hist: Selecting[DATUM, Binning[DATUM, Counting, Counting, Counting, Counting]]): HistogramMethods =
    new HistogramMethods(Factory.fromJson(hist.toJson).as[Selected[Binned[Counted, Counted, Counted, Counted]]])

  implicit def sparselyBinnedToHistogramMethods(hist: SparselyBinned[Counted, Counted]): HistogramMethods =
    if (hist.numFilled > 0)
      new HistogramMethods(
        new Selected(hist.entries, None, new Binned(hist.low.get, hist.high.get, hist.entries, hist.quantityName, hist.minBin.get to hist.maxBin.get map {i => new Counted(hist.at(i).flatMap(x => Some(x.entries)).getOrElse(0L))}, new Counted(0L), new Counted(0L), hist.nanflow))
      )
    else
      throw new RuntimeException("sparsely binned histogram has no entries")

  implicit def sparselyBinningToHistogramMethods[DATUM](hist: Selecting[DATUM, SparselyBinning[DATUM, Counting, Counting]]): HistogramMethods =
    sparselyBinnedToHistogramMethods(Factory.fromJson(hist.toJson).as[SparselyBinned[Counted, Counted]])

  implicit def selectedSparselyBinnedToHistogramMethods(hist: Selected[SparselyBinned[Counted, Counted]]): HistogramMethods =
    if (hist.value.numFilled > 0)
      new HistogramMethods(
        new Selected(hist.entries, hist.quantityName, new Binned(hist.value.low.get, hist.value.high.get, hist.value.entries, hist.value.quantityName, hist.value.minBin.get to hist.value.maxBin.get map {i => new Counted(hist.value.at(i).flatMap(x => Some(x.entries)).getOrElse(0L))}, new Counted(0L), new Counted(0L), hist.value.nanflow))
      )
    else
      throw new RuntimeException("sparsely binned histogram has no entries")

  implicit def selectedSparselyBinningToHistogramMethods[DATUM](hist: Selecting[DATUM, SparselyBinning[DATUM, Counting, Counting]]): HistogramMethods =
    selectedSparselyBinnedToHistogramMethods(Factory.fromJson(hist.toJson).as[Selected[SparselyBinned[Counted, Counted]]])
  


   def plot(xLabel:String, yLabel: String, plots: GlyphRenderer*) : Plot = {

      val xdr = new DataRange1d
      val ydr = new DataRange1d

      val plot = new Plot().x_range(xdr).y_range(ydr).tools(Pan|WheelZoom)

      val xaxis = new LinearAxis().plot(plot).location(Location.Below).axis_label(xLabel)
      val yaxis = new LinearAxis().plot(plot).location(Location.Left).axis_label(yLabel)
      plot.below <<= (xaxis :: _)
      plot.left <<= (yaxis :: _)

      val children = plots.toList
      plot.renderers := List(xaxis, yaxis):::children
      plot
    }

   //allow for default x and y labels 
   def plot(plots: GlyphRenderer*) : Plot = {

      val xdr = new DataRange1d
      val ydr = new DataRange1d

      val plot = new Plot().x_range(xdr).y_range(ydr).tools(Pan|WheelZoom)

      val xaxis = new LinearAxis().plot(plot).location(Location.Below).axis_label("x")
      val yaxis = new LinearAxis().plot(plot).location(Location.Left).axis_label("y")
      plot.below <<= (xaxis :: _)
      plot.left <<= (yaxis :: _)

      val children = plots.toList
      plot.renderers := List(xaxis, yaxis):::children
      plot
   }

   def save(plot: Plot, fname: String) {
       val document = new Document(plot)

       val html = document.save(fname)
       println(s"Wrote ${html.file}. Open ${html.url} in a web browser.")
       //html.view()
   }

   //Method that goes to bokeh-server and plots, so that we can do R-style, ROOT-style plotting
   def view(document: Document)  = ???

}

package bokeh {
  class HistogramMethods(hist: Selected[Binned[Counted, Counted, Counted, Counted]]) {

    def bokeh(markerType: String = "circle", markerSize: Int = 1, fillColor: Color = Color.Red, lineColor: Color = Color.Black) : GlyphRenderer = {

      //Prepare histogram contents for plotting
      val h = hist.value.high
      val l = hist.value.low
      val step = (h-l)/hist.value.values.length

      object source extends ColumnDataSource {
          val x = column(l to h by step)
          val y = column(hist.value.values.map(_.entries))
      }
      import source.{x,y}

      //Set marker color, fill color, line color
      val glyph = MarkerFactory(markerType).x(x).y(y).size(markerSize).fill_color(fillColor).line_color(lineColor)

      new GlyphRenderer().data_source(source).glyph(glyph)
    }
  }
}
