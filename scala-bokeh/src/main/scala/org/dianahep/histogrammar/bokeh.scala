package org.dianahep.histogrammar

import scala.language.implicitConversions

import org.dianahep.histogrammar.ascii._

import io.continuum.bokeh._

package object bokeh extends Tools {

   class BokehImplicits(plots: GlyphRenderer) {

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
   }
   implicit def implicitplot(plots: GlyphRenderer) = new BokehImplicits(plots)


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

  implicit def binnedToHistogramMethodsBokeh(hist: Binned[Counted, Counted, Counted, Counted]): HistogramMethodsBokeh =
    new HistogramMethodsBokeh(binnedToHistogramMethods(hist).selected)
  implicit def binningToHistogramMethodsBokeh[DATUM](hist: Binning[DATUM, Counting, Counting, Counting, Counting]): HistogramMethodsBokeh =
    new HistogramMethodsBokeh(binningToHistogramMethods(hist).selected)
  implicit def selectedBinnedToHistogramMethodsBokeh(hist: Selected[Binned[Counted, Counted, Counted, Counted]]): HistogramMethodsBokeh =
    new HistogramMethodsBokeh(selectedBinnedToHistogramMethods(hist).selected)
  implicit def selectingBinningToHistogramMethodsBokeh[DATUM](hist: Selecting[DATUM, Binning[DATUM, Counting, Counting, Counting, Counting]]): HistogramMethodsBokeh =
    new HistogramMethodsBokeh(selectingBinningToHistogramMethods(hist).selected)
  implicit def sparselyBinnedToHistogramMethodsBokeh(hist: SparselyBinned[Counted, Counted]): HistogramMethodsBokeh =
    new HistogramMethodsBokeh(sparselyBinnedToHistogramMethods(hist).selected)
  implicit def sparselyBinningToHistogramMethodsBokeh[DATUM](hist: Selecting[DATUM, SparselyBinning[DATUM, Counting, Counting]]): HistogramMethodsBokeh =
    new HistogramMethodsBokeh(sparselyBinningToHistogramMethods(hist).selected)
  implicit def selectedSparselyBinnedToHistogramMethodsBokeh(hist: Selected[SparselyBinned[Counted, Counted]]): HistogramMethodsBokeh =
    new HistogramMethodsBokeh(selectedSparselyBinnedToHistogramMethods(hist).selected)
  implicit def selectedSparselyBinningToHistogramMethodsBokeh[DATUM](hist: Selecting[DATUM, SparselyBinning[DATUM, Counting, Counting]]): HistogramMethodsBokeh =
    new HistogramMethodsBokeh(selectedSparselyBinningToHistogramMethods(hist).selected)

  class HistogramMethodsBokeh(hist: Selected[Binned[Counted, Counted, Counted, Counted]]) {

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
