package org.dianahep.histogrammar

import scala.language.implicitConversions

import io.continuum.bokeh._


package object bokeh extends App with Tools {
  implicit def binnedToHistogramMethods(hist: Selected[Binned[Counted, Counted, Counted, Counted]]): HistogramMethods =
    new HistogramMethods(hist)

  implicit def binningToHistogramMethods[DATUM](hist: Selecting[DATUM, Binning[DATUM, Counting, Counting, Counting, Counting]]): HistogramMethods =
    new HistogramMethods(Factory.fromJson(hist.toJson).as[Selected[Binned[Counted, Counted, Counted, Counted]]])

  implicit def sparselyBinnedToHistogramMethods(hist: Selected[SparselyBinned[Counted, Counted]]): HistogramMethods =
    if (hist.value.numFilled > 0)
      new HistogramMethods(
        new Selected(hist.entries, hist.quantityName, new Binned(hist.value.low.get, hist.value.high.get, 0.0, hist.value.quantityName, hist.value.minBin.get to hist.value.maxBin.get map {i => new Counted(hist.value.at(i).flatMap(x => Some(x.entries)).getOrElse(0L))}, new Counted(0L), new Counted(0L), hist.value.nanflow))
      )
    else
      throw new RuntimeException("sparsely binned histogram has no entries")

  implicit def sparselyBinningToHistogramMethods[DATUM](hist: Selecting[DATUM, SparselyBinning[DATUM, Counting, Counting]]): HistogramMethods =
    sparselyBinnedToHistogramMethods(Factory.fromJson(hist.toJson).as[Selected[SparselyBinned[Counted, Counted]]])
}

package bokeh {

  class HistogramMethods(hist: Selected[Binned[Counted, Counted, Counted, Counted]]) {
  
    private def colorSelector(c: String) = c match {
       case "white" => Color.White
       case "black" => Color.Black
       case "red"   => Color.Red
       case other   => throw new IllegalArgumentException(
         s"Only white, black, red colors are supported but got $other.")
    }

    def bokeh_book(markerType: String = "circle", markerSize: Int = 1, fillColor: String = "white", lineColor: String = "black") : GlyphRenderer = {

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
      val glyph = MarkerFactory(markerType).x(x).y(y).size(markerSize).fill_color(colorSelector(fillColor)).line_color(colorSelector(lineColor))

      new GlyphRenderer().data_source(source).glyph(glyph)
    }

    def bokeh_plot(xLabel:String, yLabel: String, plots: GlyphRenderer*) : Plot = {

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
    def bokeh_plot(plots: GlyphRenderer*) : Plot = {

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

    def bokeh_save(plot: Plot, fname: String) {
       val document = new Document(plot)

       val html = document.save(fname)
       println(s"Wrote ${html.file}. Open ${html.url} in a web browser.")
       //html.view()
    }

    //Method that goes to bokeh-server and plots, so that we can do R-style, ROOT-style plotting
    def bokeh_view(document: Document)  = ???

  }
}
