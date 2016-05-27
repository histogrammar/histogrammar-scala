package org.dianahep.histogrammar

import scala.language.implicitConversions

import io.continuum.bokeh.Color
import io.continuum.bokeh.ColumnDataSource
import io.continuum.bokeh.DataRange1d
import io.continuum.bokeh.Document
import io.continuum.bokeh.GlyphRenderer
import io.continuum.bokeh.LinearAxis
import io.continuum.bokeh.Location
import io.continuum.bokeh.Plot
import io.continuum.bokeh.Tools

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

  //////////////////////////////////////////////////////////////// methods for Histogram and SparselyHistogram

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
  implicit def sparselyBinningToHistogramMethodsBokeh[DATUM](hist: SparselyBinning[DATUM, Counting, Counting]): HistogramMethodsBokeh =
    new HistogramMethodsBokeh(sparselyBinningToHistogramMethods(hist).selected)
  implicit def selectedSparselyBinnedToHistogramMethodsBokeh(hist: Selected[SparselyBinned[Counted, Counted]]): HistogramMethodsBokeh =
    new HistogramMethodsBokeh(selectedSparselyBinnedToHistogramMethods(hist).selected)
  implicit def selectingSparselyBinningToHistogramMethodsBokeh[DATUM](hist: Selecting[DATUM, SparselyBinning[DATUM, Counting, Counting]]): HistogramMethodsBokeh =
    new HistogramMethodsBokeh(selectingSparselyBinningToHistogramMethods(hist).selected)

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

  //////////////////////////////////////////////////////////////// methods for Profile and SparselyProfile

  implicit def binnedToProfileMethodsBokeh(hist: Binned[Deviated, Counted, Counted, Counted]): ProfileMethodsBokeh =
    new ProfileMethodsBokeh(binnedToProfileMethods(hist).selected)
  implicit def binningToProfileMethodsBokeh[DATUM](hist: Binning[DATUM, Deviating[DATUM], Counting, Counting, Counting]): ProfileMethodsBokeh =
    new ProfileMethodsBokeh(binningToProfileMethods(hist).selected)
  implicit def selectedBinnedToProfileMethodsBokeh(hist: Selected[Binned[Deviated, Counted, Counted, Counted]]): ProfileMethodsBokeh =
    new ProfileMethodsBokeh(selectedBinnedToProfileMethods(hist).selected)
  implicit def selectingBinningToProfileMethodsBokeh[DATUM](hist: Selecting[DATUM, Binning[DATUM, Deviating[DATUM], Counting, Counting, Counting]]): ProfileMethodsBokeh =
    new ProfileMethodsBokeh(selectingBinningToProfileMethods(hist).selected)
  implicit def sparselyBinnedToProfileMethodsBokeh(hist: SparselyBinned[Deviated, Counted]): ProfileMethodsBokeh =
    new ProfileMethodsBokeh(sparselyBinnedToProfileMethods(hist).selected)
  implicit def sparselyBinningToProfileMethodsBokeh[DATUM](hist: SparselyBinning[DATUM, Deviating[DATUM], Counting]): ProfileMethodsBokeh =
    new ProfileMethodsBokeh(sparselyBinningToProfileMethods(hist).selected)
  implicit def selectedSparselyBinnedToProfileMethodsBokeh(hist: Selected[SparselyBinned[Deviated, Counted]]): ProfileMethodsBokeh =
    new ProfileMethodsBokeh(selectedSparselyBinnedToProfileMethods(hist).selected)
  implicit def selectingSparselyBinningToProfileMethodsBokeh[DATUM](hist: Selecting[DATUM, SparselyBinning[DATUM, Deviating[DATUM], Counting]]): ProfileMethodsBokeh =
    new ProfileMethodsBokeh(selectingSparselyBinningToProfileMethods(hist).selected)

  class ProfileMethodsBokeh(val selected: Selected[Binned[Deviated, Counted, Counted, Counted]])

  //////////////////////////////////////////////////////////////// methods for StackedHistogram, including cases for mixed tenses

  implicit def binnedToStackedHistogramMethodsBokeh(hist: Stacked[Binned[Counted, Counted, Counted, Counted]]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(binnedToStackedHistogramMethods(hist).stacked)
  implicit def binningToStackedHistogramMethodsBokeh[DATUM](hist: Stacking[DATUM, Binning[DATUM, Counting, Counting, Counting, Counting]]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(binningToStackedHistogramMethods(hist).stacked)
  implicit def selectedBinnedToStackedHistogramMethodsBokeh(hist: Stacked[Selected[Binned[Counted, Counted, Counted, Counted]]]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(selectedBinnedToStackedHistogramMethods(hist).stacked)
  implicit def selectingBinningToStackedHistogramMethodsBokeh[DATUM](hist: Stacking[DATUM, Selecting[DATUM, Binning[DATUM, Counting, Counting, Counting, Counting]]]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(selectingBinningToStackedHistogramMethods(hist).stacked)
  implicit def sparselyBinnedToStackedHistogramMethodsBokeh(hist: Stacked[SparselyBinned[Counted, Counted]]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(sparselyBinnedToStackedHistogramMethods(hist).stacked)
  implicit def sparselyBinningToStackingHistogramMethodsBokeh[DATUM](hist: Stacking[DATUM, SparselyBinning[DATUM, Counting, Counting]]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(sparselyBinningToStackingHistogramMethods(hist).stacked)
  implicit def selectedSparselyBinnedToStackedHistogramMethodsBokeh(hist: Stacked[Selected[SparselyBinned[Counted, Counted]]]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(selectedSparselyBinnedToStackedHistogramMethods(hist).stacked)
  implicit def selectingSparselyBinningToStackedHistogramMethodsBokeh[DATUM](hist: Stacking[DATUM, Selecting[DATUM, SparselyBinning[DATUM, Counting, Counting]]]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(selectingSparselyBinningToStackedHistogramMethods(hist).stacked)
  implicit def binnedMixedToStackedHistogramMethodsBokeh[DATUM](hist: Stacked[Binning[DATUM, Counting, Counting, Counting, Counting]]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(binnedMixedToStackedHistogramMethods(hist).stacked)
  implicit def selectedBinnedMixedToStackedHistogramMethodsBokeh[DATUM](hist: Stacked[Selecting[DATUM, Binning[DATUM, Counting, Counting, Counting, Counting]]]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(selectedBinnedMixedToStackedHistogramMethods(hist).stacked)
  implicit def sparselyBinnedMixedToStackedHistogramMethodsBokeh[DATUM](hist: Stacked[SparselyBinning[DATUM, Counting, Counting]]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(sparselyBinnedMixedToStackedHistogramMethods(hist).stacked)
  implicit def selectedSparselyBinnedMixedToStackedHistogramMethodsBokeh[DATUM](hist: Stacked[Selecting[DATUM, SparselyBinning[DATUM, Counting, Counting]]]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(selectedSparselyBinnedMixedToStackedHistogramMethods(hist).stacked)

  class StackedHistogramMethodsBokeh(val stacked: Stacked[Selected[Binned[Counted, Counted, Counted, Counted]]])

  //////////////////////////////////////////////////////////////// methods for PartitionedHistogram

  implicit def binnedToPartitionedHistogramMethodsBokeh(hist: Partitioned[Binned[Counted, Counted, Counted, Counted]]): PartitionedHistogramMethodsBokeh =
    new PartitionedHistogramMethodsBokeh(binnedToPartitionedHistogramMethods(hist).partitioned)
  implicit def binningToPartitionedHistogramMethodsBokeh[DATUM](hist: Partitioning[DATUM, Binning[DATUM, Counting, Counting, Counting, Counting]]): PartitionedHistogramMethodsBokeh =
    new PartitionedHistogramMethodsBokeh(binningToPartitionedHistogramMethods(hist).partitioned)
  implicit def selectedBinnedToPartitionedHistogramMethodsBokeh(hist: Partitioned[Selected[Binned[Counted, Counted, Counted, Counted]]]): PartitionedHistogramMethodsBokeh =
    new PartitionedHistogramMethodsBokeh(selectedBinnedToPartitionedHistogramMethods(hist).partitioned)
  implicit def selectingBinningToPartitionedHistogramMethodsBokeh[DATUM](hist: Partitioning[DATUM, Selecting[DATUM, Binning[DATUM, Counting, Counting, Counting, Counting]]]): PartitionedHistogramMethodsBokeh =
    new PartitionedHistogramMethodsBokeh(selectingBinningToPartitionedHistogramMethods(hist).partitioned)
  implicit def sparselyBinnedToPartitionedHistogramMethodsBokeh(hist: Partitioned[SparselyBinned[Counted, Counted]]): PartitionedHistogramMethodsBokeh =
    new PartitionedHistogramMethodsBokeh(sparselyBinnedToPartitionedHistogramMethods(hist).partitioned)
  implicit def sparselyBinningToPartitioningHistogramMethodsBokeh[DATUM](hist: Partitioning[DATUM, SparselyBinning[DATUM, Counting, Counting]]): PartitionedHistogramMethodsBokeh =
    new PartitionedHistogramMethodsBokeh(sparselyBinningToPartitioningHistogramMethods(hist).partitioned)
  implicit def selectedSparselyBinnedToPartitionedHistogramMethodsBokeh(hist: Partitioned[Selected[SparselyBinned[Counted, Counted]]]): PartitionedHistogramMethodsBokeh =
    new PartitionedHistogramMethodsBokeh(selectedSparselyBinnedToPartitionedHistogramMethods(hist).partitioned)
  implicit def selectingSparselyBinningToPartitionedHistogramMethodsBokeh[DATUM](hist: Partitioning[DATUM, Selecting[DATUM, SparselyBinning[DATUM, Counting, Counting]]]): PartitionedHistogramMethodsBokeh =
    new PartitionedHistogramMethodsBokeh(selectingSparselyBinningToPartitionedHistogramMethods(hist).partitioned)

  class PartitionedHistogramMethodsBokeh(val partitioned: Partitioned[Selected[Binned[Counted, Counted, Counted, Counted]]])

  //////////////////////////////////////////////////////////////// methods for FractionedHistogram

  implicit def binnedToFractionedHistogramMethodsBokeh(hist: Fractioned[Binned[Counted, Counted, Counted, Counted]]): FractionedHistogramMethodsBokeh =
    new FractionedHistogramMethodsBokeh(binnedToFractionedHistogramMethods(hist).fractioned)
  implicit def binningToFractionedHistogramMethodsBokeh[DATUM](hist: Fractioning[DATUM, Binning[DATUM, Counting, Counting, Counting, Counting]]): FractionedHistogramMethodsBokeh =
    new FractionedHistogramMethodsBokeh(binningToFractionedHistogramMethods(hist).fractioned)
  implicit def selectedBinnedToFractionedHistogramMethodsBokeh(hist: Fractioned[Selected[Binned[Counted, Counted, Counted, Counted]]]): FractionedHistogramMethodsBokeh =
    new FractionedHistogramMethodsBokeh(selectedBinnedToFractionedHistogramMethods(hist).fractioned)
  implicit def selectingBinningToFractionedHistogramMethodsBokeh[DATUM](hist: Fractioning[DATUM, Selecting[DATUM, Binning[DATUM, Counting, Counting, Counting, Counting]]]): FractionedHistogramMethodsBokeh =
    new FractionedHistogramMethodsBokeh(selectingBinningToFractionedHistogramMethods(hist).fractioned)
  implicit def sparselyBinnedToFractionedHistogramMethodsBokeh(hist: Fractioned[SparselyBinned[Counted, Counted]]): FractionedHistogramMethodsBokeh =
    new FractionedHistogramMethodsBokeh(sparselyBinnedToFractionedHistogramMethods(hist).fractioned)
  implicit def sparselyBinningToFractioningHistogramMethodsBokeh[DATUM](hist: Fractioning[DATUM, SparselyBinning[DATUM, Counting, Counting]]): FractionedHistogramMethodsBokeh =
    new FractionedHistogramMethodsBokeh(sparselyBinningToFractioningHistogramMethods(hist).fractioned)
  implicit def selectedSparselyBinnedToFractionedHistogramMethodsBokeh(hist: Fractioned[Selected[SparselyBinned[Counted, Counted]]]): FractionedHistogramMethodsBokeh =
    new FractionedHistogramMethodsBokeh(selectedSparselyBinnedToFractionedHistogramMethods(hist).fractioned)
  implicit def selectingSparselyBinningToFractionedHistogramMethodsBokeh[DATUM](hist: Fractioning[DATUM, Selecting[DATUM, SparselyBinning[DATUM, Counting, Counting]]]): FractionedHistogramMethodsBokeh =
    new FractionedHistogramMethodsBokeh(selectingSparselyBinningToFractionedHistogramMethods(hist).fractioned)

  class FractionedHistogramMethodsBokeh(val fractioned: Fractioned[Selected[Binned[Counted, Counted, Counted, Counted]]])

}
