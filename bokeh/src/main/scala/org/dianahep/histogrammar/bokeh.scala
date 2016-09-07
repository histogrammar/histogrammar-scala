// Copyright 2016 DIANA-HEP
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
import io.continuum.bokeh.Line
import io.continuum.bokeh.Rect
import io.continuum.bokeh.Circle
import io.continuum.bokeh.Diamond
import io.continuum.bokeh.Square
import io.continuum.bokeh.Cross
import io.continuum.bokeh.Triangle

import scala.collection.mutable.ArrayBuffer

package object bokeh extends Tools {

   class BokehImplicits(glyphs: GlyphRenderer) {

     def plot(xLabel:String, yLabel: String) : Plot = {

        val xdr = new DataRange1d
        val ydr = new DataRange1d

        val plot = new Plot().x_range(xdr).y_range(ydr).tools(Pan|WheelZoom)

        val xaxis = new LinearAxis().plot(plot).location(Location.Below).axis_label(xLabel)
        val yaxis = new LinearAxis().plot(plot).location(Location.Left).axis_label(yLabel)
        plot.below <<= (xaxis :: _)
        plot.left <<= (yaxis :: _)

        plot.renderers := List(xaxis, yaxis, glyphs)
        plot
      }

     //allow for default x and y labels 
     def plot() : Plot = {

        val xdr = new DataRange1d
        val ydr = new DataRange1d

        val plot = new Plot().x_range(xdr).y_range(ydr).tools(Pan|WheelZoom)

        val xaxis = new LinearAxis().plot(plot).location(Location.Below).axis_label("x")
        val yaxis = new LinearAxis().plot(plot).location(Location.Left).axis_label("y")
        plot.below <<= (xaxis :: _)
        plot.left <<= (yaxis :: _)

        plot.renderers := List(xaxis, yaxis, glyphs)
        plot
     }
   }
   implicit def implicitplot(glyphs: GlyphRenderer) = new BokehImplicits(glyphs)


   def plot(xLabel:String, yLabel: String, glyphs: GlyphRenderer*) : Plot = {

      val xdr = new DataRange1d
      val ydr = new DataRange1d

      val plot = new Plot().x_range(xdr).y_range(ydr).tools(Pan|WheelZoom)

      val xaxis = new LinearAxis().plot(plot).location(Location.Below).axis_label(xLabel)
      val yaxis = new LinearAxis().plot(plot).location(Location.Left).axis_label(yLabel)
      plot.below <<= (xaxis :: _)
      plot.left <<= (yaxis :: _)

      val children = glyphs.toList
      plot.renderers := List(xaxis, yaxis):::children
      plot
    }

   //allow for default x and y labels 
   def plot(glyphs: GlyphRenderer*) : Plot = {

      val xdr = new DataRange1d
      val ydr = new DataRange1d

      val plot = new Plot().x_range(xdr).y_range(ydr).tools(Pan|WheelZoom)

      val xaxis = new LinearAxis().plot(plot).location(Location.Below).axis_label("x")
      val yaxis = new LinearAxis().plot(plot).location(Location.Left).axis_label("y")
      plot.below <<= (xaxis :: _)
      plot.left <<= (yaxis :: _)

      val children = glyphs.toList
      plot.renderers := List(xaxis, yaxis):::children
      plot
   }

   def plot(glyphs: Array[GlyphRenderer]) : Plot = plot(glyphs: _*)

   def plot(xLabel:String, yLabel: String,glyphs: Array[GlyphRenderer]) : Plot = plot(xLabel,yLabel,glyphs: _*)

   def plot(glyphs: Array[GlyphRenderer],glyph: GlyphRenderer) : Plot = {
      val combined = glyphs:+glyph
      plot(combined: _*)
   }

   def plot(xLabel:String, yLabel: String,glyphs: Array[GlyphRenderer],glyph: GlyphRenderer) : Plot = {
      val combined = glyphs:+glyph
      plot(xLabel,yLabel,combined: _*)
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

  implicit def anyBinnedToHistogramMethodsBokeh[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Binned[Counted, U, O, N]): HistogramMethodsBokeh =
    new HistogramMethodsBokeh(anyBinnedToHistogramMethods(hist).selected)
  implicit def anyBinningToHistogramMethodsBokeh[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Binning[DATUM, Counting, U, O, N]): HistogramMethodsBokeh =
    new HistogramMethodsBokeh(anyBinningToHistogramMethods(hist).selected)
  implicit def anySelectedBinnedToHistogramMethodsBokeh[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Selected[Binned[Counted, U, O, N]]): HistogramMethodsBokeh =
    new HistogramMethodsBokeh(anySelectedBinnedToHistogramMethods(hist).selected)
  implicit def anySelectingBinningToHistogramMethodsBokeh[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, Binning[DATUM, Counting, U, O, N]]): HistogramMethodsBokeh =
    new HistogramMethodsBokeh(anySelectingBinningToHistogramMethods(hist).selected)
  implicit def anySparselyBinnedToHistogramMethodsBokeh[N <: Container[N] with NoAggregation](hist: SparselyBinned[Counted, N]): HistogramMethodsBokeh =
    new HistogramMethodsBokeh(anySparselyBinnedToHistogramMethods(hist).selected)
  implicit def anySparselyBinningToHistogramMethodsBokeh[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: SparselyBinning[DATUM, Counting, N]): HistogramMethodsBokeh =
    new HistogramMethodsBokeh(anySparselyBinningToHistogramMethods(hist).selected)
  implicit def anySelectedSparselyBinnedToHistogramMethodsBokeh[N <: Container[N] with NoAggregation](hist: Selected[SparselyBinned[Counted, N]]): HistogramMethodsBokeh =
    new HistogramMethodsBokeh(anySelectedSparselyBinnedToHistogramMethods(hist).selected)
  implicit def anySelectingSparselyBinningToHistogramMethodsBokeh[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, SparselyBinning[DATUM, Counting, N]]): HistogramMethodsBokeh =
    new HistogramMethodsBokeh(anySelectingSparselyBinningToHistogramMethods(hist).selected)

  class HistogramMethodsBokeh(hist: Selected[Binned[Counted, Counted, Counted, Counted]]) {
    def bokeh(glyphType: String = "line", glyphSize: Int = 1, fillColor: Color = Color.Red, lineColor: Color = Color.Black) : GlyphRenderer = {

      //Prepare histogram contents for plotting
      val h = hist.cut.high
      val l = hist.cut.low
      val step = (h-l)/hist.cut.values.length

      object source extends ColumnDataSource {
          val x = column(l to h by step)
          val xh = column(l+step/2 to h+step/2 by step)
          val y = column(hist.cut.values.map(_.entries))
          val yh = column(hist.cut.values.map(v=>v.entries/2))
          val ci = column(hist.confidenceIntervalValues().map(v => v*2.0)) 
      }
      import source.{x,xh,y,yh,ci}

      //Set marker color, fill color, line color
      val glyph = glyphType match {
       case "square"   => new Square().x(xh).y(yh).size(glyphSize).fill_color(fillColor).line_color(lineColor)
       case "diamond"  => new Diamond().x(xh).y(yh).size(glyphSize).fill_color(fillColor).line_color(lineColor)
       case "cross"    => new Cross().x(xh).y(yh).size(glyphSize).fill_color(fillColor).line_color(lineColor)
       case "triangle" => new Triangle().x(xh).y(yh).size(glyphSize).fill_color(fillColor).line_color(lineColor)
       case "circle"   => new Circle().x(xh).y(yh).size(glyphSize).fill_color(fillColor).line_color(lineColor)
       case "histogram"=> new Rect().x(xh).y(yh).width(step).height(y).fill_color(fillColor).line_color(lineColor)
       case "errors"   => new Rect().x(xh).y(yh).width(step).height(ci).fill_color(fillColor).line_color(lineColor)
       case other      => new Line().x(x).y(y).line_color(lineColor).line_width(glyphSize)
      }

      new GlyphRenderer().data_source(source).glyph(glyph)
    }
  }

  //////////////////////////////////////////////////////////////// methods for Profile and SparselyProfile

  implicit def anyBinnedToProfileMethodsBokeh[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Binned[Averaged, U, O, N]): ProfileMethodsBokeh =
    new ProfileMethodsBokeh(anyBinnedToProfileMethods(hist).selected)
  implicit def anyBinningToProfileMethodsBokeh[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Binning[DATUM, Averaging[DATUM], U, O, N]): ProfileMethodsBokeh =
    new ProfileMethodsBokeh(anyBinningToProfileMethods(hist).selected)
  implicit def anySelectedBinnedToProfileMethodsBokeh[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Selected[Binned[Averaged, U, O, N]]): ProfileMethodsBokeh =
    new ProfileMethodsBokeh(anySelectedBinnedToProfileMethods(hist).selected)
  implicit def anySelectingBinningToProfileMethodsBokeh[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, Binning[DATUM, Averaging[DATUM], U, O, N]]): ProfileMethodsBokeh =
    new ProfileMethodsBokeh(anySelectingBinningToProfileMethods(hist).selected)
  implicit def anySparselyBinnedToProfileMethodsBokeh[N <: Container[N] with NoAggregation](hist: SparselyBinned[Averaged, N]): ProfileMethodsBokeh =
    new ProfileMethodsBokeh(anySparselyBinnedToProfileMethods(hist).selected)
  implicit def anySparselyBinningToProfileMethodsBokeh[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: SparselyBinning[DATUM, Averaging[DATUM], N]): ProfileMethodsBokeh =
    new ProfileMethodsBokeh(anySparselyBinningToProfileMethods(hist).selected)
  implicit def anySelectedSparselyBinnedToProfileMethodsBokeh[N <: Container[N] with NoAggregation](hist: Selected[SparselyBinned[Averaged, N]]): ProfileMethodsBokeh =
    new ProfileMethodsBokeh(anySelectedSparselyBinnedToProfileMethods(hist).selected)
  implicit def anySelectingSparselyBinningToProfileMethodsBokeh[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, SparselyBinning[DATUM, Averaging[DATUM], N]]): ProfileMethodsBokeh =
    new ProfileMethodsBokeh(anySelectingSparselyBinningToProfileMethods(hist).selected)

  class ProfileMethodsBokeh(val profile: Selected[Binned[Averaged, Counted, Counted, Counted]]) {
    def bokeh(glyphType: String = "line", glyphSize: Int = 1, fillColor: Color = Color.Red, lineColor: Color = Color.Black) : GlyphRenderer = {

      //Prepare histogram contents for plotting
      val h = profile.cut.high
      val l = profile.cut.low
      val step = (h-l)/profile.cut.values.length

      object source extends ColumnDataSource {
          val x = column(l to h by step)
          val xh = column(l+step/2 to h+step/2 by step)
          val y = column(hist.cut.values.map(_.entries))
          val yh = column(hist.cut.values.map(v=>v.entries/2))
          val ci = column(hist.confidenceIntervalValues().map(v => v*2.0))
      }
      import source.{x,xh,y,yh,ci}


      val glyph = glyphType match {
       case "square"   => new Square().x(xh).y(yh).size(glyphSize).fill_color(fillColor).line_color(lineColor)
       case "diamond"  => new Diamond().x(xh).y(yh).size(glyphSize).fill_color(fillColor).line_color(lineColor)
       case "cross"    => new Cross().x(xh).y(yh).size(glyphSize).fill_color(fillColor).line_color(lineColor)
       case "triangle" => new Triangle().x(xh).y(yh).size(glyphSize).fill_color(fillColor).line_color(lineColor)
       case "circle"   => new Circle().x(xh).y(yh).size(glyphSize).fill_color(fillColor).line_color(lineColor)
       case other      => new Line().x(x).y(y).line_color(lineColor).line_width(glyphSize)
      }

      new GlyphRenderer().data_source(source).glyph(glyph)
    }
  }

  //////////////////////////////////////////////////////////////// methods for ProfileErr and SparselyProfileErr

  implicit def anyBinnedToProfileErrMethodsBokeh[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Binned[Deviated, U, O, N]): ProfileErrMethodsBokeh =
    new ProfileErrMethodsBokeh(anyBinnedToProfileErrMethods(hist).selected)
  implicit def anyBinningToProfileErrMethodsBokeh[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Binning[DATUM, Deviating[DATUM], U, O, N]): ProfileErrMethodsBokeh =
    new ProfileErrMethodsBokeh(anyBinningToProfileErrMethods(hist).selected)
  implicit def anySelectedBinnedToProfileErrMethodsBokeh[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Selected[Binned[Deviated, U, O, N]]): ProfileErrMethodsBokeh =
    new ProfileErrMethodsBokeh(anySelectedBinnedToProfileErrMethods(hist).selected)
  implicit def anySelectingBinningToProfileErrMethodsBokeh[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, Binning[DATUM, Deviating[DATUM], U, O, N]]): ProfileErrMethodsBokeh =
    new ProfileErrMethodsBokeh(anySelectingBinningToProfileErrMethods(hist).selected)
  implicit def anySparselyBinnedToProfileErrMethodsBokeh[N <: Container[N] with NoAggregation](hist: SparselyBinned[Deviated, N]): ProfileErrMethodsBokeh =
    new ProfileErrMethodsBokeh(anySparselyBinnedToProfileErrMethods(hist).selected)
  implicit def anySparselyBinningToProfileErrMethodsBokeh[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: SparselyBinning[DATUM, Deviating[DATUM], N]): ProfileErrMethodsBokeh =
    new ProfileErrMethodsBokeh(anySparselyBinningToProfileErrMethods(hist).selected)
  implicit def anySelectedSparselyBinnedToProfileErrMethodsBokeh[N <: Container[N] with NoAggregation](hist: Selected[SparselyBinned[Deviated, N]]): ProfileErrMethodsBokeh =
    new ProfileErrMethodsBokeh(anySelectedSparselyBinnedToProfileErrMethods(hist).selected)
  implicit def anySelectingSparselyBinningToProfileErrMethodsBokeh[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, SparselyBinning[DATUM, Deviating[DATUM], N]]): ProfileErrMethodsBokeh =
    new ProfileErrMethodsBokeh(anySelectingSparselyBinningToProfileErrMethods(hist).selected)

  class ProfileErrMethodsBokeh(val profile: Selected[Binned[Deviated, Counted, Counted, Counted]]) {
    def bokeh(glyphType: String = "line", glyphSize: Int = 1, fillColor: Color = Color.Red, lineColor: Color = Color.Black) : GlyphRenderer = {

      //Prepare histogram contents for plotting
      val h = profile.cut.high
      val l = profile.cut.low
      val step = (h-l)/profile.cut.values.length

      object source extends ColumnDataSource {
          val x = column(l to h by step)
          val y = column(profile.cut.values.map(v=>v.mean))
          val yerr = column(profile.cut.values.map(v => if (v.entries > 0.0) Math.sqrt(v.variance / v.entries) else 0.0))
          val xh = column(l+step/2 to h+step/2 by step)
          val yh = column(hist.cut.values.map(v=>v.entries/2))
      }
      import source.{x,xh,y,yh,yerr}

      //Set marker color, fill color, line color
      val glyph = glyphType match {
       case "square"   => new Square().x(xh).y(yh).size(glyphSize).fill_color(fillColor).line_color(lineColor)
       case "diamond"  => new Diamond().x(xh).y(yh).size(glyphSize).fill_color(fillColor).line_color(lineColor)
       case "cross"    => new Cross().x(xh).y(yh).size(glyphSize).fill_color(fillColor).line_color(lineColor)
       case "triangle" => new Triangle().x(xh).y(yh).size(glyphSize).fill_color(fillColor).line_color(lineColor)
       case "circle"   => new Circle().x(xh).y(yh).size(glyphSize).fill_color(fillColor).line_color(lineColor)
       case "errors"   => new Rect().x(xh).y(yh).width(step).height(yerr).fill_color(fillColor).line_color(lineColor)  
       case other      => new Line().x(x).y(y).line_color(lineColor).line_width(glyphSize)
      }

      new GlyphRenderer().data_source(source).glyph(glyph)
    }
  }

  //////////////////////////////////////////////////////////////// methods for StackedHistogram, including cases for mixed tenses

  implicit def anyBinnedToStackedHistogramMethodsBokeh[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: Stacked[Binned[Counted, U, O, N], SN]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(anyBinnedToStackedHistogramMethods(hist).stacked)
  implicit def anyBinningToStackedHistogramMethodsBokeh[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: Stacking[DATUM, Binning[DATUM, Counting, U, O, N], SN]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(anyBinningToStackedHistogramMethods(hist).stacked)
  implicit def anySelectedBinnedToStackedHistogramMethodsBokeh[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: Stacked[Selected[Binned[Counted, U, O, N]], SN]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(anySelectedBinnedToStackedHistogramMethods(hist).stacked)
  implicit def anySelectingBinningToStackedHistogramMethodsBokeh[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: Stacking[DATUM, Selecting[DATUM, Binning[DATUM, Counting, U, O, N]], SN]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(anySelectingBinningToStackedHistogramMethods(hist).stacked)
  implicit def anySparselyBinnedToStackedHistogramMethodsBokeh[N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: Stacked[SparselyBinned[Counted, N], SN]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(anySparselyBinnedToStackedHistogramMethods(hist).stacked)
  implicit def anySparselyBinningToStackedHistogramMethodsBokeh[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: Stacking[DATUM, SparselyBinning[DATUM, Counting, N], SN]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(anySparselyBinningToStackedHistogramMethods(hist).stacked)
  implicit def anySelectedSparselyBinnedToStackedHistogramMethodsBokeh[N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: Stacked[Selected[SparselyBinned[Counted, N]], SN]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(anySelectedSparselyBinnedToStackedHistogramMethods(hist).stacked)
  implicit def anySelectingSparselyBinningToStackedHistogramMethodsBokeh[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: Stacking[DATUM, Selecting[DATUM, SparselyBinning[DATUM, Counting, N]], SN]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(anySelectingSparselyBinningToStackedHistogramMethods(hist).stacked)
  implicit def anyBinnedMixedToStackedHistogramMethodsBokeh[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with NoAggregation](hist: Stacked[Binning[DATUM, Counting, U, O, N], SN]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(anyBinnedMixedToStackedHistogramMethods(hist).stacked)
  implicit def anySelectedBinnedMixedToStackedHistogramMethodsBokeh[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with NoAggregation](hist: Stacked[Selecting[DATUM, Binning[DATUM, Counting, U, O, N]], SN]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(anySelectedBinnedMixedToStackedHistogramMethods(hist).stacked)
  implicit def anySparselyBinnedMixedToStackedHistogramMethodsBokeh[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with NoAggregation](hist: Stacked[SparselyBinning[DATUM, Counting, N], SN]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(anySparselyBinnedMixedToStackedHistogramMethods(hist).stacked)
  implicit def anySelectedSparselyBinnedMixedToStackedHistogramMethodsBokeh[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with NoAggregation](hist: Stacked[Selecting[DATUM, SparselyBinning[DATUM, Counting, N]], SN]): StackedHistogramMethodsBokeh =
    new StackedHistogramMethodsBokeh(anySelectedSparselyBinnedMixedToStackedHistogramMethods(hist).stacked)

  class StackedHistogramMethodsBokeh(stack: Stacked[Selected[Binned[Counted, Counted, Counted, Counted]], Counted]) {
    val glyphTypeDefaults = List("circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle","circle")
    val glyphSizeDefaults = List(1,1,1,1,1,1,1,1,1,1,1,1)
    val fillColorDefaults = List(Color.Red,Color.Red,Color.Red,Color.Red,Color.Red,Color.Red,Color.Red,Color.Red,Color.Red,Color.Red,Color.Red,Color.Red)
    val lineColorDefaults = List(Color.Red,Color.Red,Color.Red,Color.Red,Color.Red,Color.Red,Color.Red,Color.Red,Color.Red,Color.Red,Color.Red,Color.Red)

    def bokeh(glyphTypes: List[String] = glyphTypeDefaults, glyphSizes: List[Int] = glyphSizeDefaults, fillColors: List[Color] = fillColorDefaults, lineColors: List[Color] = lineColorDefaults) : Array[GlyphRenderer] = {
      val nChildren = stack.children.length
      assert(glyphTypes.length >= nChildren)
      assert(glyphSizes.length >= nChildren)
      assert(fillColors.length >= nChildren)
      assert(lineColors.length >= nChildren)

      var stackedGlyphs: ArrayBuffer[GlyphRenderer] = ArrayBuffer.empty[GlyphRenderer]
      var ichild = 0

      for (p <- stack.children) {
          stackedGlyphs += p.bokeh(glyphTypes(ichild),glyphSizes(ichild),fillColors(ichild),lineColors(ichild))
          ichild += 1
      }
      stackedGlyphs.toArray
    }
  }

  //////////////////////////////////////////////////////////////// methods for PartitionedHistogram

  implicit def anyBinnedToPartitionedHistogramMethodsBokeh[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: IrregularlyBinned[Binned[Counted, U, O, N], SN]): PartitionedHistogramMethodsBokeh =
    new PartitionedHistogramMethodsBokeh(anyBinnedToPartitionedHistogramMethods(hist).partitioned)
  implicit def anyBinningToPartitionedHistogramMethodsBokeh[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: IrregularlyBinning[DATUM, Binning[DATUM, Counting, U, O, N], SN]): PartitionedHistogramMethodsBokeh =
    new PartitionedHistogramMethodsBokeh(anyBinningToPartitionedHistogramMethods(hist).partitioned)
  implicit def anySelectedBinnedToPartitionedHistogramMethodsBokeh[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: IrregularlyBinned[Selected[Binned[Counted, U, O, N]], SN]): PartitionedHistogramMethodsBokeh =
    new PartitionedHistogramMethodsBokeh(anySelectedBinnedToPartitionedHistogramMethods(hist).partitioned)
  implicit def anySelectingBinningToPartitionedHistogramMethodsBokeh[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: IrregularlyBinning[DATUM, Selecting[DATUM, Binning[DATUM, Counting, U, O, N]], SN]): PartitionedHistogramMethodsBokeh =
    new PartitionedHistogramMethodsBokeh(anySelectingBinningToPartitionedHistogramMethods(hist).partitioned)
  implicit def anySparselyBinnedToPartitionedHistogramMethodsBokeh[N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: IrregularlyBinned[SparselyBinned[Counted, N], SN]): PartitionedHistogramMethodsBokeh =
    new PartitionedHistogramMethodsBokeh(anySparselyBinnedToPartitionedHistogramMethods(hist).partitioned)
  implicit def anySparselyBinningToPartitionedHistogramMethodsBokeh[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: IrregularlyBinning[DATUM, SparselyBinning[DATUM, Counting, N], SN]): PartitionedHistogramMethodsBokeh =
    new PartitionedHistogramMethodsBokeh(anySparselyBinningToPartitionedHistogramMethods(hist).partitioned)
  implicit def anySelectedSparselyBinnedToPartitionedHistogramMethodsBokeh[N <: Container[N] with NoAggregation, SN <: Container[SN] with NoAggregation](hist: IrregularlyBinned[Selected[SparselyBinned[Counted, N]], SN]): PartitionedHistogramMethodsBokeh =
    new PartitionedHistogramMethodsBokeh(anySelectedSparselyBinnedToPartitionedHistogramMethods(hist).partitioned)
  implicit def anySelectingSparselyBinningToPartitionedHistogramMethodsBokeh[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}, SN <: Container[SN] with Aggregation{type Datum >: DATUM}](hist: IrregularlyBinning[DATUM, Selecting[DATUM, SparselyBinning[DATUM, Counting, N]], SN]): PartitionedHistogramMethodsBokeh =
    new PartitionedHistogramMethodsBokeh(anySelectingSparselyBinningToPartitionedHistogramMethods(hist).partitioned)

  class PartitionedHistogramMethodsBokeh(val partitioned: IrregularlyBinned[Selected[Binned[Counted, Counted, Counted, Counted]], Counted])

  //////////////////////////////////////////////////////////////// methods for FractionedHistogram

  implicit def anyBinnedToFractionedHistogramMethodsBokeh[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Fractioned[Binned[Counted, U, O, N], Binned[Counted, U, O, N]]): FractionedHistogramMethodsBokeh =
    new FractionedHistogramMethodsBokeh(anyBinnedToFractionedHistogramMethods(hist).fractioned)
  implicit def anyBinningToFractionedHistogramMethodsBokeh[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Fractioning[DATUM, Binning[DATUM, Counting, U, O, N]]): FractionedHistogramMethodsBokeh =
    new FractionedHistogramMethodsBokeh(anyBinningToFractionedHistogramMethods(hist).fractioned)
  implicit def anySelectedBinnedToFractionedHistogramMethodsBokeh[U <: Container[U] with NoAggregation, O <: Container[O] with NoAggregation, N <: Container[N] with NoAggregation](hist: Fractioned[Selected[Binned[Counted, U, O, N]], Selected[Binned[Counted, U, O, N]]]): FractionedHistogramMethodsBokeh =
    new FractionedHistogramMethodsBokeh(anySelectedBinnedToFractionedHistogramMethods(hist).fractioned)
  implicit def anySelectingBinningToFractionedHistogramMethodsBokeh[DATUM, U <: Container[U] with Aggregation{type Datum >: DATUM}, O <: Container[O] with Aggregation{type Datum >: DATUM}, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Fractioning[DATUM, Selecting[DATUM, Binning[DATUM, Counting, U, O, N]]]): FractionedHistogramMethodsBokeh =
    new FractionedHistogramMethodsBokeh(anySelectingBinningToFractionedHistogramMethods(hist).fractioned)
  implicit def anySparselyBinnedToFractionedHistogramMethodsBokeh[N <: Container[N] with NoAggregation](hist: Fractioned[SparselyBinned[Counted, N], SparselyBinned[Counted, N]]): FractionedHistogramMethodsBokeh =
    new FractionedHistogramMethodsBokeh(anySparselyBinnedToFractionedHistogramMethods(hist).fractioned)
  implicit def anySparselyBinningToFractionedHistogramMethodsBokeh[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Fractioning[DATUM, SparselyBinning[DATUM, Counting, N]]): FractionedHistogramMethodsBokeh =
    new FractionedHistogramMethodsBokeh(anySparselyBinningToFractionedHistogramMethods(hist).fractioned)
  implicit def anySelectedSparselyBinnedToFractionedHistogramMethodsBokeh[N <: Container[N] with NoAggregation](hist: Fractioned[Selected[SparselyBinned[Counted, N]], Selected[SparselyBinned[Counted, N]]]): FractionedHistogramMethodsBokeh =
    new FractionedHistogramMethodsBokeh(anySelectedSparselyBinnedToFractionedHistogramMethods(hist).fractioned)
  implicit def anySelectingSparselyBinningToFractionedHistogramMethodsBokeh[DATUM, N <: Container[N] with Aggregation{type Datum >: DATUM}](hist: Fractioning[DATUM, Selecting[DATUM, SparselyBinning[DATUM, Counting, N]]]): FractionedHistogramMethodsBokeh =
    new FractionedHistogramMethodsBokeh(anySelectingSparselyBinningToFractionedHistogramMethods(hist).fractioned)

  class FractionedHistogramMethodsBokeh(val fractioned: Fractioned[Selected[Binned[Counted, Counted, Counted, Counted]], Selected[Binned[Counted, Counted, Counted, Counted]]])

  //////////////////////////////////////////////////////////////// methods for TwoDimensionallyHistogram and TwoDimensionallySparselyHistogram

  implicit def binnedToTwoDimensionallyHistogramMethodsBokeh[UX <: Container[UX] with NoAggregation, OX <: Container[OX] with NoAggregation, NX <: Container[NX] with NoAggregation, UY <: Container[UY] with NoAggregation, OY <: Container[OY] with NoAggregation, NY <: Container[NY] with NoAggregation](hist: Binned[Binned[Counted, UY, OY, NY], UX, OX, NX]): TwoDimensionallyHistogramMethodsBokeh =
    new TwoDimensionallyHistogramMethodsBokeh(binnedToTwoDimensionallyHistogramMethods(hist).selected)
  implicit def binningToTwoDimensionallyHistogramMethodsBokeh[DATUM, UX <: Container[UX] with Aggregation{type Datum >: DATUM}, OX <: Container[OX] with Aggregation{type Datum >: DATUM}, NX <: Container[NX] with Aggregation{type Datum >: DATUM}, UY <: Container[UY] with Aggregation{type Datum >: DATUM}, OY <: Container[OY] with Aggregation{type Datum >: DATUM}, NY <: Container[NY] with Aggregation{type Datum >: DATUM}](hist: Binning[DATUM, Binning[DATUM, Counting, UY, OY, NY], UX, OX, NX]): TwoDimensionallyHistogramMethodsBokeh =
    new TwoDimensionallyHistogramMethodsBokeh(binningToTwoDimensionallyHistogramMethods(hist).selected)
  implicit def selectedBinnedToTwoDimensionallyHistogramMethodsBokeh[UX <: Container[UX] with NoAggregation, OX <: Container[OX] with NoAggregation, NX <: Container[NX] with NoAggregation, UY <: Container[UY] with NoAggregation, OY <: Container[OY] with NoAggregation, NY <: Container[NY] with NoAggregation](hist: Selected[Binned[Binned[Counted, UY, OY, NY], UX, OX, NX]]): TwoDimensionallyHistogramMethodsBokeh =
    new TwoDimensionallyHistogramMethodsBokeh(selectedBinnedToTwoDimensionallyHistogramMethods(hist).selected)
  implicit def selectingBinningToTwoDimensionallyHistogramMethodsBokeh[DATUM, UX <: Container[UX] with Aggregation{type Datum >: DATUM}, OX <: Container[OX] with Aggregation{type Datum >: DATUM}, NX <: Container[NX] with Aggregation{type Datum >: DATUM}, UY <: Container[UY] with Aggregation{type Datum >: DATUM}, OY <: Container[OY] with Aggregation{type Datum >: DATUM}, NY <: Container[NY] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, Binning[DATUM, Binning[DATUM, Counting, UY, OY, NY], UX, OX, NX]]): TwoDimensionallyHistogramMethodsBokeh =
    new TwoDimensionallyHistogramMethodsBokeh(selectingBinningToTwoDimensionallyHistogramMethods(hist).selected)
  implicit def sparselyBinnedToTwoDimensionallyHistogramMethodsBokeh[NX <: Container[NX] with NoAggregation, NY <: Container[NY] with NoAggregation](hist: SparselyBinned[SparselyBinned[Counted, NY], NX]): TwoDimensionallyHistogramMethodsBokeh =
    new TwoDimensionallyHistogramMethodsBokeh(sparselyBinnedToTwoDimensionallyHistogramMethods(hist).selected)
  implicit def sparselyBinningToTwoDimensionallyHistogramMethodsBokeh[DATUM, NX <: Container[NX] with Aggregation{type Datum >: DATUM}, NY <: Container[NY] with Aggregation{type Datum >: DATUM}](hist: SparselyBinning[DATUM, SparselyBinning[DATUM, Counting, NY], NX]): TwoDimensionallyHistogramMethodsBokeh =
    new TwoDimensionallyHistogramMethodsBokeh(sparselyBinningToTwoDimensionallyHistogramMethods(hist).selected)
  implicit def selectedSparselyBinnedToTwoDimensionallyHistogramMethodsBokeh[NX <: Container[NX] with NoAggregation, NY <: Container[NY] with NoAggregation](hist: Selected[SparselyBinned[SparselyBinned[Counted, NY], NX]]): TwoDimensionallyHistogramMethodsBokeh =
    new TwoDimensionallyHistogramMethodsBokeh(selectedSparselyBinnedToTwoDimensionallyHistogramMethods(hist).selected)
  implicit def selectingSparselyBinningToTwoDimensionallyHistogramMethodsBokeh[DATUM, NX <: Container[NX] with Aggregation{type Datum >: DATUM}, NY <: Container[NY] with Aggregation{type Datum >: DATUM}](hist: Selecting[DATUM, SparselyBinning[DATUM, SparselyBinning[DATUM, Counting, NY], NX]]): TwoDimensionallyHistogramMethodsBokeh =
    new TwoDimensionallyHistogramMethodsBokeh(selectingSparselyBinningToTwoDimensionallyHistogramMethods(hist).selected)

  class TwoDimensionallyHistogramMethodsBokeh(val selected: Selected[Binned[Binned[Counted, Counted, Counted, Counted], Counted, Counted, Counted]])

}
