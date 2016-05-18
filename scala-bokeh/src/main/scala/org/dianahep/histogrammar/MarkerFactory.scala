package org.dianahep.histogrammar

import scala.language.implicitConversions

import io.continuum.bokeh._

object MarkerFactory {
   
  // factory method  
  def apply(s: String): Marker = {
    val M = s match {
       case "circle" => new Circle
       case "diamond" => new Diamond
       case other => throw new IllegalArgumentException(
          s"Only circle, diamond markers are supported but got $other.")
    } 
    M
  }
}
