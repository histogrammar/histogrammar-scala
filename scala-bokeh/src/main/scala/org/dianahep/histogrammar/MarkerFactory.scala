package org.dianahep.histogrammar

import scala.language.implicitConversions

import io.continuum.bokeh._

object MarkerFactory {
   
  // factory method  
  def apply(s: String) = s match {
       case "square"   => new Square
       case "diamond"  => new Diamond
       case "cross"    => new Cross
       case "triangle" => new Triangle
       case other      => new Circle  //make circle a default Marker
   }

}
