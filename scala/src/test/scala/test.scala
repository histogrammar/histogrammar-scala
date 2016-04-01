package test.scala.histogrammar

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers

import org.dianahep.histogrammar._
import org.dianahep.histogrammar.specialized._

class DefaultSuite extends FlatSpec with Matchers {
  "stuff" must "work" in {
    case class Datum(one: Double, two: Double, three: String)

    val hist = Binning(50, 5.0, 15.0, {d: Datum => d.two})()

    0 until 10000 foreach {i =>
      hist.fill(Datum(scala.util.Random.nextDouble(), scala.util.Random.nextGaussian() + 8.0, "whatever"))
    }

    println(hist)
    println(hist.fix)

    println(hist.show)

  }
}
