package test.scala.histogrammar

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers

import org.dianahep.histogrammar._
import org.dianahep.histogrammar.specialized._

class DefaultSuite extends FlatSpec with Matchers {
  "stuff" must "work" in {
    // def histogram[DATUM](num: Int, low: Double, high: Double, key: Weighted[DATUM] => Double, weighting: Weighting[Weighted[DATUM]] = unweighted) = Binned(Count[DATUM], num, low, high, key, weighting)

    case class Datum(one: Double, two: Double, three: String)

    // val hist = histogram(10, 5.0, 15.0, {d: Datum => d.one + d.two})

    val hist = Binned(Count[Datum], 50, 5.0, 15.0, {d: Datum => d.two}, {d: Datum => d.one})

    0 until 10000 foreach {i =>
      hist(Datum(scala.util.Random.nextDouble(), scala.util.Random.nextGaussian() + 8.0, "whatever"))
    }

    println(hist)
    println(hist.show)

  }
}
