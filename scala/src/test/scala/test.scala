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

    val hist = Bin({d: Datum => d.two}, uncut[Datum], 5.0, 15.0, Seq.fill(100)(Count[Datum]), Count[Datum], Count[Datum], Count[Datum])

    0 until 10000 foreach {i =>
      hist.fill(Datum(scala.util.Random.nextDouble(), scala.util.Random.nextGaussian() + 8.0, "whatever"))
    }

    println(hist)
    println(hist.fix.show)

  }
}
