package test.scala.histogrammar

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers

import org.dianahep.histogrammar._
import org.dianahep.histogrammar.histogram._

class DefaultSuite extends FlatSpec with Matchers {
  "stuff" must "work" in {
    case class Datum(one: Boolean, two: Double, three: String)

    val hist = Histogramming(50, 5.0, 15.0, {d: Datum => d.two}, {d: Datum => d.one})

    0 until 10000 foreach {i =>
      hist.fill(Datum(scala.util.Random.nextBoolean(), scala.util.Random.nextGaussian() + 8.0, "whatever"))
    }

    println(hist.ascii)

    println(hist)
    println(hist.fix)

    val stringified = hist.toJson.stringify

    println(stringified)

    val reconstituted = Factory.fromJson[Histogrammed](stringified)

    println(reconstituted)

    println(reconstituted.ascii)

    println((hist.fix + reconstituted).ascii)

  }
}
