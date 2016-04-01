package test.scala.histogrammar

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers

import org.dianahep.histogrammar._
import org.dianahep.histogrammar.histogram._

class DefaultSuite extends FlatSpec with Matchers {
  val simple = List(3.4, 2.2, -1.8, 0.0, 7.3, -4.7, 1.6, 0.0, -3.0, -1.7)

  case class Struct(bool: Boolean, int: Int, double: Double, string: String)

  val struct = List(
    Struct(true, -2, 3.4, "one"),
    Struct(false, -1, 2.2, "two"),
    Struct(true, 0, -1.8, "three"),
    Struct(false, 1, 0.0, "four"),
    Struct(false, 2, 7.3, "five"),
    Struct(false, 3, -4.7, "six"),
    Struct(true, 4, 1.6, "seven"),
    Struct(true, 5, 0.0, "eight"),
    Struct(false, 6, -3.0, "nine"),
    Struct(true, 7, -1.7, "ten"))

  "Counting/Counted" must "work unfiltered" in {
    for (i <- 0 to 10) {
      val (left, right) = simple.splitAt(i)

      val leftCounting = Counting[Double]()
      val rightCounting = Counting[Double]()

      left.foreach(leftCounting.fill(_))
      right.foreach(rightCounting.fill(_))

      val (Counting(leftResult), Counting(rightResult)) = (leftCounting, rightCounting)

      leftResult should be (left.size)
      rightResult should be (right.size)

      val Counted(finalResult) = leftCounting + rightCounting

      finalResult should be (simple.size)
    }
  }

  it must "work with a filter" in {
    for (i <- 0 to 10) {
      val (left, right) = struct.splitAt(i)

      val leftCounting = Counting({x: Struct => x.bool})
      val rightCounting = Counting({x: Struct => x.bool})

      left.foreach(leftCounting.fill(_))
      right.foreach(rightCounting.fill(_))

      val (Counting(leftResult), Counting(rightResult)) = (leftCounting, rightCounting)

      leftResult should be (left.filter(_.bool).size)
      rightResult should be (right.filter(_.bool).size)

      val Counted(finalResult) = leftCounting + rightCounting

      finalResult should be (struct.filter(_.bool).size)
    }
  }

  it must "work with a weighting factor" in {
    for (i <- 0 to 10) {
      val (left, right) = struct.splitAt(i)

      val leftCounting = Counting({x: Struct => x.int})
      val rightCounting = Counting({x: Struct => x.int})

      left.foreach(leftCounting.fill(_))
      right.foreach(rightCounting.fill(_))

      val (Counting(leftResult), Counting(rightResult)) = (leftCounting, rightCounting)

      leftResult should be (left.map(_.int).sum)
      rightResult should be (right.map(_.int).sum)

      val Counted(finalResult) = leftCounting + rightCounting

      finalResult should be (struct.map(_.int).sum)
    }
  }

  "Summing/Summed" must "work unfiltered" in {
    for (i <- 0 to 10) {
      val (left, right) = simple.splitAt(i)

      val leftSumming = Summing({x: Double => x})
      val rightSumming = Summing({x: Double => x})

      left.foreach(leftSumming.fill(_))
      right.foreach(rightSumming.fill(_))

      val (Summing(leftResult), Summing(rightResult)) = (leftSumming, rightSumming)

      leftResult should be (left.sum)
      rightResult should be (right.sum)

      val Summed(finalResult) = leftSumming + rightSumming

      finalResult should be (simple.sum +- 1e-12)
    }
  }

  it must "work with a filter" in {
    for (i <- 0 to 10) {
      val (left, right) = struct.splitAt(i)

      val leftSumming = Summing({x: Struct => x.double}, {x: Struct => x.bool})
      val rightSumming = Summing({x: Struct => x.double}, {x: Struct => x.bool})

      left.foreach(leftSumming.fill(_))
      right.foreach(rightSumming.fill(_))

      val (Summing(leftResult), Summing(rightResult)) = (leftSumming, rightSumming)

      leftResult should be (left.filter(_.bool).map(_.double).sum)
      rightResult should be (right.filter(_.bool).map(_.double).sum)

      val Summed(finalResult) = leftSumming + rightSumming

      finalResult should be (struct.filter(_.bool).map(_.double).sum +- 1e-12)
    }
  }

  it must "work with a weighting factor" in {
    for (i <- 0 to 10) {
      val (left, right) = struct.splitAt(i)

      val leftSumming = Summing({x: Struct => x.double}, {x: Struct => x.int})
      val rightSumming = Summing({x: Struct => x.double}, {x: Struct => x.int})

      left.foreach(leftSumming.fill(_))
      right.foreach(rightSumming.fill(_))

      val (Summing(leftResult), Summing(rightResult)) = (leftSumming, rightSumming)

      leftResult should be (left.map({x => x.int * x.double}).sum)
      rightResult should be (right.map({x => x.int * x.double}).sum)

      val Summed(finalResult) = leftSumming + rightSumming

      finalResult should be (struct.map({x => x.int * x.double}).sum +- 1e-12)
    }
  }
}
