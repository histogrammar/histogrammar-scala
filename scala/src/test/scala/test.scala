package test.scala.histogrammar

import scala.language.postfixOps

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers

import org.dianahep.histogrammar._

class DefaultSuite extends FlatSpec with Matchers {
  val simple = List(3.4, 2.2, -1.8, 0.0, 7.3, -4.7, 1.6, 0.0, -3.0, -1.7)

  case class Struct(bool: Boolean, int: Int, double: Double, string: String)

  val struct = List(
    Struct(true,  -2,  3.4, "one"),
    Struct(false, -1,  2.2, "two"),
    Struct(true,   0, -1.8, "three"),
    Struct(false,  1,  0.0, "four"),
    Struct(false,  2,  7.3, "five"),
    Struct(false,  3, -4.7, "six"),
    Struct(true,   4,  1.6, "seven"),
    Struct(true,   5,  0.0, "eight"),
    Struct(false,  6, -3.0, "nine"),
    Struct(true,   7, -1.7, "ten"))

  val backward = struct.reverse

  // straightforward mean and variance to complement the Tony Finch calculations used in the module

  def mean(x: List[Double]) =
    if (x.isEmpty)
      0.0
    else
      x.sum / x.size

  def mean(x: List[Double], w: List[Double]) =
    if (w.filter(_ > 0.0).isEmpty)
      0.0
    else
      (x zip w map {case (xi, wi) => xi * Math.max(wi, 0.0)} sum) / w.filter(_ > 0.0).sum

  def variance(x: List[Double]) =
    if (x.isEmpty)
      0.0
    else
      x.map(Math.pow(_, 2)).sum / x.size - Math.pow(x.sum / x.size, 2)

  def variance(x: List[Double], w: List[Double]) =
    if (w.filter(_ > 0.0).isEmpty)
      0.0
    else
      (x zip w map {case (xi, wi) => xi * xi * Math.max(wi, 0.0)} sum) / w.filter(_ > 0.0).sum - Math.pow((x zip w map {case (xi, wi) => xi * Math.max(wi, 0.0)} sum) / w.filter(_ > 0.0).sum, 2)

  def mae(x: List[Double]) =
    if (x.isEmpty)
      0.0
    else
      x.map(Math.abs).sum / x.size

  def mae(x: List[Double], w: List[Double]) =
    if (w.filter(_ > 0.0).isEmpty)
      0.0
    else
      (x zip w map {case (xi, wi) => Math.abs(xi) * Math.max(wi, 0.0)} sum) / w.filter(_ > 0.0).sum

  def checkJson(x: Container[_]) {
    x.toJson should be (Factory.fromJson(x.toJson).toJson)
  }

  //////////////////////////////////////////////////////////////// Count/Counted/Counting

  "Count/Counting/Counted" must "work unfiltered" in {
    for (i <- 0 to 10) {
      val (left, right) = simple.splitAt(i)

      val leftCounting = Count()
      val rightCounting = Count()

      left.foreach(leftCounting.fill(_))
      right.foreach(rightCounting.fill(_))

      val (Count(leftResult), Count(rightResult)) = (leftCounting, rightCounting)

      leftResult should be (left.size)
      rightResult should be (right.size)

      val Count(finalResult) = leftCounting + rightCounting

      finalResult should be (simple.size)

      checkJson(leftCounting)
    }
  }

  "Count/Counting/Counted" must "work with a filter" in {
    for (i <- 0 to 10) {
      val (left, right) = simple.splitAt(i)

      val leftCounting = Select({x: Double => x > 0.0} named "something" cached, Count())
      val rightCounting = Select({x: Double => x > 0.0} named "something" cached, Count())

      left.foreach(leftCounting.fill(_))
      right.foreach(rightCounting.fill(_))

      val (Select(Count(leftResult)), Select(Count(rightResult))) = (leftCounting, rightCounting)

      leftResult should be (left.filter(_ > 0.0).size)
      rightResult should be (right.filter(_ > 0.0).size)

      val Select(Count(finalResult)) = leftCounting + rightCounting

      finalResult should be (simple.filter(_ > 0.0).size)

      checkJson(leftCounting)
      checkJson(rightCounting)
    }
  }

  //////////////////////////////////////////////////////////////// Sum/Summed/Summing

  "Sum/Summing/Summed" must "work unfiltered" in {
    for (i <- 0 to 10) {
      val (left, right) = simple.splitAt(i)

      val leftSumming = Sum({x: Double => x} named "something")
      val rightSumming = Sum({x: Double => x} named "something")

      left.foreach(leftSumming.fill(_))
      right.foreach(rightSumming.fill(_))

      val (Sum(leftResult), Sum(rightResult)) = (leftSumming, rightSumming)

      leftResult should be (left.sum +- 1e-12)
      rightResult should be (right.sum +- 1e-12)

      val Sum(finalResult) = leftSumming + rightSumming

      finalResult should be (simple.sum +- 1e-12)

      checkJson(leftSumming)
    }
  }

  it must "work with a filter" in {
    for (i <- 0 to 10) {
      val (left, right) = struct.splitAt(i)

      val leftSumming = Select({x: Struct => x.bool}, Sum({x: Struct => x.double}))
      val rightSumming = Select({x: Struct => x.bool}, Sum({x: Struct => x.double}))

      left.foreach(leftSumming.fill(_))
      right.foreach(rightSumming.fill(_))

      val (Select(Sum(leftResult)), Select(Sum(rightResult))) = (leftSumming, rightSumming)

      leftResult should be (left.filter(_.bool).map(_.double).sum +- 1e-12)
      rightResult should be (right.filter(_.bool).map(_.double).sum +- 1e-12)

      val Select(Sum(finalResult)) = leftSumming + rightSumming

      finalResult should be (struct.filter(_.bool).map(_.double).sum +- 1e-12)

      checkJson(leftSumming)
    }
  }

  it must "work with a weighting factor" in {
    for (i <- 0 to 10) {
      val (left, right) = struct.splitAt(i)

      val leftSumming = Select({x: Struct => x.int}, Sum({x: Struct => x.double}))
      val rightSumming = Select({x: Struct => x.int}, Sum({x: Struct => x.double}))

      left.foreach(leftSumming.fill(_))
      right.foreach(rightSumming.fill(_))

      val (Select(Sum(leftResult)), Select(Sum(rightResult))) = (leftSumming, rightSumming)

      leftResult should be (left.filter(_.int >= 0).map({x => x.int * x.double}).sum +- 1e-12)
      rightResult should be (right.filter(_.int >= 0).map({x => x.int * x.double}).sum +- 1e-12)

      val Select(Sum(finalResult)) = leftSumming + rightSumming

      finalResult should be (struct.filter(_.int >= 0).map({x => x.int * x.double}).sum +- 1e-12)

      checkJson(leftSumming)
    }
  }

  //////////////////////////////////////////////////////////////// Average/Averaged/Averaging

  "Average/Averaging/Averaged" must "work unfiltered" in {
    for (i <- 0 to 10) {
      val (left, right) = simple.splitAt(i)

      val leftAveraging = Average({x: Double => x} named "something")
      val rightAveraging = Average({x: Double => x} named "something")

      left.foreach(leftAveraging.fill(_))
      right.foreach(rightAveraging.fill(_))

      val (Average(leftResult), Average(rightResult)) = (leftAveraging, rightAveraging)

      leftResult should be (mean(left) +- 1e-12)
      rightResult should be (mean(right) +- 1e-12)

      val Average(finalResult) = leftAveraging + rightAveraging

      finalResult should be (mean(simple) +- 1e-12)

      checkJson(leftAveraging)
    }
  }

  it must "work with a filter" in {
    for (i <- 0 to 10) {
      val (left, right) = struct.splitAt(i)

      val leftAveraging = Select({x: Struct => x.bool}, Average({x: Struct => x.double}))
      val rightAveraging = Select({x: Struct => x.bool}, Average({x: Struct => x.double}))

      left.foreach(leftAveraging.fill(_))
      right.foreach(rightAveraging.fill(_))

      val (Select(Average(leftResult)), Select(Average(rightResult))) = (leftAveraging, rightAveraging)

      leftResult should be (mean(left.filter(_.bool).map(_.double)) +- 1e-12)
      rightResult should be (mean(right.filter(_.bool).map(_.double)) +- 1e-12)

      val Select(Average(finalResult)) = leftAveraging + rightAveraging

      finalResult should be (mean(struct.filter(_.bool).map(_.double)) +- 1e-12)

      checkJson(leftAveraging)
    }
  }

  it must "work with a weighting factor" in {
    for (i <- 0 to 10) {
      val (left, right) = struct.splitAt(i)

      val leftAveraging = Select({x: Struct => x.int}, Average({x: Struct => x.double}))
      val rightAveraging = Select({x: Struct => x.int}, Average({x: Struct => x.double}))

      left.foreach(leftAveraging.fill(_))
      right.foreach(rightAveraging.fill(_))

      val (Select(Average(leftResult)), Select(Average(rightResult))) = (leftAveraging, rightAveraging)

      leftResult should be (mean(left.map(_.double), left.map(_.int.toDouble)) +- 1e-12)
      rightResult should be (mean(right.map(_.double), right.map(_.int.toDouble)) +- 1e-12)

      val Select(Average(finalResult)) = leftAveraging + rightAveraging

      finalResult should be (mean(struct.map(_.double), struct.map(_.int.toDouble)) +- 1e-12)

      checkJson(leftAveraging)
    }
  }

  it must "work in reverse" in {
    for (i <- 0 to 10) {
      val (left, right) = backward.splitAt(i)

      val leftAveraging = Select({x: Struct => x.int}, Average({x: Struct => x.double}))
      val rightAveraging = Select({x: Struct => x.int}, Average({x: Struct => x.double}))

      left.foreach(leftAveraging.fill(_))
      right.foreach(rightAveraging.fill(_))

      val (Select(Average(leftResult)), Select(Average(rightResult))) = (leftAveraging, rightAveraging)

      leftResult should be (mean(left.map(_.double), left.map(_.int.toDouble)) +- 1e-12)
      rightResult should be (mean(right.map(_.double), right.map(_.int.toDouble)) +- 1e-12)

      val Select(Average(finalResult)) = leftAveraging + rightAveraging

      finalResult should be (mean(backward.map(_.double), backward.map(_.int.toDouble)) +- 1e-12)

      checkJson(leftAveraging)
    }
  }

  //////////////////////////////////////////////////////////////// Deviate/Deviated/Deviating

  "Deviate/Deviating/Deviated" must "work unfiltered" in {
    for (i <- 0 to 10) {
      val (left, right) = simple.splitAt(i)

      val leftDeviating = Deviate({x: Double => x} named "something")
      val rightDeviating = Deviate({x: Double => x} named "something")

      left.foreach(leftDeviating.fill(_))
      right.foreach(rightDeviating.fill(_))

      val (Deviate(leftResult), Deviate(rightResult)) = (leftDeviating, rightDeviating)

      leftResult should be (variance(left) +- 1e-12)
      rightResult should be (variance(right) +- 1e-12)

      val Deviate(finalResult) = leftDeviating + rightDeviating

      finalResult should be (variance(simple) +- 1e-12)

      checkJson(leftDeviating)
    }
  }

  it must "work with a filter" in {
    for (i <- 0 to 10) {
      val (left, right) = struct.splitAt(i)

      val leftDeviating = Select({x: Struct => x.bool}, Deviate({x: Struct => x.double}))
      val rightDeviating = Select({x: Struct => x.bool}, Deviate({x: Struct => x.double}))

      left.foreach(leftDeviating.fill(_))
      right.foreach(rightDeviating.fill(_))

      val (Select(Deviate(leftResult)), Select(Deviate(rightResult))) = (leftDeviating, rightDeviating)

      leftResult should be (variance(left.filter(_.bool).map(_.double)) +- 1e-12)
      rightResult should be (variance(right.filter(_.bool).map(_.double)) +- 1e-12)

      val Select(Deviate(finalResult)) = leftDeviating + rightDeviating

      finalResult should be (variance(struct.filter(_.bool).map(_.double)) +- 1e-12)

      checkJson(leftDeviating)
    }
  }

  it must "work with a weighting factor" in {
    for (i <- 0 to 10) {
      val (left, right) = struct.splitAt(i)

      val leftDeviating = Select({x: Struct => x.int}, Deviate({x: Struct => x.double}))
      val rightDeviating = Select({x: Struct => x.int}, Deviate({x: Struct => x.double}))

      left.foreach(leftDeviating.fill(_))
      right.foreach(rightDeviating.fill(_))

      val (Select(Deviate(leftResult)), Select(Deviate(rightResult))) = (leftDeviating, rightDeviating)

      leftResult should be (variance(left.map(_.double), left.map(_.int.toDouble)) +- 1e-12)
      rightResult should be (variance(right.map(_.double), right.map(_.int.toDouble)) +- 1e-12)

      val Select(Deviate(finalResult)) = leftDeviating + rightDeviating

      finalResult should be (variance(struct.map(_.double), struct.map(_.int.toDouble)) +- 1e-12)

      checkJson(leftDeviating)
    }
  }

  it must "work in reverse" in {
    for (i <- 0 to 10) {
      val (left, right) = backward.splitAt(i)

      val leftDeviating = Select({x: Struct => x.int}, Deviate({x: Struct => x.double}))
      val rightDeviating = Select({x: Struct => x.int}, Deviate({x: Struct => x.double}))

      left.foreach(leftDeviating.fill(_))
      right.foreach(rightDeviating.fill(_))

      val (Select(Deviate(leftResult)), Select(Deviate(rightResult))) = (leftDeviating, rightDeviating)

      leftResult should be (variance(left.map(_.double), left.map(_.int.toDouble)) +- 1e-12)
      rightResult should be (variance(right.map(_.double), right.map(_.int.toDouble)) +- 1e-12)

      val Select(Deviate(finalResult)) = leftDeviating + rightDeviating

      finalResult should be (variance(backward.map(_.double), backward.map(_.int.toDouble)) +- 1e-12)

      checkJson(leftDeviating)
    }
  }

  //////////////////////////////////////////////////////////////// AbsoluteErr/AbsoluteErring/AbsoluteErred

  "AbsoluteErr/AbsoluteErring/AbsoluteErred" must "work" in {
    for (i <- 0 to 10) {
      val (left, right) = simple.splitAt(i)

      val leftAbsoluteErring = AbsoluteErr({x: Double => x} named "something")
      val rightAbsoluteErring = AbsoluteErr({x: Double => x} named "something")

      left.foreach(leftAbsoluteErring.fill(_))
      right.foreach(rightAbsoluteErring.fill(_))

      val (AbsoluteErr(leftResult), AbsoluteErr(rightResult)) = (leftAbsoluteErring, rightAbsoluteErring)

      leftResult should be (mae(left) +- 1e-12)
      rightResult should be (mae(right) +- 1e-12)

      val AbsoluteErr(finalResult) = leftAbsoluteErring + rightAbsoluteErring

      finalResult should be (mae(simple) +- 1e-12)

      checkJson(leftAbsoluteErring)
    }
  }

  //////////////////////////////////////////////////////////////// Minimize/Minimizing/Minimized

  "Minimize/Minimizing/Minimized" must "work" in {
    for (i <- 0 to 10) {
      val (left, right) = simple.splitAt(i)

      val leftMinimizing = Minimize({x: Double => x} named "something")
      val rightMinimizing = Minimize({x: Double => x} named "something")

      left.foreach(leftMinimizing.fill(_))
      right.foreach(rightMinimizing.fill(_))

      val (Minimize(leftResult), Minimize(rightResult)) = (leftMinimizing, rightMinimizing)

      if (left.isEmpty) leftResult.isNaN should be (true)
      else leftResult should be (left.min +- 1e-12)
      if (right.isEmpty) rightResult.isNaN should be (true)
      else rightResult should be (right.min +- 1e-12)

      val Minimize(finalResult) = leftMinimizing + rightMinimizing

      if (simple.isEmpty) finalResult.isNaN should be (true)
      else finalResult should be (simple.min +- 1e-12)

      checkJson(leftMinimizing)
    }
  }

  //////////////////////////////////////////////////////////////// Maximize/Maximizing/Maximized

  "Maximize/Maximizing/Maximized" must "work" in {
    for (i <- 0 to 10) {
      val (left, right) = simple.splitAt(i)

      val leftMaximizing = Maximize({x: Double => x} named "something")
      val rightMaximizing = Maximize({x: Double => x} named "something")

      left.foreach(leftMaximizing.fill(_))
      right.foreach(rightMaximizing.fill(_))

      val (Maximize(leftResult), Maximize(rightResult)) = (leftMaximizing, rightMaximizing)

      if (left.isEmpty) leftResult.isNaN should be (true)
      else leftResult should be (left.max +- 1e-12)
      if (right.isEmpty) rightResult.isNaN should be (true)
      else rightResult should be (right.max +- 1e-12)

      val Maximize(finalResult) = leftMaximizing + rightMaximizing

      if (simple.isEmpty) finalResult.isNaN should be (true)
      else finalResult should be (simple.max +- 1e-12)

      checkJson(leftMaximizing)
    }
  }

  //////////////////////////////////////////////////////////////// Quantile/Quantiling/Quantiled

  "Quantile/Quantiling/Quantiled" must "work" in {
    val answers = List(
      List(java.lang.Double.NaN, -0.481328271104, -0.481328271104),
      List(3.4, -0.69120847042, -0.282087623378),
      List(-0.675, -0.736543753016, -0.724235002413),
      List(-0.58125, -0.958145383329, -0.84507676833),
      List(0.13623046875, -1.53190059408, -0.864648168945),
      List(0.302100585937, -0.819002197266, -0.258450805664),
      List(-0.942007507324, -0.629296875, -0.816923254395),
      List(0.269603994253, -0.753125, -0.0372147040231),
      List(-0.628724939722, 0.24375, -0.454229951778),
      List(-0.562639074418, -1.7, -0.676375166976),
      List(-0.481328271104, java.lang.Double.NaN, -0.481328271104),
      List(java.lang.Double.NaN, -0.329460938614, -0.329460938614),
      List(3.4, -0.457521896462, -0.0717697068155),
      List(-0.45, -0.511698266503, -0.499358613202),
      List(-0.425, -0.706904919683, -0.622333443778),
      List(0.27890625, -0.937865017361, -0.451156510417),
      List(0.599765625, -0.65764453125, -0.028939453125),
      List(-0.637327473958, -0.471875, -0.571146484375),
      List(0.536730209662, -0.595833333333, 0.196961146763),
      List(-0.423513681061, 0.4875, -0.241310944849),
      List(-0.382340803288, -1.7, -0.514106722959),
      List(-0.329460938614, java.lang.Double.NaN, -0.329460938614),
      List(java.lang.Double.NaN, -0.168649887325, -0.168649887325),
      List(3.4, -0.227037303799, 0.135666426581),
      List(-0.225, -0.265185561995, -0.257148449596),
      List(-0.23125, -0.386842979665, -0.340165085765),
      List(0.42275390625, -0.477651570638, -0.117489379883),
      List(0.889514648438, -0.394795166016, 0.247359741211),
      List(-0.322354390462, -0.264453125, -0.299193884277),
      List(0.798766766295, -0.344791666667, 0.455699236407),
      List(-0.213212483191, 0.73125, -0.0243199865526),
      List(-0.194267772368, -1.7, -0.344840995131),
      List(-0.168649887325, java.lang.Double.NaN, -0.168649887325)
    )
    var line = 0

    for (p <- List(0.25, 0.5, 0.75))
      for (i <- 0 to 10) {
        val (left, right) = simple.splitAt(i)

        val leftQuantiling = Quantile(p, {x: Double => x} named "something")
        val rightQuantiling = Quantile(p, {x: Double => x} named "something")

        left.foreach(leftQuantiling.fill(_))
        right.foreach(rightQuantiling.fill(_))

        val (Quantile(leftResult), Quantile(rightResult)) = (leftQuantiling, rightQuantiling)
        val Quantile(finalResult) = leftQuantiling + rightQuantiling

        val List(leftAnswer, rightAnswer, finalAnswer) = answers(line)
        line += 1

        if (leftAnswer.isNaN)
          leftResult === leftAnswer should be (true)
        else
          leftResult should be (leftAnswer +- 1e-10)

        if (rightAnswer.isNaN)
          rightResult === rightAnswer should be (true)
        else
          rightResult should be (rightAnswer +- 1e-10)

        if (finalAnswer.isNaN)
          finalResult === finalAnswer should be (true)
        else
          finalResult should be (finalAnswer +- 1e-10)

        checkJson(leftQuantiling)
      }
  }

  //////////////////////////////////////////////////////////////// Bag/Bagged/Bagging

  "Bag/Bagged/Bagging" must "work" in {
    val one = Bag({x: Double => x} named "something")
    simple.foreach(one.fill(_))
    one.values should be (Map(7.3 -> 1.0, 2.2 -> 1.0, -1.7 -> 1.0, -4.7 -> 1.0, 0.0 -> 2.0, -1.8 -> 1.0, -3.0 -> 1.0, 1.6 -> 1.0, 3.4 -> 1.0))

    val two = Bag({x: Double => Vector(x, x)})
    simple.foreach(two.fill(_))
    two.values should be (Map(Vector(7.3, 7.3) -> 1.0, Vector(2.2, 2.2) -> 1.0, Vector(-1.7, -1.7) -> 1.0, Vector(-4.7, -4.7) -> 1.0, Vector(0.0, 0.0) -> 2.0, Vector(-1.8, -1.8) -> 1.0, Vector(-3.0, -3.0) -> 1.0, Vector(1.6, 1.6) -> 1.0, Vector(3.4, 3.4) -> 1.0))

    val three = Bag({x: Struct => x.string.substring(0, 1)})
    struct.foreach(three.fill(_))
    three.values should be (Map("n" -> 1.0, "e" -> 1.0, "t" -> 3.0, "s" -> 2.0, "f" -> 2.0, "o" -> 1.0))

    checkJson(one)
    checkJson(two)
    checkJson(three)
  }

  it must "work with Limit" in {
    val one = Limit(20, Bag({x: Struct => x.string}))
    struct.foreach(one.fill(_))
    one.get.values should be (Map("one" -> 1.0, "two" -> 1.0, "three" -> 1.0, "four" -> 1.0, "five" -> 1.0, "six" -> 1.0, "seven" -> 1.0, "eight" -> 1.0, "nine" -> 1.0, "ten" -> 1.0))

    val two = Limit(9, Bag({x: Struct => x.string}))
    struct.foreach(two.fill(_))
    two.saturated should be (true)

    checkJson(one)
    checkJson(two)
  }

  //////////////////////////////////////////////////////////////// Sample/Sampled/Sampling

  "Sample/Sampled/Sampling" must "work" in {
    val one = Sample(100, {x: Double => x} named "something")
    simple.foreach(one.fill(_))
    one.values.toSet should be (Set((3.4, 1.0), (2.2, 1.0), (-1.8, 1.0), (0.0, 1.0), (7.3, 1.0), (-4.7, 1.0), (1.6, 1.0), (0.0, 1.0), (-3.0, 1.0), (-1.7, 1.0)))

    val two = Sample(3, {x: Double => x})
    simple.foreach(two.fill(_))
    two.values.size should be (3)
    two.size should be (3)

    val three = Sample(100, {x: Double => Vector(x, x)})
    simple.foreach(three.fill(_))
    three.values.toSet should be (Set((Vector(3.4, 3.4), 1.0), (Vector(2.2, 2.2), 1.0), (Vector(-1.8, -1.8), 1.0), (Vector(0.0, 0.0), 1.0), (Vector(7.3, 7.3), 1.0), (Vector(-4.7, -4.7), 1.0), (Vector(1.6, 1.6), 1.0), (Vector(0.0, 0.0), 1.0), (Vector(-3.0, -3.0), 1.0), (Vector(-1.7, -1.7), 1.0)))

    val four = Sample(100, {x: Struct => x.string.substring(0, 1)})
    struct.foreach(four.fill(_))
    four.values.sorted should be (Vector("e" -> 1.0, "f" -> 1.0, "f" -> 1.0, "n" -> 1.0, "o" -> 1.0, "s" -> 1.0, "s" -> 1.0, "t" -> 1.0, "t" -> 1.0, "t" -> 1.0))

    val five = Sample(100, {x: Struct => x.string})
    struct.foreach(five.fill(_))
    five.values.sorted should be (Vector("eight" -> 1.0, "five" -> 1.0, "four" -> 1.0, "nine" -> 1.0, "one" -> 1.0, "seven" -> 1.0, "six" -> 1.0, "ten" -> 1.0, "three" -> 1.0, "two" -> 1.0))

    checkJson(one)
    checkJson(two)
    checkJson(three)
    checkJson(four)
    checkJson(five)
  }

  //////////////////////////////////////////////////////////////// Bin/Binned/Binning

  "Bin/Binning/Binned" must "work with Count/Counting/Counted" in {
    val one = Bin(5, -3.0, 7.0, {x: Double => x} named "xaxis")
    simple.foreach(one.fill(_))
    one.values.map(_.entries).toList should be (List(3.0, 2.0, 2.0, 1.0, 0.0))
    one.underflow.entries should be (1.0)
    one.overflow.entries should be (1.0)
    one.nanflow.entries should be (0.0)

    val two = Select({x: Struct => x.bool}, Bin(5, -3.0, 7.0, {x: Struct => x.double}))
    struct.foreach(two.fill(_))
    two.value.values.map(_.entries).toList should be (List(2.0, 1.0, 1.0, 1.0, 0.0))
    two.value.underflow.entries should be (0.0)
    two.value.overflow.entries should be (0.0)
    two.value.nanflow.entries should be (0.0)

    checkJson(one)
    checkJson(two)
  }

  "Binning/Binned" must "work with Sum/Summing/Summed" in {
    val one = Bin(5, -3.0, 7.0, {x: Double => x} named "xaxis", Sum({x: Double => 10.0} named "yaxis"), Sum({x: Double => 10.0}), Sum({x: Double => 10.0}), Sum({x: Double => 10.0}))
    simple.foreach(one.fill(_))
    one.values.map(_.sum).toList should be (List(30.0, 20.0, 20.0, 10.0, 0.0))
    one.underflow.sum should be (10.0)
    one.overflow.sum should be (10.0)
    one.nanflow.sum should be (0.0)

    val two = Select({x: Struct => x.bool}, Bin(5, -3.0, 7.0, {x: Struct => x.double}, Sum({x: Struct => 10.0}), Sum({x: Struct => 10.0}), Sum({x: Struct => 10.0}), Sum({x: Struct => 10.0})))
    struct.foreach(two.fill(_))
    two.value.values.map(_.sum).toList should be (List(20.0, 10.0, 10.0, 10.0, 0.0))
    two.value.underflow.sum should be (0.0)
    two.value.overflow.sum should be (0.0)
    two.value.nanflow.sum should be (0.0)

    checkJson(one)
    checkJson(two)
  }

  //////////////////////////////////////////////////////////////// SparselyBin/SparselyBinned/SparselyBinning

  "SparselyBin/SparselyBinned/SparselyBinning" must "work with Count/Counting/Counted" in {
    val one = SparselyBin(1.0, {x: Double => x} named "something")
    simple.foreach(one.fill(_))
    Factory.fromJson(one.toJson).as[SparselyBinned[Counted, Counted]].bins.map({case (k, v) => (k, v.entries)}).toList should be (List(-5 -> 1.0, -3 -> 1.0, -2 -> 2.0, 0 -> 2.0, 1 -> 1.0, 2 -> 1.0, 3 -> 1.0, 7 -> 1.0))

    one.numFilled should be (8)
    one.num should be (13)
    one.low.get should be (-5.0)
    one.high.get should be (8.0)

    checkJson(one)

    val two = SparselyBin(1.0, {x: Double => x} named "something", Sum({x: Double => x} named "else"))
    simple.foreach(two.fill(_))

    checkJson(two)
  }

  //////////////////////////////////////////////////////////////// CentrallyBin/CentrallyBinned/CentrallyBinning

  "CentrallyBin/CentrallyBinned/CentrallyBinning" must "work with Count/Counting/Counted" in {
    val one = CentrallyBin(List(-3.0, -1.0, 0.0, 1.0, 3.0, 10.0), {x: Double => x} named "something")
    one.center(1.5) should be (1.0)
    one.neighbors(1.0) should be ((Some(0.0), Some(3.0)))
    one.neighbors(10.0) should be ((Some(3.0), None))
    one.range(-3.0) should be ((java.lang.Double.NEGATIVE_INFINITY, -2.0))
    one.range(-1.0) should be ((-2.0, -0.5))
    one.range(0.0) should be ((-0.5, 0.5))
    one.range(10.0) should be ((6.5, java.lang.Double.POSITIVE_INFINITY))

    simple.foreach(one.fill(_))
    Factory.fromJson(one.toJson).as[CentrallyBinned[Counted, Counted]].bins.map({case (k, v) => (k, v.entries)}).toList should be (List((-3.0,2.0), (-1.0,2.0), (0.0,2.0), (1.0,1.0), (3.0,2.0), (10.0,1.0)))

    one.pdfTimesEntries(-3.0 to 10.0 by 1.0: _*).toList should be (List(0.7407407407407407, 1.3333333333333333, 1.3333333333333333, 2.0, 0.6666666666666666, 0.4444444444444444, 0.4444444444444444, 0.4444444444444444, 0.4444444444444444, 0.4444444444444444, 1.2500000000000002, 0.0, 0.0, 0.0))
    one.cdfTimesEntries(-3.0 to 10.0 by 1.0: _*).toList should be (List(1.2592592592592593, 2.0, 3.333333333333333, 5.0, 6.333333333333333, 7.0, 7.444444444444445, 7.888888888888889, 8.333333333333334, 8.777777777777779, 9.625, 10.0, 10.0, 10.0))
    one.qfTimesEntries(-1.0 to 11.0 by 1.0: _*).toList should be (List(-4.7, -4.7, -3.35, -2.0, -1.25, -0.5, 0.0, 0.5, 2.0, 4.25, 6.5, 7.3, 7.3))

    checkJson(one)

    val two = CentrallyBin(List(-3.0, -1.0, 0.0, 1.0, 3.0, 10.0), {x: Double => x} named "something", Sum({x: Double => x} named "else"))
    checkJson(two)
  }

  //////////////////////////////////////////////////////////////// AdaptivelyBin/AdaptivelyBinned/AdaptivelyBinning

  "AdaptivelyBin/AdaptivelyBinned/AdaptivelyBinning" must "work with Count/Counting/Counted" in {
    val one = AdaptivelyBin({x: Double => x} named "something", num = 5)
    simple.foreach(one.fill(_))
    one.bins.toList map {case (k, v) => (k, v.entries)} should be (List(-3.85 -> 2.0, -1.1666666666666667 -> 3.0, 0.8 -> 2.0, 2.8 -> 2.0, 7.3 -> 1.0))
    checkJson(one)

    val two = AdaptivelyBin({x: Double => x} named "something", num = 5, value = Sum({x: Double => x} named "else"))
    simple.foreach(two.fill(_))
    checkJson(two)
  }

  //////////////////////////////////////////////////////////////// Fraction/Fractioned/Fractioning

  "Fraction/Fractioned/Fractioning" must "work with Count/Counting/Counted" in {
    val fracking = Fraction({x: Double => x > 0.0} named "something", Count())
    simple.foreach(fracking.fill(_))

    fracking.numerator.entries should be (4.0)
    fracking.denominator.entries should be (10.0)

    checkJson(fracking)
  }

  it must "work with Sum/Summing/Summed" in {
    val fracking = Fraction({x: Double => x > 0.0} named "something", Sum({x: Double => x} named "else"))
    simple.foreach(fracking.fill(_))

    fracking.numerator.sum should be (14.5 +- 1e-12)
    fracking.denominator.sum should be (3.3 +- 1e-12)

    checkJson(fracking)
  }

  it must "work with Histogram/Histogramming/Histogrammed" in {
    val fracking = Fraction({x: Double => x > 0.0}, Histogram(5, -3.0, 7.0, {x: Double => x}))
    simple.foreach(fracking.fill(_))

    fracking.numerator.numericalValues.toList should be (List(0.0, 0.0, 2.0, 1.0, 0.0))
    fracking.denominator.numericalValues.toList should be (List(3.0, 2.0, 2.0, 1.0, 0.0))

    checkJson(fracking)
  }

  //////////////////////////////////////////////////////////////// Stack/Stacked/Stacking

  "Stack/Stacked/Stacking" must "work with Count/Counting/Counted" in {
    val stacking = Stack({x: Double => x} named "something", Count(), 0.0, 2.0, 4.0, 6.0, 8.0)
    simple.foreach(stacking.fill(_))

    stacking.cuts.map({case (k, v) => (k, v.entries)}).toList should be (List(java.lang.Double.NEGATIVE_INFINITY -> 10.0, 0.0 -> 6.0, 2.0 -> 3.0, 4.0 -> 1.0, 6.0 -> 1.0, 8.0 -> 0.0))

    checkJson(stacking)
  }

  it must "work with Sum/Summing/Summed" in {
    val stacking = Stack({x: Double => x} named "something", Sum({x: Double => x} named "else"), 0.0, 2.0, 4.0, 6.0, 8.0)
    simple.foreach(stacking.fill(_))

    stacking.cuts(1)._2.sum should be (14.5 +- 1e-12)

    checkJson(stacking)
  }

  //////////////////////////////////////////////////////////////// Partition/Partitioned/Partitioning

  "Partition/Partitioned/Partitioning" must "work with Count/Counting/Counted" in {
    val partitioning = Partition({x: Double => x} named "something", Count(), 0.0, 2.0, 4.0, 6.0, 8.0)
    simple.foreach(partitioning.fill(_))
    
    partitioning.cuts.map({case (k, v) => (k, v.entries)}).toList should be (List(java.lang.Double.NEGATIVE_INFINITY -> 4.0, 0.0 -> 3.0, 2.0 -> 2.0, 4.0 -> 0.0, 6.0 -> 1.0, 8.0 -> 0.0))

    checkJson(partitioning)
  }

  it must "work with Sum/Summing/Summed" in {
    val partitioning = Partition({x: Double => x} named "something", Sum({x: Double => x} named "else"), 0.0, 2.0, 4.0, 6.0, 8.0)
    simple.foreach(partitioning.fill(_))

    partitioning.cuts(0)._2.sum should be (-11.2 +- 1e-12)
    partitioning.cuts(1)._2.sum should be (1.6 +- 1e-12)

    checkJson(partitioning)
  }

  //////////////////////////////////////////////////////////////// Categorize/Categorized/Categorizing

  "Categorize/Categorized/Categorizing" must "work" in {
    val categorizing = Categorize({x: Struct => x.string.substring(0, 1)} named "something")
    struct.foreach(categorizing.fill(_))
    categorizing.pairsMap map {case (k, v) => (k, v.entries)} should be (Map("n" -> 1.0, "e" -> 1.0, "t" -> 3.0, "s" -> 2.0, "f" -> 2.0, "o" -> 1.0))
    checkJson(categorizing)

    val categorizing2 = Categorize({x: Struct => x.string.substring(0, 1)} named "something", Sum({x: Struct => x.double} named "else"))
    struct.foreach(categorizing2.fill(_))
    checkJson(categorizing2)
  }

  //////////////////////////////////////////////////////////////// Label/Labeled/Labeling

  "Label/Labeled/Labeling" must "work with a single type" in {
    val one = Histogram(5, -3.0, 7.0, {x: Double => x})
    val two = Histogram(10, 0.0, 10.0, {x: Double => x})
    val three = Histogram(5, -3.0, 7.0, {x: Double => 2*x})

    val labeling = Label("one" -> one, "two" -> two, "three" -> three)

    simple.foreach(labeling.fill(_))

    labeling("one").numericalValues should be (Seq(3.0, 2.0, 2.0, 1.0, 0.0))
    labeling("two").numericalValues should be (Seq(2.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0))
    labeling("three").numericalValues should be (Seq(0.0, 2.0, 0.0, 2.0, 1.0))

    checkJson(labeling)
  }

  it must "permit histograms to have different cuts" in {
    val one = Histogram(10, -10, 10, {x: Double => x}, {x: Double => x > 0})
    val two = Histogram(10, -10, 10, {x: Double => x}, {x: Double => x > 5})
    val three = Histogram(10, -10, 10, {x: Double => x}, {x: Double => x < 5})

    val labeling = Label("one" -> one, "two" -> two, "three" -> three)

    simple.foreach(labeling.fill(_))

    labeling("one").numericalValues should be (Seq(0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 2.0, 0.0, 1.0, 0.0))
    labeling("two").numericalValues should be (Seq(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0))
    labeling("three").numericalValues should be (Seq(0.0, 0.0, 1.0, 1.0, 2.0, 3.0, 2.0, 0.0, 0.0, 0.0))

    checkJson(labeling)
  }

  //////////////////////////////////////////////////////////////// UntypedLabel/UntypedLabeled/UntypedLabeling

  "UntypedLabel/UntypedLabeled/UntypedLabeling" must "work with a single type" in {
    val one = Histogram(5, -3.0, 7.0, {x: Double => x})
    val two = Histogram(10, 0.0, 10.0, {x: Double => x})
    val three = Histogram(5, -3.0, 7.0, {x: Double => 2*x})

    val labeling = UntypedLabel("one" -> one, "two" -> two, "three" -> three)

    simple.foreach(labeling.fill(_))

    labeling("one").as[one.Type].numericalValues should be (Seq(3.0, 2.0, 2.0, 1.0, 0.0))
    labeling("two").as[two.Type].numericalValues should be (Seq(2.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0))
    labeling("three").as[three.Type].numericalValues should be (Seq(0.0, 2.0, 0.0, 2.0, 1.0))

    checkJson(labeling)
  }

  it must "permit histograms to have different cuts" in {
    val one = Histogram(10, -10, 10, {x: Double => x}, {x: Double => x > 0})
    val two = Histogram(10, -10, 10, {x: Double => x}, {x: Double => x > 5})
    val three = Histogram(10, -10, 10, {x: Double => x}, {x: Double => x < 5})

    val labeling = UntypedLabel("one" -> one, "two" -> two, "three" -> three)

    simple.foreach(labeling.fill(_))

    labeling("one").as[one.Type].numericalValues should be (Seq(0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 2.0, 0.0, 1.0, 0.0))
    labeling("two").as[two.Type].numericalValues should be (Seq(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0))
    labeling("three").as[three.Type].numericalValues should be (Seq(0.0, 0.0, 1.0, 1.0, 2.0, 3.0, 2.0, 0.0, 0.0, 0.0))

    checkJson(labeling)
  }

  it must "work with multiple types" in {
    val one = Histogram(5, -3.0, 7.0, {x: Double => x})
    val two = Sum({x: Double => 1.0})
    val three = Deviate({x: Double => x + 100.0})

    val mapping = UntypedLabel("one" -> one, "two" -> two, "three" -> three)

    simple.foreach(mapping.fill(_))

    mapping("one").as[one.Type].numericalValues.toList should be (List(3.0, 2.0, 2.0, 1.0, 0.0))
    mapping("two").as[two.Type].sum should be (10.0)
    mapping("three").as[three.Type].entries should be (10.0 +- 1e-12)
    mapping("three").as[three.Type].mean should be (100.33 +- 1e-12)
    mapping("three").as[three.Type].variance should be (10.8381 +- 1e-12)   // just to be different

    checkJson(mapping)
  }

  //////////////////////////////////////////////////////////////// Index/Indexed/Indexing

  "Index/Indexed/Indexing" must "work with a single type" in {
    val one = Histogram(5, -3.0, 7.0, {x: Double => x})
    val two = Histogram(10, 0.0, 10.0, {x: Double => x})
    val three = Histogram(5, -3.0, 7.0, {x: Double => 2*x})

    val indexing = Index(one, two, three)

    simple.foreach(indexing.fill(_))

    indexing(0).numericalValues should be (Seq(3.0, 2.0, 2.0, 1.0, 0.0))
    indexing(1).numericalValues should be (Seq(2.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0))
    indexing(2).numericalValues should be (Seq(0.0, 2.0, 0.0, 2.0, 1.0))

    checkJson(indexing)
  }

  it must "permit histograms to have different cuts" in {
    val one = Histogram(10, -10, 10, {x: Double => x}, {x: Double => x > 0})
    val two = Histogram(10, -10, 10, {x: Double => x}, {x: Double => x > 5})
    val three = Histogram(10, -10, 10, {x: Double => x}, {x: Double => x < 5})

    val indexing = Index(one, two, three)

    simple.foreach(indexing.fill(_))

    indexing(0).numericalValues should be (Seq(0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 2.0, 0.0, 1.0, 0.0))
    indexing(1).numericalValues should be (Seq(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0))
    indexing(2).numericalValues should be (Seq(0.0, 0.0, 1.0, 1.0, 2.0, 3.0, 2.0, 0.0, 0.0, 0.0))

    checkJson(indexing)
  }

  //////////////////////////////////////////////////////////////// Branch/Branched/Branching

  "Branch/Branched/Branching" must "permit multiple types without losing type information" in {
    val one = Histogram(5, -3.0, 7.0, {x: Double => x})
    val two = Count()
    val three = Deviate({x: Double => x + 100.0})

    val branching = Branch(one, two, three)

    simple.foreach(branching.fill(_))

    branching.i0.numericalValues.toList should be (List(3.0, 2.0, 2.0, 1.0, 0.0))
    branching.i0.numericalUnderflow should be (1.0)
    branching.i0.numericalOverflow should be (1.0)
    branching.i0.numericalNanflow should be (0.0)

    branching.i1.entries should be (10.0)

    branching.i2.entries should be (10.0 +- 1e-12)
    branching.i2.mean should be (100.33 +- 1e-12)
    branching.i2.variance should be (10.8381 +- 1e-12)

    checkJson(branching)
  }

  //////////////////////////////////////////////////////////////// Usability in fold/aggregate

  "Aggregators/Combiners" must "be usable in Spark's aggregate (which has the same signature as Scala's foldLeft + reduce)" in {
    for (i <- 0 to 10) {
      val (left, right) = simple.splitAt(i)

      val partialHists = Seq(
        left.foldLeft(Bin(5, -3.0, 7.0, {x: Double => x}))({(hist, x) => hist.fill(x); hist}),
        right.foldLeft(Bin(5, -3.0, 7.0, {x: Double => x}))({(hist, x) => hist.fill(x); hist}))

      val finalHist = partialHists.reduce(_ + _)

      finalHist.numericalValues should be (Seq(3.0, 2.0, 2.0, 1.0, 0.0))
      finalHist.numericalUnderflow should be (1.0)
      finalHist.numericalOverflow should be (1.0)
      finalHist.numericalNanflow should be (0.0)

      checkJson(finalHist)
    }

    for (i <- 0 to 10) {
      val (left, right) = simple.splitAt(i)

      val hist1 = Bin(5, -3.0, 7.0, {x: Double => x})
      val hist2 = Bin(5, -3.0, 7.0, {x: Double => x})

      val partialHists = Seq(
        left.foldLeft(hist1)(new Increment[Double, hist1.Type]),
        right.foldLeft(hist2)(new Increment[Double, hist1.Type]))

      val finalHist = partialHists.reduce(new Combine[hist1.Type])

      finalHist.numericalValues should be (Seq(3.0, 2.0, 2.0, 1.0, 0.0))
      finalHist.numericalUnderflow should be (1.0)
      finalHist.numericalOverflow should be (1.0)
      finalHist.numericalNanflow should be (0.0)

      checkJson(finalHist)
    }

    for (i <- 0 to 10) {
      val (left, right) = simple.splitAt(i)

      val hist1 = Select(unweighted[Double], Bin(5, -3.0, 7.0, {x: Double => x}))
      val hist2 = Select(unweighted[Double], Bin(5, -3.0, 7.0, {x: Double => x}))

      val collection1 = Label("hist" -> hist1)
      val collection2 = Label("hist" -> hist2)

      val partialHists = Seq(
        left.foldLeft(collection1)(new Increment[Double, collection1.Type]),
        right.foldLeft(collection2)(new Increment[Double, collection1.Type]))

      val finalHist = partialHists.reduce(new Combine[collection1.Type])

      finalHist("hist").numericalValues should be (Seq(3.0, 2.0, 2.0, 1.0, 0.0))
      finalHist("hist").numericalUnderflow should be (1.0)
      finalHist("hist").numericalOverflow should be (1.0)
      finalHist("hist").numericalNanflow should be (0.0)

      checkJson(finalHist)
    }

    // for (i <- 0 to 10) {
    //   val (left, right) = simple.splitAt(i)

    //   val hist1 = Bin(5, -3.0, 7.0, {x: Double => x})
    //   val hist2 = Bin(5, -3.0, 7.0, {x: Double => x})
    //   val sum1 = Sum({x: Double => 1.0})
    //   val sum2 = Sum({x: Double => 1.0})

    //   val collection1 = UntypedLabel("hist" -> hist1, "sum" -> sum1)
    //   val collection2 = UntypedLabel("hist" -> hist2, "sum" -> sum2)

    //   val partialHists = Seq(
    //     left.foldLeft(collection1)(new Increment[Double, collection1.Type]),
    //     right.foldLeft(collection2)(new Increment[Double, collection1.Type]))

    //   val finalHist = partialHists.reduce(new Combine[collection1.Type])

    //   finalHist("hist").as[hist1.Type].numericalValues should be (Seq(3.0, 2.0, 2.0, 1.0, 0.0))
    //   finalHist("hist").as[hist1.Type].numericalUnderflow should be (1.0)
    //   finalHist("hist").as[hist1.Type].numericalOverflow should be (1.0)
    //   finalHist("hist").as[hist1.Type].numericalNanflow should be (0.0)
    //   finalHist("sum").as[sum1.Type].sum should be (10.0)

    //   checkJson(finalHist)
    // }
  }

  //////////////////////////////////////////////////////////////// Check all specialized conversions

  "Specialized conversions" must "compile and not raise cast exceptions" in {
    val binningCounting = Bin(10, 0, 1, {x: Double => x}, Count())
    val binnedCounted = binningCounting.ed.as[Binned[Counted, Counted, Counted, Counted]]
    val selectingBinningCounting = Select({x: Double => x}, Bin(10, 0, 1, {x: Double => x}, Count()))
    val selectedBinnedCounted = selectingBinningCounting.ed.as[Selected[Binned[Counted, Counted, Counted, Counted]]]
    val sparselyBinningCounting = SparselyBin(1, {x: Double => x}, Count())
    val sparselyBinnedCounted = sparselyBinningCounting.ed.as[SparselyBinned[Counted, Counted]]
    val selectingSparselyBinningCounting = Select({x: Double => x}, SparselyBin(1, {x: Double => x}, Count()))
    val selectedSparselyBinnedCounted = selectingSparselyBinningCounting.ed.as[Selected[SparselyBinned[Counted, Counted]]]
    def takesHistogramMethods(x: HistogramMethods) { }
    takesHistogramMethods(binningCounting)
    takesHistogramMethods(binnedCounted)
    takesHistogramMethods(selectingBinningCounting)
    takesHistogramMethods(selectedBinnedCounted)
    takesHistogramMethods(sparselyBinningCounting)
    takesHistogramMethods(sparselyBinnedCounted)
    takesHistogramMethods(selectingSparselyBinningCounting)
    takesHistogramMethods(selectedSparselyBinnedCounted)
    
    val binningDeviating = Bin(10, 0, 1, {x: Double => x}, Deviate({x: Double => x}))
    val binnedDeviated = binningDeviating.ed.as[Binned[Deviated, Counted, Counted, Counted]]
    val selectingBinningDeviating = Select({x: Double => x}, Bin(10, 0, 1, {x: Double => x}, Deviate({x: Double => x})))
    val selectedBinnedDeviated = selectingBinningDeviating.ed.as[Selected[Binned[Deviated, Counted, Counted, Counted]]]
    val sparselyBinningDeviating = SparselyBin(1, {x: Double => x}, Deviate({x: Double => x}))
    val sparselyBinnedDeviated = sparselyBinningDeviating.ed.as[SparselyBinned[Deviated, Counted]]
    val selectingSparselyBinningDeviating = Select({x: Double => x}, SparselyBin(1, {x: Double => x}, Deviate({x: Double => x})))
    val selectedSparselyBinnedDeviated = selectingSparselyBinningDeviating.ed.as[Selected[SparselyBinned[Deviated, Counted]]]
    def takesProfileMethods(x: ProfileMethods) { }
    takesProfileMethods(binningDeviating)
    takesProfileMethods(binnedDeviated)
    takesProfileMethods(selectingBinningDeviating)
    takesProfileMethods(selectedBinnedDeviated)
    takesProfileMethods(sparselyBinningDeviating)
    takesProfileMethods(sparselyBinnedDeviated)
    takesProfileMethods(selectingSparselyBinningDeviating)
    takesProfileMethods(selectedSparselyBinnedDeviated)

    val stackingBinningCounting = Stack({x: Double => x}, Bin(10, 0, 1, {x: Double => x}, Count()), 1, 2, 3)
    val stackedBinnedCounted = stackingBinningCounting.ed.as[Stacked[Binned[Counted, Counted, Counted, Counted]]]
    val stackingSelectingBinningCounting = Stack({x: Double => x}, Select({x: Double => x}, Bin(10, 0, 1, {x: Double => x}, Count())), 1, 2, 3)
    val stackedSelectedBinnedCounted = stackingSelectingBinningCounting.ed.as[Stacked[Selected[Binned[Counted, Counted, Counted, Counted]]]]
    val stackingSparselyBinningCounting = Stack({x: Double => x}, SparselyBin(1, {x: Double => x}, Count()), 1, 2, 3)
    val stackedSparselyBinnedCounted = stackingSparselyBinningCounting.ed.as[Stacked[SparselyBinned[Counted, Counted]]]
    val stackingSelectingSparselyBinningCounting = Stack({x: Double => x}, Select({x: Double => x}, SparselyBin(1, {x: Double => x}, Count())), 1, 2, 3)
    val stackedSelectedSparselyBinnedCounted = stackingSelectingSparselyBinningCounting.ed.as[Stacked[Selected[SparselyBinned[Counted, Counted]]]]
    def takesStackedHistogramMethods(x: StackedHistogramMethods) { }
    takesStackedHistogramMethods(stackingBinningCounting)
    takesStackedHistogramMethods(stackedBinnedCounted)
    takesStackedHistogramMethods(stackingSelectingBinningCounting)
    takesStackedHistogramMethods(stackedSelectedBinnedCounted)
    takesStackedHistogramMethods(stackingSparselyBinningCounting)
    takesStackedHistogramMethods(stackedSparselyBinnedCounted)
    takesStackedHistogramMethods(stackingSelectingSparselyBinningCounting)
    takesStackedHistogramMethods(stackedSelectedSparselyBinnedCounted)

    val stackingBinningCounting2 = Stack.build(binningCounting, binningCounting, binningCounting)
    val stackedBinnedCounted2 = Stack.build(binnedCounted, binnedCounted, binnedCounted)
    val stackingSelectingBinningCounting2 = Stack.build(selectingBinningCounting, selectingBinningCounting, selectingBinningCounting)
    val stackedSelectedBinnedCounted2 = Stack.build(selectedBinnedCounted, selectedBinnedCounted, selectedBinnedCounted)
    val stackingSparselyBinningCounting2 = Stack.build(sparselyBinningCounting, sparselyBinningCounting, sparselyBinningCounting)
    val stackedSparselyBinnedCounted2 = Stack.build(sparselyBinnedCounted, sparselyBinnedCounted, sparselyBinnedCounted)
    val stackingSelectingSparselyBinningCounting2 = Stack.build(selectingSparselyBinningCounting, selectingSparselyBinningCounting, selectingSparselyBinningCounting)
    val stackedSelectedSparselyBinnedCounted2 = Stack.build(selectedSparselyBinnedCounted, selectedSparselyBinnedCounted, selectedSparselyBinnedCounted)
    takesStackedHistogramMethods(stackingBinningCounting2)
    takesStackedHistogramMethods(stackedBinnedCounted2)
    takesStackedHistogramMethods(stackingSelectingBinningCounting2)
    takesStackedHistogramMethods(stackedSelectedBinnedCounted2)
    takesStackedHistogramMethods(stackingSparselyBinningCounting2)
    takesStackedHistogramMethods(stackedSparselyBinnedCounted2)
    takesStackedHistogramMethods(stackingSelectingSparselyBinningCounting2)
    takesStackedHistogramMethods(stackedSelectedSparselyBinnedCounted2)

    val partitioningBinningCounting = Partition({x: Double => x}, Bin(10, 0, 1, {x: Double => x}, Count()), 1, 2, 3)
    val partitionedBinnedCounted = partitioningBinningCounting.ed.as[Partitioned[Binned[Counted, Counted, Counted, Counted]]]
    val partitioningSelectingBinningCounting = Partition({x: Double => x}, Select({x: Double => x}, Bin(10, 0, 1, {x: Double => x}, Count())), 1, 2, 3)
    val partitionedSelectedBinnedCounted = partitioningSelectingBinningCounting.ed.as[Partitioned[Selected[Binned[Counted, Counted, Counted, Counted]]]]
    val partitioningSparselyBinningCounting = Partition({x: Double => x}, SparselyBin(1, {x: Double => x}, Count()), 1, 2, 3)
    val partitionedSparselyBinnedCounted = partitioningSparselyBinningCounting.ed.as[Partitioned[SparselyBinned[Counted, Counted]]]
    val partitioningSelectingSparselyBinningCounting = Partition({x: Double => x}, Select({x: Double => x}, SparselyBin(1, {x: Double => x}, Count())), 1, 2, 3)
    val partitionedSelectedSparselyBinnedCounted = partitioningSelectingSparselyBinningCounting.ed.as[Partitioned[Selected[SparselyBinned[Counted, Counted]]]]
    def takesPartitionedHistogramMethods(x: PartitionedHistogramMethods) { }
    takesPartitionedHistogramMethods(partitioningBinningCounting)
    takesPartitionedHistogramMethods(partitionedBinnedCounted)
    takesPartitionedHistogramMethods(partitioningSelectingBinningCounting)
    takesPartitionedHistogramMethods(partitionedSelectedBinnedCounted)
    takesPartitionedHistogramMethods(partitioningSparselyBinningCounting)
    takesPartitionedHistogramMethods(partitionedSparselyBinnedCounted)
    takesPartitionedHistogramMethods(partitioningSelectingSparselyBinningCounting)
    takesPartitionedHistogramMethods(partitionedSelectedSparselyBinnedCounted)

    val fractioningBinningCounting = Fraction({x: Double => x}, Bin(10, 0, 1, {x: Double => x}, Count()))
    val fractionedBinnedCounted = fractioningBinningCounting.ed.as[Fractioned[Binned[Counted, Counted, Counted, Counted]]]
    val fractioningSelectingBinningCounting = Fraction({x: Double => x}, Select({x: Double => x}, Bin(10, 0, 1, {x: Double => x}, Count())))
    val fractionedSelectedBinnedCounted = fractioningSelectingBinningCounting.ed.as[Fractioned[Selected[Binned[Counted, Counted, Counted, Counted]]]]
    val fractioningSparselyBinningCounting = Fraction({x: Double => x}, SparselyBin(1, {x: Double => x}, Count()))
    val fractionedSparselyBinnedCounted = fractioningSparselyBinningCounting.ed.as[Fractioned[SparselyBinned[Counted, Counted]]]
    val fractioningSelectingSparselyBinningCounting = Fraction({x: Double => x}, Select({x: Double => x}, SparselyBin(1, {x: Double => x}, Count())))
    val fractionedSelectedSparselyBinnedCounted = fractioningSelectingSparselyBinningCounting.ed.as[Fractioned[Selected[SparselyBinned[Counted, Counted]]]]
    def takesFractionedHistogramMethods(x: FractionedHistogramMethods) { }
    takesFractionedHistogramMethods(fractioningBinningCounting)
    takesFractionedHistogramMethods(fractionedBinnedCounted)
    takesFractionedHistogramMethods(fractioningSelectingBinningCounting)
    takesFractionedHistogramMethods(fractionedSelectedBinnedCounted)
    takesFractionedHistogramMethods(fractioningSparselyBinningCounting)
    takesFractionedHistogramMethods(fractionedSparselyBinnedCounted)
    takesFractionedHistogramMethods(fractioningSelectingSparselyBinningCounting)
    takesFractionedHistogramMethods(fractionedSelectedSparselyBinnedCounted)

  }

}
