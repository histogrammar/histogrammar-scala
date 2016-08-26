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

package test.scala.histogrammar

import scala.language.postfixOps

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers

import org.dianahep.histogrammar._

class OriginalSuite extends FlatSpec with Matchers {
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
      java.lang.Double.NaN
    else
      x.sum / x.size

  def mean(x: List[Double], w: List[Double]) =
    if (w.filter(_ > 0.0).isEmpty)
      java.lang.Double.NaN
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

      if (left.isEmpty)
        leftResult.isNaN should be (true)
      else
      leftResult should be (mean(left) +- 1e-12)
      if (right.isEmpty)
        rightResult.isNaN should be (true)
      else
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

      if (left.filter(_.bool).isEmpty)
        leftResult.isNaN should be (true)
      else
        leftResult should be (mean(left.filter(_.bool).map(_.double)) +- 1e-12)
      if (right.filter(_.bool).isEmpty)
        rightResult.isNaN should be (true)
      else
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

      if (left.map(_.int).filter(_ > 0.0).sum == 0.0)
        leftResult.isNaN should be (true)
      else
        leftResult should be (mean(left.map(_.double), left.map(_.int.toDouble)) +- 1e-12)
      if (right.map(_.int).filter(_ > 0.0).sum == 0.0)
        rightResult.isNaN should be (true)
      else
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

      if (left.map(_.int).filter(_ > 0.0).sum == 0.0)
        leftResult.isNaN should be (true)
      else
        leftResult should be (mean(left.map(_.double), left.map(_.int.toDouble)) +- 1e-12)
      if (right.map(_.int).filter(_ > 0.0).sum == 0.0)
        rightResult.isNaN should be (true)
      else
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

      if (left.isEmpty)
        leftResult.isNaN should be (true)
      else
        leftResult should be (variance(left) +- 1e-12)
      if (right.isEmpty)
        rightResult.isNaN should be (true)
      else
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

      if (left.filter(_.bool).isEmpty)
        leftResult.isNaN should be (true)
      else
        leftResult should be (variance(left.filter(_.bool).map(_.double)) +- 1e-12)
      if (right.filter(_.bool).isEmpty)
        rightResult.isNaN should be (true)
      else
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

      if (left.map(_.int).filter(_ > 0.0).sum == 0.0)
        leftResult.isNaN should be (true)
      else
        leftResult should be (variance(left.map(_.double), left.map(_.int.toDouble)) +- 1e-12)
      if (right.map(_.int).filter(_ > 0.0).sum == 0.0)
        rightResult.isNaN should be (true)
      else
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

      if (left.map(_.int).filter(_ > 0.0).sum == 0.0)
        leftResult.isNaN should be (true)
      else
        leftResult should be (variance(left.map(_.double), left.map(_.int.toDouble)) +- 1e-12)
      if (right.map(_.int).filter(_ > 0.0).sum == 0.0)
        rightResult.isNaN should be (true)
      else
        rightResult should be (variance(right.map(_.double), right.map(_.int.toDouble)) +- 1e-12)

      val Select(Deviate(finalResult)) = leftDeviating + rightDeviating

      finalResult should be (variance(backward.map(_.double), backward.map(_.int.toDouble)) +- 1e-12)

      checkJson(leftDeviating)
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

  //////////////////////////////////////////////////////////////// Bag/Bagged/Bagging

  "Bag/Bagged/Bagging" must "work" in {
    val one = Bag({x: Double => x} named "something")
    simple.foreach(one.fill(_))
    one.values should be (Map(7.3 -> 1.0, 2.2 -> 1.0, -1.7 -> 1.0, -4.7 -> 1.0, 0.0 -> 2.0, -1.8 -> 1.0, -3.0 -> 1.0, 1.6 -> 1.0, 3.4 -> 1.0))

    val two = Bag({x: Double => Vector(x, x)}, range="N2")
    simple.foreach(two.fill(_))
    two.values should be (Map(Vector(7.3, 7.3) -> 1.0, Vector(2.2, 2.2) -> 1.0, Vector(-1.7, -1.7) -> 1.0, Vector(-4.7, -4.7) -> 1.0, Vector(0.0, 0.0) -> 2.0, Vector(-1.8, -1.8) -> 1.0, Vector(-3.0, -3.0) -> 1.0, Vector(1.6, 1.6) -> 1.0, Vector(3.4, 3.4) -> 1.0))

    val three = Bag({x: Struct => x.string.substring(0, 1)})
    struct.foreach(three.fill(_))
    three.values should be (Map("n" -> 1.0, "e" -> 1.0, "t" -> 3.0, "s" -> 2.0, "f" -> 2.0, "o" -> 1.0))

    checkJson(one)
    checkJson(two)
    checkJson(three)
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
    two.cut.values.map(_.entries).toList should be (List(2.0, 1.0, 1.0, 1.0, 0.0))
    two.cut.underflow.entries should be (0.0)
    two.cut.overflow.entries should be (0.0)
    two.cut.nanflow.entries should be (0.0)

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
    two.cut.values.map(_.sum).toList should be (List(20.0, 10.0, 10.0, 10.0, 0.0))
    two.cut.underflow.sum should be (0.0)
    two.cut.overflow.sum should be (0.0)
    two.cut.nanflow.sum should be (0.0)

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

    checkJson(one)

    val two = CentrallyBin(List(-3.0, -1.0, 0.0, 1.0, 3.0, 10.0), {x: Double => x} named "something", Sum({x: Double => x} named "else"))
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
    val stacking = Stack(List(0.0, 2.0, 4.0, 6.0, 8.0), {x: Double => x} named "something", Count())
    simple.foreach(stacking.fill(_))

    stacking.bins.map({case (k, v) => (k, v.entries)}).toList should be (List(java.lang.Double.NEGATIVE_INFINITY -> 10.0, 0.0 -> 6.0, 2.0 -> 3.0, 4.0 -> 1.0, 6.0 -> 1.0, 8.0 -> 0.0))

    checkJson(stacking)
  }

  it must "work with Sum/Summing/Summed" in {
    val stacking = Stack(List(0.0, 2.0, 4.0, 6.0, 8.0), {x: Double => x} named "something", Sum({x: Double => x} named "else"))
    simple.foreach(stacking.fill(_))

    stacking.bins(1)._2.sum should be (14.5 +- 1e-12)

    checkJson(stacking)
  }

  //////////////////////////////////////////////////////////////// IrregularlyBin/IrregularlyBinned/IrregularlyBinning

  "IrregularlyBin/IrregularlyBinned/IrregularlyBinning" must "work with Count/Counting/Counted" in {
    val partitioning = IrregularlyBin(List(0.0, 2.0, 4.0, 6.0, 8.0), {x: Double => x} named "something", Count())
    simple.foreach(partitioning.fill(_))
    
    partitioning.bins.map({case (k, v) => (k, v.entries)}).toList should be (List(java.lang.Double.NEGATIVE_INFINITY -> 4.0, 0.0 -> 3.0, 2.0 -> 2.0, 4.0 -> 0.0, 6.0 -> 1.0, 8.0 -> 0.0))

    checkJson(partitioning)
  }

  it must "work with Sum/Summing/Summed" in {
    val partitioning = IrregularlyBin(List(0.0, 2.0, 4.0, 6.0, 8.0), {x: Double => x} named "something", Sum({x: Double => x} named "else"))
    simple.foreach(partitioning.fill(_))

    partitioning.bins(0)._2.sum should be (-11.2 +- 1e-12)
    partitioning.bins(1)._2.sum should be (1.6 +- 1e-12)

    checkJson(partitioning)
  }

  //////////////////////////////////////////////////////////////// Categorize/Categorized/Categorizing

  "Categorize/Categorized/Categorizing" must "work" in {
    val categorizing = Categorize({x: Struct => x.string.substring(0, 1)} named "something")
    struct.foreach(categorizing.fill(_))
    categorizing.binsMap map {case (k, v) => (k, v.entries)} should be (Map("n" -> 1.0, "e" -> 1.0, "t" -> 3.0, "s" -> 2.0, "f" -> 2.0, "o" -> 1.0))
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
    def takesProfileMethods(x: ProfileErrMethods) { }
    takesProfileMethods(binningDeviating)
    takesProfileMethods(binnedDeviated)
    takesProfileMethods(selectingBinningDeviating)
    takesProfileMethods(selectedBinnedDeviated)
    takesProfileMethods(sparselyBinningDeviating)
    takesProfileMethods(sparselyBinnedDeviated)
    takesProfileMethods(selectingSparselyBinningDeviating)
    takesProfileMethods(selectedSparselyBinnedDeviated)

    val stackingBinningCounting = Stack(List(1.0, 2.0, 3.0), {x: Double => x}, Bin(10, 0, 1, {x: Double => x}, Count()))
    val stackedBinnedCounted = stackingBinningCounting.ed.as[Stacked[Binned[Counted, Counted, Counted, Counted], Counted]]
    val stackingSelectingBinningCounting = Stack(List(1.0, 2.0, 3.0), {x: Double => x}, Select({x: Double => x}, Bin(10, 0, 1, {x: Double => x}, Count())))
    val stackedSelectedBinnedCounted = stackingSelectingBinningCounting.ed.as[Stacked[Selected[Binned[Counted, Counted, Counted, Counted]], Counted]]
    val stackingSparselyBinningCounting = Stack(List(1.0, 2.0, 3.0), {x: Double => x}, SparselyBin(1, {x: Double => x}, Count()))
    val stackedSparselyBinnedCounted = stackingSparselyBinningCounting.ed.as[Stacked[SparselyBinned[Counted, Counted], Counted]]
    val stackingSelectingSparselyBinningCounting = Stack(List(1.0, 2.0, 3.0), {x: Double => x}, Select({x: Double => x}, SparselyBin(1, {x: Double => x}, Count())))
    val stackedSelectedSparselyBinnedCounted = stackingSelectingSparselyBinningCounting.ed.as[Stacked[Selected[SparselyBinned[Counted, Counted]], Counted]]
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

    val partitioningBinningCounting = IrregularlyBin(List(1.0, 2.0, 3.0), {x: Double => x}, Bin(10, 0, 1, {x: Double => x}, Count()))
    val partitionedBinnedCounted = partitioningBinningCounting.ed.as[IrregularlyBinned[Binned[Counted, Counted, Counted, Counted], Counted]]
    val partitioningSelectingBinningCounting = IrregularlyBin(List(1.0, 2.0, 3.0), {x: Double => x}, Select({x: Double => x}, Bin(10, 0, 1, {x: Double => x}, Count())))
    val partitionedSelectedBinnedCounted = partitioningSelectingBinningCounting.ed.as[IrregularlyBinned[Selected[Binned[Counted, Counted, Counted, Counted]], Counted]]
    val partitioningSparselyBinningCounting = IrregularlyBin(List(1.0, 2.0, 3.0), {x: Double => x}, SparselyBin(1, {x: Double => x}, Count()))
    val partitionedSparselyBinnedCounted = partitioningSparselyBinningCounting.ed.as[IrregularlyBinned[SparselyBinned[Counted, Counted], Counted]]
    val partitioningSelectingSparselyBinningCounting = IrregularlyBin(List(1.0, 2.0, 3.0), {x: Double => x}, Select({x: Double => x}, SparselyBin(1, {x: Double => x}, Count())))
    val partitionedSelectedSparselyBinnedCounted = partitioningSelectingSparselyBinningCounting.ed.as[IrregularlyBinned[Selected[SparselyBinned[Counted, Counted]], Counted]]
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
