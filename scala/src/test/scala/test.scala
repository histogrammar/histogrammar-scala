package test.scala.histogrammar

import scala.language.postfixOps

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers

import org.dianahep.histogrammar._
import org.dianahep.histogrammar.specialized.histogram._

class DefaultSuite extends FlatSpec with Matchers {
  // val simple = List(3.4, 2.2, -1.8, 0.0, 7.3, -4.7, 1.6, 0.0, -3.0, -1.7)

  // case class Struct(bool: Boolean, int: Int, double: Double, string: String)

  // val struct = List(
  //   Struct(true,  -2,  3.4, "one"),
  //   Struct(false, -1,  2.2, "two"),
  //   Struct(true,   0, -1.8, "three"),
  //   Struct(false,  1,  0.0, "four"),
  //   Struct(false,  2,  7.3, "five"),
  //   Struct(false,  3, -4.7, "six"),
  //   Struct(true,   4,  1.6, "seven"),
  //   Struct(true,   5,  0.0, "eight"),
  //   Struct(false,  6, -3.0, "nine"),
  //   Struct(true,   7, -1.7, "ten"))

  // val backward = struct.reverse

  // // straightforward mean and variance to complement the Tony Finch calculations used in the module

  // def mean(x: List[Double]) =
  //   if (x.isEmpty)
  //     0.0
  //   else
  //     x.sum / x.size

  // def mean(x: List[Double], w: List[Double]) =
  //   if (w.filter(_ > 0.0).isEmpty)
  //     0.0
  //   else
  //     (x zip w map {case (xi, wi) => xi * Math.max(wi, 0.0)} sum) / w.filter(_ > 0.0).sum

  // def variance(x: List[Double]) =
  //   if (x.isEmpty)
  //     0.0
  //   else
  //     x.map(Math.pow(_, 2)).sum / x.size - Math.pow(x.sum / x.size, 2)

  // def variance(x: List[Double], w: List[Double]) =
  //   if (w.filter(_ > 0.0).isEmpty)
  //     0.0
  //   else
  //     (x zip w map {case (xi, wi) => xi * xi * Math.max(wi, 0.0)} sum) / w.filter(_ > 0.0).sum - Math.pow((x zip w map {case (xi, wi) => xi * Math.max(wi, 0.0)} sum) / w.filter(_ > 0.0).sum, 2)

  // def mae(x: List[Double]) =
  //   if (x.isEmpty)
  //     0.0
  //   else
  //     x.map(Math.abs).sum / x.size

  // def mae(x: List[Double], w: List[Double]) =
  //   if (w.filter(_ > 0.0).isEmpty)
  //     0.0
  //   else
  //     (x zip w map {case (xi, wi) => Math.abs(xi) * Math.max(wi, 0.0)} sum) / w.filter(_ > 0.0).sum

  // //////////////////////////////////////////////////////////////// Count/Counted/Counting

  // "Count/Counting/Counted" must "work unfiltered" in {
  //   for (i <- 0 to 10) {
  //     val (left, right) = simple.splitAt(i)

  //     val leftCounting = Count[Double]()
  //     val rightCounting = Count[Double]()

  //     left.foreach(leftCounting.fill(_))
  //     right.foreach(rightCounting.fill(_))

  //     val (Count(leftResult), Count(rightResult)) = (leftCounting, rightCounting)

  //     leftResult should be (left.size)
  //     rightResult should be (right.size)

  //     val Count(finalResult) = leftCounting + rightCounting

  //     finalResult should be (simple.size)

  //     Factory.fromJson[Counted](leftCounting.toJson.stringify) should be (leftCounting.toContainer)
  //   }
  // }

  // it must "work with a filter" in {
  //   for (i <- 0 to 10) {
  //     val (left, right) = struct.splitAt(i)

  //     val leftCounting = Count({x: Struct => x.bool})
  //     val rightCounting = Count({x: Struct => x.bool})

  //     left.foreach(leftCounting.fill(_))
  //     right.foreach(rightCounting.fill(_))

  //     val (Count(leftResult), Count(rightResult)) = (leftCounting, rightCounting)

  //     leftResult should be (left.filter(_.bool).size)
  //     rightResult should be (right.filter(_.bool).size)

  //     val Count(finalResult) = leftCounting + rightCounting

  //     finalResult should be (struct.filter(_.bool).size)

  //     Factory.fromJson[Counted](leftCounting.toJson.stringify) should be (leftCounting.toContainer)
  //   }
  // }

  // it must "work with a weighting factor" in {
  //   for (i <- 0 to 10) {
  //     val (left, right) = struct.splitAt(i)

  //     val leftCounting = Count({x: Struct => x.int})
  //     val rightCounting = Count({x: Struct => x.int})

  //     left.foreach(leftCounting.fill(_))
  //     right.foreach(rightCounting.fill(_))

  //     val (Count(leftResult), Count(rightResult)) = (leftCounting, rightCounting)

  //     leftResult should be (left.filter(_.int >= 0).map(_.int).sum)
  //     rightResult should be (right.filter(_.int >= 0).map(_.int).sum)

  //     val Count(finalResult) = leftCounting + rightCounting

  //     finalResult should be (struct.filter(_.int >= 0).map(_.int).sum)

  //     Factory.fromJson[Counted](leftCounting.toJson.stringify) should be (leftCounting.toContainer)
  //   }
  // }

  // //////////////////////////////////////////////////////////////// Sum/Summed/Summing

  // "Sum/Summing/Summed" must "work unfiltered" in {
  //   for (i <- 0 to 10) {
  //     val (left, right) = simple.splitAt(i)

  //     val leftSumming = Sum({x: Double => x})
  //     val rightSumming = Sum({x: Double => x})

  //     left.foreach(leftSumming.fill(_))
  //     right.foreach(rightSumming.fill(_))

  //     val (Sum(leftResult), Sum(rightResult)) = (leftSumming, rightSumming)

  //     leftResult should be (left.sum +- 1e-12)
  //     rightResult should be (right.sum +- 1e-12)

  //     val Sum(finalResult) = leftSumming + rightSumming

  //     finalResult should be (simple.sum +- 1e-12)

  //     Factory.fromJson[Summed](leftSumming.toJson.stringify) should be (leftSumming.toContainer)
  //   }
  // }

  // it must "work with a filter" in {
  //   for (i <- 0 to 10) {
  //     val (left, right) = struct.splitAt(i)

  //     val leftSumming = Sum({x: Struct => x.double}, {x: Struct => x.bool})
  //     val rightSumming = Sum({x: Struct => x.double}, {x: Struct => x.bool})

  //     left.foreach(leftSumming.fill(_))
  //     right.foreach(rightSumming.fill(_))

  //     val (Sum(leftResult), Sum(rightResult)) = (leftSumming, rightSumming)

  //     leftResult should be (left.filter(_.bool).map(_.double).sum +- 1e-12)
  //     rightResult should be (right.filter(_.bool).map(_.double).sum +- 1e-12)

  //     val Sum(finalResult) = leftSumming + rightSumming

  //     finalResult should be (struct.filter(_.bool).map(_.double).sum +- 1e-12)

  //     Factory.fromJson[Summed](leftSumming.toJson.stringify) should be (leftSumming.toContainer)
  //   }
  // }

  // it must "work with a weighting factor" in {
  //   for (i <- 0 to 10) {
  //     val (left, right) = struct.splitAt(i)

  //     val leftSumming = Sum({x: Struct => x.double}, {x: Struct => x.int})
  //     val rightSumming = Sum({x: Struct => x.double}, {x: Struct => x.int})

  //     left.foreach(leftSumming.fill(_))
  //     right.foreach(rightSumming.fill(_))

  //     val (Sum(leftResult), Sum(rightResult)) = (leftSumming, rightSumming)

  //     leftResult should be (left.filter(_.int >= 0).map({x => x.int * x.double}).sum +- 1e-12)
  //     rightResult should be (right.filter(_.int >= 0).map({x => x.int * x.double}).sum +- 1e-12)

  //     val Sum(finalResult) = leftSumming + rightSumming

  //     finalResult should be (struct.filter(_.int >= 0).map({x => x.int * x.double}).sum +- 1e-12)

  //     Factory.fromJson[Summed](leftSumming.toJson.stringify) should be (leftSumming.toContainer)
  //   }
  // }

  // //////////////////////////////////////////////////////////////// Average/Averaged/Averaging

  // "Average/Averaging/Averaged" must "work unfiltered" in {
  //   for (i <- 0 to 10) {
  //     val (left, right) = simple.splitAt(i)

  //     val leftAveraging = Average({x: Double => x})
  //     val rightAveraging = Average({x: Double => x})

  //     left.foreach(leftAveraging.fill(_))
  //     right.foreach(rightAveraging.fill(_))

  //     val (Average(_, leftResult), Average(_, rightResult)) = (leftAveraging, rightAveraging)

  //     leftResult should be (mean(left) +- 1e-12)
  //     rightResult should be (mean(right) +- 1e-12)

  //     val Average(_, finalResult) = leftAveraging + rightAveraging

  //     finalResult should be (mean(simple) +- 1e-12)

  //     Factory.fromJson[Averaged](leftAveraging.toJson.stringify) should be (leftAveraging.toContainer)
  //   }
  // }

  // it must "work with a filter" in {
  //   for (i <- 0 to 10) {
  //     val (left, right) = struct.splitAt(i)

  //     val leftAveraging = Average({x: Struct => x.double}, {x: Struct => x.bool})
  //     val rightAveraging = Average({x: Struct => x.double}, {x: Struct => x.bool})

  //     left.foreach(leftAveraging.fill(_))
  //     right.foreach(rightAveraging.fill(_))

  //     val (Average(_, leftResult), Average(_, rightResult)) = (leftAveraging, rightAveraging)

  //     leftResult should be (mean(left.filter(_.bool).map(_.double)) +- 1e-12)
  //     rightResult should be (mean(right.filter(_.bool).map(_.double)) +- 1e-12)

  //     val Average(_, finalResult) = leftAveraging + rightAveraging

  //     finalResult should be (mean(struct.filter(_.bool).map(_.double)) +- 1e-12)

  //     Factory.fromJson[Averaged](leftAveraging.toJson.stringify) should be (leftAveraging.toContainer)
  //   }
  // }

  // it must "work with a weighting factor" in {
  //   for (i <- 0 to 10) {
  //     val (left, right) = struct.splitAt(i)

  //     val leftAveraging = Average({x: Struct => x.double}, {x: Struct => x.int})
  //     val rightAveraging = Average({x: Struct => x.double}, {x: Struct => x.int})

  //     left.foreach(leftAveraging.fill(_))
  //     right.foreach(rightAveraging.fill(_))

  //     val (Average(_, leftResult), Average(_, rightResult)) = (leftAveraging, rightAveraging)

  //     leftResult should be (mean(left.map(_.double), left.map(_.int.toDouble)) +- 1e-12)
  //     rightResult should be (mean(right.map(_.double), right.map(_.int.toDouble)) +- 1e-12)

  //     val Average(_, finalResult) = leftAveraging + rightAveraging

  //     finalResult should be (mean(struct.map(_.double), struct.map(_.int.toDouble)) +- 1e-12)

  //     Factory.fromJson[Averaged](leftAveraging.toJson.stringify) should be (leftAveraging.toContainer)
  //   }
  // }

  // it must "work in reverse" in {
  //   for (i <- 0 to 10) {
  //     val (left, right) = backward.splitAt(i)

  //     val leftAveraging = Average({x: Struct => x.double}, {x: Struct => x.int})
  //     val rightAveraging = Average({x: Struct => x.double}, {x: Struct => x.int})

  //     left.foreach(leftAveraging.fill(_))
  //     right.foreach(rightAveraging.fill(_))

  //     val (Average(_, leftResult), Average(_, rightResult)) = (leftAveraging, rightAveraging)

  //     leftResult should be (mean(left.map(_.double), left.map(_.int.toDouble)) +- 1e-12)
  //     rightResult should be (mean(right.map(_.double), right.map(_.int.toDouble)) +- 1e-12)

  //     val Average(_, finalResult) = leftAveraging + rightAveraging

  //     finalResult should be (mean(backward.map(_.double), backward.map(_.int.toDouble)) +- 1e-12)

  //     Factory.fromJson[Averaged](leftAveraging.toJson.stringify) should be (leftAveraging.toContainer)
  //   }
  // }

  // //////////////////////////////////////////////////////////////// Deviate/Deviated/Deviating

  // "Deviate/Deviating/Deviated" must "work unfiltered" in {
  //   for (i <- 0 to 10) {
  //     val (left, right) = simple.splitAt(i)

  //     val leftDeviating = Deviate({x: Double => x})
  //     val rightDeviating = Deviate({x: Double => x})

  //     left.foreach(leftDeviating.fill(_))
  //     right.foreach(rightDeviating.fill(_))

  //     val (Deviate(_, _, leftResult), Deviate(_, _, rightResult)) = (leftDeviating, rightDeviating)

  //     leftResult should be (variance(left) +- 1e-12)
  //     rightResult should be (variance(right) +- 1e-12)

  //     val Deviate(_, _, finalResult) = leftDeviating + rightDeviating

  //     finalResult should be (variance(simple) +- 1e-12)

  //     Factory.fromJson[Deviated](leftDeviating.toJson.stringify) should be (leftDeviating.toContainer)
  //   }
  // }

  // it must "work with a filter" in {
  //   for (i <- 0 to 10) {
  //     val (left, right) = struct.splitAt(i)

  //     val leftDeviating = Deviate({x: Struct => x.double}, {x: Struct => x.bool})
  //     val rightDeviating = Deviate({x: Struct => x.double}, {x: Struct => x.bool})

  //     left.foreach(leftDeviating.fill(_))
  //     right.foreach(rightDeviating.fill(_))

  //     val (Deviate(_, _, leftResult), Deviate(_, _, rightResult)) = (leftDeviating, rightDeviating)

  //     leftResult should be (variance(left.filter(_.bool).map(_.double)) +- 1e-12)
  //     rightResult should be (variance(right.filter(_.bool).map(_.double)) +- 1e-12)

  //     val Deviate(_, _, finalResult) = leftDeviating + rightDeviating

  //     finalResult should be (variance(struct.filter(_.bool).map(_.double)) +- 1e-12)

  //     Factory.fromJson[Deviated](leftDeviating.toJson.stringify) should be (leftDeviating.toContainer)
  //   }
  // }

  // it must "work with a weighting factor" in {
  //   for (i <- 0 to 10) {
  //     val (left, right) = struct.splitAt(i)

  //     val leftDeviating = Deviate({x: Struct => x.double}, {x: Struct => x.int})
  //     val rightDeviating = Deviate({x: Struct => x.double}, {x: Struct => x.int})

  //     left.foreach(leftDeviating.fill(_))
  //     right.foreach(rightDeviating.fill(_))

  //     val (Deviate(_, _, leftResult), Deviate(_, _, rightResult)) = (leftDeviating, rightDeviating)

  //     leftResult should be (variance(left.map(_.double), left.map(_.int.toDouble)) +- 1e-12)
  //     rightResult should be (variance(right.map(_.double), right.map(_.int.toDouble)) +- 1e-12)

  //     val Deviate(_, _, finalResult) = leftDeviating + rightDeviating

  //     finalResult should be (variance(struct.map(_.double), struct.map(_.int.toDouble)) +- 1e-12)

  //     Factory.fromJson[Deviated](leftDeviating.toJson.stringify) should be (leftDeviating.toContainer)
  //   }
  // }

  // it must "work in reverse" in {
  //   for (i <- 0 to 10) {
  //     val (left, right) = backward.splitAt(i)

  //     val leftDeviating = Deviate({x: Struct => x.double}, {x: Struct => x.int})
  //     val rightDeviating = Deviate({x: Struct => x.double}, {x: Struct => x.int})

  //     left.foreach(leftDeviating.fill(_))
  //     right.foreach(rightDeviating.fill(_))

  //     val (Deviate(_, _, leftResult), Deviate(_, _, rightResult)) = (leftDeviating, rightDeviating)

  //     leftResult should be (variance(left.map(_.double), left.map(_.int.toDouble)) +- 1e-12)
  //     rightResult should be (variance(right.map(_.double), right.map(_.int.toDouble)) +- 1e-12)

  //     val Deviate(_, _, finalResult) = leftDeviating + rightDeviating

  //     finalResult should be (variance(backward.map(_.double), backward.map(_.int.toDouble)) +- 1e-12)

  //     Factory.fromJson[Deviated](leftDeviating.toJson.stringify) should be (leftDeviating.toContainer)
  //   }
  // }

  // //////////////////////////////////////////////////////////////// AbsoluteErr/AbsoluteErring/AbsoluteErred

  // "AbsoluteErr/AbsoluteErring/AbsoluteErred" must "work" in {
  //   for (i <- 0 to 10) {
  //     val (left, right) = simple.splitAt(i)

  //     val leftAbsoluteErring = AbsoluteErr({x: Double => x})
  //     val rightAbsoluteErring = AbsoluteErr({x: Double => x})

  //     left.foreach(leftAbsoluteErring.fill(_))
  //     right.foreach(rightAbsoluteErring.fill(_))

  //     val (AbsoluteErr(_, leftResult), AbsoluteErr(_, rightResult)) = (leftAbsoluteErring, rightAbsoluteErring)

  //     leftResult should be (mae(left) +- 1e-12)
  //     rightResult should be (mae(right) +- 1e-12)

  //     val AbsoluteErr(_, finalResult) = leftAbsoluteErring + rightAbsoluteErring

  //     finalResult should be (mae(simple) +- 1e-12)

  //     Factory.fromJson[AbsoluteErred](leftAbsoluteErring.toJson.stringify) should be (leftAbsoluteErring.toContainer)
  //   }
  // }

  // //////////////////////////////////////////////////////////////// Minimize/Minimizing/Minimized

  // "Minimize/Minimizing/Minimized" must "work" in {
  //   for (i <- 0 to 10) {
  //     val (left, right) = simple.splitAt(i)

  //     val leftMinimizing = Minimize({x: Double => x})
  //     val rightMinimizing = Minimize({x: Double => x})

  //     left.foreach(leftMinimizing.fill(_))
  //     right.foreach(rightMinimizing.fill(_))

  //     val (Minimize(leftResult), Minimize(rightResult)) = (leftMinimizing, rightMinimizing)

  //     if (left.isEmpty) leftResult.isNaN should be (true)
  //     else leftResult should be (left.min +- 1e-12)
  //     if (right.isEmpty) rightResult.isNaN should be (true)
  //     else rightResult should be (right.min +- 1e-12)

  //     val Minimize(finalResult) = leftMinimizing + rightMinimizing

  //     if (simple.isEmpty) finalResult.isNaN should be (true)
  //     else finalResult should be (simple.min +- 1e-12)

  //     Factory.fromJson[Minimized](leftMinimizing.toJson.stringify) should be (leftMinimizing.toContainer)
  //   }
  // }

  // //////////////////////////////////////////////////////////////// Maximize/Maximizing/Maximized

  // "Maximize/Maximizing/Maximized" must "work" in {
  //   for (i <- 0 to 10) {
  //     val (left, right) = simple.splitAt(i)

  //     val leftMaximizing = Maximize({x: Double => x})
  //     val rightMaximizing = Maximize({x: Double => x})

  //     left.foreach(leftMaximizing.fill(_))
  //     right.foreach(rightMaximizing.fill(_))

  //     val (Maximize(leftResult), Maximize(rightResult)) = (leftMaximizing, rightMaximizing)

  //     if (left.isEmpty) leftResult.isNaN should be (true)
  //     else leftResult should be (left.max +- 1e-12)
  //     if (right.isEmpty) rightResult.isNaN should be (true)
  //     else rightResult should be (right.max +- 1e-12)

  //     val Maximize(finalResult) = leftMaximizing + rightMaximizing

  //     if (simple.isEmpty) finalResult.isNaN should be (true)
  //     else finalResult should be (simple.max +- 1e-12)

  //     Factory.fromJson[Maximized](leftMaximizing.toJson.stringify) should be (leftMaximizing.toContainer)
  //   }
  // }

  // //////////////////////////////////////////////////////////////// Bin/Binned/Binning

  // "Bin/Binning/Binned" must "work with Count/Counting/Counted" in {
  //   val one = Bin(5, -3.0, 7.0, {x: Double => x})
  //   simple.foreach(one.fill(_))
  //   one.toContainer.values.toList should be (List(Count.ed(3.0), Count.ed(2.0), Count.ed(2.0), Count.ed(1.0), Count.ed(0.0)))
  //   one.toContainer.underflow should be (Count.ed(1.0))
  //   one.toContainer.overflow should be (Count.ed(1.0))
  //   one.toContainer.nanflow should be (Count.ed(0.0))

  //   Factory.fromJson[Binned[Counted, Counted, Counted, Counted]](one.toJson.stringify) should be (one.toContainer)

  //   val two = Bin(5, -3.0, 7.0, {x: Struct => x.double}, {x: Struct => x.bool})
  //   struct.foreach(two.fill(_))
  //   two.toContainer.values.toList should be (List(Count.ed(2.0), Count.ed(1.0), Count.ed(1.0), Count.ed(1.0), Count.ed(0.0)))
  //   two.toContainer.underflow should be (Count.ed(0.0))
  //   two.toContainer.overflow should be (Count.ed(0.0))
  //   two.toContainer.nanflow should be (Count.ed(0.0))

  //   Factory.fromJson[Binned[Counted, Counted, Counted, Counted]](two.toJson.stringify) should be (two.toContainer)
  // }

  // "Binning/Binned" must "work with Sum/Summing/Summed" in {
  //   val one = Bin(5, -3.0, 7.0, {x: Double => x}, unweighted[Double], Sum({x: Double => 10.0}), Sum({x: Double => 10.0}), Sum({x: Double => 10.0}), Sum({x: Double => 10.0}))
  //   simple.foreach(one.fill(_))
  //   one.toContainer.values.toList should be (List(Sum.ed(30.0), Sum.ed(20.0), Sum.ed(20.0), Sum.ed(10.0), Sum.ed(0.0)))
  //   one.toContainer.underflow should be (Sum.ed(10.0))
  //   one.toContainer.overflow should be (Sum.ed(10.0))
  //   one.toContainer.nanflow should be (Sum.ed(0.0))

  //   Factory.fromJson[Binned[Summed, Summed, Summed, Summed]](one.toJson.stringify) should be (one.toContainer)

  //   val two = Bin(5, -3.0, 7.0, {x: Struct => x.double}, {x: Struct => x.bool}, Sum({x: Struct => 10.0}), Sum({x: Struct => 10.0}), Sum({x: Struct => 10.0}), Sum({x: Struct => 10.0}))
  //   struct.foreach(two.fill(_))
  //   two.toContainer.values.toList should be (List(Sum.ed(20.0), Sum.ed(10.0), Sum.ed(10.0), Sum.ed(10.0), Sum.ed(0.0)))
  //   two.toContainer.underflow should be (Sum.ed(0.0))
  //   two.toContainer.overflow should be (Sum.ed(0.0))
  //   two.toContainer.nanflow should be (Sum.ed(0.0))

  //   Factory.fromJson[Binned[Summed, Summed, Summed, Summed]](two.toJson.stringify) should be (two.toContainer)
  // }

  // //////////////////////////////////////////////////////////////// SparselyBin/SparselyBinned/SparselyBinning

  // "SparselyBin/SparselyBinned/SparselyBinning" must "work with Count/Counting/Counted" in {
  //   val one = SparselyBin(1.0, {x: Double => x})
  //   simple.foreach(one.fill(_))
  //   one.toContainer.values.toList should be (List(-5 -> Count.ed(1.0), -3 -> Count.ed(1.0), -2 -> Count.ed(2.0), 0 -> Count.ed(2.0), 1 -> Count.ed(1.0), 2 -> Count.ed(1.0), 3 -> Count.ed(1.0), 7 -> Count.ed(1.0)))

  //   one.numFilled should be (8)
  //   one.toContainer.numFilled should be (8)
  //   one.num should be (12)
  //   one.toContainer.num should be (12)
  //   one.low should be (-5.0)
  //   one.toContainer.low should be (-5.0)
  //   one.high should be (8.0)
  //   one.toContainer.high should be (8.0)

  //   Factory.fromJson[SparselyBinned[Counted, Counted]](one.toJson.stringify) should be (one.toContainer)
  // }

  // //////////////////////////////////////////////////////////////// Fraction/Fractioned/Fractioning

  // "Fraction/Fractioned/Fractioning" must "work with Count/Counting/Counted" in {
  //   val fracking = Fraction({x: Double => x > 0.0}, Count[Double]())
  //   simple.foreach(fracking.fill(_))

  //   val fracked = fracking.toContainer

  //   fracked.numerator.value should be (4.0)
  //   fracked.denominator.value should be (10.0)

  //   Factory.fromJson[Fractioned[Counted]](fracking.toJson.stringify) should be (fracked)
  // }

  // it must "work with Sum/Summing/Summed" in {
  //   val fracking = Fraction({x: Double => x > 0.0}, Sum({x: Double => x}))
  //   simple.foreach(fracking.fill(_))

  //   val fracked = fracking.toContainer

  //   fracked.numerator.value should be (14.5 +- 1e-12)
  //   fracked.denominator.value should be (3.3 +- 1e-12)

  //   Factory.fromJson[Fractioned[Counted]](fracking.toJson.stringify) should be (fracked)
  // }

  // it must "work with Histogram/Histogramming/Histogrammed" in {
  //   val fracking = Fraction({x: Double => x > 0.0}, Histogram(5, -3.0, 7.0, {x: Double => x}))
  //   simple.foreach(fracking.fill(_))

  //   val fracked = fracking.toContainer

  //   fracked.numerator.values.toList should be (List(Count.ed(0.0), Count.ed(0.0), Count.ed(2.0), Count.ed(1.0), Count.ed(0.0)))
  //   fracked.denominator.values.toList should be (List(Count.ed(3.0), Count.ed(2.0), Count.ed(2.0), Count.ed(1.0), Count.ed(0.0)))

  //   fracked match {
  //     case Fraction(
  //       Bin(Seq(Count(0.0), Count(0.0), Count(2.0), Count(1.0), Count(0.0)), _, _, _),
  //       Bin(Seq(Count(3.0), Count(2.0), Count(2.0), Count(1.0), Count(0.0)), _, _, _)) => 1 should be (1)
  //     case _ => 0 should be (1)
  //   }

  //   Factory.fromJson[Fractioned[Counted]](fracking.toJson.stringify) should be (fracked)
  // }

  // //////////////////////////////////////////////////////////////// Stack/Stacked/Stacking

  // "Stack/Stacked/Stacking" must "work with Count/Counting/Counted" in {
  //   val stacking = Stack(Count[Double](), {x: Double => x}, 0.0, 2.0, 4.0, 6.0, 8.0)
  //   simple.foreach(stacking.fill(_))

  //   stacking.toContainer.cuts.toList should be (List(java.lang.Double.NEGATIVE_INFINITY -> Count.ed(10.0), 0.0 -> Count.ed(6.0), 2.0 -> Count.ed(3.0), 4.0 -> Count.ed(1.0), 6.0 -> Count.ed(1.0), 8.0 -> Count.ed(0.0)))

  //   Factory.fromJson[Stacked[Counted]](stacking.toJson.stringify) should be (stacking.toContainer)
  // }

  // it must "work with Sum/Summing/Summed" in {
  //   val stacking = Stack(Sum({x: Double => x}), {x: Double => x}, 0.0, 2.0, 4.0, 6.0, 8.0)
  //   simple.foreach(stacking.fill(_))

  //   stacking.toContainer.cuts(0)._2.value should be (3.3 +- 1e-12)
  //   stacking.toContainer.cuts(1)._2.value should be (14.5 +- 1e-12)

  //   Factory.fromJson[Stacked[Summed]](stacking.toJson.stringify) should be (stacking.toContainer)
  // }

  // //////////////////////////////////////////////////////////////// Partition/Partitioned/Partitioning

  // "Partition/Partitioned/Partitioning" must "work with Count/Counting/Counted" in {
  //   val partitioning = Partition(Count[Double](), {x: Double => x}, 0.0, 2.0, 4.0, 6.0, 8.0)
  //   simple.foreach(partitioning.fill(_))

  //   partitioning.toContainer.cuts.toList should be (List(java.lang.Double.NEGATIVE_INFINITY -> Count.ed(4.0), 0.0 -> Count.ed(3.0), 2.0 -> Count.ed(2.0), 4.0 -> Count.ed(0.0), 6.0 -> Count.ed(1.0), 8.0 -> Count.ed(0.0)))

  //   Factory.fromJson[Partitioned[Counted]](partitioning.toJson.stringify) should be (partitioning.toContainer)
  // }

  // it must "work with Sum/Summing/Summed" in {
  //   val partitioning = Partition(Sum({x: Double => x}), {x: Double => x}, 0.0, 2.0, 4.0, 6.0, 8.0)
  //   simple.foreach(partitioning.fill(_))

  //   partitioning.toContainer.cuts(0)._2.value should be (-11.2 +- 1e-12)
  //   partitioning.toContainer.cuts(1)._2.value should be (1.6 +- 1e-12)

  //   Factory.fromJson[Partitioned[Summed]](partitioning.toJson.stringify) should be (partitioning.toContainer)
  // }

  // //////////////////////////////////////////////////////////////// Categorize/Categorized/Categorizing

  // "Categorize/Categorized/Categorizing" must "work" in {
  //   val categorizing = Categorize({x: Struct => x.string.substring(0, 1)})
  //   struct.foreach(categorizing.fill(_))

  //   categorizing.toContainer.pairsMap should be (scala.collection.immutable.Map("n" -> Count.ed(1.0), "e" -> Count.ed(1.0), "t" -> Count.ed(3.0), "s" -> Count.ed(2.0), "f" -> Count.ed(2.0), "o" -> Count.ed(1.0)))

  //   Factory.fromJson[Categorized[Counted]](categorizing.toJson.stringify) should be (categorizing.toContainer)
  // }

  // //////////////////////////////////////////////////////////////// NameMap/NameMapped/NameMapping

  // "NameMap/NameMapped/NameMapping" must "work with multiple types" in {
  //   val one = Histogram(5, -3.0, 7.0, {x: Double => x})
  //   val two = Count[Double]()
  //   val three = Deviate({x: Double => x + 100.0})

  //   val mapping = scala.collection.immutable.Map("one" -> one, "two" -> two, "three" -> three)

  //   simple.foreach(mapping.fill(_))

  //   val mapped = mapping.toContainer

  //   val onetoContainer = mapped[Binned[Counted, Counted, Counted, Counted]]("one")
  //   onetoContainer.values.toList should be (List(Count.ed(3.0), Count.ed(2.0), Count.ed(2.0), Count.ed(1.0), Count.ed(0.0)))
  //   onetoContainer.underflow should be (Count.ed(1.0))
  //   onetoContainer.overflow should be (Count.ed(1.0))
  //   onetoContainer.nanflow should be (Count.ed(0.0))
  //   onetoContainer should be (one.toContainer)

  //   mapped[Counted]("two") should be (Count.ed(10.0))

  //   mapped[Deviated]("three").count should be (10.0 +- 1e-12)
  //   mapped[Deviated]("three").mean should be (100.33 +- 1e-12)
  //   mapped[Deviated]("three").variance should be (10.8381 +- 1e-12)

  //   Factory.fromJson[NameMapped](mapping.toJson.stringify) should be (mapped)
  // }

  // it must "permit histograms to have different cuts" in {
  //   val one = Histogram(10, -10, 10, {x: Double => x}, {x: Double => x > 0})
  //   val two = Histogram(10, -10, 10, {x: Double => x}, {x: Double => x > 5})
  //   val three = Histogram(10, -10, 10, {x: Double => x}, {x: Double => x < 5})

  //   val mapping = scala.collection.immutable.Map("one" -> one, "two" -> two, "three" -> three)

  //   simple.foreach(mapping.fill(_))

  //   val mapped = mapping.toContainer

  //   mapped[Histogrammed]("one").numericValues should be (Seq(0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 2.0, 0.0, 1.0, 0.0))
  //   mapped[Histogrammed]("two").numericValues should be (Seq(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0))
  //   mapped[Histogrammed]("three").numericValues should be (Seq(0.0, 0.0, 1.0, 1.0, 2.0, 3.0, 2.0, 0.0, 0.0, 0.0))
  // }

  // //////////////////////////////////////////////////////////////// Tuple/Tupled/Tupling

  // "Tuple/Tupled/Tupling" must "work with multiple types" in {
  //   val one = Histogram(5, -3.0, 7.0, {x: Double => x})
  //   val two = Count[Double]()
  //   val three = Deviate({x: Double => x + 100.0})

  //   val tupling = (one, two, three)

  //   simple.foreach(tupling.fill(_))

  //   val tupled = tupling.toContainer

  //   val onetoContainer = tupled._1
  //   onetoContainer.values.toList should be (List(Count.ed(3.0), Count.ed(2.0), Count.ed(2.0), Count.ed(1.0), Count.ed(0.0)))
  //   onetoContainer.underflow should be (Count.ed(1.0))
  //   onetoContainer.overflow should be (Count.ed(1.0))
  //   onetoContainer.nanflow should be (Count.ed(0.0))
  //   onetoContainer should be (one.toContainer)

  //   tupled._2 should be (Count.ed(10.0))

  //   tupled._3.count should be (10.0 +- 1e-12)
  //   tupled._3.mean should be (100.33 +- 1e-12)
  //   tupled._3.variance should be (10.8381 +- 1e-12)

  //   Factory.fromJson[Tupled3[Histogrammed, Counted, Deviated]](tupling.toJson.stringify) should be (tupled)
  // }

  // //////////////////////////////////////////////////////////////// Usability in fold/aggregate

  // "Aggregators/Combiners" must "be usable in Spark's aggregate (which has the same signature as Scala's foldLeft + reduce)" in {
  //   for (i <- 0 to 10) {
  //     val (left, right) = simple.splitAt(i)

  //     val partialHists = Seq(
  //       left.foldLeft(Bin(5, -3.0, 7.0, {x: Double => x}))({(hist, x) => hist.fill(x)}),
  //       right.foldLeft(Bin(5, -3.0, 7.0, {x: Double => x}))({(hist, x) => hist.fill(x)}))

  //     val finalHist = partialHists.reduce(_ + _)

  //     println(finalHist.ascii)

  //   }







  // }

}
