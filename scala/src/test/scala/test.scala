package test.scala.histogrammar

import scala.language.postfixOps

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

  //////////////////////////////////////////////////////////////// Counted/Counting

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

      ContainerFactory.fromJson[Counted](leftCounting.toJson.stringify) should be (leftCounting.fix)
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

      ContainerFactory.fromJson[Counted](leftCounting.toJson.stringify) should be (leftCounting.fix)
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

      ContainerFactory.fromJson[Counted](leftCounting.toJson.stringify) should be (leftCounting.fix)
    }
  }

  //////////////////////////////////////////////////////////////// Summed/Summing

  "Summing/Summed" must "work unfiltered" in {
    for (i <- 0 to 10) {
      val (left, right) = simple.splitAt(i)

      val leftSumming = Summing({x: Double => x})
      val rightSumming = Summing({x: Double => x})

      left.foreach(leftSumming.fill(_))
      right.foreach(rightSumming.fill(_))

      val (Summing(leftResult), Summing(rightResult)) = (leftSumming, rightSumming)

      leftResult should be (left.sum +- 1e-12)
      rightResult should be (right.sum +- 1e-12)

      val Summed(finalResult) = leftSumming + rightSumming

      finalResult should be (simple.sum +- 1e-12)

      ContainerFactory.fromJson[Summed](leftSumming.toJson.stringify) should be (leftSumming.fix)
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

      leftResult should be (left.filter(_.bool).map(_.double).sum +- 1e-12)
      rightResult should be (right.filter(_.bool).map(_.double).sum +- 1e-12)

      val Summed(finalResult) = leftSumming + rightSumming

      finalResult should be (struct.filter(_.bool).map(_.double).sum +- 1e-12)

      ContainerFactory.fromJson[Summed](leftSumming.toJson.stringify) should be (leftSumming.fix)
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

      leftResult should be (left.map({x => x.int * x.double}).sum +- 1e-12)
      rightResult should be (right.map({x => x.int * x.double}).sum +- 1e-12)

      val Summed(finalResult) = leftSumming + rightSumming

      finalResult should be (struct.map({x => x.int * x.double}).sum +- 1e-12)

      ContainerFactory.fromJson[Summed](leftSumming.toJson.stringify) should be (leftSumming.fix)
    }
  }

  //////////////////////////////////////////////////////////////// Averaged/Averaging

  "Averaging/Averaged" must "work unfiltered" in {
    for (i <- 0 to 10) {
      val (left, right) = simple.splitAt(i)

      val leftAveraging = Averaging({x: Double => x})
      val rightAveraging = Averaging({x: Double => x})

      left.foreach(leftAveraging.fill(_))
      right.foreach(rightAveraging.fill(_))

      val (Averaging(_, leftResult), Averaging(_, rightResult)) = (leftAveraging, rightAveraging)

      leftResult should be (mean(left) +- 1e-12)
      rightResult should be (mean(right) +- 1e-12)

      val Averaged(_, finalResult) = leftAveraging + rightAveraging

      finalResult should be (mean(simple) +- 1e-12)

      ContainerFactory.fromJson[Averaged](leftAveraging.toJson.stringify) should be (leftAveraging.fix)
    }
  }

  it must "work with a filter" in {
    for (i <- 0 to 10) {
      val (left, right) = struct.splitAt(i)

      val leftAveraging = Averaging({x: Struct => x.double}, {x: Struct => x.bool})
      val rightAveraging = Averaging({x: Struct => x.double}, {x: Struct => x.bool})

      left.foreach(leftAveraging.fill(_))
      right.foreach(rightAveraging.fill(_))

      val (Averaging(_, leftResult), Averaging(_, rightResult)) = (leftAveraging, rightAveraging)

      leftResult should be (mean(left.filter(_.bool).map(_.double)) +- 1e-12)
      rightResult should be (mean(right.filter(_.bool).map(_.double)) +- 1e-12)

      val Averaged(_, finalResult) = leftAveraging + rightAveraging

      finalResult should be (mean(struct.filter(_.bool).map(_.double)) +- 1e-12)

      ContainerFactory.fromJson[Averaged](leftAveraging.toJson.stringify) should be (leftAveraging.fix)
    }
  }

  it must "work with a weighting factor" in {
    for (i <- 0 to 10) {
      val (left, right) = struct.splitAt(i)

      val leftAveraging = Averaging({x: Struct => x.double}, {x: Struct => x.int})
      val rightAveraging = Averaging({x: Struct => x.double}, {x: Struct => x.int})

      left.foreach(leftAveraging.fill(_))
      right.foreach(rightAveraging.fill(_))

      val (Averaging(_, leftResult), Averaging(_, rightResult)) = (leftAveraging, rightAveraging)

      leftResult should be (mean(left.map(_.double), left.map(_.int.toDouble)) +- 1e-12)
      rightResult should be (mean(right.map(_.double), right.map(_.int.toDouble)) +- 1e-12)

      val Averaged(_, finalResult) = leftAveraging + rightAveraging

      finalResult should be (mean(struct.map(_.double), struct.map(_.int.toDouble)) +- 1e-12)

      ContainerFactory.fromJson[Averaged](leftAveraging.toJson.stringify) should be (leftAveraging.fix)
    }
  }

  it must "work in reverse" in {
    for (i <- 0 to 10) {
      val (left, right) = backward.splitAt(i)

      val leftAveraging = Averaging({x: Struct => x.double}, {x: Struct => x.int})
      val rightAveraging = Averaging({x: Struct => x.double}, {x: Struct => x.int})

      left.foreach(leftAveraging.fill(_))
      right.foreach(rightAveraging.fill(_))

      val (Averaging(_, leftResult), Averaging(_, rightResult)) = (leftAveraging, rightAveraging)

      leftResult should be (mean(left.map(_.double), left.map(_.int.toDouble)) +- 1e-12)
      rightResult should be (mean(right.map(_.double), right.map(_.int.toDouble)) +- 1e-12)

      val Averaged(_, finalResult) = leftAveraging + rightAveraging

      finalResult should be (mean(backward.map(_.double), backward.map(_.int.toDouble)) +- 1e-12)

      ContainerFactory.fromJson[Averaged](leftAveraging.toJson.stringify) should be (leftAveraging.fix)
    }
  }

  //////////////////////////////////////////////////////////////// Deviated/Deviating

  "Deviating/Deviated" must "work unfiltered" in {
    for (i <- 0 to 10) {
      val (left, right) = simple.splitAt(i)

      val leftDeviating = Deviating({x: Double => x})
      val rightDeviating = Deviating({x: Double => x})

      left.foreach(leftDeviating.fill(_))
      right.foreach(rightDeviating.fill(_))

      val (Deviating(_, _, leftResult), Deviating(_, _, rightResult)) = (leftDeviating, rightDeviating)

      leftResult should be (variance(left) +- 1e-12)
      rightResult should be (variance(right) +- 1e-12)

      val Deviated(_, _, finalResult) = leftDeviating + rightDeviating

      finalResult should be (variance(simple) +- 1e-12)

      ContainerFactory.fromJson[Deviated](leftDeviating.toJson.stringify) should be (leftDeviating.fix)
    }
  }

  it must "work with a filter" in {
    for (i <- 0 to 10) {
      val (left, right) = struct.splitAt(i)

      val leftDeviating = Deviating({x: Struct => x.double}, {x: Struct => x.bool})
      val rightDeviating = Deviating({x: Struct => x.double}, {x: Struct => x.bool})

      left.foreach(leftDeviating.fill(_))
      right.foreach(rightDeviating.fill(_))

      val (Deviating(_, _, leftResult), Deviating(_, _, rightResult)) = (leftDeviating, rightDeviating)

      leftResult should be (variance(left.filter(_.bool).map(_.double)) +- 1e-12)
      rightResult should be (variance(right.filter(_.bool).map(_.double)) +- 1e-12)

      val Deviated(_, _, finalResult) = leftDeviating + rightDeviating

      finalResult should be (variance(struct.filter(_.bool).map(_.double)) +- 1e-12)

      ContainerFactory.fromJson[Deviated](leftDeviating.toJson.stringify) should be (leftDeviating.fix)
    }
  }

  it must "work with a weighting factor" in {
    for (i <- 0 to 10) {
      val (left, right) = struct.splitAt(i)

      val leftDeviating = Deviating({x: Struct => x.double}, {x: Struct => x.int})
      val rightDeviating = Deviating({x: Struct => x.double}, {x: Struct => x.int})

      left.foreach(leftDeviating.fill(_))
      right.foreach(rightDeviating.fill(_))

      val (Deviating(_, _, leftResult), Deviating(_, _, rightResult)) = (leftDeviating, rightDeviating)

      leftResult should be (variance(left.map(_.double), left.map(_.int.toDouble)) +- 1e-12)
      rightResult should be (variance(right.map(_.double), right.map(_.int.toDouble)) +- 1e-12)

      val Deviated(_, _, finalResult) = leftDeviating + rightDeviating

      finalResult should be (variance(struct.map(_.double), struct.map(_.int.toDouble)) +- 1e-12)

      ContainerFactory.fromJson[Deviated](leftDeviating.toJson.stringify) should be (leftDeviating.fix)
    }
  }

  it must "work in reverse" in {
    for (i <- 0 to 10) {
      val (left, right) = backward.splitAt(i)

      val leftDeviating = Deviating({x: Struct => x.double}, {x: Struct => x.int})
      val rightDeviating = Deviating({x: Struct => x.double}, {x: Struct => x.int})

      left.foreach(leftDeviating.fill(_))
      right.foreach(rightDeviating.fill(_))

      val (Deviating(_, _, leftResult), Deviating(_, _, rightResult)) = (leftDeviating, rightDeviating)

      leftResult should be (variance(left.map(_.double), left.map(_.int.toDouble)) +- 1e-12)
      rightResult should be (variance(right.map(_.double), right.map(_.int.toDouble)) +- 1e-12)

      val Deviated(_, _, finalResult) = leftDeviating + rightDeviating

      finalResult should be (variance(backward.map(_.double), backward.map(_.int.toDouble)) +- 1e-12)

      ContainerFactory.fromJson[Deviated](leftDeviating.toJson.stringify) should be (leftDeviating.fix)
    }
  }

  //////////////////////////////////////////////////////////////// Binned/Binning

  "Binning/Binned" must "work with Counting/Counted" in {
    val one = Binning(5, -3.0, 7.0, {x: Double => x})()
    simple.foreach(one.fill(_))
    one.fix.values.toList should be (List(Counted(3.0), Counted(2.0), Counted(2.0), Counted(1.0), Counted(0.0)))
    one.fix.underflow should be (Counted(1.0))
    one.fix.overflow should be (Counted(1.0))
    one.fix.nanflow should be (Counted(0.0))

    ContainerFactory.fromJson[Binned[Counted, Counted, Counted, Counted]](one.toJson.stringify) should be (one.fix)

    val two = Binning(5, -3.0, 7.0, {x: Struct => x.double}, {x: Struct => x.bool})()
    struct.foreach(two.fill(_))
    two.fix.values.toList should be (List(Counted(2.0), Counted(1.0), Counted(1.0), Counted(1.0), Counted(0.0)))
    two.fix.underflow should be (Counted(0.0))
    two.fix.overflow should be (Counted(0.0))
    two.fix.nanflow should be (Counted(0.0))

    ContainerFactory.fromJson[Binned[Counted, Counted, Counted, Counted]](two.toJson.stringify) should be (two.fix)
  }

  "Binning/Binned" must "work with Summing/Summed" in {
    val one = Binning(5, -3.0, 7.0, {x: Double => x})(Summing({x: Double => 10.0}), Summing({x: Double => 10.0}), Summing({x: Double => 10.0}), Summing({x: Double => 10.0}))
    simple.foreach(one.fill(_))
    one.fix.values.toList should be (List(Summed(30.0), Summed(20.0), Summed(20.0), Summed(10.0), Summed(0.0)))
    one.fix.underflow should be (Summed(10.0))
    one.fix.overflow should be (Summed(10.0))
    one.fix.nanflow should be (Summed(0.0))

    ContainerFactory.fromJson[Binned[Summed, Summed, Summed, Summed]](one.toJson.stringify) should be (one.fix)

    val two = Binning(5, -3.0, 7.0, {x: Struct => x.double}, {x: Struct => x.bool})(Summing({x: Struct => 10.0}), Summing({x: Struct => 10.0}), Summing({x: Struct => 10.0}), Summing({x: Struct => 10.0}))
    struct.foreach(two.fill(_))
    two.fix.values.toList should be (List(Summed(20.0), Summed(10.0), Summed(10.0), Summed(10.0), Summed(0.0)))
    two.fix.underflow should be (Summed(0.0))
    two.fix.overflow should be (Summed(0.0))
    two.fix.nanflow should be (Summed(0.0))

    ContainerFactory.fromJson[Binned[Summed, Summed, Summed, Summed]](two.toJson.stringify) should be (two.fix)
  }

  //////////////////////////////////////////////////////////////// SparselyBinned/SparselyBinning

  "SparselyBinned/SparselyBinning" must "work with Counting/Counted" in {
    val one = SparselyBinning(1.0, {x: Double => x})()
    simple.foreach(one.fill(_))
    one.fix.values.toList should be (List(-5 -> Counted(1.0), -3 -> Counted(1.0), -2 -> Counted(2.0), 0 -> Counted(2.0), 1 -> Counted(1.0), 2 -> Counted(1.0), 3 -> Counted(1.0), 7 -> Counted(1.0)))

    one.numFilled should be (8)
    one.fix.numFilled should be (8)
    one.num should be (12)
    one.fix.num should be (12)
    one.low should be (-5.0)
    one.fix.low should be (-5.0)
    one.high should be (8.0)
    one.fix.high should be (8.0)

    // println(one.ascii)

    ContainerFactory.fromJson[SparselyBinned[Counted, Counted]](one.toJson.stringify) should be (one.fix)
  }

  //////////////////////////////////////////////////////////////// Mapped/Mapping

  "Mapped/Mapping" must "work with multiple types" in {
    val one = Histogramming(5, -3.0, 7.0, {x: Double => x})
    val two = Counting[Double]()
    val three = Deviating({x: Double => x + 100.0})

    val mapping = Mapping("one" -> one, "two" -> two, "three" -> three)

    simple.foreach(mapping.fill(_))

    val mapped = mapping.fix

    val onefix = mapped[Binned[Counted, Counted, Counted, Counted]]("one")
    onefix.values.toList should be (List(Counted(3.0), Counted(2.0), Counted(2.0), Counted(1.0), Counted(0.0)))
    onefix.underflow should be (Counted(1.0))
    onefix.overflow should be (Counted(1.0))
    onefix.nanflow should be (Counted(0.0))
    onefix should be (one.fix)

    mapped[Counted]("two") should be (Counted(10.0))

    mapped[Deviated]("three").count should be (10.0 +- 1e-12)
    mapped[Deviated]("three").mean should be (100.33 +- 1e-12)
    mapped[Deviated]("three").variance should be (10.8381 +- 1e-12)

    ContainerFactory.fromJson[Mapped](mapping.toJson.stringify) should be (mapped)
  }

  it must "permit histograms to have different cuts" in {
    val one = Histogramming(10, -10, 10, {x: Double => x}, {x: Double => x > 0})
    val two = Histogramming(10, -10, 10, {x: Double => x}, {x: Double => x > 5})
    val three = Histogramming(10, -10, 10, {x: Double => x}, {x: Double => x < 5})

    val mapping = Mapping("one" -> one, "two" -> two, "three" -> three)

    simple.foreach(mapping.fill(_))

    val mapped = mapping.fix

    mapped[Histogrammed]("one").numericValues should be (Seq(0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 2.0, 0.0, 1.0, 0.0))
    mapped[Histogrammed]("two").numericValues should be (Seq(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0))
    mapped[Histogrammed]("three").numericValues should be (Seq(0.0, 0.0, 1.0, 1.0, 2.0, 3.0, 2.0, 0.0, 0.0, 0.0))
  }

  //////////////////////////////////////////////////////////////// Branched/Branching

  "Branched/Branching" must "work with multiple types" in {
    val one = Histogramming(5, -3.0, 7.0, {x: Double => x})
    val two = Counting[Double]()
    val three = Deviating({x: Double => x + 100.0})

    val branching = (one, two, three)

    simple.foreach(branching.fill(_))

    val branched = branching.fix

    val onefix = branched._1
    onefix.values.toList should be (List(Counted(3.0), Counted(2.0), Counted(2.0), Counted(1.0), Counted(0.0)))
    onefix.underflow should be (Counted(1.0))
    onefix.overflow should be (Counted(1.0))
    onefix.nanflow should be (Counted(0.0))
    onefix should be (one.fix)

    branched._2 should be (Counted(10.0))

    branched._3.count should be (10.0 +- 1e-12)
    branched._3.mean should be (100.33 +- 1e-12)
    branched._3.variance should be (10.8381 +- 1e-12)

    ContainerFactory.fromJson[Branched3[Histogrammed, Counted, Deviated]](branching.toJson.stringify) should be (branched)
  }

}
