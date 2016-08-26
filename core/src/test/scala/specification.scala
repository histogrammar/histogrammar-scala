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
import org.dianahep.histogrammar.json._

case class X(positive: Double, boolean: Boolean, strings: String, noholes: Double, withholes: Double)

class SpecificationSuite extends FlatSpec with Matchers {
  // used for all equality operations, on both Container and Json subclasses
  val tolerance = 1e-6
  org.dianahep.histogrammar.util.relativeTolerance = tolerance
  org.dianahep.histogrammar.util.absoluteTolerance = tolerance

  def expr(result: Json) = result.asInstanceOf[JsonObject].pairs.head._2.asInstanceOf[JsonString].value

  def compare(x: Container[_], y: Container[_], name: String) {
    if (x != y) {
      System.err.println(name)
      System.err.println("-------------------------------------------------------------+-------------------------------------------------------------")
      val left = x.toJson.pretty().split("\n")
      val right = y.toJson.pretty().split("\n")
      for ((leftline, rightline) <- left zip right)
        System.err.println(leftline.padTo(60, ' ') + (if (leftline != rightline) " > " else " | ") + rightline)
      false should be (true)
    }
  }

  def tests[CONTAINER <: Container[CONTAINER] with Aggregation{type Datum >: X}](result: Json, testData: Seq[X], which: String, h1: CONTAINER) {
    val m = result.asInstanceOf[JsonObject].pairs.toMap
    val expr = m(JsonString("expr")).asInstanceOf[JsonString].value
    val zero = Factory.fromJson(m(JsonString("zero-" + which)))
    val one = Factory.fromJson(m(JsonString("one-" + which)))
    val two = Factory.fromJson(m(JsonString("two-" + which)))

    compare(h1.toImmutable, zero, which + " ZERO in " + expr)
    compare((h1 + h1).toImmutable, zero, which + " ZERO + ZERO in " + expr)
    compare(h1.zero.toImmutable, zero, which + " ZERO.zero in " + expr)

    val h2 = h1.copy

    for (x <- testData) {
      h1.fill(x)
      h2.fill(x)
    }

    compare(h1.toImmutable, one, which + " ONE in " + expr)
    compare(h1.zero.toImmutable, zero, which + " ONE.zero() in " + expr)
    compare((h1 + h1.zero).toImmutable, one, which + " ONE + ZERO in " + expr)
    compare((h1.zero + h1).toImmutable, one, which + " ZERO + ONE in " + expr)

    compare((h1 + h2).toImmutable, two, which + " TWO VIA PLUS in " + expr)

    for (x <- testData)
      h1.fill(x)

    compare(h1.toImmutable, two, which + " TWO VIA FILL in " + expr)
  }

  it must "work" in {
    // val testDataJson = Json.parse(new java.util.Scanner(new java.net.URL(s"http://histogrammar.org/test/${Version.specification}/test-data.json").openStream).useDelimiter("\\A").next)
    // val testResultsJson = Json.parse(new java.util.Scanner(new java.net.URL(s"http://histogrammar.org/test/${Version.specification}/test-results.json").openStream).useDelimiter("\\A").next)

    val testDataJson = Json.parse(new java.util.Scanner(new java.io.FileInputStream("/tmp/test-data.json")).useDelimiter("\\A").next)
    val testResultsJson = Json.parse(new java.util.Scanner(new java.io.FileInputStream("/tmp/test-results.json")).useDelimiter("\\A").next)

    val Some(dataArray: JsonArray) = testDataJson
    val testData = dataArray.to[JsonObject].map(_.pairs.toMap).map(x => X(
      x(JsonString("positive")).asInstanceOf[JsonNumber].toDouble,
      x(JsonString("boolean")) match {
        case JsonTrue => true
        case JsonFalse => false
        case _ => throw new Exception
      },
      x(JsonString("strings")).asInstanceOf[JsonString].value,
      x(JsonString("noholes")).asInstanceOf[JsonNumber].toDouble,
      x(JsonString("withholes")) match {
        case JsonString("nan") => java.lang.Double.NaN
        case JsonString("inf") => java.lang.Double.POSITIVE_INFINITY
        case JsonString("-inf") => java.lang.Double.NEGATIVE_INFINITY
        case JsonNumber(y) => y
      })).toArray

    val resultsIterator = testResultsJson.get.asInstanceOf[JsonArray].elements.toIterator

    var result = resultsIterator.next()
    expr(result) should be ("Count()")
    tests(result, testData, "anonymous", Count())

    result = resultsIterator.next()
    expr(result) should be ("""Count("0.5 * weight")""")

    result = resultsIterator.next()
    expr(result) should be ("""Sum("positive")""")

    result = resultsIterator.next()
    expr(result) should be ("""Sum("noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Sum("2 * noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Sum("withholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Average("positive")""")

    result = resultsIterator.next()
    expr(result) should be ("""Average("noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Average("2 * noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Average("withholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Deviate("positive")""")

    result = resultsIterator.next()
    expr(result) should be ("""Deviate("noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Deviate("2 * noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Deviate("withholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Minimize("positive")""")

    result = resultsIterator.next()
    expr(result) should be ("""Minimize("noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Minimize("2 * noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Minimize("withholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Maximize("positive")""")

    result = resultsIterator.next()
    expr(result) should be ("""Maximize("noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Maximize("2 * noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Maximize("withholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Bag("round(noholes)", "N")""")

    result = resultsIterator.next()
    expr(result) should be ("""Bag("round(withholes)", "N")""")

    result = resultsIterator.next()
    expr(result) should be ("""Bag("strings", "S")""")

    result = resultsIterator.next()
    expr(result) should be ("""Bag("[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3")""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "positive")""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "2 * noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "positive", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "2 * noholes", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Average("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Average("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Average("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Average("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Average("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Average("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Average("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Average("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Deviate("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Deviate("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Deviate("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Deviate("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Deviate("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Deviate("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Deviate("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Deviate("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(60, -3.0, 3.0, "positive", Bin(10, -3.0, 3.0, "noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "positive")""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "2 * noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "positive", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "2 * noholes", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Average("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Average("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Average("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Average("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Average("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Average("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Average("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Average("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Deviate("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Deviate("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Deviate("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Deviate("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Deviate("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Deviate("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Deviate("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Deviate("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Bin(1000, -3.0, 3.0, "positive", Bin(10, -3.0, 3.0, "noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "positive")""")
    tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.positive}))

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "2 * noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "withholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "positive", Count("0.5 * weight"))""")
    tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.positive}, Count({weight: Double => 0.5 * weight})))

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "noholes", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "2 * noholes", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "withholes", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "noholes", Average("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "noholes", Average("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "noholes", Average("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "noholes", Average("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "withholes", Average("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "withholes", Average("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "withholes", Average("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "withholes", Average("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "noholes", Deviate("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "noholes", Deviate("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "noholes", Deviate("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "noholes", Deviate("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "withholes", Deviate("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "withholes", Deviate("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "withholes", Deviate("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "withholes", Deviate("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""SparselyBin(0.1, "positive", Bin(10, -3.0, 3.0, "noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "positive")""")
    tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.positive}))

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes")""")
    tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes}))

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "2 * noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "positive", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "2 * noholes", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Average("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Average("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Average("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Average("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Average("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Average("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Average("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Average("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Deviate("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Deviate("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Deviate("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Deviate("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Deviate("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Deviate("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Deviate("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Deviate("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "positive", Bin(10, -3.0, 3.0, "noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "positive")""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "2 * noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "positive", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "2 * noholes", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "positive", Bin(10, -3.0, 3.0, "noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Categorize("strings")""")

    result = resultsIterator.next()
    expr(result) should be ("""Categorize("strings", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Categorize("strings", Average("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Categorize("strings", Average("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Categorize("strings", Average("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Categorize("strings", Average("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Categorize("strings", Deviate("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Categorize("strings", Deviate("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Categorize("strings", Deviate("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Categorize("strings", Deviate("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Categorize("strings", Bin(10, -3.0, 3.0, "noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Fraction("boolean")""")

    result = resultsIterator.next()
    expr(result) should be ("""Fraction("positive")""")

    result = resultsIterator.next()
    expr(result) should be ("""Fraction("noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Fraction("withholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Fraction("boolean", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Fraction("positive", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Fraction("noholes", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Fraction("withholes", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Fraction("boolean", Average("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Fraction("positive", Average("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Fraction("noholes", Average("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Fraction("withholes", Average("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Fraction("boolean", Deviate("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Fraction("positive", Deviate("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Fraction("noholes", Deviate("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Fraction("withholes", Deviate("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Fraction("boolean", Bin(10, -3.0, 3.0, "noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Fraction("positive", Bin(10, -3.0, 3.0, "noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Fraction("noholes", Bin(10, -3.0, 3.0, "noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Fraction("withholes", Bin(10, -3.0, 3.0, "noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "positive")""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "2 * noholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes")""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "positive", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "2 * noholes", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Count("0.5 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("positive"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("2 * noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "positive", Bin(10, -3.0, 3.0, "noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("boolean", Sum("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("boolean", Average("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("boolean", Deviate("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("boolean", Bag("round(withholes)", "N"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("boolean", Bag("strings", "S"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("boolean", Bag("[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("boolean", Bin(10, -3.0, 3.0, "noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("positive", Sum("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("positive", Average("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("positive", Deviate("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("positive", Bag("round(withholes)", "N"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("positive", Bag("strings", "S"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("positive", Bag("[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("positive", Bin(10, -3.0, 3.0, "noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("noholes", Sum("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("noholes", Average("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("noholes", Deviate("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("noholes", Bag("round(withholes)", "N"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("noholes", Bag("strings", "S"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("noholes", Bag("[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("noholes", Bin(10, -3.0, 3.0, "noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("withholes", Sum("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("withholes", Average("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("withholes", Deviate("noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("withholes", Bag("round(withholes)", "N"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("withholes", Bag("strings", "S"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("withholes", Bag("[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Select("withholes", Bin(10, -3.0, 3.0, "noholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Label(a=Count(), b=Count("0.0"), c=Count("0.5 * weight"), d=Count("2 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Label(a=Sum("positive"), b=Sum("noholes"), c=Sum("2 * noholes"), d=Sum("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Label(a=Average("positive"), b=Average("noholes"), c=Average("2 * noholes"), d=Average("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Label(a=Deviate("positive"), b=Deviate("noholes"), c=Deviate("2 * noholes"), d=Deviate("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Label(a=Bag("round(withholes)", "N"), b=Bag("strings", "S"), c=Bag("[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Label(a=Bin(10, -3.0, 3.0, "positive"), b=Bin(20, -3.0, 3.0, "noholes", Count("0.5 * weight")), c=Bin(30, -3.0, 3.0, "withholes", Count("2 * weight")))""")

    result = resultsIterator.next()
    expr(result) should be ("""UntypedLabel(a=Sum("positive"), b=Average("noholes"), c=Deviate("2 * noholes"), d=Bag("strings", "S"))""")

    result = resultsIterator.next()
    expr(result) should be ("""UntypedLabel(a=Bin(10, -3.0, 3.0, "withholes"), b=Bin(20, -3.0, 3.0, "withholes", Average("positive")), c=SparselyBin(0.1, "withholes"), d=CentrallyBin([-3, -2, -1, 1, 2, 3], "withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Index(Count(), Count("0.0"), Count("0.5 * weight"), Count("2 * weight"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Index(Sum("positive"), Sum("noholes"), Sum("2 * noholes"), Sum("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Index(Average("positive"), Average("noholes"), Average("2 * noholes"), Average("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Index(Deviate("positive"), Deviate("noholes"), Deviate("2 * noholes"), Deviate("withholes"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Index(Bag("round(withholes)", "N"), Bag("strings", "S"), Bag("[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Index(Bin(10, -3.0, 3.0, "positive"), Bin(20, -3.0, 3.0, "noholes", Count("0.5 * weight")), Bin(30, -3.0, 3.0, "withholes", Count("2 * weight")))""")

    result = resultsIterator.next()
    expr(result) should be ("""Branch(Sum("positive"), Average("noholes"), Deviate("2 * noholes"), Bag("strings", "S"))""")

    result = resultsIterator.next()
    expr(result) should be ("""Branch(Bin(10, -3.0, 3.0, "withholes"), Bin(20, -3.0, 3.0, "withholes", Average("positive")), SparselyBin(0.1, "withholes"), CentrallyBin([-3, -2, -1, 1, 2, 3], "withholes"))""")
    tests(result, testData, "anonymous", Branch(Bin(10, -3.0, 3.0, {x: X => x.withholes}), Bin(20, -3.0, 3.0, {x: X => x.withholes}, Average({x: X => x.positive})), SparselyBin(0.1, {x: X => x.withholes}), CentrallyBin(Seq(-3, -2, -1, 1, 2, 3), {x: X => x.withholes})))

    resultsIterator.isEmpty should be (true)
  }

}
