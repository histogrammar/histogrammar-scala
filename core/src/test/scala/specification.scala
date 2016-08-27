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
      System.err.println("---------------------------------------------------------------------------------------------------------------------------")
      System.err.println(name)
      System.err.println("-------------------------------------------------------------+-------------------------------------------------------------")
      System.err.println("Scala                                                        | Specification                                               ")
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

  def round(x: Double) =
    if (x.isNaN  ||  x.isInfinite)
      x
    else
      Math.round(x).toDouble

  it must "work" in {
    val testDataJson = try {
      // Json.parse(new java.util.Scanner(new java.net.URL(s"http://histogrammar.org/test/${Version.specification}/test-data.json").openStream).useDelimiter("\\A").next)
      Json.parse(new java.util.Scanner(new java.io.FileInputStream("/tmp/test-data.json")).useDelimiter("\\A").next)
    }
    catch {
      case err: Exception =>
        System.err.println(s"Could not download test data from http://histogrammar.org/test/${Version.specification}/test-data.json because of $err")
        None
    }

    val testResultsJson = try {
      // Json.parse(new java.util.Scanner(new java.net.URL(s"http://histogrammar.org/test/${Version.specification}/test-results.json").openStream).useDelimiter("\\A").next)
      Json.parse(new java.util.Scanner(new java.io.FileInputStream("/tmp/test-results.json")).useDelimiter("\\A").next)
    }
    catch {
      case err: Exception =>
        System.err.println(s"Could not download test results from http://histogrammar.org/test/${Version.specification}/test-results.json because of $err")
        None
    }

    (testDataJson, testResultsJson) match {
      case (Some(dataArray: JsonArray), Some(resultsArray: JsonArray)) =>

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

        val resultsIterator = resultsArray.elements.toIterator

        var result = resultsIterator.next()
        expr(result) should be ("""Count()""")
        tests(result, testData, "anonymous", Count())
        tests(result, testData, "named", Count())

        result = resultsIterator.next()
        expr(result) should be ("""Count("0.5 * weight")""")
        tests(result, testData, "anonymous", Count({weight: Double => 0.5 * weight}))
        tests(result, testData, "named", Count({weight: Double => 0.5 * weight}))

        result = resultsIterator.next()
        expr(result) should be ("""Sum("positive")""")
        tests(result, testData, "anonymous", Sum({x: X => x.positive}))
        tests(result, testData, "named", Sum({x: X => x.positive} named "positive"))

        result = resultsIterator.next()
        expr(result) should be ("""Sum("noholes")""")
        tests(result, testData, "anonymous", Sum({x: X => x.noholes}))
        tests(result, testData, "named", Sum({x: X => x.noholes} named "noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Sum("2 * noholes")""")
        tests(result, testData, "anonymous", Sum({x: X => 2 * x.noholes}))
        tests(result, testData, "named", Sum({x: X => 2 * x.noholes} named "2 * noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Sum("withholes")""")
        tests(result, testData, "anonymous", Sum({x: X => x.withholes}))
        tests(result, testData, "named", Sum({x: X => x.withholes} named "withholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Average("positive")""")
        tests(result, testData, "anonymous", Average({x: X => x.positive}))
        tests(result, testData, "named", Average({x: X => x.positive} named "positive"))

        result = resultsIterator.next()
        expr(result) should be ("""Average("noholes")""")
        tests(result, testData, "anonymous", Average({x: X => x.noholes}))
        tests(result, testData, "named", Average({x: X => x.noholes} named "noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Average("2 * noholes")""")
        tests(result, testData, "anonymous", Average({x: X => 2 * x.noholes}))
        tests(result, testData, "named", Average({x: X => 2 * x.noholes} named "2 * noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Average("withholes")""")
        tests(result, testData, "anonymous", Average({x: X => x.withholes}))
        tests(result, testData, "named", Average({x: X => x.withholes} named "withholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Deviate("positive")""")
        tests(result, testData, "anonymous", Deviate({x: X => x.positive}))
        tests(result, testData, "named", Deviate({x: X => x.positive} named "positive"))

        result = resultsIterator.next()
        expr(result) should be ("""Deviate("noholes")""")
        tests(result, testData, "anonymous", Deviate({x: X => x.noholes}))
        tests(result, testData, "named", Deviate({x: X => x.noholes} named "noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Deviate("2 * noholes")""")
        tests(result, testData, "anonymous", Deviate({x: X => 2 * x.noholes}))
        tests(result, testData, "named", Deviate({x: X => 2 * x.noholes} named "2 * noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Deviate("withholes")""")
        tests(result, testData, "anonymous", Deviate({x: X => x.withholes}))
        tests(result, testData, "named", Deviate({x: X => x.withholes} named "withholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Minimize("positive")""")
        tests(result, testData, "anonymous", Minimize({x: X => x.positive}))
        tests(result, testData, "named", Minimize({x: X => x.positive} named "positive"))

        result = resultsIterator.next()
        expr(result) should be ("""Minimize("noholes")""")
        tests(result, testData, "anonymous", Minimize({x: X => x.noholes}))
        tests(result, testData, "named", Minimize({x: X => x.noholes} named "noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Minimize("2 * noholes")""")
        tests(result, testData, "anonymous", Minimize({x: X => 2 * x.noholes}))
        tests(result, testData, "named", Minimize({x: X => 2 * x.noholes} named "2 * noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Minimize("withholes")""")
        tests(result, testData, "anonymous", Minimize({x: X => x.withholes}))
        tests(result, testData, "named", Minimize({x: X => x.withholes} named "withholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Maximize("positive")""")
        tests(result, testData, "anonymous", Maximize({x: X => x.positive}))
        tests(result, testData, "named", Maximize({x: X => x.positive} named "positive"))

        result = resultsIterator.next()
        expr(result) should be ("""Maximize("noholes")""")
        tests(result, testData, "anonymous", Maximize({x: X => x.noholes}))
        tests(result, testData, "named", Maximize({x: X => x.noholes} named "noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Maximize("2 * noholes")""")
        tests(result, testData, "anonymous", Maximize({x: X => 2 * x.noholes}))
        tests(result, testData, "named", Maximize({x: X => 2 * x.noholes} named "2 * noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Maximize("withholes")""")
        tests(result, testData, "anonymous", Maximize({x: X => x.withholes}))
        tests(result, testData, "named", Maximize({x: X => x.withholes} named "withholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Bag("round(noholes)", "N")""")
        tests(result, testData, "anonymous", Bag({x: X => round(x.noholes)}, "N"))
        tests(result, testData, "named", Bag({x: X => round(x.noholes)} named "round(noholes)", "N"))

        result = resultsIterator.next()
        expr(result) should be ("""Bag("round(withholes)", "N")""")
        tests(result, testData, "anonymous", Bag({x: X => round(x.withholes)}, "N"))
        tests(result, testData, "named", Bag({x: X => round(x.withholes)} named "round(withholes)", "N"))

        result = resultsIterator.next()
        expr(result) should be ("""Bag("strings", "S")""")
        tests(result, testData, "anonymous", Bag({x: X => x.strings}, "S"))
        tests(result, testData, "named", Bag({x: X => x.strings} named "strings", "S"))

        result = resultsIterator.next()
        expr(result) should be ("""Bag("[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3")""")
        tests(result, testData, "anonymous", Bag({x: X => Vector(round(x.withholes), 2*round(x.withholes), 3*round(x.withholes))}, "N3"))
        tests(result, testData, "named", Bag({x: X => Vector(round(x.withholes), 2*round(x.withholes), 3*round(x.withholes))} named "[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3"))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "positive")""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.positive}))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.positive} named "positive"))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes")""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.noholes}))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.noholes} named "noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "2 * noholes")""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => 2 * x.noholes}))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => 2 * x.noholes} named "2 * noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes")""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.withholes}))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.withholes} named "withholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "positive", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.positive}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.positive} named "positive", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.noholes}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.noholes} named "noholes", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "2 * noholes", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => 2 * x.noholes}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => 2 * x.noholes} named "2 * noholes", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.withholes}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.withholes} named "withholes", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Average("positive"))""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.noholes}, Average({x: X => x.positive})))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.noholes} named "noholes", Average({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Average("noholes"))""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.noholes}, Average({x: X => x.noholes})))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.noholes} named "noholes", Average({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Average("2 * noholes"))""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.noholes}, Average({x: X => 2 * x.noholes})))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.noholes} named "noholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Average("withholes"))""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.noholes}, Average({x: X => x.withholes})))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.noholes} named "noholes", Average({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Average("positive"))""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.withholes}, Average({x: X => x.positive})))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.withholes} named "withholes", Average({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Average("noholes"))""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.withholes}, Average({x: X => x.noholes})))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.withholes} named "withholes", Average({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Average("2 * noholes"))""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.withholes}, Average({x: X => 2 * x.noholes})))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.withholes} named "withholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Average("withholes"))""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.withholes}, Average({x: X => x.withholes})))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.withholes} named "withholes", Average({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Deviate("positive"))""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.noholes}, Deviate({x: X => x.positive})))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.noholes} named "noholes", Deviate({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Deviate("noholes"))""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.noholes}, Deviate({x: X => x.noholes})))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.noholes} named "noholes", Deviate({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Deviate("2 * noholes"))""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.noholes}, Deviate({x: X => 2 * x.noholes})))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.noholes} named "noholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Deviate("withholes"))""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.noholes}, Deviate({x: X => x.withholes})))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.noholes} named "noholes", Deviate({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Deviate("positive"))""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.withholes}, Deviate({x: X => x.positive})))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.withholes} named "withholes", Deviate({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Deviate("noholes"))""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.withholes}, Deviate({x: X => x.noholes})))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.withholes} named "withholes", Deviate({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Deviate("2 * noholes"))""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.withholes}, Deviate({x: X => 2 * x.noholes})))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.withholes} named "withholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Deviate("withholes"))""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.withholes}, Deviate({x: X => x.withholes})))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.withholes} named "withholes", Deviate({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(60, -3.0, 3.0, "positive", Bin(10, -3.0, 3.0, "noholes"))""")
        tests(result, testData, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.positive}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
        tests(result, testData, "named", Bin(60, -3.0, 3.0, {x: X => x.positive} named "positive", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "positive")""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.positive}))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.positive} named "positive"))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes")""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.noholes}))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.noholes} named "noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "2 * noholes")""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => 2 * x.noholes}))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => 2 * x.noholes} named "2 * noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes")""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.withholes}))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.withholes} named "withholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "positive", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.positive}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.positive} named "positive", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.noholes}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.noholes} named "noholes", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "2 * noholes", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => 2 * x.noholes}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => 2 * x.noholes} named "2 * noholes", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.withholes}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.withholes} named "withholes", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Average("positive"))""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.noholes}, Average({x: X => x.positive})))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.noholes} named "noholes", Average({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Average("noholes"))""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.noholes}, Average({x: X => x.noholes})))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.noholes} named "noholes", Average({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Average("2 * noholes"))""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.noholes}, Average({x: X => 2 * x.noholes})))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.noholes} named "noholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Average("withholes"))""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.noholes}, Average({x: X => x.withholes})))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.noholes} named "noholes", Average({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Average("positive"))""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.withholes}, Average({x: X => x.positive})))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.withholes} named "withholes", Average({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Average("noholes"))""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.withholes}, Average({x: X => x.noholes})))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.withholes} named "withholes", Average({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Average("2 * noholes"))""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.withholes}, Average({x: X => 2 * x.noholes})))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.withholes} named "withholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Average("withholes"))""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.withholes}, Average({x: X => x.withholes})))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.withholes} named "withholes", Average({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Deviate("positive"))""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.noholes}, Deviate({x: X => x.positive})))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.noholes} named "noholes", Deviate({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Deviate("noholes"))""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.noholes}, Deviate({x: X => x.noholes})))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.noholes} named "noholes", Deviate({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Deviate("2 * noholes"))""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.noholes}, Deviate({x: X => 2 * x.noholes})))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.noholes} named "noholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Deviate("withholes"))""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.noholes}, Deviate({x: X => x.withholes})))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.noholes} named "noholes", Deviate({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Deviate("positive"))""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.withholes}, Deviate({x: X => x.positive})))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.withholes} named "withholes", Deviate({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Deviate("noholes"))""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.withholes}, Deviate({x: X => x.noholes})))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.withholes} named "withholes", Deviate({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Deviate("2 * noholes"))""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.withholes}, Deviate({x: X => 2 * x.noholes})))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.withholes} named "withholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Deviate("withholes"))""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.withholes}, Deviate({x: X => x.withholes})))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.withholes} named "withholes", Deviate({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Bin(1000, -3.0, 3.0, "positive", Bin(10, -3.0, 3.0, "noholes"))""")
        tests(result, testData, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.positive}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
        tests(result, testData, "named", Bin(1000, -3.0, 3.0, {x: X => x.positive} named "positive", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "positive")""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.positive}))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.positive} named "positive"))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "noholes")""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.noholes}))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.noholes} named "noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "2 * noholes")""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => 2 * x.noholes}))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => 2 * x.noholes} named "2 * noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "withholes")""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.withholes}))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.withholes} named "withholes"))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "positive", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.positive}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.positive} named "positive", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "noholes", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.noholes}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.noholes} named "noholes", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "2 * noholes", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => 2 * x.noholes}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => 2 * x.noholes} named "2 * noholes", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "withholes", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.withholes}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.withholes} named "withholes", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "noholes", Average("positive"))""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.noholes}, Average({x: X => x.positive})))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.noholes} named "noholes", Average({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "noholes", Average("noholes"))""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.noholes}, Average({x: X => x.noholes})))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.noholes} named "noholes", Average({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "noholes", Average("2 * noholes"))""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.noholes}, Average({x: X => 2 * x.noholes})))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.noholes} named "noholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "noholes", Average("withholes"))""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.noholes}, Average({x: X => x.withholes})))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.noholes} named "noholes", Average({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "withholes", Average("positive"))""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.withholes}, Average({x: X => x.positive})))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.withholes} named "withholes", Average({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "withholes", Average("noholes"))""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.withholes}, Average({x: X => x.noholes})))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.withholes} named "withholes", Average({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "withholes", Average("2 * noholes"))""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.withholes}, Average({x: X => 2 * x.noholes})))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.withholes} named "withholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "withholes", Average("withholes"))""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.withholes}, Average({x: X => x.withholes})))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.withholes} named "withholes", Average({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "noholes", Deviate("positive"))""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.noholes}, Deviate({x: X => x.positive})))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.noholes} named "noholes", Deviate({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "noholes", Deviate("noholes"))""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.noholes}, Deviate({x: X => x.noholes})))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.noholes} named "noholes", Deviate({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "noholes", Deviate("2 * noholes"))""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.noholes}, Deviate({x: X => 2 * x.noholes})))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.noholes} named "noholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "noholes", Deviate("withholes"))""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.noholes}, Deviate({x: X => x.withholes})))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.noholes} named "noholes", Deviate({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "withholes", Deviate("positive"))""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.withholes}, Deviate({x: X => x.positive})))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.withholes} named "withholes", Deviate({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "withholes", Deviate("noholes"))""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.withholes}, Deviate({x: X => x.noholes})))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.withholes} named "withholes", Deviate({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "withholes", Deviate("2 * noholes"))""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.withholes}, Deviate({x: X => 2 * x.noholes})))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.withholes} named "withholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "withholes", Deviate("withholes"))""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.withholes}, Deviate({x: X => x.withholes})))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.withholes} named "withholes", Deviate({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""SparselyBin(0.1, "positive", Bin(10, -3.0, 3.0, "noholes"))""")
        tests(result, testData, "anonymous", SparselyBin(0.1, {x: X => x.positive}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
        tests(result, testData, "named", SparselyBin(0.1, {x: X => x.positive} named "positive", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "positive")""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.positive}))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.positive} named "positive"))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes")""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes}))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes} named "noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "2 * noholes")""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => 2 * x.noholes}))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => 2 * x.noholes} named "2 * noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes")""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes}))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes} named "withholes"))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "positive", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.positive}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.positive} named "positive", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes} named "noholes", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "2 * noholes", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => 2 * x.noholes}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => 2 * x.noholes} named "2 * noholes", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes} named "withholes", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Average("positive"))""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes}, Average({x: X => x.positive})))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes} named "noholes", Average({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Average("noholes"))""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes}, Average({x: X => x.noholes})))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes} named "noholes", Average({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Average("2 * noholes"))""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes}, Average({x: X => 2 * x.noholes})))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes} named "noholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Average("withholes"))""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes}, Average({x: X => x.withholes})))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes} named "noholes", Average({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Average("positive"))""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes}, Average({x: X => x.positive})))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes} named "withholes", Average({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Average("noholes"))""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes}, Average({x: X => x.noholes})))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes} named "withholes", Average({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Average("2 * noholes"))""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes}, Average({x: X => 2 * x.noholes})))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes} named "withholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Average("withholes"))""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes}, Average({x: X => x.withholes})))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes} named "withholes", Average({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Deviate("positive"))""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes}, Deviate({x: X => x.positive})))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes} named "noholes", Deviate({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Deviate("noholes"))""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes}, Deviate({x: X => x.noholes})))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes} named "noholes", Deviate({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Deviate("2 * noholes"))""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes}, Deviate({x: X => 2 * x.noholes})))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes} named "noholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Deviate("withholes"))""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes}, Deviate({x: X => x.withholes})))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes} named "noholes", Deviate({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Deviate("positive"))""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes}, Deviate({x: X => x.positive})))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes} named "withholes", Deviate({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Deviate("noholes"))""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes}, Deviate({x: X => x.noholes})))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes} named "withholes", Deviate({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Deviate("2 * noholes"))""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes}, Deviate({x: X => 2 * x.noholes})))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes} named "withholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Deviate("withholes"))""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes}, Deviate({x: X => x.withholes})))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes} named "withholes", Deviate({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "positive", Bin(10, -3.0, 3.0, "noholes"))""")
        tests(result, testData, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.positive}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
        tests(result, testData, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.positive} named "positive", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "positive")""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive}))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive} named "positive"))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes")""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "2 * noholes")""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => 2 * x.noholes}))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => 2 * x.noholes} named "2 * noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes")""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes"))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "positive", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive} named "positive", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "2 * noholes", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => 2 * x.noholes}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => 2 * x.noholes} named "2 * noholes", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("positive"))""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Average({x: X => x.positive})))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Average({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("noholes"))""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Average({x: X => x.noholes})))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Average({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("2 * noholes"))""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Average({x: X => 2 * x.noholes})))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("withholes"))""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Average({x: X => x.withholes})))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Average({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("positive"))""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Average({x: X => x.positive})))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Average({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("noholes"))""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Average({x: X => x.noholes})))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Average({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("2 * noholes"))""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Average({x: X => 2 * x.noholes})))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("withholes"))""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Average({x: X => x.withholes})))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Average({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("positive"))""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Deviate({x: X => x.positive})))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Deviate({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("noholes"))""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Deviate({x: X => x.noholes})))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Deviate({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("2 * noholes"))""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Deviate({x: X => 2 * x.noholes})))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("withholes"))""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Deviate({x: X => x.withholes})))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Deviate({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("positive"))""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Deviate({x: X => x.positive})))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Deviate({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("noholes"))""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Deviate({x: X => x.noholes})))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Deviate({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("2 * noholes"))""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Deviate({x: X => 2 * x.noholes})))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("withholes"))""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Deviate({x: X => x.withholes})))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Deviate({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "positive", Bin(10, -3.0, 3.0, "noholes"))""")
        tests(result, testData, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
        tests(result, testData, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive} named "positive", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Categorize("strings")""")
        tests(result, testData, "anonymous", Categorize({x: X => x.strings}))
        tests(result, testData, "named", Categorize({x: X => x.strings} named "strings"))

        result = resultsIterator.next()
        expr(result) should be ("""Categorize("strings", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", Categorize({x: X => x.strings}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", Categorize({x: X => x.strings} named "strings", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""Categorize("strings", Average("positive"))""")
        tests(result, testData, "anonymous", Categorize({x: X => x.strings}, Average({x: X => x.positive})))
        tests(result, testData, "named", Categorize({x: X => x.strings} named "strings", Average({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""Categorize("strings", Average("noholes"))""")
        tests(result, testData, "anonymous", Categorize({x: X => x.strings}, Average({x: X => x.noholes})))
        tests(result, testData, "named", Categorize({x: X => x.strings} named "strings", Average({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Categorize("strings", Average("2 * noholes"))""")
        tests(result, testData, "anonymous", Categorize({x: X => x.strings}, Average({x: X => 2 * x.noholes})))
        tests(result, testData, "named", Categorize({x: X => x.strings} named "strings", Average({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Categorize("strings", Average("withholes"))""")
        tests(result, testData, "anonymous", Categorize({x: X => x.strings}, Average({x: X => x.withholes})))
        tests(result, testData, "named", Categorize({x: X => x.strings} named "strings", Average({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Categorize("strings", Deviate("positive"))""")
        tests(result, testData, "anonymous", Categorize({x: X => x.strings}, Deviate({x: X => x.positive})))
        tests(result, testData, "named", Categorize({x: X => x.strings} named "strings", Deviate({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""Categorize("strings", Deviate("noholes"))""")
        tests(result, testData, "anonymous", Categorize({x: X => x.strings}, Deviate({x: X => x.noholes})))
        tests(result, testData, "named", Categorize({x: X => x.strings} named "strings", Deviate({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Categorize("strings", Deviate("2 * noholes"))""")
        tests(result, testData, "anonymous", Categorize({x: X => x.strings}, Deviate({x: X => 2 * x.noholes})))
        tests(result, testData, "named", Categorize({x: X => x.strings} named "strings", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Categorize("strings", Deviate("withholes"))""")
        tests(result, testData, "anonymous", Categorize({x: X => x.strings}, Deviate({x: X => x.withholes})))
        tests(result, testData, "named", Categorize({x: X => x.strings} named "strings", Deviate({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Categorize("strings", Bin(10, -3.0, 3.0, "noholes"))""")
        tests(result, testData, "anonymous", Categorize({x: X => x.strings}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
        tests(result, testData, "named", Categorize({x: X => x.strings} named "strings", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Fraction("boolean")""")
        tests(result, testData, "anonymous", Fraction({x: X => x.boolean}))
        tests(result, testData, "named", Fraction({x: X => x.boolean} named "boolean"))

        result = resultsIterator.next()
        expr(result) should be ("""Fraction("positive")""")
        tests(result, testData, "anonymous", Fraction({x: X => x.positive}))
        tests(result, testData, "named", Fraction({x: X => x.positive} named "positive"))

        result = resultsIterator.next()
        expr(result) should be ("""Fraction("noholes")""")
        tests(result, testData, "anonymous", Fraction({x: X => x.noholes}))
        tests(result, testData, "named", Fraction({x: X => x.noholes} named "noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Fraction("withholes")""")
        tests(result, testData, "anonymous", Fraction({x: X => x.withholes}))
        tests(result, testData, "named", Fraction({x: X => x.withholes} named "withholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Fraction("boolean", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", Fraction({x: X => x.boolean}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", Fraction({x: X => x.boolean} named "boolean", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""Fraction("positive", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", Fraction({x: X => x.positive}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", Fraction({x: X => x.positive} named "positive", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""Fraction("noholes", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", Fraction({x: X => x.noholes}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", Fraction({x: X => x.noholes} named "noholes", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""Fraction("withholes", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", Fraction({x: X => x.withholes}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", Fraction({x: X => x.withholes} named "withholes", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""Fraction("boolean", Average("noholes"))""")
        tests(result, testData, "anonymous", Fraction({x: X => x.boolean}, Average({x: X => x.noholes})))
        tests(result, testData, "named", Fraction({x: X => x.boolean} named "boolean", Average({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Fraction("positive", Average("noholes"))""")
        tests(result, testData, "anonymous", Fraction({x: X => x.positive}, Average({x: X => x.noholes})))
        tests(result, testData, "named", Fraction({x: X => x.positive} named "positive", Average({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Fraction("noholes", Average("noholes"))""")
        tests(result, testData, "anonymous", Fraction({x: X => x.noholes}, Average({x: X => x.noholes})))
        tests(result, testData, "named", Fraction({x: X => x.noholes} named "noholes", Average({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Fraction("withholes", Average("noholes"))""")
        tests(result, testData, "anonymous", Fraction({x: X => x.withholes}, Average({x: X => x.noholes})))
        tests(result, testData, "named", Fraction({x: X => x.withholes} named "withholes", Average({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Fraction("boolean", Deviate("noholes"))""")
        tests(result, testData, "anonymous", Fraction({x: X => x.boolean}, Deviate({x: X => x.noholes})))
        tests(result, testData, "named", Fraction({x: X => x.boolean} named "boolean", Deviate({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Fraction("positive", Deviate("noholes"))""")
        tests(result, testData, "anonymous", Fraction({x: X => x.positive}, Deviate({x: X => x.noholes})))
        tests(result, testData, "named", Fraction({x: X => x.positive} named "positive", Deviate({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Fraction("noholes", Deviate("noholes"))""")
        tests(result, testData, "anonymous", Fraction({x: X => x.noholes}, Deviate({x: X => x.noholes})))
        tests(result, testData, "named", Fraction({x: X => x.noholes} named "noholes", Deviate({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Fraction("withholes", Deviate("noholes"))""")
        tests(result, testData, "anonymous", Fraction({x: X => x.withholes}, Deviate({x: X => x.noholes})))
        tests(result, testData, "named", Fraction({x: X => x.withholes} named "withholes", Deviate({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Fraction("boolean", Bin(10, -3.0, 3.0, "noholes"))""")
        tests(result, testData, "anonymous", Fraction({x: X => x.boolean}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
        tests(result, testData, "named", Fraction({x: X => x.boolean} named "boolean", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Fraction("positive", Bin(10, -3.0, 3.0, "noholes"))""")
        tests(result, testData, "anonymous", Fraction({x: X => x.positive}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
        tests(result, testData, "named", Fraction({x: X => x.positive} named "positive", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Fraction("noholes", Bin(10, -3.0, 3.0, "noholes"))""")
        tests(result, testData, "anonymous", Fraction({x: X => x.noholes}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
        tests(result, testData, "named", Fraction({x: X => x.noholes} named "noholes", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Fraction("withholes", Bin(10, -3.0, 3.0, "noholes"))""")
        tests(result, testData, "anonymous", Fraction({x: X => x.withholes}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
        tests(result, testData, "named", Fraction({x: X => x.withholes} named "withholes", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "positive")""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive}))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive} named "positive"))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes")""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "2 * noholes")""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => 2 * x.noholes}))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => 2 * x.noholes} named "2 * noholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes")""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes"))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "positive", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive} named "positive", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "2 * noholes", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => 2 * x.noholes}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => 2 * x.noholes} named "2 * noholes", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Count("0.5 * weight"))""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Count({weight: Double => 0.5 * weight})))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Count({weight: Double => 0.5 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("positive"))""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Average({x: X => x.positive})))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Average({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("noholes"))""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Average({x: X => x.noholes})))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Average({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("2 * noholes"))""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Average({x: X => 2 * x.noholes})))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("withholes"))""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Average({x: X => x.withholes})))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Average({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("positive"))""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Average({x: X => x.positive})))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Average({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("noholes"))""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Average({x: X => x.noholes})))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Average({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("2 * noholes"))""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Average({x: X => 2 * x.noholes})))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("withholes"))""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Average({x: X => x.withholes})))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Average({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("positive"))""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Deviate({x: X => x.positive})))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Deviate({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("noholes"))""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Deviate({x: X => x.noholes})))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Deviate({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("2 * noholes"))""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Deviate({x: X => 2 * x.noholes})))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("withholes"))""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Deviate({x: X => x.withholes})))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Deviate({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("positive"))""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Deviate({x: X => x.positive})))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Deviate({x: X => x.positive} named "positive")))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("noholes"))""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Deviate({x: X => x.noholes})))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Deviate({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("2 * noholes"))""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Deviate({x: X => 2 * x.noholes})))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("withholes"))""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Deviate({x: X => x.withholes})))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Deviate({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "positive", Bin(10, -3.0, 3.0, "noholes"))""")
        tests(result, testData, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
        tests(result, testData, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive} named "positive", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("boolean", Sum("noholes"))""")
        tests(result, testData, "anonymous", Select({x: X => x.boolean}, Sum({x: X => x.noholes})))
        tests(result, testData, "named", Select({x: X => x.boolean} named "boolean", Sum({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("boolean", Average("noholes"))""")
        tests(result, testData, "anonymous", Select({x: X => x.boolean}, Average({x: X => x.noholes})))
        tests(result, testData, "named", Select({x: X => x.boolean} named "boolean", Average({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("boolean", Deviate("noholes"))""")
        tests(result, testData, "anonymous", Select({x: X => x.boolean}, Deviate({x: X => x.noholes})))
        tests(result, testData, "named", Select({x: X => x.boolean} named "boolean", Deviate({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("boolean", Bag("round(withholes)", "N"))""")
        tests(result, testData, "anonymous", Select({x: X => x.boolean}, Bag({x: X => round(x.withholes)}, "N")))
        tests(result, testData, "named", Select({x: X => x.boolean} named "boolean", Bag({x: X => round(x.withholes)} named "round(withholes)", "N")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("boolean", Bag("strings", "S"))""")
        tests(result, testData, "anonymous", Select({x: X => x.boolean}, Bag({x: X => x.strings}, "S")))
        tests(result, testData, "named", Select({x: X => x.boolean} named "boolean", Bag({x: X => x.strings} named "strings", "S")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("boolean", Bag("[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3"))""")
        tests(result, testData, "anonymous", Select({x: X => x.boolean}, Bag({x: X => Vector(round(x.withholes), 2*round(x.withholes), 3*round(x.withholes))}, "N3")))
        tests(result, testData, "named", Select({x: X => x.boolean} named "boolean", Bag({x: X => Vector(round(x.withholes), 2*round(x.withholes), 3*round(x.withholes))} named "[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("boolean", Bin(10, -3.0, 3.0, "noholes"))""")
        tests(result, testData, "anonymous", Select({x: X => x.boolean}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
        tests(result, testData, "named", Select({x: X => x.boolean} named "boolean", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("positive", Sum("noholes"))""")
        tests(result, testData, "anonymous", Select({x: X => x.positive}, Sum({x: X => x.noholes})))
        tests(result, testData, "named", Select({x: X => x.positive} named "positive", Sum({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("positive", Average("noholes"))""")
        tests(result, testData, "anonymous", Select({x: X => x.positive}, Average({x: X => x.noholes})))
        tests(result, testData, "named", Select({x: X => x.positive} named "positive", Average({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("positive", Deviate("noholes"))""")
        tests(result, testData, "anonymous", Select({x: X => x.positive}, Deviate({x: X => x.noholes})))
        tests(result, testData, "named", Select({x: X => x.positive} named "positive", Deviate({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("positive", Bag("round(withholes)", "N"))""")
        tests(result, testData, "anonymous", Select({x: X => x.positive}, Bag({x: X => round(x.withholes)}, "N")))
        tests(result, testData, "named", Select({x: X => x.positive} named "positive", Bag({x: X => round(x.withholes)} named "round(withholes)", "N")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("positive", Bag("strings", "S"))""")
        tests(result, testData, "anonymous", Select({x: X => x.positive}, Bag({x: X => x.strings}, "S")))
        tests(result, testData, "named", Select({x: X => x.positive} named "positive", Bag({x: X => x.strings} named "strings", "S")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("positive", Bag("[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3"))""")
        tests(result, testData, "anonymous", Select({x: X => x.positive}, Bag({x: X => Vector(round(x.withholes), 2*round(x.withholes), 3*round(x.withholes))}, "N3")))
        tests(result, testData, "named", Select({x: X => x.positive} named "positive", Bag({x: X => Vector(round(x.withholes), 2*round(x.withholes), 3*round(x.withholes))} named "[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("positive", Bin(10, -3.0, 3.0, "noholes"))""")
        tests(result, testData, "anonymous", Select({x: X => x.positive}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
        tests(result, testData, "named", Select({x: X => x.positive} named "positive", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("noholes", Sum("noholes"))""")
        tests(result, testData, "anonymous", Select({x: X => x.noholes}, Sum({x: X => x.noholes})))
        tests(result, testData, "named", Select({x: X => x.noholes} named "noholes", Sum({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("noholes", Average("noholes"))""")
        tests(result, testData, "anonymous", Select({x: X => x.noholes}, Average({x: X => x.noholes})))
        tests(result, testData, "named", Select({x: X => x.noholes} named "noholes", Average({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("noholes", Deviate("noholes"))""")
        tests(result, testData, "anonymous", Select({x: X => x.noholes}, Deviate({x: X => x.noholes})))
        tests(result, testData, "named", Select({x: X => x.noholes} named "noholes", Deviate({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("noholes", Bag("round(withholes)", "N"))""")
        tests(result, testData, "anonymous", Select({x: X => x.noholes}, Bag({x: X => round(x.withholes)}, "N")))
        tests(result, testData, "named", Select({x: X => x.noholes} named "noholes", Bag({x: X => round(x.withholes)} named "round(withholes)", "N")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("noholes", Bag("strings", "S"))""")
        tests(result, testData, "anonymous", Select({x: X => x.noholes}, Bag({x: X => x.strings}, "S")))
        tests(result, testData, "named", Select({x: X => x.noholes} named "noholes", Bag({x: X => x.strings} named "strings", "S")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("noholes", Bag("[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3"))""")
        tests(result, testData, "anonymous", Select({x: X => x.noholes}, Bag({x: X => Vector(round(x.withholes), 2*round(x.withholes), 3*round(x.withholes))}, "N3")))
        tests(result, testData, "named", Select({x: X => x.noholes} named "noholes", Bag({x: X => Vector(round(x.withholes), 2*round(x.withholes), 3*round(x.withholes))} named "[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("noholes", Bin(10, -3.0, 3.0, "noholes"))""")
        tests(result, testData, "anonymous", Select({x: X => x.noholes}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
        tests(result, testData, "named", Select({x: X => x.noholes} named "noholes", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("withholes", Sum("noholes"))""")
        tests(result, testData, "anonymous", Select({x: X => x.withholes}, Sum({x: X => x.noholes})))
        tests(result, testData, "named", Select({x: X => x.withholes} named "withholes", Sum({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("withholes", Average("noholes"))""")
        tests(result, testData, "anonymous", Select({x: X => x.withholes}, Average({x: X => x.noholes})))
        tests(result, testData, "named", Select({x: X => x.withholes} named "withholes", Average({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("withholes", Deviate("noholes"))""")
        tests(result, testData, "anonymous", Select({x: X => x.withholes}, Deviate({x: X => x.noholes})))
        tests(result, testData, "named", Select({x: X => x.withholes} named "withholes", Deviate({x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("withholes", Bag("round(withholes)", "N"))""")
        tests(result, testData, "anonymous", Select({x: X => x.withholes}, Bag({x: X => round(x.withholes)}, "N")))
        tests(result, testData, "named", Select({x: X => x.withholes} named "withholes", Bag({x: X => round(x.withholes)} named "round(withholes)", "N")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("withholes", Bag("strings", "S"))""")
        tests(result, testData, "anonymous", Select({x: X => x.withholes}, Bag({x: X => x.strings}, "S")))
        tests(result, testData, "named", Select({x: X => x.withholes} named "withholes", Bag({x: X => x.strings} named "strings", "S")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("withholes", Bag("[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3"))""")
        tests(result, testData, "anonymous", Select({x: X => x.withholes}, Bag({x: X => Vector(round(x.withholes), 2*round(x.withholes), 3*round(x.withholes))}, "N3")))
        tests(result, testData, "named", Select({x: X => x.withholes} named "withholes", Bag({x: X => Vector(round(x.withholes), 2*round(x.withholes), 3*round(x.withholes))} named "[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3")))

        result = resultsIterator.next()
        expr(result) should be ("""Select("withholes", Bin(10, -3.0, 3.0, "noholes"))""")
        tests(result, testData, "anonymous", Select({x: X => x.withholes}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
        tests(result, testData, "named", Select({x: X => x.withholes} named "withholes", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Label(a=Count(), b=Count("0.0"), c=Count("0.5 * weight"), d=Count("2 * weight"))""")
        tests(result, testData, "anonymous", Label("a" -> Count(), "b" -> Count({weight: Double => 0.0}), "c" -> Count({weight: Double => 0.5 * weight}), "d" -> Count({weight: Double => 2 * weight})))
        tests(result, testData, "named", Label("a" -> Count(), "b" -> Count({weight: Double => 0.0}), "c" -> Count({weight: Double => 0.5 * weight}), "d" -> Count({weight: Double => 2 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""Label(a=Sum("positive"), b=Sum("noholes"), c=Sum("2 * noholes"), d=Sum("withholes"))""")
        tests(result, testData, "anonymous", Label("a" -> Sum({x: X => x.positive}), "b" -> Sum({x: X => x.noholes}), "c" -> Sum({x: X => 2 * x.noholes}), "d" -> Sum({x: X => x.withholes})))
        tests(result, testData, "named", Label("a" -> Sum({x: X => x.positive} named "positive"), "b" -> Sum({x: X => x.noholes} named "noholes"), "c" -> Sum({x: X => 2 * x.noholes} named "2 * noholes"), "d" -> Sum({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Label(a=Average("positive"), b=Average("noholes"), c=Average("2 * noholes"), d=Average("withholes"))""")
        tests(result, testData, "anonymous", Label("a" -> Average({x: X => x.positive}), "b" -> Average({x: X => x.noholes}), "c" -> Average({x: X => 2 * x.noholes}), "d" -> Average({x: X => x.withholes})))
        tests(result, testData, "named", Label("a" -> Average({x: X => x.positive} named "positive"), "b" -> Average({x: X => x.noholes} named "noholes"), "c" -> Average({x: X => 2 * x.noholes} named "2 * noholes"), "d" -> Average({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Label(a=Deviate("positive"), b=Deviate("noholes"), c=Deviate("2 * noholes"), d=Deviate("withholes"))""")
        tests(result, testData, "anonymous", Label("a" -> Deviate({x: X => x.positive}), "b" -> Deviate({x: X => x.noholes}), "c" -> Deviate({x: X => 2 * x.noholes}), "d" -> Deviate({x: X => x.withholes})))
        tests(result, testData, "named", Label("a" -> Deviate({x: X => x.positive} named "positive"), "b" -> Deviate({x: X => x.noholes} named "noholes"), "c" -> Deviate({x: X => 2 * x.noholes} named "2 * noholes"), "d" -> Deviate({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Label(a=Bin(10, -3.0, 3.0, "positive"), b=Bin(20, -3.0, 3.0, "noholes", Count("0.5 * weight")), c=Bin(30, -3.0, 3.0, "withholes", Count("2 * weight")))""")
        tests(result, testData, "anonymous", Label("a" -> Bin(10, -3.0, 3.0, {x: X => x.positive}), "b" -> Bin(20, -3.0, 3.0, {x: X => x.noholes}, Count({weight: Double => 0.5 * weight})), "c" -> Bin(30, -3.0, 3.0, {x: X => x.withholes}, Count({weight: Double => 2 * weight}))))
        tests(result, testData, "named", Label("a" -> Bin(10, -3.0, 3.0, {x: X => x.positive} named "positive"), "b" -> Bin(20, -3.0, 3.0, {x: X => x.noholes} named "noholes", Count({weight: Double => 0.5 * weight})), "c" -> Bin(30, -3.0, 3.0, {x: X => x.withholes} named "withholes", Count({weight: Double => 2 * weight}))))

        result = resultsIterator.next()
        expr(result) should be ("""UntypedLabel(a=Sum("positive"), b=Average("noholes"), c=Deviate("2 * noholes"), d=Bag("strings", "S"))""")
        tests(result, testData, "anonymous", UntypedLabel("a" -> Sum({x: X => x.positive}), "b" -> Average({x: X => x.noholes}), "c" -> Deviate({x: X => 2 * x.noholes}), "d" -> Bag({x: X => x.strings}, "S")))
        tests(result, testData, "named", UntypedLabel("a" -> Sum({x: X => x.positive} named "positive"), "b" -> Average({x: X => x.noholes} named "noholes"), "c" -> Deviate({x: X => 2 * x.noholes} named "2 * noholes"), "d" -> Bag({x: X => x.strings} named "strings", "S")))

        result = resultsIterator.next()
        expr(result) should be ("""UntypedLabel(a=Bin(10, -3.0, 3.0, "withholes"), b=Bin(20, -3.0, 3.0, "withholes", Average("positive")), c=SparselyBin(0.1, "withholes"), d=CentrallyBin([-3, -2, -1, 1, 2, 3], "withholes"))""")
        tests(result, testData, "anonymous", UntypedLabel("a" -> Bin(10, -3.0, 3.0, {x: X => x.withholes}), "b" -> Bin(20, -3.0, 3.0, {x: X => x.withholes}, Average({x: X => x.positive})), "c" -> SparselyBin(0.1, {x: X => x.withholes}), "d" -> CentrallyBin(Seq(-3, -2, -1, 1, 2, 3), {x: X => x.withholes})))
        tests(result, testData, "named", UntypedLabel("a" -> Bin(10, -3.0, 3.0, {x: X => x.withholes} named "withholes"), "b" -> Bin(20, -3.0, 3.0, {x: X => x.withholes} named "withholes", Average({x: X => x.positive} named "positive")), "c" -> SparselyBin(0.1, {x: X => x.withholes} named "withholes"), "d" -> CentrallyBin(Seq(-3, -2, -1, 1, 2, 3), {x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Index(Count(), Count("0.0"), Count("0.5 * weight"), Count("2 * weight"))""")
        tests(result, testData, "anonymous", Index(Count(), Count({weight: Double => 0.0}), Count({weight: Double => 0.5 * weight}), Count({weight: Double => 2 * weight})))
        tests(result, testData, "named", Index(Count(), Count({weight: Double => 0.0}), Count({weight: Double => 0.5 * weight}), Count({weight: Double => 2 * weight})))

        result = resultsIterator.next()
        expr(result) should be ("""Index(Sum("positive"), Sum("noholes"), Sum("2 * noholes"), Sum("withholes"))""")
        tests(result, testData, "anonymous", Index(Sum({x: X => x.positive}), Sum({x: X => x.noholes}), Sum({x: X => 2 * x.noholes}), Sum({x: X => x.withholes})))
        tests(result, testData, "named", Index(Sum({x: X => x.positive} named "positive"), Sum({x: X => x.noholes} named "noholes"), Sum({x: X => 2 * x.noholes} named "2 * noholes"), Sum({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Index(Average("positive"), Average("noholes"), Average("2 * noholes"), Average("withholes"))""")
        tests(result, testData, "anonymous", Index(Average({x: X => x.positive}), Average({x: X => x.noholes}), Average({x: X => 2 * x.noholes}), Average({x: X => x.withholes})))
        tests(result, testData, "named", Index(Average({x: X => x.positive} named "positive"), Average({x: X => x.noholes} named "noholes"), Average({x: X => 2 * x.noholes} named "2 * noholes"), Average({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Index(Deviate("positive"), Deviate("noholes"), Deviate("2 * noholes"), Deviate("withholes"))""")
        tests(result, testData, "anonymous", Index(Deviate({x: X => x.positive}), Deviate({x: X => x.noholes}), Deviate({x: X => 2 * x.noholes}), Deviate({x: X => x.withholes})))
        tests(result, testData, "named", Index(Deviate({x: X => x.positive} named "positive"), Deviate({x: X => x.noholes} named "noholes"), Deviate({x: X => 2 * x.noholes} named "2 * noholes"), Deviate({x: X => x.withholes} named "withholes")))

        result = resultsIterator.next()
        expr(result) should be ("""Index(Bin(10, -3.0, 3.0, "positive"), Bin(20, -3.0, 3.0, "noholes", Count("0.5 * weight")), Bin(30, -3.0, 3.0, "withholes", Count("2 * weight")))""")
        tests(result, testData, "anonymous", Index(Bin(10, -3.0, 3.0, {x: X => x.positive}), Bin(20, -3.0, 3.0, {x: X => x.noholes}, Count({weight: Double => 0.5 * weight})), Bin(30, -3.0, 3.0, {x: X => x.withholes}, Count({weight: Double => 2 * weight}))))
        tests(result, testData, "named", Index(Bin(10, -3.0, 3.0, {x: X => x.positive} named "positive"), Bin(20, -3.0, 3.0, {x: X => x.noholes} named "noholes", Count({weight: Double => 0.5 * weight})), Bin(30, -3.0, 3.0, {x: X => x.withholes} named "withholes", Count({weight: Double => 2 * weight}))))

        result = resultsIterator.next()
        expr(result) should be ("""Branch(Sum("positive"), Average("noholes"), Deviate("2 * noholes"), Bag("strings", "S"))""")
        tests(result, testData, "anonymous", Branch(Sum({x: X => x.positive}), Average({x: X => x.noholes}), Deviate({x: X => 2 * x.noholes}), Bag({x: X => x.strings}, "S")))
        tests(result, testData, "named", Branch(Sum({x: X => x.positive} named "positive"), Average({x: X => x.noholes} named "noholes"), Deviate({x: X => 2 * x.noholes} named "2 * noholes"), Bag({x: X => x.strings} named "strings", "S")))

        result = resultsIterator.next()
        expr(result) should be ("""Branch(Bin(10, -3.0, 3.0, "withholes"), Bin(20, -3.0, 3.0, "withholes", Average("positive")), SparselyBin(0.1, "withholes"), CentrallyBin([-3, -2, -1, 1, 2, 3], "withholes"))""")
        tests(result, testData, "anonymous", Branch(Bin(10, -3.0, 3.0, {x: X => x.withholes}), Bin(20, -3.0, 3.0, {x: X => x.withholes}, Average({x: X => x.positive})), SparselyBin(0.1, {x: X => x.withholes}), CentrallyBin(Seq(-3, -2, -1, 1, 2, 3), {x: X => x.withholes})))
        tests(result, testData, "named", Branch(Bin(10, -3.0, 3.0, {x: X => x.withholes} named "withholes"), Bin(20, -3.0, 3.0, {x: X => x.withholes} named "withholes", Average({x: X => x.positive} named "positive")), SparselyBin(0.1, {x: X => x.withholes} named "withholes"), CentrallyBin(Seq(-3, -2, -1, 1, 2, 3), {x: X => x.withholes} named "withholes")))

        resultsIterator.isEmpty should be (true)

      case _ =>
        System.err.println("    skipping specification tests...")
    }
  }
}
