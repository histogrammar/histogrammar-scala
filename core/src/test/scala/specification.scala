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
import org.scalatest.Ignore

import org.dianahep.histogrammar._
import org.dianahep.histogrammar.json._

case class X(positive: Double, boolean: Boolean, strings: String, noholes: Double, withholes: Double)

// Turned off because it uses too much memory in JDK 7 and gets killed by Travis-CI.
class SpecificationSuite extends FlatSpec with Matchers {

  // used for all equality operations, on both Container and Json subclasses
  val tolerance = 1e-12
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

    System.err.println(s"${which.padTo(9, ' ')} $expr")

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

  val testDataJson = try {
    Json.parse(new java.util.Scanner(new java.net.URL(s"http://histogrammar.org/test/${Version.specification}/test-data.json").openStream).useDelimiter("\\A").next)
  }
  catch {
    case err: Exception =>
      System.err.println(s"Could not download test data from http://histogrammar.org/test/${Version.specification}/test-data.json because of $err")
      None
  }

  val testResultsJson = try {
    Json.parse(new java.util.Scanner(new java.net.URL(s"http://histogrammar.org/test/${Version.specification}/test-results.json").openStream).useDelimiter("\\A").next)
  }
  catch {
    case err: Exception =>
      System.err.println(s"Could not download test results from http://histogrammar.org/test/${Version.specification}/test-results.json because of $err")
      None
  }

  val testData = testDataJson match {
    case Some(dataArray: JsonArray) =>
      Some(dataArray.to[JsonObject].map(_.pairs.toMap).map(x => X(
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
        })).toArray)
    case _ => None
  }

  val resultsIterator = testResultsJson match {
    case Some(resultsArray: JsonArray) => Some(resultsArray.elements.toIterator)
    case _ => None
  }

  var number: Int = 1

  "test001" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 1) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Count()""")
      tests(result, testData.get, "anonymous", Count())
      tests(result, testData.get, "named", Count())
    }
  }

  "test002" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 2) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Count("0.5 * weight")""")
      tests(result, testData.get, "anonymous", Count({weight: Double => 0.5 * weight}))
      tests(result, testData.get, "named", Count({weight: Double => 0.5 * weight}))
    }
  }

  "test003" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 3) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Sum("positive")""")
      tests(result, testData.get, "anonymous", Sum({x: X => x.positive}))
      tests(result, testData.get, "named", Sum({x: X => x.positive} named "positive"))
    }
  }

  "test004" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 4) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Sum("noholes")""")
      tests(result, testData.get, "anonymous", Sum({x: X => x.noholes}))
      tests(result, testData.get, "named", Sum({x: X => x.noholes} named "noholes"))
    }
  }

  "test005" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 5) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Sum("2 * noholes")""")
      tests(result, testData.get, "anonymous", Sum({x: X => 2 * x.noholes}))
      tests(result, testData.get, "named", Sum({x: X => 2 * x.noholes} named "2 * noholes"))
    }
  }

  "test006" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 6) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Sum("withholes")""")
      tests(result, testData.get, "anonymous", Sum({x: X => x.withholes}))
      tests(result, testData.get, "named", Sum({x: X => x.withholes} named "withholes"))
    }
  }

  "test007" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 7) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Average("positive")""")
      tests(result, testData.get, "anonymous", Average({x: X => x.positive}))
      tests(result, testData.get, "named", Average({x: X => x.positive} named "positive"))
    }
  }

  "test008" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 8) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Average("noholes")""")
      tests(result, testData.get, "anonymous", Average({x: X => x.noholes}))
      tests(result, testData.get, "named", Average({x: X => x.noholes} named "noholes"))
    }
  }

  "test009" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 9) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Average("2 * noholes")""")
      tests(result, testData.get, "anonymous", Average({x: X => 2 * x.noholes}))
      tests(result, testData.get, "named", Average({x: X => 2 * x.noholes} named "2 * noholes"))
    }
  }

  "test010" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 10) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Average("withholes")""")
      tests(result, testData.get, "anonymous", Average({x: X => x.withholes}))
      tests(result, testData.get, "named", Average({x: X => x.withholes} named "withholes"))
    }
  }

  "test011" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 11) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Deviate("positive")""")
      tests(result, testData.get, "anonymous", Deviate({x: X => x.positive}))
      tests(result, testData.get, "named", Deviate({x: X => x.positive} named "positive"))
    }
  }

  "test012" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 12) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Deviate("noholes")""")
      tests(result, testData.get, "anonymous", Deviate({x: X => x.noholes}))
      tests(result, testData.get, "named", Deviate({x: X => x.noholes} named "noholes"))
    }
  }

  "test013" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 13) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Deviate("2 * noholes")""")
      tests(result, testData.get, "anonymous", Deviate({x: X => 2 * x.noholes}))
      tests(result, testData.get, "named", Deviate({x: X => 2 * x.noholes} named "2 * noholes"))
    }
  }

  "test014" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 14) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Deviate("withholes")""")
      tests(result, testData.get, "anonymous", Deviate({x: X => x.withholes}))
      tests(result, testData.get, "named", Deviate({x: X => x.withholes} named "withholes"))
    }
  }

  "test015" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 15) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Minimize("positive")""")
      tests(result, testData.get, "anonymous", Minimize({x: X => x.positive}))
      tests(result, testData.get, "named", Minimize({x: X => x.positive} named "positive"))
    }
  }

  "test016" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 16) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Minimize("noholes")""")
      tests(result, testData.get, "anonymous", Minimize({x: X => x.noholes}))
      tests(result, testData.get, "named", Minimize({x: X => x.noholes} named "noholes"))
    }
  }

  "test017" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 17) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Minimize("2 * noholes")""")
      tests(result, testData.get, "anonymous", Minimize({x: X => 2 * x.noholes}))
      tests(result, testData.get, "named", Minimize({x: X => 2 * x.noholes} named "2 * noholes"))
    }
  }

  "test018" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 18) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Minimize("withholes")""")
      tests(result, testData.get, "anonymous", Minimize({x: X => x.withholes}))
      tests(result, testData.get, "named", Minimize({x: X => x.withholes} named "withholes"))
    }
  }

  "test019" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 19) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Maximize("positive")""")
      tests(result, testData.get, "anonymous", Maximize({x: X => x.positive}))
      tests(result, testData.get, "named", Maximize({x: X => x.positive} named "positive"))
    }
  }

  "test020" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 20) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Maximize("noholes")""")
      tests(result, testData.get, "anonymous", Maximize({x: X => x.noholes}))
      tests(result, testData.get, "named", Maximize({x: X => x.noholes} named "noholes"))
    }
  }

  "test021" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 21) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Maximize("2 * noholes")""")
      tests(result, testData.get, "anonymous", Maximize({x: X => 2 * x.noholes}))
      tests(result, testData.get, "named", Maximize({x: X => 2 * x.noholes} named "2 * noholes"))
    }
  }

  "test022" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 22) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Maximize("withholes")""")
      tests(result, testData.get, "anonymous", Maximize({x: X => x.withholes}))
      tests(result, testData.get, "named", Maximize({x: X => x.withholes} named "withholes"))
    }
  }

  "test023" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 23) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bag("round(noholes)", "N")""")
      tests(result, testData.get, "anonymous", Bag({x: X => round(x.noholes)}, "N"))
      tests(result, testData.get, "named", Bag({x: X => round(x.noholes)} named "round(noholes)", "N"))
    }
  }

  "test024" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 24) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bag("round(withholes)", "N")""")
      tests(result, testData.get, "anonymous", Bag({x: X => round(x.withholes)}, "N"))
      tests(result, testData.get, "named", Bag({x: X => round(x.withholes)} named "round(withholes)", "N"))
    }
  }

  "test025" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 25) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bag("strings", "S")""")
      tests(result, testData.get, "anonymous", Bag({x: X => x.strings}, "S"))
      tests(result, testData.get, "named", Bag({x: X => x.strings} named "strings", "S"))
    }
  }

  "test026" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 26) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bag("[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3")""")
      tests(result, testData.get, "anonymous", Bag({x: X => Vector(round(x.withholes), 2*round(x.withholes), 3*round(x.withholes))}, "N3"))
      tests(result, testData.get, "named", Bag({x: X => Vector(round(x.withholes), 2*round(x.withholes), 3*round(x.withholes))} named "[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3"))
    }
  }

  "test027" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 27) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "positive")""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.positive}))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.positive} named "positive"))
    }
  }

  "test028" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 28) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes")""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.noholes}))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.noholes} named "noholes"))
    }
  }

  "test029" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 29) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "2 * noholes")""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => 2 * x.noholes}))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => 2 * x.noholes} named "2 * noholes"))
    }
  }

  "test030" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 30) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes")""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.withholes}))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.withholes} named "withholes"))
    }
  }

  "test031" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 31) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "positive", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.positive}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.positive} named "positive", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test032" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 32) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.noholes}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.noholes} named "noholes", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test033" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 33) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "2 * noholes", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => 2 * x.noholes}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => 2 * x.noholes} named "2 * noholes", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test034" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 34) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.withholes}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.withholes} named "withholes", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test035" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 35) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Average("positive"))""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.noholes}, Average({x: X => x.positive})))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.noholes} named "noholes", Average({x: X => x.positive} named "positive")))
    }
  }

  "test036" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 36) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Average("noholes"))""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.noholes}, Average({x: X => x.noholes})))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.noholes} named "noholes", Average({x: X => x.noholes} named "noholes")))
    }
  }

  "test037" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 37) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Average("2 * noholes"))""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.noholes}, Average({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.noholes} named "noholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test038" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 38) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Average("withholes"))""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.noholes}, Average({x: X => x.withholes})))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.noholes} named "noholes", Average({x: X => x.withholes} named "withholes")))
    }
  }

  "test039" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 39) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Average("positive"))""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.withholes}, Average({x: X => x.positive})))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.withholes} named "withholes", Average({x: X => x.positive} named "positive")))
    }
  }

  "test040" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 40) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Average("noholes"))""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.withholes}, Average({x: X => x.noholes})))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.withholes} named "withholes", Average({x: X => x.noholes} named "noholes")))
    }
  }

  "test041" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 41) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Average("2 * noholes"))""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.withholes}, Average({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.withholes} named "withholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test042" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 42) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Average("withholes"))""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.withholes}, Average({x: X => x.withholes})))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.withholes} named "withholes", Average({x: X => x.withholes} named "withholes")))
    }
  }

  "test043" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 43) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Deviate("positive"))""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.noholes}, Deviate({x: X => x.positive})))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.noholes} named "noholes", Deviate({x: X => x.positive} named "positive")))
    }
  }

  "test044" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 44) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Deviate("noholes"))""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.noholes}, Deviate({x: X => x.noholes})))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.noholes} named "noholes", Deviate({x: X => x.noholes} named "noholes")))
    }
  }

  "test045" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 45) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Deviate("2 * noholes"))""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.noholes}, Deviate({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.noholes} named "noholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test046" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 46) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "noholes", Deviate("withholes"))""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.noholes}, Deviate({x: X => x.withholes})))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.noholes} named "noholes", Deviate({x: X => x.withholes} named "withholes")))
    }
  }

  "test047" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 47) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Deviate("positive"))""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.withholes}, Deviate({x: X => x.positive})))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.withholes} named "withholes", Deviate({x: X => x.positive} named "positive")))
    }
  }

  "test048" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 48) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Deviate("noholes"))""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.withholes}, Deviate({x: X => x.noholes})))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.withholes} named "withholes", Deviate({x: X => x.noholes} named "noholes")))
    }
  }

  "test049" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 49) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Deviate("2 * noholes"))""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.withholes}, Deviate({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.withholes} named "withholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test050" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 50) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "withholes", Deviate("withholes"))""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.withholes}, Deviate({x: X => x.withholes})))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.withholes} named "withholes", Deviate({x: X => x.withholes} named "withholes")))
    }
  }

  "test051" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 51) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(60, -3.0, 3.0, "positive", Bin(10, -3.0, 3.0, "noholes"))""")
      tests(result, testData.get, "anonymous", Bin(60, -3.0, 3.0, {x: X => x.positive}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
      tests(result, testData.get, "named", Bin(60, -3.0, 3.0, {x: X => x.positive} named "positive", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))
    }
  }

  "test052" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 52) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "positive")""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.positive}))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.positive} named "positive"))
    }
  }

  "test053" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 53) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes")""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.noholes}))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.noholes} named "noholes"))
    }
  }

  "test054" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 54) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "2 * noholes")""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => 2 * x.noholes}))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => 2 * x.noholes} named "2 * noholes"))
    }
  }

  "test055" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 55) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes")""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.withholes}))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.withholes} named "withholes"))
    }
  }

  "test056" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 56) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "positive", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.positive}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.positive} named "positive", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test057" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 57) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.noholes}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.noholes} named "noholes", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test058" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 58) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "2 * noholes", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => 2 * x.noholes}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => 2 * x.noholes} named "2 * noholes", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test059" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 59) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.withholes}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.withholes} named "withholes", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test060" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 60) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Average("positive"))""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.noholes}, Average({x: X => x.positive})))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.noholes} named "noholes", Average({x: X => x.positive} named "positive")))
    }
  }

  "test061" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 61) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Average("noholes"))""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.noholes}, Average({x: X => x.noholes})))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.noholes} named "noholes", Average({x: X => x.noholes} named "noholes")))
    }
  }

  "test062" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 62) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Average("2 * noholes"))""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.noholes}, Average({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.noholes} named "noholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test063" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 63) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Average("withholes"))""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.noholes}, Average({x: X => x.withholes})))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.noholes} named "noholes", Average({x: X => x.withholes} named "withholes")))
    }
  }

  "test064" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 64) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Average("positive"))""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.withholes}, Average({x: X => x.positive})))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.withholes} named "withholes", Average({x: X => x.positive} named "positive")))
    }
  }

  "test065" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 65) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Average("noholes"))""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.withholes}, Average({x: X => x.noholes})))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.withholes} named "withholes", Average({x: X => x.noholes} named "noholes")))
    }
  }

  "test066" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 66) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Average("2 * noholes"))""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.withholes}, Average({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.withholes} named "withholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test067" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 67) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Average("withholes"))""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.withholes}, Average({x: X => x.withholes})))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.withholes} named "withholes", Average({x: X => x.withholes} named "withholes")))
    }
  }

  "test068" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 68) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Deviate("positive"))""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.noholes}, Deviate({x: X => x.positive})))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.noholes} named "noholes", Deviate({x: X => x.positive} named "positive")))
    }
  }

  "test069" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 69) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Deviate("noholes"))""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.noholes}, Deviate({x: X => x.noholes})))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.noholes} named "noholes", Deviate({x: X => x.noholes} named "noholes")))
    }
  }

  "test070" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 70) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Deviate("2 * noholes"))""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.noholes}, Deviate({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.noholes} named "noholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test071" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 71) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "noholes", Deviate("withholes"))""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.noholes}, Deviate({x: X => x.withholes})))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.noholes} named "noholes", Deviate({x: X => x.withholes} named "withholes")))
    }
  }

  "test072" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 72) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Deviate("positive"))""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.withholes}, Deviate({x: X => x.positive})))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.withholes} named "withholes", Deviate({x: X => x.positive} named "positive")))
    }
  }

  "test073" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 73) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Deviate("noholes"))""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.withholes}, Deviate({x: X => x.noholes})))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.withholes} named "withholes", Deviate({x: X => x.noholes} named "noholes")))
    }
  }

  "test074" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 74) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Deviate("2 * noholes"))""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.withholes}, Deviate({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.withholes} named "withholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test075" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 75) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "withholes", Deviate("withholes"))""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.withholes}, Deviate({x: X => x.withholes})))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.withholes} named "withholes", Deviate({x: X => x.withholes} named "withholes")))
    }
  }

  "test076" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 76) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Bin(1000, -3.0, 3.0, "positive", Bin(10, -3.0, 3.0, "noholes"))""")
      tests(result, testData.get, "anonymous", Bin(1000, -3.0, 3.0, {x: X => x.positive}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
      tests(result, testData.get, "named", Bin(1000, -3.0, 3.0, {x: X => x.positive} named "positive", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))
    }
  }

  "test077" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 77) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "positive")""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.positive}))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.positive} named "positive"))
    }
  }

  "test078" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 78) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "noholes")""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.noholes}))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.noholes} named "noholes"))
    }
  }

  "test079" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 79) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "2 * noholes")""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => 2 * x.noholes}))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => 2 * x.noholes} named "2 * noholes"))
    }
  }

  "test080" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 80) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "withholes")""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.withholes}))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.withholes} named "withholes"))
    }
  }

  "test081" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 81) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "positive", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.positive}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.positive} named "positive", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test082" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 82) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "noholes", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.noholes}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.noholes} named "noholes", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test083" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 83) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "2 * noholes", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => 2 * x.noholes}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => 2 * x.noholes} named "2 * noholes", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test084" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 84) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "withholes", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.withholes}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.withholes} named "withholes", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test085" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 85) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "noholes", Average("positive"))""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.noholes}, Average({x: X => x.positive})))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.noholes} named "noholes", Average({x: X => x.positive} named "positive")))
    }
  }

  "test086" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 86) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "noholes", Average("noholes"))""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.noholes}, Average({x: X => x.noholes})))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.noholes} named "noholes", Average({x: X => x.noholes} named "noholes")))
    }
  }

  "test087" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 87) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "noholes", Average("2 * noholes"))""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.noholes}, Average({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.noholes} named "noholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test088" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 88) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "noholes", Average("withholes"))""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.noholes}, Average({x: X => x.withholes})))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.noholes} named "noholes", Average({x: X => x.withholes} named "withholes")))
    }
  }

  "test089" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 89) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "withholes", Average("positive"))""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.withholes}, Average({x: X => x.positive})))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.withholes} named "withholes", Average({x: X => x.positive} named "positive")))
    }
  }

  "test090" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 90) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "withholes", Average("noholes"))""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.withholes}, Average({x: X => x.noholes})))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.withholes} named "withholes", Average({x: X => x.noholes} named "noholes")))
    }
  }

  "test091" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 91) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "withholes", Average("2 * noholes"))""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.withholes}, Average({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.withholes} named "withholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test092" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 92) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "withholes", Average("withholes"))""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.withholes}, Average({x: X => x.withholes})))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.withholes} named "withholes", Average({x: X => x.withholes} named "withholes")))
    }
  }

  "test093" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 93) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "noholes", Deviate("positive"))""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.noholes}, Deviate({x: X => x.positive})))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.noholes} named "noholes", Deviate({x: X => x.positive} named "positive")))
    }
  }

  "test094" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 94) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "noholes", Deviate("noholes"))""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.noholes}, Deviate({x: X => x.noholes})))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.noholes} named "noholes", Deviate({x: X => x.noholes} named "noholes")))
    }
  }

  "test095" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 95) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "noholes", Deviate("2 * noholes"))""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.noholes}, Deviate({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.noholes} named "noholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test096" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 96) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "noholes", Deviate("withholes"))""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.noholes}, Deviate({x: X => x.withholes})))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.noholes} named "noholes", Deviate({x: X => x.withholes} named "withholes")))
    }
  }

  "test097" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 97) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "withholes", Deviate("positive"))""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.withholes}, Deviate({x: X => x.positive})))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.withholes} named "withholes", Deviate({x: X => x.positive} named "positive")))
    }
  }

  "test098" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 98) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "withholes", Deviate("noholes"))""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.withholes}, Deviate({x: X => x.noholes})))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.withholes} named "withholes", Deviate({x: X => x.noholes} named "noholes")))
    }
  }

  "test099" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 99) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "withholes", Deviate("2 * noholes"))""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.withholes}, Deviate({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.withholes} named "withholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test100" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 100) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "withholes", Deviate("withholes"))""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.withholes}, Deviate({x: X => x.withholes})))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.withholes} named "withholes", Deviate({x: X => x.withholes} named "withholes")))
    }
  }

  "test101" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 101) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""SparselyBin(0.1, "positive", Bin(10, -3.0, 3.0, "noholes"))""")
      tests(result, testData.get, "anonymous", SparselyBin(0.1, {x: X => x.positive}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
      tests(result, testData.get, "named", SparselyBin(0.1, {x: X => x.positive} named "positive", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))
    }
  }

  "test102" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 102) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "positive")""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.positive}))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.positive} named "positive"))
    }
  }

  "test103" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 103) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes")""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes}))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes} named "noholes"))
    }
  }

  "test104" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 104) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "2 * noholes")""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => 2 * x.noholes}))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => 2 * x.noholes} named "2 * noholes"))
    }
  }

  "test105" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 105) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes")""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes}))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes} named "withholes"))
    }
  }

  "test106" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 106) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "positive", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.positive}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.positive} named "positive", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test107" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 107) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes} named "noholes", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test108" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 108) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "2 * noholes", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => 2 * x.noholes}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => 2 * x.noholes} named "2 * noholes", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test109" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 109) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes} named "withholes", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test110" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 110) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Average("positive"))""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes}, Average({x: X => x.positive})))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes} named "noholes", Average({x: X => x.positive} named "positive")))
    }
  }

  "test111" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 111) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Average("noholes"))""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes}, Average({x: X => x.noholes})))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes} named "noholes", Average({x: X => x.noholes} named "noholes")))
    }
  }

  "test112" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 112) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Average("2 * noholes"))""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes}, Average({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes} named "noholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test113" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 113) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Average("withholes"))""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes}, Average({x: X => x.withholes})))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes} named "noholes", Average({x: X => x.withholes} named "withholes")))
    }
  }

  "test114" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 114) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Average("positive"))""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes}, Average({x: X => x.positive})))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes} named "withholes", Average({x: X => x.positive} named "positive")))
    }
  }

  "test115" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 115) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Average("noholes"))""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes}, Average({x: X => x.noholes})))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes} named "withholes", Average({x: X => x.noholes} named "noholes")))
    }
  }

  "test116" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 116) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Average("2 * noholes"))""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes}, Average({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes} named "withholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test117" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 117) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Average("withholes"))""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes}, Average({x: X => x.withholes})))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes} named "withholes", Average({x: X => x.withholes} named "withholes")))
    }
  }

  "test118" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 118) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Deviate("positive"))""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes}, Deviate({x: X => x.positive})))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes} named "noholes", Deviate({x: X => x.positive} named "positive")))
    }
  }

  "test119" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 119) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Deviate("noholes"))""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes}, Deviate({x: X => x.noholes})))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes} named "noholes", Deviate({x: X => x.noholes} named "noholes")))
    }
  }

  "test120" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 120) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Deviate("2 * noholes"))""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes}, Deviate({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes} named "noholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test121" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 121) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "noholes", Deviate("withholes"))""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes}, Deviate({x: X => x.withholes})))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.noholes} named "noholes", Deviate({x: X => x.withholes} named "withholes")))
    }
  }

  "test122" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 122) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Deviate("positive"))""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes}, Deviate({x: X => x.positive})))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes} named "withholes", Deviate({x: X => x.positive} named "positive")))
    }
  }

  "test123" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 123) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Deviate("noholes"))""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes}, Deviate({x: X => x.noholes})))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes} named "withholes", Deviate({x: X => x.noholes} named "noholes")))
    }
  }

  "test124" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 124) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Deviate("2 * noholes"))""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes}, Deviate({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes} named "withholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test125" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 125) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "withholes", Deviate("withholes"))""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes}, Deviate({x: X => x.withholes})))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.withholes} named "withholes", Deviate({x: X => x.withholes} named "withholes")))
    }
  }

  "test126" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 126) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""CentrallyBin([-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0], "positive", Bin(10, -3.0, 3.0, "noholes"))""")
      tests(result, testData.get, "anonymous", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.positive}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
      tests(result, testData.get, "named", CentrallyBin(Seq(-3.0, -1.5, -1.0, -0.5, 0.0, 0.5, 1.0, 1.5, 3.0), {x: X => x.positive} named "positive", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))
    }
  }

  "test127" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 127) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "positive")""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive}))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive} named "positive"))
    }
  }

  "test128" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 128) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes")""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes"))
    }
  }

  "test129" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 129) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "2 * noholes")""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => 2 * x.noholes}))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => 2 * x.noholes} named "2 * noholes"))
    }
  }

  "test130" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 130) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes")""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes"))
    }
  }

  "test131" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 131) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "positive", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive} named "positive", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test132" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 132) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test133" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 133) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "2 * noholes", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => 2 * x.noholes}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => 2 * x.noholes} named "2 * noholes", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test134" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 134) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test135" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 135) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("positive"))""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Average({x: X => x.positive})))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Average({x: X => x.positive} named "positive")))
    }
  }

  "test136" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 136) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("noholes"))""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Average({x: X => x.noholes})))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Average({x: X => x.noholes} named "noholes")))
    }
  }

  "test137" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 137) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("2 * noholes"))""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Average({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test138" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 138) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("withholes"))""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Average({x: X => x.withholes})))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Average({x: X => x.withholes} named "withholes")))
    }
  }

  "test139" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 139) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("positive"))""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Average({x: X => x.positive})))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Average({x: X => x.positive} named "positive")))
    }
  }

  "test140" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 140) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("noholes"))""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Average({x: X => x.noholes})))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Average({x: X => x.noholes} named "noholes")))
    }
  }

  "test141" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 141) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("2 * noholes"))""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Average({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test142" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 142) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("withholes"))""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Average({x: X => x.withholes})))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Average({x: X => x.withholes} named "withholes")))
    }
  }

  "test143" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 143) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("positive"))""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Deviate({x: X => x.positive})))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Deviate({x: X => x.positive} named "positive")))
    }
  }

  "test144" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 144) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("noholes"))""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Deviate({x: X => x.noholes})))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Deviate({x: X => x.noholes} named "noholes")))
    }
  }

  "test145" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 145) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("2 * noholes"))""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Deviate({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test146" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 146) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("withholes"))""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Deviate({x: X => x.withholes})))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Deviate({x: X => x.withholes} named "withholes")))
    }
  }

  "test147" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 147) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("positive"))""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Deviate({x: X => x.positive})))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Deviate({x: X => x.positive} named "positive")))
    }
  }

  "test148" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 148) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("noholes"))""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Deviate({x: X => x.noholes})))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Deviate({x: X => x.noholes} named "noholes")))
    }
  }

  "test149" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 149) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("2 * noholes"))""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Deviate({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test150" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 150) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("withholes"))""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Deviate({x: X => x.withholes})))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Deviate({x: X => x.withholes} named "withholes")))
    }
  }

  "test151" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 151) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""IrregularlyBin([-3, -2, -1, 0, 1, 2, 3], "positive", Bin(10, -3.0, 3.0, "noholes"))""")
      tests(result, testData.get, "anonymous", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
      tests(result, testData.get, "named", IrregularlyBin(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive} named "positive", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))
    }
  }

  "test152" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 152) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Categorize("strings")""")
      tests(result, testData.get, "anonymous", Categorize({x: X => x.strings}))
      tests(result, testData.get, "named", Categorize({x: X => x.strings} named "strings"))
    }
  }

  "test153" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 153) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Categorize("strings", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", Categorize({x: X => x.strings}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", Categorize({x: X => x.strings} named "strings", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test154" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 154) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Categorize("strings", Average("positive"))""")
      tests(result, testData.get, "anonymous", Categorize({x: X => x.strings}, Average({x: X => x.positive})))
      tests(result, testData.get, "named", Categorize({x: X => x.strings} named "strings", Average({x: X => x.positive} named "positive")))
    }
  }

  "test155" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 155) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Categorize("strings", Average("noholes"))""")
      tests(result, testData.get, "anonymous", Categorize({x: X => x.strings}, Average({x: X => x.noholes})))
      tests(result, testData.get, "named", Categorize({x: X => x.strings} named "strings", Average({x: X => x.noholes} named "noholes")))
    }
  }

  "test156" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 156) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Categorize("strings", Average("2 * noholes"))""")
      tests(result, testData.get, "anonymous", Categorize({x: X => x.strings}, Average({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", Categorize({x: X => x.strings} named "strings", Average({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test157" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 157) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Categorize("strings", Average("withholes"))""")
      tests(result, testData.get, "anonymous", Categorize({x: X => x.strings}, Average({x: X => x.withholes})))
      tests(result, testData.get, "named", Categorize({x: X => x.strings} named "strings", Average({x: X => x.withholes} named "withholes")))
    }
  }

  "test158" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 158) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Categorize("strings", Deviate("positive"))""")
      tests(result, testData.get, "anonymous", Categorize({x: X => x.strings}, Deviate({x: X => x.positive})))
      tests(result, testData.get, "named", Categorize({x: X => x.strings} named "strings", Deviate({x: X => x.positive} named "positive")))
    }
  }

  "test159" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 159) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Categorize("strings", Deviate("noholes"))""")
      tests(result, testData.get, "anonymous", Categorize({x: X => x.strings}, Deviate({x: X => x.noholes})))
      tests(result, testData.get, "named", Categorize({x: X => x.strings} named "strings", Deviate({x: X => x.noholes} named "noholes")))
    }
  }

  "test160" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 160) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Categorize("strings", Deviate("2 * noholes"))""")
      tests(result, testData.get, "anonymous", Categorize({x: X => x.strings}, Deviate({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", Categorize({x: X => x.strings} named "strings", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test161" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 161) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Categorize("strings", Deviate("withholes"))""")
      tests(result, testData.get, "anonymous", Categorize({x: X => x.strings}, Deviate({x: X => x.withholes})))
      tests(result, testData.get, "named", Categorize({x: X => x.strings} named "strings", Deviate({x: X => x.withholes} named "withholes")))
    }
  }

  "test162" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 162) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Categorize("strings", Bin(10, -3.0, 3.0, "noholes"))""")
      tests(result, testData.get, "anonymous", Categorize({x: X => x.strings}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
      tests(result, testData.get, "named", Categorize({x: X => x.strings} named "strings", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))
    }
  }

  "test163" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 163) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Fraction("boolean")""")
      tests(result, testData.get, "anonymous", Fraction({x: X => x.boolean}))
      tests(result, testData.get, "named", Fraction({x: X => x.boolean} named "boolean"))
    }
  }

  "test164" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 164) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Fraction("positive")""")
      tests(result, testData.get, "anonymous", Fraction({x: X => x.positive}))
      tests(result, testData.get, "named", Fraction({x: X => x.positive} named "positive"))
    }
  }

  "test165" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 165) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Fraction("noholes")""")
      tests(result, testData.get, "anonymous", Fraction({x: X => x.noholes}))
      tests(result, testData.get, "named", Fraction({x: X => x.noholes} named "noholes"))
    }
  }

  "test166" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 166) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Fraction("withholes")""")
      tests(result, testData.get, "anonymous", Fraction({x: X => x.withholes}))
      tests(result, testData.get, "named", Fraction({x: X => x.withholes} named "withholes"))
    }
  }

  "test167" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 167) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Fraction("boolean", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", Fraction({x: X => x.boolean}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", Fraction({x: X => x.boolean} named "boolean", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test168" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 168) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Fraction("positive", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", Fraction({x: X => x.positive}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", Fraction({x: X => x.positive} named "positive", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test169" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 169) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Fraction("noholes", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", Fraction({x: X => x.noholes}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", Fraction({x: X => x.noholes} named "noholes", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test170" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 170) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Fraction("withholes", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", Fraction({x: X => x.withholes}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", Fraction({x: X => x.withholes} named "withholes", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test171" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 171) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Fraction("boolean", Average("noholes"))""")
      tests(result, testData.get, "anonymous", Fraction({x: X => x.boolean}, Average({x: X => x.noholes})))
      tests(result, testData.get, "named", Fraction({x: X => x.boolean} named "boolean", Average({x: X => x.noholes} named "noholes")))
    }
  }

  "test172" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 172) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Fraction("positive", Average("noholes"))""")
      tests(result, testData.get, "anonymous", Fraction({x: X => x.positive}, Average({x: X => x.noholes})))
      tests(result, testData.get, "named", Fraction({x: X => x.positive} named "positive", Average({x: X => x.noholes} named "noholes")))
    }
  }

  "test173" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 173) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Fraction("noholes", Average("noholes"))""")
      tests(result, testData.get, "anonymous", Fraction({x: X => x.noholes}, Average({x: X => x.noholes})))
      tests(result, testData.get, "named", Fraction({x: X => x.noholes} named "noholes", Average({x: X => x.noholes} named "noholes")))
    }
  }

  "test174" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 174) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Fraction("withholes", Average("noholes"))""")
      tests(result, testData.get, "anonymous", Fraction({x: X => x.withholes}, Average({x: X => x.noholes})))
      tests(result, testData.get, "named", Fraction({x: X => x.withholes} named "withholes", Average({x: X => x.noholes} named "noholes")))
    }
  }

  "test175" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 175) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Fraction("boolean", Deviate("noholes"))""")
      tests(result, testData.get, "anonymous", Fraction({x: X => x.boolean}, Deviate({x: X => x.noholes})))
      tests(result, testData.get, "named", Fraction({x: X => x.boolean} named "boolean", Deviate({x: X => x.noholes} named "noholes")))
    }
  }

  "test176" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 176) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Fraction("positive", Deviate("noholes"))""")
      tests(result, testData.get, "anonymous", Fraction({x: X => x.positive}, Deviate({x: X => x.noholes})))
      tests(result, testData.get, "named", Fraction({x: X => x.positive} named "positive", Deviate({x: X => x.noholes} named "noholes")))
    }
  }

  "test177" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 177) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Fraction("noholes", Deviate("noholes"))""")
      tests(result, testData.get, "anonymous", Fraction({x: X => x.noholes}, Deviate({x: X => x.noholes})))
      tests(result, testData.get, "named", Fraction({x: X => x.noholes} named "noholes", Deviate({x: X => x.noholes} named "noholes")))
    }
  }

  "test178" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 178) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Fraction("withholes", Deviate("noholes"))""")
      tests(result, testData.get, "anonymous", Fraction({x: X => x.withholes}, Deviate({x: X => x.noholes})))
      tests(result, testData.get, "named", Fraction({x: X => x.withholes} named "withholes", Deviate({x: X => x.noholes} named "noholes")))
    }
  }

  "test179" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 179) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Fraction("boolean", Bin(10, -3.0, 3.0, "noholes"))""")
      tests(result, testData.get, "anonymous", Fraction({x: X => x.boolean}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
      tests(result, testData.get, "named", Fraction({x: X => x.boolean} named "boolean", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))
    }
  }

  "test180" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 180) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Fraction("positive", Bin(10, -3.0, 3.0, "noholes"))""")
      tests(result, testData.get, "anonymous", Fraction({x: X => x.positive}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
      tests(result, testData.get, "named", Fraction({x: X => x.positive} named "positive", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))
    }
  }

  "test181" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 181) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Fraction("noholes", Bin(10, -3.0, 3.0, "noholes"))""")
      tests(result, testData.get, "anonymous", Fraction({x: X => x.noholes}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
      tests(result, testData.get, "named", Fraction({x: X => x.noholes} named "noholes", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))
    }
  }

  "test182" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 182) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Fraction("withholes", Bin(10, -3.0, 3.0, "noholes"))""")
      tests(result, testData.get, "anonymous", Fraction({x: X => x.withholes}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
      tests(result, testData.get, "named", Fraction({x: X => x.withholes} named "withholes", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))
    }
  }

  "test183" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 183) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "positive")""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive}))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive} named "positive"))
    }
  }

  "test184" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 184) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes")""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes"))
    }
  }

  "test185" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 185) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "2 * noholes")""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => 2 * x.noholes}))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => 2 * x.noholes} named "2 * noholes"))
    }
  }

  "test186" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 186) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes")""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes"))
    }
  }

  "test187" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 187) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "positive", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive} named "positive", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test188" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 188) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test189" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 189) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "2 * noholes", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => 2 * x.noholes}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => 2 * x.noholes} named "2 * noholes", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test190" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 190) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Count("0.5 * weight"))""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Count({weight: Double => 0.5 * weight})))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Count({weight: Double => 0.5 * weight})))
    }
  }

  "test191" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 191) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("positive"))""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Average({x: X => x.positive})))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Average({x: X => x.positive} named "positive")))
    }
  }

  "test192" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 192) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("noholes"))""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Average({x: X => x.noholes})))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Average({x: X => x.noholes} named "noholes")))
    }
  }

  "test193" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 193) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("2 * noholes"))""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Average({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test194" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 194) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Average("withholes"))""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Average({x: X => x.withholes})))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Average({x: X => x.withholes} named "withholes")))
    }
  }

  "test195" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 195) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("positive"))""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Average({x: X => x.positive})))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Average({x: X => x.positive} named "positive")))
    }
  }

  "test196" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 196) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("noholes"))""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Average({x: X => x.noholes})))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Average({x: X => x.noholes} named "noholes")))
    }
  }

  "test197" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 197) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("2 * noholes"))""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Average({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Average({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test198" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 198) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Average("withholes"))""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Average({x: X => x.withholes})))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Average({x: X => x.withholes} named "withholes")))
    }
  }

  "test199" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 199) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("positive"))""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Deviate({x: X => x.positive})))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Deviate({x: X => x.positive} named "positive")))
    }
  }

  "test200" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 200) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("noholes"))""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Deviate({x: X => x.noholes})))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Deviate({x: X => x.noholes} named "noholes")))
    }
  }

  "test201" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 201) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("2 * noholes"))""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Deviate({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test202" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 202) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "noholes", Deviate("withholes"))""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes}, Deviate({x: X => x.withholes})))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.noholes} named "noholes", Deviate({x: X => x.withholes} named "withholes")))
    }
  }

  "test203" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 203) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("positive"))""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Deviate({x: X => x.positive})))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Deviate({x: X => x.positive} named "positive")))
    }
  }

  "test204" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 204) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("noholes"))""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Deviate({x: X => x.noholes})))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Deviate({x: X => x.noholes} named "noholes")))
    }
  }

  "test205" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 205) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("2 * noholes"))""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Deviate({x: X => 2 * x.noholes})))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Deviate({x: X => 2 * x.noholes} named "2 * noholes")))
    }
  }

  "test206" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 206) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "withholes", Deviate("withholes"))""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes}, Deviate({x: X => x.withholes})))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.withholes} named "withholes", Deviate({x: X => x.withholes} named "withholes")))
    }
  }

  "test207" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 207) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Stack([-3, -2, -1, 0, 1, 2, 3], "positive", Bin(10, -3.0, 3.0, "noholes"))""")
      tests(result, testData.get, "anonymous", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
      tests(result, testData.get, "named", Stack(Seq(-3, -2, -1, 0, 1, 2, 3), {x: X => x.positive} named "positive", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))
    }
  }

  "test208" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 208) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("boolean", Sum("noholes"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.boolean}, Sum({x: X => x.noholes})))
      tests(result, testData.get, "named", Select({x: X => x.boolean} named "boolean", Sum({x: X => x.noholes} named "noholes")))
    }
  }

  "test209" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 209) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("boolean", Average("noholes"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.boolean}, Average({x: X => x.noholes})))
      tests(result, testData.get, "named", Select({x: X => x.boolean} named "boolean", Average({x: X => x.noholes} named "noholes")))
    }
  }

  "test210" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 210) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("boolean", Deviate("noholes"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.boolean}, Deviate({x: X => x.noholes})))
      tests(result, testData.get, "named", Select({x: X => x.boolean} named "boolean", Deviate({x: X => x.noholes} named "noholes")))
    }
  }

  "test211" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 211) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("boolean", Bag("round(withholes)", "N"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.boolean}, Bag({x: X => round(x.withholes)}, "N")))
      tests(result, testData.get, "named", Select({x: X => x.boolean} named "boolean", Bag({x: X => round(x.withholes)} named "round(withholes)", "N")))
    }
  }

  "test212" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 212) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("boolean", Bag("strings", "S"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.boolean}, Bag({x: X => x.strings}, "S")))
      tests(result, testData.get, "named", Select({x: X => x.boolean} named "boolean", Bag({x: X => x.strings} named "strings", "S")))
    }
  }

  "test213" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 213) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("boolean", Bag("[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.boolean}, Bag({x: X => Vector(round(x.withholes), 2*round(x.withholes), 3*round(x.withholes))}, "N3")))
      tests(result, testData.get, "named", Select({x: X => x.boolean} named "boolean", Bag({x: X => Vector(round(x.withholes), 2*round(x.withholes), 3*round(x.withholes))} named "[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3")))
    }
  }

  "test214" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 214) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("boolean", Bin(10, -3.0, 3.0, "noholes"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.boolean}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
      tests(result, testData.get, "named", Select({x: X => x.boolean} named "boolean", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))
    }
  }

  "test215" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 215) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("positive", Sum("noholes"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.positive}, Sum({x: X => x.noholes})))
      tests(result, testData.get, "named", Select({x: X => x.positive} named "positive", Sum({x: X => x.noholes} named "noholes")))
    }
  }

  "test216" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 216) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("positive", Average("noholes"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.positive}, Average({x: X => x.noholes})))
      tests(result, testData.get, "named", Select({x: X => x.positive} named "positive", Average({x: X => x.noholes} named "noholes")))
    }
  }

  "test217" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 217) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("positive", Deviate("noholes"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.positive}, Deviate({x: X => x.noholes})))
      tests(result, testData.get, "named", Select({x: X => x.positive} named "positive", Deviate({x: X => x.noholes} named "noholes")))
    }
  }

  "test218" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 218) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("positive", Bag("round(withholes)", "N"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.positive}, Bag({x: X => round(x.withholes)}, "N")))
      tests(result, testData.get, "named", Select({x: X => x.positive} named "positive", Bag({x: X => round(x.withholes)} named "round(withholes)", "N")))
    }
  }

  "test219" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 219) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("positive", Bag("strings", "S"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.positive}, Bag({x: X => x.strings}, "S")))
      tests(result, testData.get, "named", Select({x: X => x.positive} named "positive", Bag({x: X => x.strings} named "strings", "S")))
    }
  }

  "test220" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 220) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("positive", Bag("[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.positive}, Bag({x: X => Vector(round(x.withholes), 2*round(x.withholes), 3*round(x.withholes))}, "N3")))
      tests(result, testData.get, "named", Select({x: X => x.positive} named "positive", Bag({x: X => Vector(round(x.withholes), 2*round(x.withholes), 3*round(x.withholes))} named "[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3")))
    }
  }

  "test221" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 221) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("positive", Bin(10, -3.0, 3.0, "noholes"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.positive}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
      tests(result, testData.get, "named", Select({x: X => x.positive} named "positive", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))
    }
  }

  "test222" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 222) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("noholes", Sum("noholes"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.noholes}, Sum({x: X => x.noholes})))
      tests(result, testData.get, "named", Select({x: X => x.noholes} named "noholes", Sum({x: X => x.noholes} named "noholes")))
    }
  }

  "test223" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 223) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("noholes", Average("noholes"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.noholes}, Average({x: X => x.noholes})))
      tests(result, testData.get, "named", Select({x: X => x.noholes} named "noholes", Average({x: X => x.noholes} named "noholes")))
    }
  }

  "test224" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 224) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("noholes", Deviate("noholes"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.noholes}, Deviate({x: X => x.noholes})))
      tests(result, testData.get, "named", Select({x: X => x.noholes} named "noholes", Deviate({x: X => x.noholes} named "noholes")))
    }
  }

  "test225" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 225) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("noholes", Bag("round(withholes)", "N"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.noholes}, Bag({x: X => round(x.withholes)}, "N")))
      tests(result, testData.get, "named", Select({x: X => x.noholes} named "noholes", Bag({x: X => round(x.withholes)} named "round(withholes)", "N")))
    }
  }

  "test226" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 226) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("noholes", Bag("strings", "S"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.noholes}, Bag({x: X => x.strings}, "S")))
      tests(result, testData.get, "named", Select({x: X => x.noholes} named "noholes", Bag({x: X => x.strings} named "strings", "S")))
    }
  }

  "test227" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 227) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("noholes", Bag("[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.noholes}, Bag({x: X => Vector(round(x.withholes), 2*round(x.withholes), 3*round(x.withholes))}, "N3")))
      tests(result, testData.get, "named", Select({x: X => x.noholes} named "noholes", Bag({x: X => Vector(round(x.withholes), 2*round(x.withholes), 3*round(x.withholes))} named "[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3")))
    }
  }

  "test228" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 228) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("noholes", Bin(10, -3.0, 3.0, "noholes"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.noholes}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
      tests(result, testData.get, "named", Select({x: X => x.noholes} named "noholes", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))
    }
  }

  "test229" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 229) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("withholes", Sum("noholes"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.withholes}, Sum({x: X => x.noholes})))
      tests(result, testData.get, "named", Select({x: X => x.withholes} named "withholes", Sum({x: X => x.noholes} named "noholes")))
    }
  }

  "test230" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 230) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("withholes", Average("noholes"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.withholes}, Average({x: X => x.noholes})))
      tests(result, testData.get, "named", Select({x: X => x.withholes} named "withholes", Average({x: X => x.noholes} named "noholes")))
    }
  }

  "test231" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 231) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("withholes", Deviate("noholes"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.withholes}, Deviate({x: X => x.noholes})))
      tests(result, testData.get, "named", Select({x: X => x.withholes} named "withholes", Deviate({x: X => x.noholes} named "noholes")))
    }
  }

  "test232" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 232) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("withholes", Bag("round(withholes)", "N"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.withholes}, Bag({x: X => round(x.withholes)}, "N")))
      tests(result, testData.get, "named", Select({x: X => x.withholes} named "withholes", Bag({x: X => round(x.withholes)} named "round(withholes)", "N")))
    }
  }

  "test233" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 233) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("withholes", Bag("strings", "S"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.withholes}, Bag({x: X => x.strings}, "S")))
      tests(result, testData.get, "named", Select({x: X => x.withholes} named "withholes", Bag({x: X => x.strings} named "strings", "S")))
    }
  }

  "test234" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 234) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("withholes", Bag("[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.withholes}, Bag({x: X => Vector(round(x.withholes), 2*round(x.withholes), 3*round(x.withholes))}, "N3")))
      tests(result, testData.get, "named", Select({x: X => x.withholes} named "withholes", Bag({x: X => Vector(round(x.withholes), 2*round(x.withholes), 3*round(x.withholes))} named "[round(withholes), 2*round(withholes), 3*round(withholes)]", "N3")))
    }
  }

  "test235" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 235) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Select("withholes", Bin(10, -3.0, 3.0, "noholes"))""")
      tests(result, testData.get, "anonymous", Select({x: X => x.withholes}, Bin(10, -3.0, 3.0, {x: X => x.noholes})))
      tests(result, testData.get, "named", Select({x: X => x.withholes} named "withholes", Bin(10, -3.0, 3.0, {x: X => x.noholes} named "noholes")))
    }
  }

  "test236" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 236) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Label(a=Count(), b=Count("0.0"), c=Count("0.5 * weight"), d=Count("2 * weight"))""")
      tests(result, testData.get, "anonymous", Label("a" -> Count(), "b" -> Count({weight: Double => 0.0}), "c" -> Count({weight: Double => 0.5 * weight}), "d" -> Count({weight: Double => 2 * weight})))
      tests(result, testData.get, "named", Label("a" -> Count(), "b" -> Count({weight: Double => 0.0}), "c" -> Count({weight: Double => 0.5 * weight}), "d" -> Count({weight: Double => 2 * weight})))
    }
  }

  "test237" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 237) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Label(a=Sum("positive"), b=Sum("noholes"), c=Sum("2 * noholes"), d=Sum("withholes"))""")
      tests(result, testData.get, "anonymous", Label("a" -> Sum({x: X => x.positive}), "b" -> Sum({x: X => x.noholes}), "c" -> Sum({x: X => 2 * x.noholes}), "d" -> Sum({x: X => x.withholes})))
      tests(result, testData.get, "named", Label("a" -> Sum({x: X => x.positive} named "positive"), "b" -> Sum({x: X => x.noholes} named "noholes"), "c" -> Sum({x: X => 2 * x.noholes} named "2 * noholes"), "d" -> Sum({x: X => x.withholes} named "withholes")))
    }
  }

  "test238" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 238) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Label(a=Average("positive"), b=Average("noholes"), c=Average("2 * noholes"), d=Average("withholes"))""")
      tests(result, testData.get, "anonymous", Label("a" -> Average({x: X => x.positive}), "b" -> Average({x: X => x.noholes}), "c" -> Average({x: X => 2 * x.noholes}), "d" -> Average({x: X => x.withholes})))
      tests(result, testData.get, "named", Label("a" -> Average({x: X => x.positive} named "positive"), "b" -> Average({x: X => x.noholes} named "noholes"), "c" -> Average({x: X => 2 * x.noholes} named "2 * noholes"), "d" -> Average({x: X => x.withholes} named "withholes")))
    }
  }

  "test239" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 239) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Label(a=Deviate("positive"), b=Deviate("noholes"), c=Deviate("2 * noholes"), d=Deviate("withholes"))""")
      tests(result, testData.get, "anonymous", Label("a" -> Deviate({x: X => x.positive}), "b" -> Deviate({x: X => x.noholes}), "c" -> Deviate({x: X => 2 * x.noholes}), "d" -> Deviate({x: X => x.withholes})))
      tests(result, testData.get, "named", Label("a" -> Deviate({x: X => x.positive} named "positive"), "b" -> Deviate({x: X => x.noholes} named "noholes"), "c" -> Deviate({x: X => 2 * x.noholes} named "2 * noholes"), "d" -> Deviate({x: X => x.withholes} named "withholes")))
    }
  }

  "test240" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 240) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Label(a=Bin(10, -3.0, 3.0, "positive"), b=Bin(20, -3.0, 3.0, "noholes", Count("0.5 * weight")), c=Bin(30, -3.0, 3.0, "withholes", Count("2 * weight")))""")
      tests(result, testData.get, "anonymous", Label("a" -> Bin(10, -3.0, 3.0, {x: X => x.positive}), "b" -> Bin(20, -3.0, 3.0, {x: X => x.noholes}, Count({weight: Double => 0.5 * weight})), "c" -> Bin(30, -3.0, 3.0, {x: X => x.withholes}, Count({weight: Double => 2 * weight}))))
      tests(result, testData.get, "named", Label("a" -> Bin(10, -3.0, 3.0, {x: X => x.positive} named "positive"), "b" -> Bin(20, -3.0, 3.0, {x: X => x.noholes} named "noholes", Count({weight: Double => 0.5 * weight})), "c" -> Bin(30, -3.0, 3.0, {x: X => x.withholes} named "withholes", Count({weight: Double => 2 * weight}))))
    }
  }

  "test241" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 241) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""UntypedLabel(a=Sum("positive"), b=Average("noholes"), c=Deviate("2 * noholes"), d=Bag("strings", "S"))""")
      tests(result, testData.get, "anonymous", UntypedLabel("a" -> Sum({x: X => x.positive}), "b" -> Average({x: X => x.noholes}), "c" -> Deviate({x: X => 2 * x.noholes}), "d" -> Bag({x: X => x.strings}, "S")))
      tests(result, testData.get, "named", UntypedLabel("a" -> Sum({x: X => x.positive} named "positive"), "b" -> Average({x: X => x.noholes} named "noholes"), "c" -> Deviate({x: X => 2 * x.noholes} named "2 * noholes"), "d" -> Bag({x: X => x.strings} named "strings", "S")))
    }
  }

  "test242" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 242) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""UntypedLabel(a=Bin(10, -3.0, 3.0, "withholes"), b=Bin(20, -3.0, 3.0, "withholes", Average("positive")), c=SparselyBin(0.1, "withholes"), d=CentrallyBin([-3, -2, -1, 1, 2, 3], "withholes"))""")
      tests(result, testData.get, "anonymous", UntypedLabel("a" -> Bin(10, -3.0, 3.0, {x: X => x.withholes}), "b" -> Bin(20, -3.0, 3.0, {x: X => x.withholes}, Average({x: X => x.positive})), "c" -> SparselyBin(0.1, {x: X => x.withholes}), "d" -> CentrallyBin(Seq(-3, -2, -1, 1, 2, 3), {x: X => x.withholes})))
      tests(result, testData.get, "named", UntypedLabel("a" -> Bin(10, -3.0, 3.0, {x: X => x.withholes} named "withholes"), "b" -> Bin(20, -3.0, 3.0, {x: X => x.withholes} named "withholes", Average({x: X => x.positive} named "positive")), "c" -> SparselyBin(0.1, {x: X => x.withholes} named "withholes"), "d" -> CentrallyBin(Seq(-3, -2, -1, 1, 2, 3), {x: X => x.withholes} named "withholes")))
    }
  }

  "test243" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 243) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Index(Count(), Count("0.0"), Count("0.5 * weight"), Count("2 * weight"))""")
      tests(result, testData.get, "anonymous", Index(Count(), Count({weight: Double => 0.0}), Count({weight: Double => 0.5 * weight}), Count({weight: Double => 2 * weight})))
      tests(result, testData.get, "named", Index(Count(), Count({weight: Double => 0.0}), Count({weight: Double => 0.5 * weight}), Count({weight: Double => 2 * weight})))
    }
  }

  "test244" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 244) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Index(Sum("positive"), Sum("noholes"), Sum("2 * noholes"), Sum("withholes"))""")
      tests(result, testData.get, "anonymous", Index(Sum({x: X => x.positive}), Sum({x: X => x.noholes}), Sum({x: X => 2 * x.noholes}), Sum({x: X => x.withholes})))
      tests(result, testData.get, "named", Index(Sum({x: X => x.positive} named "positive"), Sum({x: X => x.noholes} named "noholes"), Sum({x: X => 2 * x.noholes} named "2 * noholes"), Sum({x: X => x.withholes} named "withholes")))
    }
  }

  "test245" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 245) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Index(Average("positive"), Average("noholes"), Average("2 * noholes"), Average("withholes"))""")
      tests(result, testData.get, "anonymous", Index(Average({x: X => x.positive}), Average({x: X => x.noholes}), Average({x: X => 2 * x.noholes}), Average({x: X => x.withholes})))
      tests(result, testData.get, "named", Index(Average({x: X => x.positive} named "positive"), Average({x: X => x.noholes} named "noholes"), Average({x: X => 2 * x.noholes} named "2 * noholes"), Average({x: X => x.withholes} named "withholes")))
    }
  }

  "test246" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 246) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Index(Deviate("positive"), Deviate("noholes"), Deviate("2 * noholes"), Deviate("withholes"))""")
      tests(result, testData.get, "anonymous", Index(Deviate({x: X => x.positive}), Deviate({x: X => x.noholes}), Deviate({x: X => 2 * x.noholes}), Deviate({x: X => x.withholes})))
      tests(result, testData.get, "named", Index(Deviate({x: X => x.positive} named "positive"), Deviate({x: X => x.noholes} named "noholes"), Deviate({x: X => 2 * x.noholes} named "2 * noholes"), Deviate({x: X => x.withholes} named "withholes")))
    }
  }

  "test247" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 247) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Index(Bin(10, -3.0, 3.0, "positive"), Bin(20, -3.0, 3.0, "noholes", Count("0.5 * weight")), Bin(30, -3.0, 3.0, "withholes", Count("2 * weight")))""")
      tests(result, testData.get, "anonymous", Index(Bin(10, -3.0, 3.0, {x: X => x.positive}), Bin(20, -3.0, 3.0, {x: X => x.noholes}, Count({weight: Double => 0.5 * weight})), Bin(30, -3.0, 3.0, {x: X => x.withholes}, Count({weight: Double => 2 * weight}))))
      tests(result, testData.get, "named", Index(Bin(10, -3.0, 3.0, {x: X => x.positive} named "positive"), Bin(20, -3.0, 3.0, {x: X => x.noholes} named "noholes", Count({weight: Double => 0.5 * weight})), Bin(30, -3.0, 3.0, {x: X => x.withholes} named "withholes", Count({weight: Double => 2 * weight}))))
    }
  }

  "test248" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 248) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Branch(Sum("positive"), Average("noholes"), Deviate("2 * noholes"), Bag("strings", "S"))""")
      tests(result, testData.get, "anonymous", Branch(Sum({x: X => x.positive}), Average({x: X => x.noholes}), Deviate({x: X => 2 * x.noholes}), Bag({x: X => x.strings}, "S")))
      tests(result, testData.get, "named", Branch(Sum({x: X => x.positive} named "positive"), Average({x: X => x.noholes} named "noholes"), Deviate({x: X => 2 * x.noholes} named "2 * noholes"), Bag({x: X => x.strings} named "strings", "S")))
    }
  }

  "test249" must "work" in {
    resultsIterator foreach {ri =>
      while (number != 249) Thread.sleep(1)
      val result = ri.next()
      number += 1
      expr(result) should be ("""Branch(Bin(10, -3.0, 3.0, "withholes"), Bin(20, -3.0, 3.0, "withholes", Average("positive")), SparselyBin(0.1, "withholes"), CentrallyBin([-3, -2, -1, 1, 2, 3], "withholes"))""")
      tests(result, testData.get, "anonymous", Branch(Bin(10, -3.0, 3.0, {x: X => x.withholes}), Bin(20, -3.0, 3.0, {x: X => x.withholes}, Average({x: X => x.positive})), SparselyBin(0.1, {x: X => x.withholes}), CentrallyBin(Seq(-3, -2, -1, 1, 2, 3), {x: X => x.withholes})))
      tests(result, testData.get, "named", Branch(Bin(10, -3.0, 3.0, {x: X => x.withholes} named "withholes"), Bin(20, -3.0, 3.0, {x: X => x.withholes} named "withholes", Average({x: X => x.positive} named "positive")), SparselyBin(0.1, {x: X => x.withholes} named "withholes"), CentrallyBin(Seq(-3, -2, -1, 1, 2, 3), {x: X => x.withholes} named "withholes")))
    }
  }

}
