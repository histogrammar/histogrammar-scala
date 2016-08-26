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

case class Datum(positive: Double, boolean: Boolean, strings: String, noholes: Double, withholes: Double)

class SpecificationSuite extends FlatSpec with Matchers {
  it must "work" in {
    // val testDataJson = Json.parse(new java.util.Scanner(new java.net.URL(s"http://histogrammar.org/test/${Version.specification}/test-data.json").openStream).useDelimiter("\\A").next)
    // val testResultsJson = Json.parse(new java.util.Scanner(new java.net.URL(s"http://histogrammar.org/test/${Version.specification}/test-results.json").openStream).useDelimiter("\\A").next)

    val testDataJson = Json.parse(new java.util.Scanner(new java.io.FileInputStream("/tmp/test-data.json")).useDelimiter("\\A").next)
    val testResultsJson = Json.parse(new java.util.Scanner(new java.io.FileInputStream("/tmp/test-results.json")).useDelimiter("\\A").next)

    val Some(dataArray: JsonArray) = testDataJson
    val testData = dataArray.to[JsonObject].map(_.pairs.toMap).map(x => Datum(
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




  }

}
