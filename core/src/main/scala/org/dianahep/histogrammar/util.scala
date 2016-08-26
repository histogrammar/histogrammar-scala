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

package org.dianahep.histogrammar

import scala.util.Random

import scala.collection.SortedMap
import scala.collection.SortedSet

import org.dianahep.histogrammar._

/** Supporting functions, mostly called by those in [[org.dianahep.histogrammar]]. */
package object util {
  // /** The natural [[org.dianahep.histogrammar.util.MetricOrdering]] for `Double` precision numbers. */
  // implicit val doubleOrdering: MetricOrdering[Double] = new MetricOrdering[Double] {
  //   def difference(x: Double, y: Double) = x - y
  // }

  /** Relative tolerance for numerical equality. Only affects `equals` checks, not `fill` or `+`. */
  var relativeTolerance: Double = 0.0
  /** Absolute tolerance for numerical equality. Only affects `equals` checks, not `fill` or `+`. */
  var absoluteTolerance: Double = 0.0
}

package util {
  //////////////////////////////////////////////////////////////// type-level programming to test compatibility of user-defined functions

  private[histogrammar] trait Compatible[-X, -Y]
  private[histogrammar] object Compatible {
    implicit object BothAreCounting extends Compatible[Counting, Counting]
    implicit object XIsCounting extends Compatible[Counting, AggregationOnData]
    implicit object YIsCounting extends Compatible[AggregationOnData, Counting]
    implicit def dataAreCompatible[X <: AggregationOnData, Y <: AggregationOnData](implicit evidence: X#Datum =:= Y#Datum) = new Compatible[X, Y] {}
  }

  //////////////////////////////////////////////////////////////// handling key set comparisons with optional keys

  object KeySetComparisons {
    trait KeySet {
      def required: Set[String]
      def optional: Set[String]

      def maybe(string: String) = {
        val t = this
        new KeySet {
          def required = t.required
          def optional = t.optional ++ Set(string)
        }
      }
    }

    implicit class KeySetFromSet(test: Set[String]) extends KeySet {
      def required = test
      def optional = Set[String]()
      def has(that: KeySet) = (that.required subsetOf test)  &&  (test subsetOf (that.required ++ that.optional))
    }
  }

}
