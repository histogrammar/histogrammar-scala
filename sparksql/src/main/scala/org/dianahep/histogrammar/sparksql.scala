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

import scala.collection.JavaConversions._
import scala.reflect.ClassTag

import org.apache.spark.sql.types.StringType
import org.apache.spark.sql.types.DoubleType
import org.apache.spark.sql.Column
import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.Row

package object sparksql {
  implicit class UserFcnFromColumn[+RANGE](@scala.transient val col: Column) extends UserFcn[Row, RANGE] {
    var index = -1
    val name = Some(col.toString)
    def hasCache = false
    def apply[SUB <: Row](row: SUB): RANGE = row.get(index).asInstanceOf[RANGE]
  }

  implicit class DataFrameHistogrammarMethods(df: DataFrame) {
    def histogrammar[CONTAINER <: Container[CONTAINER] with Aggregation{type Datum = Row} : ClassTag](container: CONTAINER) = {
      var index = 0
      val columns = List.newBuilder[Column]

      def gatherColumns(x: Container[_]) {
        x match {
          case y: NumericalQuantity[_] => y.quantity match {
            case z: UserFcnFromColumn[_] =>
              z.index = index
              index += 1
              columns += z.col.cast(DoubleType)
            case _ =>
              throw new IllegalArgumentException("primitives passed to SQLContext.histogrammar must have spark.sql.Columns for fill rules")
          }

          case y: CategoricalQuantity[_] => y.quantity match {
            case z: UserFcnFromColumn[_] =>
              z.index = index
              index += 1
              columns += z.col.cast(StringType)
            case _ =>
              throw new IllegalArgumentException("primitives passed to SQLContext.histogrammar must have spark.sql.Columns for fill rules")
          }

          case y: AnyQuantity[_, _] => y.quantity match {
            case z: UserFcnFromColumn[_] =>
              z.index = index
              index += 1
              columns += z.col
            case _ =>
              throw new IllegalArgumentException("primitives passed to SQLContext.histogrammar must have spark.sql.Columns for fill rules")
          }

          case _ => // primitive doesn't have a fill rule
        }

        x.children.foreach(gatherColumns)
      }
      gatherColumns(container)

      df.select(columns.result: _*).rdd.aggregate(container)({(h: CONTAINER, d: Row) => h.fill(d); h}, {(h1: CONTAINER, h2: CONTAINER) => h1 + h2})
    }
  }
}

package sparksql.pyspark {
  import scala.language.existentials
  import sparksql._

  class AggregatorConverter {
    type Agg = C forSome {type C <: Container[C] with Aggregation{type Datum >: Row}}

    def average(quantity: Column) = Average[Row](quantity)

    def bag(quantity: Column, range: String) = range match {
      case "N" => Bag[Row, Double](quantity, range)
      case "S" => Bag[Row, String](quantity, range)
      case _ => Bag[Row, Seq[Double]](quantity, range)
    }

    def bin(num: Int, low: Double, high: Double, quantity: Column, value: Agg, underflow: Agg, overflow: Agg, nanflow: Agg) =
      Bin(num, low, high, quantity, value.copy, underflow, overflow, nanflow)

    def branch(i0: Agg) = new Branching(0.0, i0, BranchingNil)
    def branch(i0: Agg, i1: Agg) = new Branching(0.0, i0, new Branching(0.0, i1, BranchingNil))
    def branch(i0: Agg, i1: Agg, i2: Agg) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, BranchingNil)))
    def branch(i0: Agg, i1: Agg, i2: Agg, i3: Agg) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, BranchingNil))))
    def branch(i0: Agg, i1: Agg, i2: Agg, i3: Agg, i4: Agg) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, BranchingNil)))))
    def branch(i0: Agg, i1: Agg, i2: Agg, i3: Agg, i4: Agg, i5: Agg) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, new Branching(0.0, i5, BranchingNil))))))
    def branch(i0: Agg, i1: Agg, i2: Agg, i3: Agg, i4: Agg, i5: Agg, i6: Agg) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, new Branching(0.0, i5, new Branching(0.0, i6, BranchingNil)))))))
    def branch(i0: Agg, i1: Agg, i2: Agg, i3: Agg, i4: Agg, i5: Agg, i6: Agg, i7: Agg) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, new Branching(0.0, i5, new Branching(0.0, i6, new Branching(0.0, i7, BranchingNil))))))))
    def branch(i0: Agg, i1: Agg, i2: Agg, i3: Agg, i4: Agg, i5: Agg, i6: Agg, i7: Agg, i8: Agg) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, new Branching(0.0, i5, new Branching(0.0, i6, new Branching(0.0, i7, new Branching(0.0, i8, BranchingNil)))))))))
    def branch(i0: Agg, i1: Agg, i2: Agg, i3: Agg, i4: Agg, i5: Agg, i6: Agg, i7: Agg, i8: Agg, i9: Agg) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, new Branching(0.0, i5, new Branching(0.0, i6, new Branching(0.0, i7, new Branching(0.0, i8, new Branching(0.0, i9, BranchingNil))))))))))

    def categorize(quantity: Column, value: Agg) = Categorize(quantity, value.copy)

    def centrallyBin(bins: java.lang.Iterable[Double], quantity: Column, value: Agg, nanflow: Agg) = CentrallyBin(bins, quantity, value.copy, nanflow)

    def count() = Count()   // TODO: handle transform

    def deviate(quantity: Column) = Deviate(quantity)

    def fraction(quantity: Column, value: Agg) = Fraction(quantity, value.copy)

    def index(values: java.lang.Iterable[Agg]) = Index(values.toSeq: _*)

    def irregularlyBin(bins: java.lang.Iterable[Double], quantity: Column, value: Agg, nanflow: Agg) = IrregularlyBin(bins, quantity, value.copy, nanflow)

    def label(pairs: java.lang.Iterable[(String, Agg)]) = new Labeling(0.0, pairs.toSeq.asInstanceOf[Seq[(String, Averaging[Row])]]: _*)

    def maximize(quantity: Column) = Maximize(quantity)

    def minimize(quantity: Column) = Minimize(quantity)

    def select(quantity: Column, cut: Agg) = Select(quantity, cut)

    def sparselyBin(binWidth: Double, quantity: Column, value: Agg, nanflow: Agg, origin: Double) = SparselyBin(binWidth, quantity, value.copy, nanflow, origin)

    def stack(bins: java.lang.Iterable[Double], quantity: Column, value: Agg, nanflow: Agg) = Stack(bins, quantity, value.copy, nanflow)

    def sum(quantity: Column) = Sum(quantity)

    def untypedLabel(pairs: java.lang.Iterable[(String, Agg)]) = new UntypedLabeling(0.0, pairs.head.asInstanceOf[(String, Averaging[Row])], pairs.tail.toSeq.asInstanceOf[Seq[(String, Averaging[Row])]]: _*)
  }
}
