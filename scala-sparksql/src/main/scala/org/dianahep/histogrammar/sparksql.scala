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
