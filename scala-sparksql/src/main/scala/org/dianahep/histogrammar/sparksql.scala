package org.dianahep.histogrammar

import scala.reflect.ClassTag

import org.apache.spark.sql.types.StringType
import org.apache.spark.sql.types.DoubleType
import org.apache.spark.sql.Column
import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.Row

package object sparksql {
  implicit class UserFcnFromColumn[+RANGE](var col: Column) extends UserFcn[Row, RANGE] {
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
              z.col = null  // because Columns are not Serializable
            case _ =>
              throw new IllegalArgumentException("primitives passed to SQLContext.histogrammar must have spark.sql.Columns for fill rules")
          }

          case y: CategoricalQuantity[_] => y.quantity match {
            case z: UserFcnFromColumn[_] =>
              z.index = index
              index += 1
              columns += z.col.cast(StringType)
              z.col = null  // because Columns are not Serializable
            case _ =>
              throw new IllegalArgumentException("primitives passed to SQLContext.histogrammar must have spark.sql.Columns for fill rules")
          }

          case y: AnyQuantity[_, _] => y.quantity match {
            case z: UserFcnFromColumn[_] =>
              z.index = index
              index += 1
              columns += z.col
              z.col = null  // because Columns are not Serializable
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
