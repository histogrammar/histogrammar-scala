package org.dianahep.histogrammar

import org.apache.spark.sql.Column
import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.Row

// val df = sqlContext.read.load("taus_from_WW_13TeV_pythia8")


package sparksql {

}

package object sparksql {
  implicit class UserFcnFromColumn[+RANGE](val col: Column) extends UserFcn[, RANGE] {

  }

  implicit class DataFrameHistogrammarMethods(df: DataFrame) {
  }
}
