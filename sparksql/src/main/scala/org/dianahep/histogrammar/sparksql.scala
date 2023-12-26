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

//import scala.collection.JavaConversions._
import scala.jdk.CollectionConverters._
import scala.language.existentials
import scala.reflect.ClassTag

import org.apache.spark.sql.types.StringType
import org.apache.spark.sql.types.DoubleType
import org.apache.spark.sql.Column
import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.Row

package object sparksql {
  import org.dianahep.histogrammar.util.Compatible

  type Agg = C forSome {type C <: Container[C] with Aggregation{type Datum = Row}}

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

      def gatherColumns(x: Container[_]): Unit = {
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

    def Average(quantity: UserFcn[Row, Double]) = histogrammar(org.dianahep.histogrammar.Average[Row](quantity))

    def Bag[RANGE : ClassTag](quantity: UserFcn[Row, RANGE], range: String = "") = histogrammar(org.dianahep.histogrammar.Bag[Row, RANGE](quantity, range))

    def Bin[V <: Container[V] with Aggregation{type Datum >: Row}, U <: Container[U] with Aggregation{type Datum >: Row}, O <: Container[O] with Aggregation{type Datum >: Row}, N <: Container[N] with Aggregation{type Datum >: Row}](num: Int, low: Double, high: Double, quantity: UserFcn[Row, Double], value: => V = Count(), underflow: U = Count(), overflow: O = Count(), nanflow: N = Count()) = histogrammar(org.dianahep.histogrammar.Bin[Row, V, U, O, N](num, low, high, quantity, value, underflow, overflow, nanflow))

    def Branch[C0 <: Container[C0] with Aggregation](i0: C0) = histogrammar(new Branching(0.0, i0, BranchingNil).asInstanceOf[Agg])
    def Branch[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation](i0: C0, i1: C1)(implicit e01: C0 Compatible C1) = histogrammar(new Branching(0.0, i0, new Branching(0.0, i1, BranchingNil)).asInstanceOf[Agg])
    def Branch[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation](i0: C0, i1: C1, i2: C2)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2) = histogrammar(new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, BranchingNil))).asInstanceOf[Agg])
    def Branch[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3) = histogrammar(new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, BranchingNil)))).asInstanceOf[Agg])
    def Branch[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4) = histogrammar(new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, BranchingNil))))).asInstanceOf[Agg])
    def Branch[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4, e05: C0 Compatible C5) = histogrammar(new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, new Branching(0.0, i5, BranchingNil)))))).asInstanceOf[Agg])
    def Branch[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4, e05: C0 Compatible C5, e06: C0 Compatible C6) = histogrammar(new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, new Branching(0.0, i5, new Branching(0.0, i6, BranchingNil))))))).asInstanceOf[Agg])
    def Branch[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation, C7 <: Container[C7] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4, e05: C0 Compatible C5, e06: C0 Compatible C6, e07: C0 Compatible C7) = histogrammar(new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, new Branching(0.0, i5, new Branching(0.0, i6, new Branching(0.0, i7, BranchingNil)))))))).asInstanceOf[Agg])
    def Branch[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation, C7 <: Container[C7] with Aggregation, C8 <: Container[C8] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7, i8: C8)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4, e05: C0 Compatible C5, e06: C0 Compatible C6, e07: C0 Compatible C7, e08: C0 Compatible C8) = histogrammar(new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, new Branching(0.0, i5, new Branching(0.0, i6, new Branching(0.0, i7, new Branching(0.0, i8, BranchingNil))))))))).asInstanceOf[Agg])
    def Branch[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation, C7 <: Container[C7] with Aggregation, C8 <: Container[C8] with Aggregation, C9 <: Container[C9] with Aggregation](i0: C0, i1: C1, i2: C2, i3: C3, i4: C4, i5: C5, i6: C6, i7: C7, i8: C8, i9: C9)(implicit e01: C0 Compatible C1, e02: C0 Compatible C2, e03: C0 Compatible C3, e04: C0 Compatible C4, e05: C0 Compatible C5, e06: C0 Compatible C6, e07: C0 Compatible C7, e08: C0 Compatible C8, e09: C0 Compatible C9) = histogrammar(new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, new Branching(0.0, i5, new Branching(0.0, i6, new Branching(0.0, i7, new Branching(0.0, i8, new Branching(0.0, i9, BranchingNil)))))))))).asInstanceOf[Agg])

    def Categorize[V <: Container[V] with Aggregation{type Datum >: Row}](quantity: UserFcn[Row, String], value: => V = Count()) = histogrammar(org.dianahep.histogrammar.Categorize[Row, V](quantity, value))

    def CentrallyBin[V <: Container[V] with Aggregation{type Datum >: Row}, N <: Container[N] with Aggregation{type Datum >: Row}](bins: Iterable[Double], quantity: UserFcn[Row, Double], value: => V = Count(), nanflow: N = Count()) = histogrammar(org.dianahep.histogrammar.CentrallyBin[Row, V, N](bins, quantity, value, nanflow))

    def Count(transform: UserFcn[Double, Double] = org.dianahep.histogrammar.Count.Identity) = histogrammar(org.dianahep.histogrammar.Count(transform).asInstanceOf[CONTAINER forSome {type CONTAINER <: Container[CONTAINER] with Aggregation{type Datum = Row}}])

    def Deviate(quantity: UserFcn[Row, Double]) = histogrammar(org.dianahep.histogrammar.Deviate[Row](quantity))

    def Fraction[V <: Container[V] with Aggregation{type Datum >: Row}](quantity: UserFcn[Row, Double], value: => V = Count()) = histogrammar(org.dianahep.histogrammar.Fraction[Row, V](quantity, value))

    def Index[V <: Container[V] with Aggregation](values: V*) = histogrammar(org.dianahep.histogrammar.Index[V](values: _*).asInstanceOf[Agg])

    def IrregularlyBin[V <: Container[V] with Aggregation{type Datum >: Row}, N <: Container[N] with Aggregation{type Datum >: Row}](bins: Iterable[Double], quantity: UserFcn[Row, Double], value: => V = Count(), nanflow: N = Count()) = histogrammar(org.dianahep.histogrammar.IrregularlyBin[Row, V, N](bins, quantity, value, nanflow))

    def Label[V <: Container[V] with Aggregation](pairs: (String, V)*) = histogrammar(org.dianahep.histogrammar.Label[V](pairs: _*).asInstanceOf[Agg])

    def Minimize(quantity: UserFcn[Row, Double]) = histogrammar(org.dianahep.histogrammar.Minimize[Row](quantity))

    def Maximize(quantity: UserFcn[Row, Double]) = histogrammar(org.dianahep.histogrammar.Maximize[Row](quantity))

    def Select[V <: Container[V] with Aggregation{type Datum >: Row}](quantity: UserFcn[Row, Double], cut: V = Count()) = histogrammar(org.dianahep.histogrammar.Select[Row, V](quantity, cut))

    def SparselyBin[V <: Container[V] with Aggregation{type Datum >: Row}, N <: Container[N] with Aggregation{type Datum >: Row}](binWidth: Double, quantity: UserFcn[Row, Double], value: => V = Count(), nanflow: N = Count(), origin: Double = 0.0) = histogrammar(org.dianahep.histogrammar.SparselyBin[Row, V, N](binWidth, quantity, value, nanflow, origin))

    def Stack[V <: Container[V] with Aggregation{type Datum >: Row}, N <: Container[N] with Aggregation{type Datum >: Row}](bins: Iterable[Double], quantity: UserFcn[Row, Double], value: => V = Count(), nanflow: N = Count()) = histogrammar(org.dianahep.histogrammar.Stack[Row, V, N](bins, quantity, value, nanflow))

    def Sum(quantity: UserFcn[Row, Double]) = histogrammar(org.dianahep.histogrammar.Sum[Row](quantity))

    def UntypedLabel[F <: Container[F] with Aggregation](first: (String, F), rest: (String, Container[_] with Aggregation)*) = histogrammar(org.dianahep.histogrammar.UntypedLabel[Row, F](first, rest: _*).asInstanceOf[Agg])
  }
}

package sparksql.pyspark {
  import sparksql._

  class AggregatorConverter {
    type Agg = C forSome {type C <: Container[C] with Aggregation{type Datum >: Row}}

    def histogrammar[CONTAINER <: Container[CONTAINER] with Aggregation{type Datum = Row}](df: DataFrame, container: CONTAINER) = df.histogrammar(container)(ClassTag(container.getClass))

    def Average(quantity: Column) = org.dianahep.histogrammar.Average[Row](quantity)

    def Bag(quantity: Column, range: String) = range match {
      case "N" => org.dianahep.histogrammar.Bag[Row, Double](quantity, range)
      case "S" => org.dianahep.histogrammar.Bag[Row, String](quantity, range)
      case _ => org.dianahep.histogrammar.Bag[Row, Seq[Double]](quantity, range)
    }

    def Bin(num: Int, low: Double, high: Double, quantity: Column, value: Agg, underflow: Agg, overflow: Agg, nanflow: Agg) =
      org.dianahep.histogrammar.Bin(num, low, high, quantity, value.copy, underflow, overflow, nanflow)

    def Branch(i0: Agg) = new Branching(0.0, i0, BranchingNil)
    def Branch(i0: Agg, i1: Agg) = new Branching(0.0, i0, new Branching(0.0, i1, BranchingNil))
    def Branch(i0: Agg, i1: Agg, i2: Agg) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, BranchingNil)))
    def Branch(i0: Agg, i1: Agg, i2: Agg, i3: Agg) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, BranchingNil))))
    def Branch(i0: Agg, i1: Agg, i2: Agg, i3: Agg, i4: Agg) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, BranchingNil)))))
    def Branch(i0: Agg, i1: Agg, i2: Agg, i3: Agg, i4: Agg, i5: Agg) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, new Branching(0.0, i5, BranchingNil))))))
    def Branch(i0: Agg, i1: Agg, i2: Agg, i3: Agg, i4: Agg, i5: Agg, i6: Agg) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, new Branching(0.0, i5, new Branching(0.0, i6, BranchingNil)))))))
    def Branch(i0: Agg, i1: Agg, i2: Agg, i3: Agg, i4: Agg, i5: Agg, i6: Agg, i7: Agg) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, new Branching(0.0, i5, new Branching(0.0, i6, new Branching(0.0, i7, BranchingNil))))))))
    def Branch(i0: Agg, i1: Agg, i2: Agg, i3: Agg, i4: Agg, i5: Agg, i6: Agg, i7: Agg, i8: Agg) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, new Branching(0.0, i5, new Branching(0.0, i6, new Branching(0.0, i7, new Branching(0.0, i8, BranchingNil)))))))))
    def Branch(i0: Agg, i1: Agg, i2: Agg, i3: Agg, i4: Agg, i5: Agg, i6: Agg, i7: Agg, i8: Agg, i9: Agg) = new Branching(0.0, i0, new Branching(0.0, i1, new Branching(0.0, i2, new Branching(0.0, i3, new Branching(0.0, i4, new Branching(0.0, i5, new Branching(0.0, i6, new Branching(0.0, i7, new Branching(0.0, i8, new Branching(0.0, i9, BranchingNil))))))))))

    def Categorize(quantity: Column, value: Agg) = org.dianahep.histogrammar.Categorize(quantity, value.copy)

    def CentrallyBin(bins: scala.collection.Iterable[Double], quantity: Column, value: Agg, nanflow: Agg) = org.dianahep.histogrammar.CentrallyBin(bins, quantity, value.copy, nanflow)

    def Count() = org.dianahep.histogrammar.Count()   // TODO: handle transform

    def Deviate(quantity: Column) = org.dianahep.histogrammar.Deviate(quantity)

    def Fraction(quantity: Column, value: Agg) = org.dianahep.histogrammar.Fraction(quantity, value.copy)

    def Index(values: scala.collection.Iterable[Agg]) = org.dianahep.histogrammar.Index(values.toSeq: _*)

    def IrregularlyBin(bins: scala.collection.Iterable[Double], quantity: Column, value: Agg, nanflow: Agg) = org.dianahep.histogrammar.IrregularlyBin(bins, quantity, value.copy, nanflow)

    def Label(pairs: scala.collection.Iterable[(String, Agg)]) = new Labeling(0.0, pairs.toSeq.asInstanceOf[Seq[(String, Averaging[Row])]]: _*)

    def Maximize(quantity: Column) = org.dianahep.histogrammar.Maximize(quantity)

    def Minimize(quantity: Column) = org.dianahep.histogrammar.Minimize(quantity)

    def Select(quantity: Column, cut: Agg) = org.dianahep.histogrammar.Select(quantity, cut)

    def SparselyBin(binWidth: Double, quantity: Column, value: Agg, nanflow: Agg, origin: Double) = org.dianahep.histogrammar.SparselyBin(binWidth, quantity, value.copy, nanflow, origin)

    def Stack(bins: scala.collection.Iterable[Double], quantity: Column, value: Agg, nanflow: Agg) = org.dianahep.histogrammar.Stack(bins, quantity, value.copy, nanflow)

    def Sum(quantity: Column) = org.dianahep.histogrammar.Sum(quantity)

    def UntypedLabel(pairs: scala.collection.Iterable[(String, Agg)]) = new UntypedLabeling(0.0, pairs.head.asInstanceOf[(String, Averaging[Row])], pairs.tail.toSeq.asInstanceOf[Seq[(String, Averaging[Row])]]: _*)
  }
}
