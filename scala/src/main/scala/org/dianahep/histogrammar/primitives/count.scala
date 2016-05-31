// Copyright 2016 Jim Pivarski
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

package org.dianahep

import org.dianahep.histogrammar.json._
import org.dianahep.histogrammar.util._

package histogrammar {
  //////////////////////////////////////////////////////////////// Count/Counted/Counting

  /** Count data, ignoring their content. (Actually a sum of weights.)
    * 
    * Factory produces mutable [[org.dianahep.histogrammar.Counting]] and immutable [[org.dianahep.histogrammar.Counted]] objects.
    */
  object Count extends Factory {
    val name = "Count"
    val help = "Count data, ignoring their content. (Actually a sum of weights.)"
    val detailedHelp = """Count()"""

    /** Create an immutable [[org.dianahep.histogrammar.Counted]] from arguments (instead of JSON).
      * 
      * @param entries Weighted number of entries (sum of all observed weights).
      */
    def ed(entries: Double) = new Counted(entries)

    /** Create an empty, mutable [[org.dianahep.histogrammar.Counting]].
      * 
      * @param quantity Numerical function to track.
      */
    def apply() = new Counting(0.0)

    /** Synonym for `apply`. */
    def ing = apply()

    /** Use [[org.dianahep.histogrammar.Counted]] in Scala pattern-matching. */
    def unapply(x: Counted) = Some(x.entries)
    /** Use [[org.dianahep.histogrammar.Counting]] in Scala pattern-matching. */
    def unapply(x: Counting) = Some(x.entries)

    import KeySetComparisons._
    def fromJsonFragment(json: Json, nameFromParent: Option[String]): Container[_] with NoAggregation = json match {
      case JsonFloat(entries) => new Counted(entries)
      case _ => throw new JsonFormatException(json, name)
    }

    // // Confidence interval formulas for counting statistics (e.g. bin-by-bin).
    // // For more information, see https://www.ine.pt/revstat/pdf/rs120203.pdf

    // /** Compute the normal confidence interval on `number` (where `number` is a count of events in a Poisson process).
    //   * 
    //   * The normal confidence interval is `number + z * sqrt(number)`. It has good properties for large numbers.
    //   * 
    //   * @param z Informally, the "number of sigmas." Formally, `z` is the `1 - alpha/2` quantile of the standard normal distribution, where `alpha` is the error quantile. For example, for a 95% confidence level, the error (`alpha`) is 5%, so z = 1.96. Note that `z` is signed. `z = 0` yields the central value, `z = -1` yields "one sigma" below the central value, and `z = 1` yields "one sigma" above the central value.
    //   */
    // def normalConfidenceInterval(number: Double, z: Double): Double = normalConfidenceInterval(number, List(z): _*).head

    // /** Compute the normal confidence interval on `number` (where `number` is a count of events in a Poisson process).
    //   * 
    //   * The normal confidence interval is `number + z * sqrt(number)`. It has good properties for large numbers.
    //   * 
    //   * @param z Informally, the "number of sigmas." Formally, `z` is the `1 - alpha/2` quantile of the standard normal distribution, where `alpha` is the error quantile. For example, for a 95% confidence level, the error (`alpha`) is 5%, so z = 1.96. Note that `z` is signed. `z = 0` yields the central value, `z = -1` yields "one sigma" below the central value, and `z = 1` yields "one sigma" above the central value.
    //   */
    // def normalConfidenceInterval(number: Double, zs: Double*): Seq[Double] =
    //   zs.map(z => number + z * Math.sqrt(number))

    // /** Compute the Wilson confidence interval on `number` (where `number` is a count of events in a Poisson process).
    //   * 
    //   * The Wilson confidence interval is `number * (1 - 1/(9*number) + z / (3 * sqrt(number)))**3`. It has good properties for small integers, but not for numbers below one.
    //   * 
    //   * @param z Informally, the "number of sigmas." Formally, `z` is the `1 - alpha/2` quantile of the standard normal distribution, where `alpha` is the error quantile. For example, for a 95% confidence level, the error (`alpha`) is 5%, so z = 1.96. Note that `z` is signed. `z = 0` yields the central value, `z = -1` yields "one sigma" below the central value, and `z = 1` yields "one sigma" above the central value.
    //   */
    // def wilsonConfidenceInterval(number: Double, z: Double): Double = wilsonConfidenceInterval(number, List(z): _*).head

    // /** Compute the Wilson confidence interval on `number` (where `number` is a count of events in a Poisson process).
    //   * 
    //   * The Wilson confidence interval is `number * (1 - 1/(9*number) + z / (3 * sqrt(number)))**3`. It has good properties for small integers, but not for numbers below one.
    //   * 
    //   * @param z Informally, the "number of sigmas." Formally, `z` is the `1 - alpha/2` quantile of the standard normal distribution, where `alpha` is the error quantile. For example, for a 95% confidence level, the error (`alpha`) is 5%, so z = 1.96. Note that `z` is signed. `z = 0` yields the central value, `z = -1` yields "one sigma" below the central value, and `z = 1` yields "one sigma" above the central value.
    //   */
    // def wilsonConfidenceInterval(number: Double, zs: Double*): Seq[Double] =
    //   zs.map(z => number * Math.pow(1.0 - 1.0/(9.0*number) + z / (3.0 * Math.sqrt(number)), 3))

  }

  /** An accumulated count (sum of weights) of data, ignoring its content.
    * 
    * Use the factory [[org.dianahep.histogrammar.Count]] to construct an instance.
    * 
    * @param entries Weighted number of entries (sum of all weights).
    */
  class Counted private[histogrammar](val entries: Double) extends Container[Counted] with NoAggregation {
    type Type = Counted
    type EdType = Counted
    def factory = Count

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def zero = new Counted(0.0)
    def +(that: Counted): Counted = new Counted(this.entries + that.entries)

    def children = Nil

    def toJsonFragment(suppressName: Boolean) = JsonFloat(entries)

    override def toString() = s"<Counted $entries>"
    override def equals(that: Any) = that match {
      case that: Counted => this.entries == that.entries
      case _ => false
    }
    override def hashCode() = entries.hashCode
  }

  /** Accumulating a count (sum of weights) of data, ignoring its content.
    * 
    * Use the factory [[org.dianahep.histogrammar.Count]] to construct an instance.
    * 
    * This is the only container with [[org.dianahep.histogrammar.Aggregation]] that doesn't have a configurable data type: its `Datum` is `Any`. It is primarily for the sake of this container that `Aggregation` is contravariant.
    * 
    * @param entries Weighted number of entries (sum of all observed weights).
    */
  class Counting private[histogrammar](var entries: Double) extends Container[Counting] with Aggregation {
    type Type = Counting
    type EdType = Counted
    type Datum = Any
    def factory = Count

    if (entries < 0.0)
      throw new ContainerException(s"entries ($entries) cannot be negative")

    def zero = new Counting(0.0)
    def +(that: Counting): Counting = new Counting(this.entries + that.entries)

    def fill[SUB <: Any](datum: SUB, weight: Double = 1.0) {
      // no possibility of exception from here on out (for rollback)
      if (weight > 0.0)
        entries += weight
    }

    def children = Nil

    def toJsonFragment(suppressName: Boolean) = JsonFloat(entries)

    override def toString() = s"<Counting $entries>"
    override def equals(that: Any) = that match {
      case that: Counting => this.entries == that.entries
      case _ => false
    }
    override def hashCode() = entries.hashCode
  }
}




// Note on statistics:

// Could follow http://statpages.info/confint.html to provide exact Clopper-Pearson bounds for Poisson (Count) and Binomial (Fraction) confidence levels.

// Inefficient calculation (JavaScript) that assumes the number of entries is an integer (and scales with it!):

// var vTL=2.5; var vTU=2.5; var vCL=95

// function CalcCL(form) {
//     vTL = eval(form.TL.value)
//     vTU = eval(form.TU.value)
//     vCL = 100-(vTL+vTU)
//     form.CL.value = ''+vCL
// }

// function CalcTails(form) {
//     vCL = eval(form.CL.value)
//     vTU = (100-vCL)/2
//     vTL = vTU
//     form.TL.value = ''+vTL
//     form.TU.value = ''+vTU
// }

// function CalcBin(form) {
//     var vx = eval(form.x.value)
//     var vN = eval(form.N.value)
//     var vP = vx/vN
//     form.P.value = Fmt(vP)
//     if(vx==0) {
//         form.DL.value = "0.0000"
//     }
//     else {
//         var v=vP/2;
//         vsL=0;
//         vsH=vP;
//         var p=vTL/100;
//         while ((vsH-vsL)>1e-5) {
//             if(BinP(vN,v,vx,vN)>p) {
//                 vsH=v;
//                 v=(vsL+v)/2
//             }
//             else {
//                 vsL=v;
//                 v=(v+vsH)/2
//             }
//         }
//         form.DL.value = Fmt(v)
//     }
//     if(vx==vN) {
//         form.DU.value = "1.0000"
//     }
//     else {
//         var v=(1+vP)/2;
//         vsL=vP;
//         vsH=1;
//         var p=vTU/100
//         while ((vsH-vsL)>1e-5) {
//             if (BinP(vN,v,0,vx)<p) {
//                 vsH=v;
//                 v=(vsL+v)/2
//             }
//             else {
//                 vsL=v;
//                 v=(v+vsH)/2
//             }
//         }
//         form.DU.value = Fmt(v)
//     }
// }

// function BinP(N,p,x1,x2) {
//     var q=p/(1-p);
//     var k=0;
//     var v = 1;
//     var s=0;
//     var tot=0
//     while (k<=N) {
//         tot=tot+v
//         if (k>=x1 & k<=x2) {
//             s=s+v
//         }
//         if (tot>1e30) {
//             s=s/1e30;
//             tot=tot/1e30;
//             v=v/1e30
//         }
//         k=k+1;
//         v=v*q*(N+1-k)/k
//     }
//     return s/tot
// }

// function CalcPois(form) {
//     var vZ = eval(form.Z.value)
//     if(vZ==0) {
//         form.QL.value = "0.0000"
//     }
//     else {
//         var v=0.5;
//         var dv=0.5;
//         var p=vTL/100
//         while (dv>1e-7) {
//             dv=dv/2;
//             if (PoisP((1+vZ)*v/(1-v),vZ,1e10)>p) {
//                 v=v-dv
//             }
//             else {
//                 v=v+dv
//             }
//         }
//         form.QL.value = Fmt((1+vZ)*v/(1-v))
//     }
//     if(vTU==0) {
//         form.QU.value = "Infinity"
//     }
//     else {
//         var v=0.5;
//         var dv=0.5;
//         var p=vTU/100
//         while (dv>1e-7) {
//             dv=dv/2;
//             if (PoisP((1+vZ)*v/(1-v),0,vZ)<p) {
//                 v=v-dv
//             }
//             else {
//                 v=v+dv
//             }
//         }
//         form.QU.value = Fmt((1+vZ)*v/(1-v))
//     }
// }

// function PoisP(Z,x1,x2) {
//     var q=1; var tot=0; var s=0; var k=0
//     while(k<Z || q>(tot*1e-10)) {
//         tot=tot+q
//         if(k>=x1 & k<=x2) {
//             s=s+q
//         }
//         if (tot>1e30) {
//             s=s/1e30;
//             tot=tot/1e30;
//             q=q/1e30
//         }
//         k=k+1;
//         q=q*Z/k
//     }
//     return s/tot
// }

// function Fmt(x) { 
//     var v
//     if (x>=0) {
//         v=''+(x+0.00005)
//     }
//     else {
//         v=''+(x-0.00005)
//     }
//     return v.substring(0,v.indexOf('.')+5)
// }
