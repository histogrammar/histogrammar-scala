package org.dianahep

import scala.language.existentials

import scala.collection.immutable.ListMap
import scala.language.implicitConversions

import org.dianahep.histogrammar.json._

package histogrammar {
  class ContainerException(message: String, cause: Exception = null) extends Exception(message, cause)

  //////////////////////////////////////////////////////////////// general definition of an container, its factory, and mix-in

  // creates containers (from arguments or JSON)
  trait Factory {
    def name: String
    def help: String
    def detailedHelp: String
    def fromJsonFragment(json: Json): Container[_]
  }
  object Factory {
    private var known = ListMap[String, Factory]()

    def registered = known

    def register(factory: Factory) {
      known = known.updated(factory.name, factory)
    }

    register(Count)
    register(Sum)
    register(Average)
    register(Deviate)
    register(AbsoluteErr)
    register(Minimize)
    register(Maximize)
    register(Quantile)

    register(Bag)

    register(Bin)
    register(SparselyBin)
    register(CentrallyBin)
    register(AdaptivelyBin)
    register(Fraction)
    register(Stack)
    register(Partition)
    register(Categorize)

    register(Label)
    register(UntypedLabel)
    register(Index)
    register(Branch)

    def apply(name: String) = known.get(name) match {
      case Some(x) => x
      case None => throw new ContainerException(s"unrecognized container (is it a custom container that hasn't been registered?): $name")
    }

    def fromJson(str: String): Container[_] = Json.parse(str) match {
      case Some(json) => fromJson(json)
      case None => throw new InvalidJsonException(str)
    }

    def fromJson(json: Json): Container[_] = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("type", "data")) =>
        val get = pairs.toMap

        val name = get("type") match {
          case JsonString(x) => x
          case x => throw new JsonFormatException(x, "type")
        }

        Factory(name).fromJsonFragment(get("data"))

      case _ => throw new JsonFormatException(json, "Factory")
    }
  }

  // container of data that is, by itself, immutable
  trait Container[CONTAINER <: Container[CONTAINER]] extends Serializable {
    type Type
    def factory: Factory

    def entries: Double                  // Double because it's a sum of weights, not a integral count
    def zero: CONTAINER                  // empty container with the same parameters
    def +(that: CONTAINER): CONTAINER    // an immutable operation
    def copy = this + zero               // easy way to guarantee a new copy; can overload with more efficient methods

    def toJson: Json = JsonObject("type" -> JsonString(factory.name), "data" -> toJsonFragment)
    def toJsonFragment: Json
    def as[OTHER <: Container[OTHER]] = this.asInstanceOf[OTHER]
  }

  // mix-in to provide aggregation (and hence mutability)
  trait Aggregation {
    type Datum                           // more flexible as an abstract type than a type parameter (implemented as contravariant)

    def entries_=(x: Double)             // "entries" is a var, not val, for Containers with Aggregation
    def fill(datum: Datum) {             // fill and fillWeighted change the container in-place
      fillWeighted(datum, 1.0)
    }
    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double)   // see? fillWeighed accepts a subtype of Datum: CONTRAvariant
  }

  trait AggregationOnData extends Aggregation   // everything except Count, which ignores the data (its Datum is Any)
}

package object histogrammar {
  def help = Factory.registered map {case (name, factory) => f"${name}%-15s ${factory.help}"} mkString("\n")

  def increment[CONTAINER <: Container[CONTAINER] with Aggregation] = {(h: CONTAINER, x: h.Datum) => h.fill(x); h}
  def combine[CONTAINER <: Container[CONTAINER]] = {(h1: CONTAINER, h2: CONTAINER) => h1 + h2}

  def incrementUntypedLabel[DATUM] = {(h: UntypedLabeling[DATUM], x: DATUM) => h.fill(x); h}
  def combineUntypedLabel[DATUM] = {(h1: UntypedLabeling[DATUM], h2: UntypedLabeling[DATUM]) => h1 + h2}

  //////////////////////////////////////////////////////////////// define implicits

  implicit class Selection[-DATUM](f: DATUM => Double) extends Serializable {
    def apply[SUB <: DATUM](x: SUB): Double = f(x)
  }
  implicit def booleanToSelection[DATUM](f: DATUM => Boolean) = Selection({x: DATUM => if (f(x)) 1.0 else 0.0})
  implicit def byteToSelection[DATUM](f: DATUM => Byte) = Selection({x: DATUM => f(x).toDouble})
  implicit def shortToSelection[DATUM](f: DATUM => Short) = Selection({x: DATUM => f(x).toDouble})
  implicit def intToSelection[DATUM](f: DATUM => Int) = Selection({x: DATUM => f(x).toDouble})
  implicit def longToSelection[DATUM](f: DATUM => Long) = Selection({x: DATUM => f(x).toDouble})
  implicit def floatToSelection[DATUM](f: DATUM => Float) = Selection({x: DATUM => f(x).toDouble})

  def unweighted[DATUM] = new Selection[DATUM]({x: DATUM => 1.0})

  implicit class NumericalFcn[-DATUM](f: DATUM => Double) extends Serializable {
    def apply[SUB <: DATUM](x: SUB): Double = f(x)
  }
  implicit def byteToNumericalFcn[DATUM](f: DATUM => Byte) = NumericalFcn({x: DATUM => f(x).toDouble})
  implicit def shortToNumericalFcn[DATUM](f: DATUM => Short) = NumericalFcn({x: DATUM => f(x).toDouble})
  implicit def intToNumericalFcn[DATUM](f: DATUM => Int) = NumericalFcn({x: DATUM => f(x).toDouble})
  implicit def longToNumericalFcn[DATUM](f: DATUM => Long) = NumericalFcn({x: DATUM => f(x).toDouble})
  implicit def floatToNumericalFcn[DATUM](f: DATUM => Float) = NumericalFcn({x: DATUM => f(x).toDouble})

  implicit class CategoricalFcn[-DATUM](f: DATUM => String) extends Serializable {
    def apply[SUB <: DATUM](x: SUB): String = f(x)
  }

  implicit class MultivariateFcn[-DATUM](f: DATUM => Iterable[Double]) extends Serializable {
    def apply[SUB <: DATUM](x: SUB): Vector[Double] = f(x).toVector
  }
  implicit def scalarToMultivariateFcn[DATUM](f: DATUM => Double) = MultivariateFcn({x: DATUM => Vector(f(x))})

  // used to check floating-point equality with a new rule: NaN == NaN
  implicit class nanEquality(val x: Double) extends AnyVal {
    def ===(that: Double) = (this.x.isNaN  &&  that.isNaN)  ||  this.x == that
  }

  // provide i0 through i9 as though they were members of Branched
  implicit class Branched0[C0 <: Container[C0], TAIL <: BranchedList](x: Branched[C0, TAIL]) {
    def i0 = x.head
  }
  implicit class Branched1[C0 <: Container[C0], C1 <: Container[C1], TAIL <: BranchedList](x: Branched[C0, Branched[C1, TAIL]]) {
    def i1 = x.tail.head
  }
  implicit class Branched2[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, TAIL]]]) {
    def i2 = x.tail.tail.head
  }
  implicit class Branched3[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, Branched[C3, TAIL]]]]) {
    def i3 = x.tail.tail.tail.head
  }
  implicit class Branched4[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, Branched[C3, Branched[C4, TAIL]]]]]) {
    def i4 = x.tail.tail.tail.tail.head
  }
  implicit class Branched5[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, Branched[C3, Branched[C4, Branched[C5, TAIL]]]]]]) {
    def i5 = x.tail.tail.tail.tail.tail.head
  }
  implicit class Branched6[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, Branched[C3, Branched[C4, Branched[C5, Branched[C6, TAIL]]]]]]]) {
    def i6 = x.tail.tail.tail.tail.tail.tail.head
  }
  implicit class Branched7[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, Branched[C3, Branched[C4, Branched[C5, Branched[C6, Branched[C7, TAIL]]]]]]]]) {
    def i7 = x.tail.tail.tail.tail.tail.tail.tail.head
  }
  implicit class Branched8[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, Branched[C3, Branched[C4, Branched[C5, Branched[C6, Branched[C7, Branched[C8, TAIL]]]]]]]]]) {
    def i8 = x.tail.tail.tail.tail.tail.tail.tail.tail.head
  }
  implicit class Branched9[C0 <: Container[C0], C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], TAIL <: BranchedList](x: Branched[C0, Branched[C1, Branched[C2, Branched[C3, Branched[C4, Branched[C5, Branched[C6, Branched[C7, Branched[C8, Branched[C9, TAIL]]]]]]]]]]) {
    def i9 = x.tail.tail.tail.tail.tail.tail.tail.tail.tail.head
  }

  // provide i0 through i9 as though they were members of Branching
  implicit class Branching0[C0 <: Container[C0] with Aggregation, TAIL <: BranchingList](x: Branching[C0, TAIL]) {
    def i0 = x.head
  }
  implicit class Branching1[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, TAIL <: BranchingList](x: Branching[C0, Branching[C1, TAIL]]) {
    def i1 = x.tail.head
  }
  implicit class Branching2[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, TAIL <: BranchingList](x: Branching[C0, Branching[C1, Branching[C2, TAIL]]]) {
    def i2 = x.tail.tail.head
  }
  implicit class Branching3[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, TAIL <: BranchingList](x: Branching[C0, Branching[C1, Branching[C2, Branching[C3, TAIL]]]]) {
    def i3 = x.tail.tail.tail.head
  }
  implicit class Branching4[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, TAIL <: BranchingList](x: Branching[C0, Branching[C1, Branching[C2, Branching[C3, Branching[C4, TAIL]]]]]) {
    def i4 = x.tail.tail.tail.tail.head
  }
  implicit class Branching5[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, TAIL <: BranchingList](x: Branching[C0, Branching[C1, Branching[C2, Branching[C3, Branching[C4, Branching[C5, TAIL]]]]]]) {
    def i5 = x.tail.tail.tail.tail.tail.head
  }
  implicit class Branching6[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation, TAIL <: BranchingList](x: Branching[C0, Branching[C1, Branching[C2, Branching[C3, Branching[C4, Branching[C5, Branching[C6, TAIL]]]]]]]) {
    def i6 = x.tail.tail.tail.tail.tail.tail.head
  }
  implicit class Branching7[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation, C7 <: Container[C7] with Aggregation, TAIL <: BranchingList](x: Branching[C0, Branching[C1, Branching[C2, Branching[C3, Branching[C4, Branching[C5, Branching[C6, Branching[C7, TAIL]]]]]]]]) {
    def i7 = x.tail.tail.tail.tail.tail.tail.tail.head
  }
  implicit class Branching8[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation, C7 <: Container[C7] with Aggregation, C8 <: Container[C8] with Aggregation, TAIL <: BranchingList](x: Branching[C0, Branching[C1, Branching[C2, Branching[C3, Branching[C4, Branching[C5, Branching[C6, Branching[C7, Branching[C8, TAIL]]]]]]]]]) {
    def i8 = x.tail.tail.tail.tail.tail.tail.tail.tail.head
  }
  implicit class Branching9[C0 <: Container[C0] with Aggregation, C1 <: Container[C1] with Aggregation, C2 <: Container[C2] with Aggregation, C3 <: Container[C3] with Aggregation, C4 <: Container[C4] with Aggregation, C5 <: Container[C5] with Aggregation, C6 <: Container[C6] with Aggregation, C7 <: Container[C7] with Aggregation, C8 <: Container[C8] with Aggregation, C9 <: Container[C9] with Aggregation, TAIL <: BranchingList](x: Branching[C0, Branching[C1, Branching[C2, Branching[C3, Branching[C4, Branching[C5, Branching[C6, Branching[C7, Branching[C8, Branching[C9, TAIL]]]]]]]]]]) {
    def i9 = x.tail.tail.tail.tail.tail.tail.tail.tail.tail.head
  }

}
