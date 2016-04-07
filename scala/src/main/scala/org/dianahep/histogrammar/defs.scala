package org.dianahep

import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.language.existentials
import scala.language.implicitConversions

import org.dianahep.histogrammar.json._

package histogrammar {
  class AggregatorException(message: String, cause: Exception = null) extends Exception(message, cause)

  //////////////////////////////////////////////////////////////// data model (user's data are implicitly converted to this)

  case class Weighted[DATUM](datum: DATUM, weight: Double = 1.0)

  //////////////////////////////////////////////////////////////// general definition of an container/aggregator

  // creates containers (from arguments or JSON) and aggregators (from arguments)
  trait Factory {
    def name: String
    def fromJsonFragment(json: Json): Container[_]
  }
  object Factory {
    private var known = scala.collection.immutable.Map[String, Factory]()

    def registered = known.keys.toList

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
    register(Bin)
    register(SparselyBin)
    register(Fraction)
    register(Stack)
    register(Partition)
    register(Categorize)
    register(NameMap)
    register(Tuple)

    def apply(name: String) = known.get(name) match {
      case Some(x) => x
      case None => throw new AggregatorException(s"unrecognized aggregator (is it a custom aggregator that hasn't been registered?): $name")
    }

    def fromJson[CONTAINER <: Container[CONTAINER]](str: String): CONTAINER = Json.parse(str) match {
      case Some(json) => fromJson[CONTAINER](json)
      case None => throw new InvalidJsonException(str)
    }

    def fromJson[CONTAINER <: Container[CONTAINER]](json: Json): CONTAINER = json match {
      case JsonObject(pairs @ _*) if (pairs.keySet == Set("type", "data")) =>
        val get = pairs.toMap

        val name = get("type") match {
          case JsonString(x) => x
          case x => throw new JsonFormatException(x, "type")
        }

        Factory(name).fromJsonFragment(get("data")).asInstanceOf[CONTAINER]

      case _ => throw new JsonFormatException(json, "Factory")
    }
  }

  // immutable container of data
  trait Container[CONTAINER <: Container[CONTAINER]] extends Serializable {
    def factory: Factory

    def +(that: CONTAINER): CONTAINER

    def toJson: Json = JsonObject("type" -> JsonString(factory.name), "data" -> toJsonFragment)
    def toJsonFragment: Json
  }

  // mix-in to add mutability
  trait Aggregation[DATUM] {
    def fill(x: DATUM) {
      fillWeighted(Weighted(x))
    }
    def fillWeighted(x: Weighted[DATUM])
  }
}

package object histogrammar {
  //////////////////////////////////////////////////////////////// define implicits

  implicit class Selection[DATUM](f: DATUM => Double) {
    def apply(x: DATUM) = f(x)
  }
  implicit def booleanToSelection[DATUM](f: DATUM => Boolean) = Selection({x: DATUM => if (f(x)) 1.0 else 0.0})
  implicit def byteToSelection[DATUM](f: DATUM => Byte) = Selection({x: DATUM => f(x).toDouble})
  implicit def shortToSelection[DATUM](f: DATUM => Short) = Selection({x: DATUM => f(x).toDouble})
  implicit def intToSelection[DATUM](f: DATUM => Int) = Selection({x: DATUM => f(x).toDouble})
  implicit def longToSelection[DATUM](f: DATUM => Long) = Selection({x: DATUM => f(x).toDouble})
  implicit def floatToSelection[DATUM](f: DATUM => Float) = Selection({x: DATUM => f(x).toDouble})

  def unweighted[DATUM] = Selection[DATUM]({x: DATUM => 1.0})

  implicit class NumericalFcn[DATUM](f: DATUM => Double) {
    def apply(x: DATUM) = f(x)
  }
  implicit def byteToNumericalFcn[DATUM](f: DATUM => Byte) = NumericalFcn({x: DATUM => f(x).toDouble})
  implicit def shortToNumericalFcn[DATUM](f: DATUM => Short) = NumericalFcn({x: DATUM => f(x).toDouble})
  implicit def intToNumericalFcn[DATUM](f: DATUM => Int) = NumericalFcn({x: DATUM => f(x).toDouble})
  implicit def longToNumericalFcn[DATUM](f: DATUM => Long) = NumericalFcn({x: DATUM => f(x).toDouble})
  implicit def floatToNumericalFcn[DATUM](f: DATUM => Float) = NumericalFcn({x: DATUM => f(x).toDouble})

  implicit class CategoricalFcn[DATUM](f: DATUM => String) {
    def apply(x: DATUM) = f(x)
  }

  // used to check floating-point equality with a new rule: NaN == NaN
  implicit class nanEquality(val x: Double) extends AnyVal {
    def ===(that: Double) = (this.x.isNaN  &&  that.isNaN)  ||  this.x == that
  }

  def increment[DATUM, CONTAINER <: Container[CONTAINER]](zero: CONTAINER with Aggregation[DATUM]) =
    {(h: CONTAINER with Aggregation[DATUM], x: DATUM) => h.fill(x); h}

  def combine[DATUM, CONTAINER <: Container[CONTAINER]](zero: CONTAINER with Aggregation[DATUM]) =
    {(h1: CONTAINER with Aggregation[DATUM], h2: CONTAINER with Aggregation[DATUM]) => h1 + h2}


  // // Scala maps become NameMaps
  // implicit def mapToNameMapped(map: Map[String, Container[_]]) = new NameMapped(map.toSeq: _*)
  // implicit def mapToNameMapping[DATUM](map: Map[String, Container[_] with Aggregation[DATUM]]) = new NameMapping(map.toSeq: _*)

  // // Scala tuples become Tupled
  // implicit def tupleToTupled2[C1 <: Container[C1], C2 <: Container[C2]](x: Tuple2[C1, C2]) = new Tupled2(x._1, x._2)
  // implicit def tupleToTupled3[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](x: Tuple3[C1, C2, C3]) = new Tupled3(x._1, x._2, x._3)
  // implicit def tupleToTupled4[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](x: Tuple4[C1, C2, C3, C4]) = new Tupled4(x._1, x._2, x._3, x._4)
  // implicit def tupleToTupled5[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](x: Tuple5[C1, C2, C3, C4, C5]) = new Tupled5(x._1, x._2, x._3, x._4, x._5)
  // implicit def tupleToTupled6[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](x: Tuple6[C1, C2, C3, C4, C5, C6]) = new Tupled6(x._1, x._2, x._3, x._4, x._5, x._6)
  // implicit def tupleToTupled7[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](x: Tuple7[C1, C2, C3, C4, C5, C6, C7]) = new Tupled7(x._1, x._2, x._3, x._4, x._5, x._6, x._7)
  // implicit def tupleToTupled8[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](x: Tuple8[C1, C2, C3, C4, C5, C6, C7, C8]) = new Tupled8(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8)
  // implicit def tupleToTupled9[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](x: Tuple9[C1, C2, C3, C4, C5, C6, C7, C8, C9]) = new Tupled9(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9)
  // implicit def tupleToTupled10[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], C10 <: Container[C10]](x: Tuple10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]) = new Tupled10(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10)

  // // Scala tuples become Tupling
  // implicit def tupleToTupling2[DATUM, C1 <: Container[C1], C2 <: Container[C2]](x: Tuple2[C1, C2]) = new Tupling2[DATUM, C1, C2](x._1, x._2)
  // implicit def tupleToTupling3[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](x: Tuple3[C1, C2, C3]) = new Tupling3[DATUM, C1, C2, C3](x._1, x._2, x._3)
  // implicit def tupleToTupling4[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](x: Tuple4[C1, C2, C3, C4]) = new Tupling4[DATUM, C1, C2, C3, C4](x._1, x._2, x._3, x._4)
  // implicit def tupleToTupling5[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](x: Tuple5[C1, C2, C3, C4, C5]) = new Tupling5[DATUM, C1, C2, C3, C4, C5](x._1, x._2, x._3, x._4, x._5)
  // implicit def tupleToTupling6[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](x: Tuple6[C1, C2, C3, C4, C5, C6]) = new Tupling6[DATUM, C1, C2, C3, C4, C5, C6](x._1, x._2, x._3, x._4, x._5, x._6)
  // implicit def tupleToTupling7[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](x: Tuple7[C1, C2, C3, C4, C5, C6, C7]) = new Tupling7[DATUM, C1, C2, C3, C4, C5, C6, C7](x._1, x._2, x._3, x._4, x._5, x._6, x._7)
  // implicit def tupleToTupling8[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](x: Tuple8[C1, C2, C3, C4, C5, C6, C7, C8]) = new Tupling8[DATUM, C1, C2, C3, C4, C5, C6, C7, C8](x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8)
  // implicit def tupleToTupling9[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](x: Tuple9[C1, C2, C3, C4, C5, C6, C7, C8, C9]) = new Tupling9[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9](x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9)
  // implicit def tupleToTupling10[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], C10 <: Container[C10]](x: Tuple10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]) = new Tupling10[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10](x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10)
}
