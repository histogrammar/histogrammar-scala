package org.dianahep

import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.language.existentials
import scala.language.implicitConversions

import org.dianahep.histogrammar.json._

package histogrammar {
  class AggregatorException(message: String, cause: Exception = null) extends Exception(message, cause)

  //////////////////////////////////////////////////////////////// data model (user's data are implicitly converted to this)

  case class Weighted[DATUM](datum: DATUM, weight: Double = 1.0) {
    def reweight(w: Double): Weighted[DATUM] = copy(weight = weight*w)
    def contributes = weight > 0.0
  }

  case class Selection[DATUM](f: Weighted[DATUM] => Double) extends Function1[Weighted[DATUM], Double] {
    def apply(x: Weighted[DATUM]) = f(x)
  }

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

    def apply(name: String) = known.get(name) match {
      case Some(x) => x
      case None => throw new AggregatorException(s"unrecognized aggregator (is it a custom aggregator that hasn't been registered?): $name")
    }

    def fromJson[CONTAINER <: Container[CONTAINER]](str: String): CONTAINER = Json.parse(str) match {
      case Some(json) => fromJson(json).asInstanceOf[CONTAINER]
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

  // immutable container of data; the result of an aggregation
  trait Container[CONTAINER <: Container[CONTAINER]] extends Serializable {
    def factory: Factory

    def +(that: CONTAINER): CONTAINER

    def toJson: Json = JsonObject("type" -> JsonString(factory.name), "data" -> toJsonFragment)
    def toJsonFragment: Json
  }

  // mutable aggregator of data; produces a container
  trait Aggregator[DATUM, CONTAINER <: Container[CONTAINER]] extends Serializable {
    def factory: Factory

    def fill(x: Weighted[DATUM])

    def fix: CONTAINER

    // provide the same methods as Container by passing everything through fix
    def +(that: CONTAINER) = fix.+(that)
    def +(that: Aggregator[DATUM, CONTAINER]) = fix.+(that.fix)
    def toJson = fix.toJson
    def toJsonFragment = fix.toJsonFragment
  }
}

package object histogrammar {
  //////////////////////////////////////////////////////////////// register container factories

  Factory.register(Count)
  Factory.register(Sum)
  Factory.register(Average)
  Factory.register(Deviate)
  Factory.register(Bin)
  Factory.register(SparselyBin)
  Factory.register(Map)
  Factory.register(Tuple)

  //////////////////////////////////////////////////////////////// define implicits

  type NumericalFcn[DATUM] = Weighted[DATUM] => Double
  type NumericalFcn2D[DATUM] = Weighted[DATUM] => (Double, Double)
  type NumericalFcn3D[DATUM] = Weighted[DATUM] => (Double, Double, Double)
  type NumericalFcnND[DATUM] = Weighted[DATUM] => Seq[Double]
  type CategoricalFcn[DATUM] = Weighted[DATUM] => String

  def unweighted[DATUM] = Selection({x: Weighted[DATUM] => 1.0})

  implicit def countingTypeDoesntMatter[DATUM](counting: Counting[_]) = counting.asInstanceOf[Counting[DATUM]]

  implicit def toWeighted[DATUM](datum: DATUM) = Weighted(datum)
  implicit def domainToWeighted[DOMAIN, RANGE](f: DOMAIN => RANGE) = {x: Weighted[DOMAIN] => f(x.datum)}

  implicit def booleanToSelection[DATUM](f: DATUM => Boolean) = Selection({x: Weighted[DATUM] => if (f(x.datum)) 1.0 else 0.0})
  implicit def byteToSelection[DATUM](f: DATUM => Byte) = Selection({x: Weighted[DATUM] => f(x.datum).toDouble})
  implicit def shortToSelection[DATUM](f: DATUM => Short) = Selection({x: Weighted[DATUM] => f(x.datum).toDouble})
  implicit def intToSelection[DATUM](f: DATUM => Int) = Selection({x: Weighted[DATUM] => f(x.datum).toDouble})
  implicit def longToSelection[DATUM](f: DATUM => Long) = Selection({x: Weighted[DATUM] => f(x.datum).toDouble})
  implicit def floatToSelection[DATUM](f: DATUM => Float) = Selection({x: Weighted[DATUM] => f(x.datum).toDouble})
  implicit def doubleToSelection[DATUM](f: DATUM => Double) = Selection({x: Weighted[DATUM] => f(x.datum)})
  implicit def weightedBooleanToSelection[DATUM](f: Weighted[DATUM] => Boolean) = Selection({x: Weighted[DATUM] => if (f(x)) 1.0 else 0.0})
  implicit def weightedByteToSelection[DATUM](f: Weighted[DATUM] => Byte) = Selection({x: Weighted[DATUM] => f(x).toDouble})
  implicit def weightedShortToSelection[DATUM](f: Weighted[DATUM] => Short) = Selection({x: Weighted[DATUM] => f(x).toDouble})
  implicit def weightedIntToSelection[DATUM](f: Weighted[DATUM] => Int) = Selection({x: Weighted[DATUM] => f(x).toDouble})
  implicit def weightedLongToSelection[DATUM](f: Weighted[DATUM] => Long) = Selection({x: Weighted[DATUM] => f(x).toDouble})
  implicit def weightedFloatToSelection[DATUM](f: Weighted[DATUM] => Float) = Selection({x: Weighted[DATUM] => f(x).toDouble})
  implicit def weightedDoubleToSelection[DATUM](f: Weighted[DATUM] => Double) = Selection(f)

  implicit def mapToMapped(map: scala.collection.immutable.Map[String, Container[_]]) = new Mapped(map.toSeq: _*)
  implicit def mapToMapping[DATUM](map: scala.collection.immutable.Map[String, Aggregator[DATUM, _]]) = new Mapping(map.toSeq: _*)

  implicit def tupleToTupled2[C1 <: Container[C1], C2 <: Container[C2]](x: Tuple2[C1, C2]) = new Tupled2(x._1, x._2)
  implicit def tupleToTupled3[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](x: Tuple3[C1, C2, C3]) = new Tupled3(x._1, x._2, x._3)
  implicit def tupleToTupled4[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](x: Tuple4[C1, C2, C3, C4]) = new Tupled4(x._1, x._2, x._3, x._4)
  implicit def tupleToTupled5[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](x: Tuple5[C1, C2, C3, C4, C5]) = new Tupled5(x._1, x._2, x._3, x._4, x._5)
  implicit def tupleToTupled6[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](x: Tuple6[C1, C2, C3, C4, C5, C6]) = new Tupled6(x._1, x._2, x._3, x._4, x._5, x._6)
  implicit def tupleToTupled7[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](x: Tuple7[C1, C2, C3, C4, C5, C6, C7]) = new Tupled7(x._1, x._2, x._3, x._4, x._5, x._6, x._7)
  implicit def tupleToTupled8[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](x: Tuple8[C1, C2, C3, C4, C5, C6, C7, C8]) = new Tupled8(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8)
  implicit def tupleToTupled9[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](x: Tuple9[C1, C2, C3, C4, C5, C6, C7, C8, C9]) = new Tupled9(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9)
  implicit def tupleToTupled10[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], C10 <: Container[C10]](x: Tuple10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]) = new Tupled10(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10)

  implicit def tupleToTupling2[DATUM, C1 <: Container[C1], C2 <: Container[C2]](x: Tuple2[Aggregator[DATUM, C1], Aggregator[DATUM, C2]]) = new Tupling2(x._1, x._2)
  implicit def tupleToTupling3[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](x: Tuple3[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3]]) = new Tupling3(x._1, x._2, x._3)
  implicit def tupleToTupling4[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](x: Tuple4[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3], Aggregator[DATUM, C4]]) = new Tupling4(x._1, x._2, x._3, x._4)
  implicit def tupleToTupling5[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](x: Tuple5[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3], Aggregator[DATUM, C4], Aggregator[DATUM, C5]]) = new Tupling5(x._1, x._2, x._3, x._4, x._5)
  implicit def tupleToTupling6[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](x: Tuple6[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3], Aggregator[DATUM, C4], Aggregator[DATUM, C5], Aggregator[DATUM, C6]]) = new Tupling6(x._1, x._2, x._3, x._4, x._5, x._6)
  implicit def tupleToTupling7[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](x: Tuple7[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3], Aggregator[DATUM, C4], Aggregator[DATUM, C5], Aggregator[DATUM, C6], Aggregator[DATUM, C7]]) = new Tupling7(x._1, x._2, x._3, x._4, x._5, x._6, x._7)
  implicit def tupleToTupling8[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](x: Tuple8[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3], Aggregator[DATUM, C4], Aggregator[DATUM, C5], Aggregator[DATUM, C6], Aggregator[DATUM, C7], Aggregator[DATUM, C8]]) = new Tupling8(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8)
  implicit def tupleToTupling9[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](x: Tuple9[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3], Aggregator[DATUM, C4], Aggregator[DATUM, C5], Aggregator[DATUM, C6], Aggregator[DATUM, C7], Aggregator[DATUM, C8], Aggregator[DATUM, C9]]) = new Tupling9(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9)
  implicit def tupleToTupling10[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], C10 <: Container[C10]](x: Tuple10[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3], Aggregator[DATUM, C4], Aggregator[DATUM, C5], Aggregator[DATUM, C6], Aggregator[DATUM, C7], Aggregator[DATUM, C8], Aggregator[DATUM, C9], Aggregator[DATUM, C10]]) = new Tupling10(x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10)
}
