package org.dianahep

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

    // register(Count)
    register(Sum)
    // register(Average)
    // register(Deviate)
    // register(AbsoluteErr)
    // register(Minimize)
    // register(Maximize)
    register(Bin)
    // register(SparselyBin)
    // register(Fraction)
    // register(Stack)
    // register(Partition)
    // register(Categorize)

    def apply(name: String) = known.get(name) match {
      case Some(x) => x
      case None => throw new ContainerException(s"unrecognized container (is it a custom container that hasn't been registered?): $name")
    }

    def fromJson[CONTAINER <: Container[_]](str: String): CONTAINER = Json.parse(str) match {
      case Some(json) => fromJson[CONTAINER](json)
      case None => throw new InvalidJsonException(str)
    }

    def fromJson[CONTAINER <: Container[_]](json: Json): CONTAINER = json match {
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

  // container of data that is, by itself, immutable
  trait Container[CONTAINER <: Container[CONTAINER]] extends Serializable {
    def factory: Factory

    def +(that: CONTAINER): CONTAINER

    def toJson: Json = JsonObject("type" -> JsonString(factory.name), "data" -> toJsonFragment)
    def toJsonFragment: Json
    def withTypeOf[OTHER <: CONTAINER](other: OTHER) = this.asInstanceOf[OTHER]
  }

  // mix-in to provide aggregation (and hence mutability)
  trait Aggregation {
    type Datum

    def fill(datum: Datum) {
      fillWeighted(datum, 1.0)
    }
    def fillWeighted[SUB <: Datum](datum: SUB, weight: Double)
  }
}

package object histogrammar {
  def help = Factory.registered map {case (name, factory) => f"${name}%-15s ${factory.help}"} mkString("\n")

  // def increment[DATUM, CONTAINER <: Container[DATUM, CONTAINER]] =
  //   {(h: CONTAINER, x: DATUM) => h.fill(x); h}

  // def increment[DATUM, CONTAINER <: Container[DATUM, CONTAINER]](zero: CONTAINER) =
  //   {(h: CONTAINER, x: DATUM) => h.fill(x); h}

  // def combine[DATUM, CONTAINER <: Container[DATUM, CONTAINER]] =
  //   {(h1: CONTAINER, h2: CONTAINER) => h1 + h2}

  // def combine[DATUM, CONTAINER <: Container[DATUM, CONTAINER]](zero: CONTAINER) =
  //   {(h1: CONTAINER, h2: CONTAINER) => h1 + h2}

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

  // used to check floating-point equality with a new rule: NaN == NaN
  implicit class nanEquality(val x: Double) extends AnyVal {
    def ===(that: Double) = (this.x.isNaN  &&  that.isNaN)  ||  this.x == that
  }
}
