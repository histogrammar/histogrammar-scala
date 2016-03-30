package org.dianahep.histogrammar

import scala.language.implicitConversions

package object json {
  implicit def optionToJson(x: Option[Json]) = x match {case None => JsonNull; case Some(y) => y}
  implicit def booleanToJson(x: Boolean) = if (x) JsonTrue else JsonFalse
  implicit def byteToJson(x: Byte) = JsonInt(x)
  implicit def shortToJson(x: Short) = JsonInt(x)
  implicit def intToJson(x: Int) = JsonInt(x)
  implicit def longToJson(x: Long) = JsonInt(x)
  implicit def floatToJson(x: Float) = JsonFloat(x)
  implicit def doubleToJson(x: Double) = JsonFloat(x)
  implicit def charToJson(x: Char) = JsonString(x.toString)
  implicit def stringToJson(x: String) = JsonString(x)
  implicit def iterableToJson(x: Iterable[Json]) = JsonArray(x.toSeq: _*)
  implicit def mapToJson(x: Iterable[(JsonString, Json)]) = JsonObject(x.toSeq: _*)
  implicit def mapToJson(x: Map[JsonString, Json]) = JsonObject(x.toSeq: _*)
}

package json {
  case class ParseState(str: String, var pos: Int) {
    private var savePos = 0
    def save() {savePos = pos}
    def restore() {pos = savePos}
    def get = str(pos)
    def get(n: Int) = str.substring(pos, pos + n)
    def update(n: Int) = {pos += n}
    def remaining = str.size - pos
  }

  sealed trait Json
  object Json {
    def unapply(p: ParseState): Option[Json]
  }

  trait JsonPrimitive extends Json

  case object JsonNull extends JsonPrimitive {
    override def toString() = "null"
    def unapply(p: ParseState) =
      if (p.remaining >= 4  &&  p.get(4) == "null") {
        p.update(4)
        Some(JsonNull)
      }
      else
        None
  }

  trait JsonBoolean extends JsonPrimitive

  case object JsonTrue extends JsonBoolean {
    override def toString() = "true"
    def unapply(p: ParseState) =
      if (p.remaining >= 4  &&  p.get(4) == "true") {
        p.update(4)
        Some(JsonTrue)
      }
      else
        None
  }

  case object JsonFalse extends JsonBoolean {
    override def toString() = "false"
    def unapply(p: ParseState) =
      if (p.remaining >= 5  &&  p.get(5) == "false") {
        p.update(5)
        Some(JsonFalse)
      }
      else
        None
  }

  trait JsonNumber extends JsonPrimitive
  object JsonNumber {
    def unapply(p: ParseState) =
      if (p.remaining >= 1  &&  (('0' <= p.get  &&  p.get <= '9')  ||  p.get == '-')) {
        p.save()
        val sb = java.lang.StringBuilder
        while (p.remaining >= 1  &&  (('0' <= p.get  &&  p.get <= '9')  ||  p.get == '-'  ||  p.get == '+'  ||  p.get == 'e'  ||  p.get == 'E'  ||  p.get == '.')) {
          sb.append(p.get)
          p.update(1)
        }
        val str = sb.toString
        try {
          Some(JsonInt(java.lang.Long.parseLong(str)))
        }
        catch {
          case _: java.lang.NumberFormatException =>
            try {
              Some(JsonFloat(java.lang.Double.parseDouble(str)))
            }
            catch {
              case _: java.lang.NumberFormatException =>
                p.restore()
                None
            }
        }
      }
      else
        None
  }

  case class JsonInt(value: Long) extends JsonNumber {
    override def toString() = value.toString
  }

  case class JsonFloat(value: Double) extends JsonNumber {
    override def toString() = value.toString
  }

  case class JsonString(value: String) extends JsonPrimitive {
    // http://stackoverflow.com/a/16652683/1623645
    override def toString() = {
      val sb = new java.lang.StringBuilder(value.size + 4)
      var t: String = ""
      sb.append('"')
      value foreach {c =>
        c match {
          case '"' | '\\' => sb.append('\\'); sb.append(c)
          case '/'        => sb.append('\\'); sb.append(c)
          case '\b'       => sb.append("\\b")
          case '\f'       => sb.append("\\f")
          case '\n'       => sb.append("\\n")
          case '\r'       => sb.append("\\r")
          case '\t'       => sb.append("\\t")
          case _ if (c < 32  ||  c >= 127) =>
            val t = "000" + java.lang.Integer.toHexString(c)
            sb.append("\\u")
            sb.append(t.substring(t.size - 4))
          case _ =>
            sb.append(c)
        }
      }
      sb.append('"')
      sb.toString
    }
  }
  object JsonString {
    def unapply(p: ParseState) =
      if (p.remaining >= 1  &&  p.get == '"') {
        p.save()
        val sb = new java.lang.StringBuilder
        p.update(1)
        while (p.remaining >= 1  &&  p.get != '"') {
          if (p.get == '\\') {
            p.update(1)
            if (p.remaining >= 1)
              p.get match {
                case '"'  => sb.append('"');  p.update(1)
                case '\\' => sb.append('\\'); p.update(1)
                case '/'  => sb.append('/');  p.update(1)
                case 'b'  => sb.append('\b'); p.update(1)
                case 'f'  => sb.append('\f'); p.update(1)
                case 'n'  => sb.append('\n'); p.update(1)
                case 'r'  => sb.append('\r'); p.update(1)
                case 't'  => sb.append('\t'); p.update(1)
                case 'u' if (p.remaining >= 5) =>
                  p.update(1)
                  sb.append(java.lang.Integer.parseInt(p.get(4)).toChar)
                  p.update(4)
                case _ =>
                  p.restore()
                  return None
              }
            else {
              p.restore()
              return None
            }
          }
          else {
            sb.append(p.get)
            p.update(1)
          }
        }
        if (p.get == '"') {
          p.update(1)
          Some(JsonString(sb.toString))
        }
        else {
          p.restore()
          None
        }
      }
      else
        None
  }

  trait JsonContainer extends Json

  case class JsonArray(elements: Json*) extends JsonContainer {
    override def toString() = "[" + elements.map(_.toString).mkString(", ") + "]"
  }

  case class JsonObject(pairs: (JsonString, Json)*) extends JsonContainer {
    override def toString() = "{" + pairs.map({case (k, v) => k.toString + ": " + v.toString}).mkString(", ") + "}"
  }

  // object parse extends Function1[String, Json] {
  //   def apply(x: String): Json = {
  //   }
  // }

  package check {
    sealed trait Validity {
      def apply(x: Json): Boolean
      def |(that: Validity) = Or(this, that)
      def &(that: Validity) = And(this, that)
    }

    case object Null extends Validity {
      def apply(x: Json) = x match {case JsonNull => true; case _ => false}
      override def toString() = "check.Null"
    }

    case object Primitive extends Validity {
      def apply(x: Json) = x match {case _: JsonPrimitive => true; case _ => false}
      override def toString() = "check.Primitive"
    }

    case object Boolean extends Validity {
      def apply(x: Json) = x match {case _: JsonBoolean => true; case _ => false}
      override def toString() = "check.Boolean"
    }

    case object Number extends Validity {
      def apply(x: Json) = x match {case _: JsonNumber => true; case _ => false}
      override def toString() = "check.Number"
    }

    case object Int extends Validity {
      def apply(x: Json) = x match {case _: JsonInt => true; case _ => false}
      override def toString() = "check.Int"
    }

    case object Float extends Validity {
      def apply(x: Json) = x match {case _: JsonFloat => true; case _ => false}
      override def toString() = "check.Float"
    }

    case object String extends Validity {
      def apply(x: Json) = x match {case _: JsonString => true; case _ => false}
      override def toString() = "check.String"
    }

    case class String(regex: scala.util.matching.Regex) extends Validity {
      def apply(x: Json) = x match {case JsonString(y) => !(regex unapplySeq y).isEmpty; case _ => false}
      override def toString() = s"check.String($regex)"
    }

    case object Container extends Validity {
      def apply(x: Json) = x match {case _: JsonContainer => true; case _ => false}
      override def toString() = "check.Container"
    }

    case class Container(items: Validity) extends Validity {
      def apply(x: Json) = x match {
        case JsonArray(elements @ _*) => elements.forall(items(_))
        case JsonObject(pairs @ _*) => pairs.map(_._2).forall(items(_))
        case _ => false
      }
      override def toString() = s"check.Container($items)"
    }

    case object Array extends Validity {
      def apply(x: Json) = x match {case _: JsonArray => true; case _ => false}
      override def toString() = "check.Array"
    }

    case class Array(items: Validity) extends Validity {
      def apply(x: Json) = x match {
        case JsonArray(elements @ _*) => elements.forall(items(_))
        case _ => false
      }
      override def toString() = s"check.Array($items)"
    }

    case object Object extends Validity {
      def apply(x: Json) = x match {case _: JsonObject => true; case _ => false}
      override def toString() = "check.Object"
    }

    case class Object(items: Validity) extends Validity {
      def apply(x: Json) = x match {
        case JsonObject(pairs @ _*) => pairs.map(_._2).forall(items(_))
        case _ => false
      }
      override def toString() = s"check.Object($items)"
    }

    case class Or(possibilities: Validity*) extends Validity {
      def apply(x: Json) = possibilities.exists(p => p(x))
      override def toString() = possibilities.mkString(" | ")
    }

    case class And(possibilities: Validity*) extends Validity {
      def apply(x: Json) = possibilities.forall(p => p(x))
      override def toString() = "(" + possibilities.mkString(" & ") + ")"
    }
  }
}
