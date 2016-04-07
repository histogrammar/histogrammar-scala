package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Tuple/Tupled/Tupling

  // this is a *type-safe* tuple, with explicit cases for 2 through 10 items
  object Tuple extends Factory {
    val name = "Tuple"

    def container[C1 <: Container[C1], C2 <: Container[C2]](_1: C1, _2: C2) = new Tupled2[C1, C2](_1, _2)
    def container[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](_1: C1, _2: C2, _3: C3) = new Tupled3[C1, C2, C3](_1, _2, _3)
    def container[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](_1: C1, _2: C2, _3: C3, _4: C4) = new Tupled4[C1, C2, C3, C4](_1, _2, _3, _4)
    def container[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5) = new Tupled5[C1, C2, C3, C4, C5](_1, _2, _3, _4, _5)
    def container[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6) = new Tupled6[C1, C2, C3, C4, C5, C6](_1, _2, _3, _4, _5, _6)
    def container[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7) = new Tupled7[C1, C2, C3, C4, C5, C6, C7](_1, _2, _3, _4, _5, _6, _7)
    def container[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7, _8: C8) = new Tupled8[C1, C2, C3, C4, C5, C6, C7, C8](_1, _2, _3, _4, _5, _6, _7, _8)
    def container[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7, _8: C8, _9: C9) = new Tupled9[C1, C2, C3, C4, C5, C6, C7, C8, C9](_1, _2, _3, _4, _5, _6, _7, _8, _9)
    def container[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], C10 <: Container[C10]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7, _8: C8, _9: C9, _10: C10) = new Tupled10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10](_1, _2, _3, _4, _5, _6, _7, _8, _9, _10)

    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2]](_1: C1, _2: C2) = new Tupling2[DATUM, C1, C2](_1, _2)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](_1: C1, _2: C2, _3: C3) = new Tupling3[DATUM, C1, C2, C3](_1, _2, _3)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](_1: C1, _2: C2, _3: C3, _4: C4) = new Tupling4[DATUM, C1, C2, C3, C4](_1, _2, _3, _4)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5) = new Tupling5[DATUM, C1, C2, C3, C4, C5](_1, _2, _3, _4, _5)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6) = new Tupling6[DATUM, C1, C2, C3, C4, C5, C6](_1, _2, _3, _4, _5, _6)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7) = new Tupling7[DATUM, C1, C2, C3, C4, C5, C6, C7](_1, _2, _3, _4, _5, _6, _7)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7, _8: C8) = new Tupling8[DATUM, C1, C2, C3, C4, C5, C6, C7, C8](_1, _2, _3, _4, _5, _6, _7, _8)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7, _8: C8, _9: C9) = new Tupling9[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9](_1, _2, _3, _4, _5, _6, _7, _8, _9)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], C10 <: Container[C10]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7, _8: C8, _9: C9, _10: C10) = new Tupling10[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10](_1, _2, _3, _4, _5, _6, _7, _8, _9, _10)

    def unapply[C1 <: Container[C1], C2 <: Container[C2]](x: Tupled2[C1, C2]) = Some((x._1, x._2))
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](x: Tupled3[C1, C2, C3]) = Some((x._1, x._2, x._3))
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](x: Tupled4[C1, C2, C3, C4]) = Some((x._1, x._2, x._3, x._4))
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](x: Tupled5[C1, C2, C3, C4, C5]) = Some((x._1, x._2, x._3, x._4, x._5))
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](x: Tupled6[C1, C2, C3, C4, C5, C6]) = Some((x._1, x._2, x._3, x._4, x._5, x._6))
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](x: Tupled7[C1, C2, C3, C4, C5, C6, C7]) = Some((x._1, x._2, x._3, x._4, x._5, x._6, x._7))
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](x: Tupled8[C1, C2, C3, C4, C5, C6, C7, C8]) = Some((x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8))
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](x: Tupled9[C1, C2, C3, C4, C5, C6, C7, C8, C9]) = Some((x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9))
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], C10 <: Container[C10]](x: Tupled10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]) = Some((x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10))

    def unapply[DATUM, C1 <: Container[C1], C2 <: Container[C2]](x: Tupling2[DATUM, C1, C2]) = Some((x._1, x._2))
    def unapply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](x: Tupling3[DATUM, C1, C2, C3]) = Some((x._1, x._2, x._3))
    def unapply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](x: Tupling4[DATUM, C1, C2, C3, C4]) = Some((x._1, x._2, x._3, x._4))
    def unapply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](x: Tupling5[DATUM, C1, C2, C3, C4, C5]) = Some((x._1, x._2, x._3, x._4, x._5))
    def unapply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](x: Tupling6[DATUM, C1, C2, C3, C4, C5, C6]) = Some((x._1, x._2, x._3, x._4, x._5, x._6))
    def unapply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](x: Tupling7[DATUM, C1, C2, C3, C4, C5, C6, C7]) = Some((x._1, x._2, x._3, x._4, x._5, x._6), x._7)
    def unapply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](x: Tupling8[DATUM, C1, C2, C3, C4, C5, C6, C7, C8]) = Some((x._1, x._2, x._3, x._4, x._5, x._6), x._7, x._8)
    def unapply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](x: Tupling9[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9]) = Some((x._1, x._2, x._3, x._4, x._5, x._6), x._7, x._8, x._9)
    def unapply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], C10 <: Container[C10]](x: Tupling10[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]) = Some((x._1, x._2, x._3, x._4, x._5, x._6), x._7, x._8, x._9, x._10)

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonArray(elements @ _*) if (elements.size >= 2) =>
        val subs = elements.zipWithIndex map {
          case (JsonObject(pairs @ _*), i) if (pairs.keySet == Set("type", "data")) =>
            val get = pairs.toMap
            (get("type"), get("data")) match {
              case (JsonString(factory), sub) => Factory(factory).fromJsonFragment(sub)
              case _ => throw new JsonFormatException(json, name + s" item $i")
            }
          case _ => throw new JsonFormatException(json, name + " item")
        }
        if (subs.size == 2) new Tupled2[Container[_], Container[_]](subs(0), subs(1)).asInstanceOf[Container[_]]
        else if (subs.size == 3) new Tupled3[Container[_], Container[_], Container[_]](subs(0), subs(1), subs(2)).asInstanceOf[Container[_]]
        else if (subs.size == 4) new Tupled4[Container[_], Container[_], Container[_], Container[_]](subs(0), subs(1), subs(2), subs(3)).asInstanceOf[Container[_]]
        else if (subs.size == 5) new Tupled5[Container[_], Container[_], Container[_], Container[_], Container[_]](subs(0), subs(1), subs(2), subs(3), subs(4)).asInstanceOf[Container[_]]
        else if (subs.size == 6) new Tupled6[Container[_], Container[_], Container[_], Container[_], Container[_], Container[_]](subs(0), subs(1), subs(2), subs(3), subs(4), subs(5)).asInstanceOf[Container[_]]
        else if (subs.size == 7) new Tupled7[Container[_], Container[_], Container[_], Container[_], Container[_], Container[_], Container[_]](subs(0), subs(1), subs(2), subs(3), subs(4), subs(5), subs(6)).asInstanceOf[Container[_]]
        else if (subs.size == 8) new Tupled8[Container[_], Container[_], Container[_], Container[_], Container[_], Container[_], Container[_], Container[_]](subs(0), subs(1), subs(2), subs(3), subs(4), subs(5), subs(6), subs(7)).asInstanceOf[Container[_]]
        else if (subs.size == 9) new Tupled9[Container[_], Container[_], Container[_], Container[_], Container[_], Container[_], Container[_], Container[_], Container[_]](subs(0), subs(1), subs(2), subs(3), subs(4), subs(5), subs(6), subs(7), subs(8)).asInstanceOf[Container[_]]
        else if (subs.size == 10) new Tupled10[Container[_], Container[_], Container[_], Container[_], Container[_], Container[_], Container[_], Container[_], Container[_], Container[_]](subs(0), subs(1), subs(2), subs(3), subs(4), subs(5), subs(6), subs(7), subs(8), subs(9)).asInstanceOf[Container[_]]
        else
          throw new JsonFormatException(json, name + " (too many branches in tuple)")

      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def typedata[CONTAINER <: Container[CONTAINER]](sub: CONTAINER) = JsonObject("type" -> JsonString(sub.factory.name), "data" -> sub.toJsonFragment)
  }

  class Tupled2[C1 <: Container[C1], C2 <: Container[C2]](val _1: C1, val _2: C2) extends Container[Tupled2[C1, C2]] {
    def factory = Tuple
    def +(that: Tupled2[C1, C2]) = new Tupled2[C1, C2](this._1 + that._1, this._2 + that._2)
    def toJsonFragment = JsonArray(Tuple.typedata(_1), Tuple.typedata(_2))
    override def toString = s"""Tupled2[${_1}, ${_2}]"""
  }
  class Tupled3[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](val _1: C1, val _2: C2, val _3: C3) extends Container[Tupled3[C1, C2, C3]] {
    def factory = Tuple
    def +(that: Tupled3[C1, C2, C3]) = new Tupled3[C1, C2, C3](this._1 + that._1, this._2 + that._2, this._3 + that._3)
    def toJsonFragment = JsonArray(Tuple.typedata(_1), Tuple.typedata(_2), Tuple.typedata(_3))
    override def toString = s"""Tupled3[${_1}, ${_2}, ${_3}]"""
  }
  class Tupled4[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](val _1: C1, val _2: C2, val _3: C3, val _4: C4) extends Container[Tupled4[C1, C2, C3, C4]] {
    def factory = Tuple
    def +(that: Tupled4[C1, C2, C3, C4]) = new Tupled4[C1, C2, C3, C4](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4)
    def toJsonFragment = JsonArray(Tuple.typedata(_1), Tuple.typedata(_2), Tuple.typedata(_3), Tuple.typedata(_4))
    override def toString = s"""Tupled4[${_1}, ${_2}, ${_3}, ${_4}]"""
  }
  class Tupled5[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](val _1: C1, val _2: C2, val _3: C3, val _4: C4, val _5: C5) extends Container[Tupled5[C1, C2, C3, C4, C5]] {
    def factory = Tuple
    def +(that: Tupled5[C1, C2, C3, C4, C5]) = new Tupled5[C1, C2, C3, C4, C5](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5)
    def toJsonFragment = JsonArray(Tuple.typedata(_1), Tuple.typedata(_2), Tuple.typedata(_3), Tuple.typedata(_4), Tuple.typedata(_5))
    override def toString = s"""Tupled5[${_1}, ${_2}, ${_3}, ${_4}, ${_5}]"""
  }
  class Tupled6[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](val _1: C1, val _2: C2, val _3: C3, val _4: C4, val _5: C5, val _6: C6) extends Container[Tupled6[C1, C2, C3, C4, C5, C6]] {
    def factory = Tuple
    def +(that: Tupled6[C1, C2, C3, C4, C5, C6]) = new Tupled6[C1, C2, C3, C4, C5, C6](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6)
    def toJsonFragment = JsonArray(Tuple.typedata(_1), Tuple.typedata(_2), Tuple.typedata(_3), Tuple.typedata(_4), Tuple.typedata(_5), Tuple.typedata(_6))
    override def toString = s"""Tupled6[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}]"""
  }
  class Tupled7[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](val _1: C1, val _2: C2, val _3: C3, val _4: C4, val _5: C5, val _6: C6, val _7: C7) extends Container[Tupled7[C1, C2, C3, C4, C5, C6, C7]] {
    def factory = Tuple
    def +(that: Tupled7[C1, C2, C3, C4, C5, C6, C7]) = new Tupled7[C1, C2, C3, C4, C5, C6, C7](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7)
    def toJsonFragment = JsonArray(Tuple.typedata(_1), Tuple.typedata(_2), Tuple.typedata(_3), Tuple.typedata(_4), Tuple.typedata(_5), Tuple.typedata(_6), Tuple.typedata(_7))
    override def toString = s"""Tupled7[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}]"""
  }
  class Tupled8[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](val _1: C1, val _2: C2, val _3: C3, val _4: C4, val _5: C5, val _6: C6, val _7: C7, val _8: C8) extends Container[Tupled8[C1, C2, C3, C4, C5, C6, C7, C8]] {
    def factory = Tuple
    def +(that: Tupled8[C1, C2, C3, C4, C5, C6, C7, C8]) = new Tupled8[C1, C2, C3, C4, C5, C6, C7, C8](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7, this._8 + that._8)
    def toJsonFragment = JsonArray(Tuple.typedata(_1), Tuple.typedata(_2), Tuple.typedata(_3), Tuple.typedata(_4), Tuple.typedata(_5), Tuple.typedata(_6), Tuple.typedata(_7), Tuple.typedata(_8))
    override def toString = s"""Tupled8[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}, ${_8}]"""
  }
  class Tupled9[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](val _1: C1, val _2: C2, val _3: C3, val _4: C4, val _5: C5, val _6: C6, val _7: C7, val _8: C8, val _9: C9) extends Container[Tupled9[C1, C2, C3, C4, C5, C6, C7, C8, C9]] {
    def factory = Tuple
    def +(that: Tupled9[C1, C2, C3, C4, C5, C6, C7, C8, C9]) = new Tupled9[C1, C2, C3, C4, C5, C6, C7, C8, C9](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7, this._8 + that._8, this._9 + that._9)
    def toJsonFragment = JsonArray(Tuple.typedata(_1), Tuple.typedata(_2), Tuple.typedata(_3), Tuple.typedata(_4), Tuple.typedata(_5), Tuple.typedata(_6), Tuple.typedata(_7), Tuple.typedata(_8), Tuple.typedata(_9))
    override def toString = s"""Tupled9[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}, ${_8}, ${_9}]"""
  }
  class Tupled10[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], C10 <: Container[C10]](val _1: C1, val _2: C2, val _3: C3, val _4: C4, val _5: C5, val _6: C6, val _7: C7, val _8: C8, val _9: C9, val _10: C10) extends Container[Tupled10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]] {
    def factory = Tuple
    def +(that: Tupled10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]) = new Tupled10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7, this._8 + that._8, this._9 + that._9, this._10 + that._10)
    def toJsonFragment = JsonArray(Tuple.typedata(_1), Tuple.typedata(_2), Tuple.typedata(_3), Tuple.typedata(_4), Tuple.typedata(_5), Tuple.typedata(_6), Tuple.typedata(_7), Tuple.typedata(_8), Tuple.typedata(_9), Tuple.typedata(_10))
    override def toString = s"""Tupled10[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}, ${_8}, ${_9}, ${_10}]"""
  }

  class Tupling2[DATUM, C1 <: Container[C1], C2 <: Container[C2]](val _1: C1, val _2: C2) extends Container[Tupling2[DATUM, C1, C2]] with Aggregation[DATUM] {
    def factory = Tuple
    def +(that: Tupling2[DATUM, C1, C2]) = new Tupling2[DATUM, C1, C2](this._1 + that._1, this._2 + that._2)
    if (!_1.isInstanceOf[Aggregation[DATUM]]  ||  !_2.isInstanceOf[Aggregation[DATUM]])
      throw new AggregatorException(s"Tuple branches should be built with Aggregation enabled (ending in -ing)")
    def fillWeighted(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _2.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
      }
    }
    def toJsonFragment = JsonArray(Tuple.typedata[C1](_1), Tuple.typedata[C2](_2))
    override def toString = s"""Tupling2[${_1}, ${_2}]"""
  }
  class Tupling3[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](val _1: C1, val _2: C2, val _3: C3) extends Container[Tupling3[DATUM, C1, C2, C3]] with Aggregation[DATUM] {
    def factory = Tuple
    def +(that: Tupling3[DATUM, C1, C2, C3]) = new Tupling3[DATUM, C1, C2, C3](this._1 + that._1, this._2 + that._2, this._3 + that._3)
    if (!_1.isInstanceOf[Aggregation[DATUM]]  ||  !_2.isInstanceOf[Aggregation[DATUM]]  ||  !_3.isInstanceOf[Aggregation[DATUM]])
      throw new AggregatorException(s"Tuple branches should be built with Aggregation enabled (ending in -ing)")
    def fillWeighted(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _2.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _3.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
      }
    }
    def toJsonFragment = JsonArray(Tuple.typedata[C1](_1), Tuple.typedata[C2](_2), Tuple.typedata[C3](_3))
    override def toString = s"""Tupling3[${_1}, ${_2}, ${_3}]"""
  }
  class Tupling4[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](val _1: C1, val _2: C2, val _3: C3, val _4: C4) extends Container[Tupling4[DATUM, C1, C2, C3, C4]] with Aggregation[DATUM] {
    def factory = Tuple
    def +(that: Tupling4[DATUM, C1, C2, C3, C4]) = new Tupling4[DATUM, C1, C2, C3, C4](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4)
    if (!_1.isInstanceOf[Aggregation[DATUM]]  ||  !_2.isInstanceOf[Aggregation[DATUM]]  ||  !_3.isInstanceOf[Aggregation[DATUM]]  ||  !_4.isInstanceOf[Aggregation[DATUM]])
      throw new AggregatorException(s"Tuple branches should be built with Aggregation enabled (ending in -ing)")
    def fillWeighted(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _2.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _3.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _4.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
      }
    }
    def toJsonFragment = JsonArray(Tuple.typedata[C1](_1), Tuple.typedata[C2](_2), Tuple.typedata[C3](_3), Tuple.typedata[C4](_4))
    override def toString = s"""Tupling4[${_1}, ${_2}, ${_3}, ${_4}]"""
  }
  class Tupling5[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](val _1: C1, val _2: C2, val _3: C3, val _4: C4, val _5: C5) extends Container[Tupling5[DATUM, C1, C2, C3, C4, C5]] with Aggregation[DATUM] {
    def factory = Tuple
    def +(that: Tupling5[DATUM, C1, C2, C3, C4, C5]) = new Tupling5[DATUM, C1, C2, C3, C4, C5](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5)
    if (!_1.isInstanceOf[Aggregation[DATUM]]  ||  !_2.isInstanceOf[Aggregation[DATUM]]  ||  !_3.isInstanceOf[Aggregation[DATUM]]  ||  !_4.isInstanceOf[Aggregation[DATUM]]  ||  !_5.isInstanceOf[Aggregation[DATUM]])
      throw new AggregatorException(s"Tuple branches should be built with Aggregation enabled (ending in -ing)")
    def fillWeighted(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _2.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _3.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _4.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _5.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
      }
    }
    def toJsonFragment = JsonArray(Tuple.typedata[C1](_1), Tuple.typedata[C2](_2), Tuple.typedata[C3](_3), Tuple.typedata[C4](_4), Tuple.typedata[C5](_5))
    override def toString = s"""Tupling5[${_1}, ${_2}, ${_3}, ${_4}, ${_5}]"""
  }
  class Tupling6[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](val _1: C1, val _2: C2, val _3: C3, val _4: C4, val _5: C5, val _6: C6) extends Container[Tupling6[DATUM, C1, C2, C3, C4, C5, C6]] with Aggregation[DATUM] {
    def factory = Tuple
    def +(that: Tupling6[DATUM, C1, C2, C3, C4, C5, C6]) = new Tupling6[DATUM, C1, C2, C3, C4, C5, C6](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6)
    if (!_1.isInstanceOf[Aggregation[DATUM]]  ||  !_2.isInstanceOf[Aggregation[DATUM]]  ||  !_3.isInstanceOf[Aggregation[DATUM]]  ||  !_4.isInstanceOf[Aggregation[DATUM]]  ||  !_5.isInstanceOf[Aggregation[DATUM]]  ||  !_6.isInstanceOf[Aggregation[DATUM]])
      throw new AggregatorException(s"Tuple branches should be built with Aggregation enabled (ending in -ing)")
    def fillWeighted(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _2.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _3.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _4.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _5.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _6.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
      }
    }
    def toJsonFragment = JsonArray(Tuple.typedata[C1](_1), Tuple.typedata[C2](_2), Tuple.typedata[C3](_3), Tuple.typedata[C4](_4), Tuple.typedata[C5](_5), Tuple.typedata[C6](_6))
    override def toString = s"""Tupling6[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}]"""
  }
  class Tupling7[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](val _1: C1, val _2: C2, val _3: C3, val _4: C4, val _5: C5, val _6: C6, val _7: C7) extends Container[Tupling7[DATUM, C1, C2, C3, C4, C5, C6, C7]] with Aggregation[DATUM] {
    def factory = Tuple
    def +(that: Tupling7[DATUM, C1, C2, C3, C4, C5, C6, C7]) = new Tupling7[DATUM, C1, C2, C3, C4, C5, C6, C7](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7)
    if (!_1.isInstanceOf[Aggregation[DATUM]]  ||  !_2.isInstanceOf[Aggregation[DATUM]]  ||  !_3.isInstanceOf[Aggregation[DATUM]]  ||  !_4.isInstanceOf[Aggregation[DATUM]]  ||  !_5.isInstanceOf[Aggregation[DATUM]]  ||  !_6.isInstanceOf[Aggregation[DATUM]]  ||  !_7.isInstanceOf[Aggregation[DATUM]])
      throw new AggregatorException(s"Tuple branches should be built with Aggregation enabled (ending in -ing)")
    def fillWeighted(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _2.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _3.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _4.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _5.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _6.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _7.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
      }
    }
    def toJsonFragment = JsonArray(Tuple.typedata[C1](_1), Tuple.typedata[C2](_2), Tuple.typedata[C3](_3), Tuple.typedata[C4](_4), Tuple.typedata[C5](_5), Tuple.typedata[C6](_6), Tuple.typedata[C7](_7))
    override def toString = s"""Tupling7[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}]"""
  }
  class Tupling8[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](val _1: C1, val _2: C2, val _3: C3, val _4: C4, val _5: C5, val _6: C6, val _7: C7, val _8: C8) extends Container[Tupling8[DATUM, C1, C2, C3, C4, C5, C6, C7, C8]] with Aggregation[DATUM] {
    def factory = Tuple
    def +(that: Tupling8[DATUM, C1, C2, C3, C4, C5, C6, C7, C8]) = new Tupling8[DATUM, C1, C2, C3, C4, C5, C6, C7, C8](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7, this._8 + that._8)
    if (!_1.isInstanceOf[Aggregation[DATUM]]  ||  !_2.isInstanceOf[Aggregation[DATUM]]  ||  !_3.isInstanceOf[Aggregation[DATUM]]  ||  !_4.isInstanceOf[Aggregation[DATUM]]  ||  !_5.isInstanceOf[Aggregation[DATUM]]  ||  !_6.isInstanceOf[Aggregation[DATUM]]  ||  !_7.isInstanceOf[Aggregation[DATUM]]  ||  !_8.isInstanceOf[Aggregation[DATUM]])
      throw new AggregatorException(s"Tuple branches should be built with Aggregation enabled (ending in -ing)")
    def fillWeighted(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _2.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _3.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _4.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _5.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _6.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _7.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _8.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
      }
    }
    def toJsonFragment = JsonArray(Tuple.typedata[C1](_1), Tuple.typedata[C2](_2), Tuple.typedata[C3](_3), Tuple.typedata[C4](_4), Tuple.typedata[C5](_5), Tuple.typedata[C6](_6), Tuple.typedata[C7](_7), Tuple.typedata[C8](_8))
    override def toString = s"""Tupling8[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}, ${_8}]"""
  }
  class Tupling9[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](val _1: C1, val _2: C2, val _3: C3, val _4: C4, val _5: C5, val _6: C6, val _7: C7, val _8: C8, val _9: C9) extends Container[Tupling9[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9]] with Aggregation[DATUM] {
    def factory = Tuple
    def +(that: Tupling9[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9]) = new Tupling9[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7, this._8 + that._8, this._9 + that._9)
    if (!_1.isInstanceOf[Aggregation[DATUM]]  ||  !_2.isInstanceOf[Aggregation[DATUM]]  ||  !_3.isInstanceOf[Aggregation[DATUM]]  ||  !_4.isInstanceOf[Aggregation[DATUM]]  ||  !_5.isInstanceOf[Aggregation[DATUM]]  ||  !_6.isInstanceOf[Aggregation[DATUM]]  ||  !_7.isInstanceOf[Aggregation[DATUM]]  ||  !_8.isInstanceOf[Aggregation[DATUM]]  ||  !_9.isInstanceOf[Aggregation[DATUM]])
      throw new AggregatorException(s"Tuple branches should be built with Aggregation enabled (ending in -ing)")
    def fillWeighted(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _2.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _3.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _4.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _5.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _6.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _7.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _8.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _9.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
      }
    }
    def toJsonFragment = JsonArray(Tuple.typedata[C1](_1), Tuple.typedata[C2](_2), Tuple.typedata[C3](_3), Tuple.typedata[C4](_4), Tuple.typedata[C5](_5), Tuple.typedata[C6](_6), Tuple.typedata[C7](_7), Tuple.typedata[C8](_8), Tuple.typedata[C9](_9))
    override def toString = s"""Tupling9[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}, ${_8}, ${_9}]"""
  }
  class Tupling10[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], C10 <: Container[C10]](val _1: C1, val _2: C2, val _3: C3, val _4: C4, val _5: C5, val _6: C6, val _7: C7, val _8: C8, val _9: C9, val _10: C10) extends Container[Tupling10[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]] with Aggregation[DATUM] {
    def factory = Tuple
    def +(that: Tupling10[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]) = new Tupling10[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7, this._8 + that._8, this._9 + that._9, this._10 + that._10)
    if (!_1.isInstanceOf[Aggregation[DATUM]]  ||  !_2.isInstanceOf[Aggregation[DATUM]]  ||  !_3.isInstanceOf[Aggregation[DATUM]]  ||  !_4.isInstanceOf[Aggregation[DATUM]]  ||  !_5.isInstanceOf[Aggregation[DATUM]]  ||  !_6.isInstanceOf[Aggregation[DATUM]]  ||  !_7.isInstanceOf[Aggregation[DATUM]]  ||  !_8.isInstanceOf[Aggregation[DATUM]]  ||  !_9.isInstanceOf[Aggregation[DATUM]]  ||  !_10.isInstanceOf[Aggregation[DATUM]])
      throw new AggregatorException(s"Tuple branches should be built with Aggregation enabled (ending in -ing)")
    def fillWeighted(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _2.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _3.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _4.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _5.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _6.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _7.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _8.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _9.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
        _10.asInstanceOf[Aggregation[DATUM]].fillWeighted(x)
      }
    }
    def toJsonFragment = JsonArray(Tuple.typedata[C1](_1), Tuple.typedata[C2](_2), Tuple.typedata[C3](_3), Tuple.typedata[C4](_4), Tuple.typedata[C5](_5), Tuple.typedata[C6](_6), Tuple.typedata[C7](_7), Tuple.typedata[C8](_8), Tuple.typedata[C9](_9), Tuple.typedata[C10](_10))
    override def toString = s"""Tupling10[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}, ${_8}, ${_9}, ${_10}]"""
  }
}
