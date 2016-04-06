package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Tuple/Tupled/Tupling

  // this is a *type-safe* tuple, with explicit cases for 2 through 10 items
  object Tuple extends Factory {
    val name = "Tuple"

    def ed[C1 <: Container[C1], C2 <: Container[C2]](_1: C1, _2: C2) = new Tupled2[C1, C2](_1, _2)
    def ed[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](_1: C1, _2: C2, _3: C3) = new Tupled3[C1, C2, C3](_1, _2, _3)
    def ed[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](_1: C1, _2: C2, _3: C3, _4: C4) = new Tupled4[C1, C2, C3, C4](_1, _2, _3, _4)
    def ed[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5) = new Tupled5[C1, C2, C3, C4, C5](_1, _2, _3, _4, _5)
    def ed[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6) = new Tupled6[C1, C2, C3, C4, C5, C6](_1, _2, _3, _4, _5, _6)
    def ed[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7) = new Tupled7[C1, C2, C3, C4, C5, C6, C7](_1, _2, _3, _4, _5, _6, _7)
    def ed[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7, _8: C8) = new Tupled8[C1, C2, C3, C4, C5, C6, C7, C8](_1, _2, _3, _4, _5, _6, _7, _8)
    def ed[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7, _8: C8, _9: C9) = new Tupled9[C1, C2, C3, C4, C5, C6, C7, C8, C9](_1, _2, _3, _4, _5, _6, _7, _8, _9)
    def ed[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], C10 <: Container[C10]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7, _8: C8, _9: C9, _10: C10) = new Tupled10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10](_1, _2, _3, _4, _5, _6, _7, _8, _9, _10)

    def ing[DATUM, C1 <: Container[C1], C2 <: Container[C2]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2]) = new Tupling2[DATUM, C1, C2](_1, _2)
    def ing[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3]) = new Tupling3[DATUM, C1, C2, C3](_1, _2, _3)
    def ing[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4]) = new Tupling4[DATUM, C1, C2, C3, C4](_1, _2, _3, _4)
    def ing[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5]) = new Tupling5[DATUM, C1, C2, C3, C4, C5](_1, _2, _3, _4, _5)
    def ing[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6]) = new Tupling6[DATUM, C1, C2, C3, C4, C5, C6](_1, _2, _3, _4, _5, _6)
    def ing[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6], _7: Aggregator[DATUM, C7]) = new Tupling7[DATUM, C1, C2, C3, C4, C5, C6, C7](_1, _2, _3, _4, _5, _6, _7)
    def ing[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6], _7: Aggregator[DATUM, C7], _8: Aggregator[DATUM, C8]) = new Tupling8[DATUM, C1, C2, C3, C4, C5, C6, C7, C8](_1, _2, _3, _4, _5, _6, _7, _8)
    def ing[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6], _7: Aggregator[DATUM, C7], _8: Aggregator[DATUM, C8], _9: Aggregator[DATUM, C9]) = new Tupling9[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9](_1, _2, _3, _4, _5, _6, _7, _8, _9)
    def ing[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], C10 <: Container[C10]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6], _7: Aggregator[DATUM, C7], _8: Aggregator[DATUM, C8], _9: Aggregator[DATUM, C9], _10: Aggregator[DATUM, C10]) = new Tupling10[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10](_1, _2, _3, _4, _5, _6, _7, _8, _9, _10)

    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2]) = ing(_1, _2)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3]) = ing(_1, _2, _3)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4]) = ing(_1, _2, _3, _4)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5]) = ing(_1, _2, _3, _4, _5)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6]) = ing(_1, _2, _3, _4, _5, _6)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6], _7: Aggregator[DATUM, C7]) = ing(_1, _2, _3, _4, _5, _6, _7)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6], _7: Aggregator[DATUM, C7], _8: Aggregator[DATUM, C8]) = ing(_1, _2, _3, _4, _5, _6, _7, _8)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6], _7: Aggregator[DATUM, C7], _8: Aggregator[DATUM, C8], _9: Aggregator[DATUM, C9]) = ing(_1, _2, _3, _4, _5, _6, _7, _8, _9)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], C10 <: Container[C10]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6], _7: Aggregator[DATUM, C7], _8: Aggregator[DATUM, C8], _9: Aggregator[DATUM, C9], _10: Aggregator[DATUM, C10]) = ing(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10)

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
          throw new JsonFormatException(json, name + " (too many branches)")

      case _ => throw new JsonFormatException(json, name)
    }

    private[histogrammar] def typedata[CONTAINER <: Container[CONTAINER]](sub: CONTAINER) = JsonObject("type" -> JsonString(sub.factory.name), "data" -> sub.toJsonFragment)
  }

  class Tupled2[C1 <: Container[C1], C2 <: Container[C2]](_1: C1, _2: C2) extends scala.Tuple2[C1, C2](_1, _2) with Container[Tupled2[C1, C2]] {
    def factory = Tuple
    def +(that: Tupled2[C1, C2]) = new Tupled2[C1, C2](this._1 + that._1, this._2 + that._2)
    def +[DATUM](that: Tupling2[DATUM, C1, C2]) = new Tupling2[DATUM, C1, C2](this._1 + that._1, this._2 + that._2)
    def toJsonFragment = JsonArray(Tuple.typedata(_1), Tuple.typedata(_2))
    override def toString = s"""Tupled2[${_1}, ${_2}]"""
  }
  class Tupled3[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](_1: C1, _2: C2, _3: C3) extends scala.Tuple3[C1, C2, C3](_1, _2, _3) with Container[Tupled3[C1, C2, C3]] {
    def factory = Tuple
    def +(that: Tupled3[C1, C2, C3]) = new Tupled3[C1, C2, C3](this._1 + that._1, this._2 + that._2, this._3 + that._3)
    def +[DATUM](that: Tupling3[DATUM, C1, C2, C3]) = new Tupling3[DATUM, C1, C2, C3](this._1 + that._1, this._2 + that._2, this._3 + that._3)
    def toJsonFragment = JsonArray(Tuple.typedata(_1), Tuple.typedata(_2), Tuple.typedata(_3))
    override def toString = s"""Tupled3[${_1}, ${_2}, ${_3}]"""
  }
  class Tupled4[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](_1: C1, _2: C2, _3: C3, _4: C4) extends scala.Tuple4[C1, C2, C3, C4](_1, _2, _3, _4) with Container[Tupled4[C1, C2, C3, C4]] {
    def factory = Tuple
    def +(that: Tupled4[C1, C2, C3, C4]) = new Tupled4[C1, C2, C3, C4](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4)
    def +[DATUM](that: Tupling4[DATUM, C1, C2, C3, C4]) = new Tupling4[DATUM, C1, C2, C3, C4](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4)
    def toJsonFragment = JsonArray(Tuple.typedata(_1), Tuple.typedata(_2), Tuple.typedata(_3), Tuple.typedata(_4))
    override def toString = s"""Tupled4[${_1}, ${_2}, ${_3}, ${_4}]"""
  }
  class Tupled5[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5) extends scala.Tuple5[C1, C2, C3, C4, C5](_1, _2, _3, _4, _5) with Container[Tupled5[C1, C2, C3, C4, C5]] {
    def factory = Tuple
    def +(that: Tupled5[C1, C2, C3, C4, C5]) = new Tupled5[C1, C2, C3, C4, C5](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5)
    def +[DATUM](that: Tupling5[DATUM, C1, C2, C3, C4, C5]) = new Tupling5[DATUM, C1, C2, C3, C4, C5](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5)
    def toJsonFragment = JsonArray(Tuple.typedata(_1), Tuple.typedata(_2), Tuple.typedata(_3), Tuple.typedata(_4), Tuple.typedata(_5))
    override def toString = s"""Tupled5[${_1}, ${_2}, ${_3}, ${_4}, ${_5}]"""
  }
  class Tupled6[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6) extends scala.Tuple6[C1, C2, C3, C4, C5, C6](_1, _2, _3, _4, _5, _6) with Container[Tupled6[C1, C2, C3, C4, C5, C6]] {
    def factory = Tuple
    def +(that: Tupled6[C1, C2, C3, C4, C5, C6]) = new Tupled6[C1, C2, C3, C4, C5, C6](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6)
    def +[DATUM](that: Tupled6[DATUM, C1, C2, C3, C4, C5, C6]) = new Tupling6[DATUM, C1, C2, C3, C4, C5, C6](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6)
    def toJsonFragment = JsonArray(Tuple.typedata(_1), Tuple.typedata(_2), Tuple.typedata(_3), Tuple.typedata(_4), Tuple.typedata(_5), Tuple.typedata(_6))
    override def toString = s"""Tupled6[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}]"""
  }
  class Tupled7[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7) extends scala.Tuple7[C1, C2, C3, C4, C5, C6, C7](_1, _2, _3, _4, _5, _6, _7) with Container[Tupled7[C1, C2, C3, C4, C5, C6, C7]] {
    def factory = Tuple
    def +(that: Tupled7[C1, C2, C3, C4, C5, C6, C7]) = new Tupled7[C1, C2, C3, C4, C5, C6, C7](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7)
    def +[DATUM](that: Tupling7[DATUM, C1, C2, C3, C4, C5, C6, C7]) = new Tupling7[DATUM, C1, C2, C3, C4, C5, C6, C7](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7)
    def toJsonFragment = JsonArray(Tuple.typedata(_1), Tuple.typedata(_2), Tuple.typedata(_3), Tuple.typedata(_4), Tuple.typedata(_5), Tuple.typedata(_6), Tuple.typedata(_7))
    override def toString = s"""Tupled7[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}]"""
  }
  class Tupled8[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7, _8: C8) extends scala.Tuple8[C1, C2, C3, C4, C5, C6, C7, C8](_1, _2, _3, _4, _5, _6, _7, _8) with Container[Tupled8[C1, C2, C3, C4, C5, C6, C7, C8]] {
    def factory = Tuple
    def +(that: Tupled8[C1, C2, C3, C4, C5, C6, C7, C8]) = new Tupled8[C1, C2, C3, C4, C5, C6, C7, C8](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7, this._8 + that._8)
    def +[DATUM](that: Tupling8[DATUM, C1, C2, C3, C4, C5, C6, C7, C8]) = new Tupling8[DATUM, C1, C2, C3, C4, C5, C6, C7, C8](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7, this._8 + that._8)
    def toJsonFragment = JsonArray(Tuple.typedata(_1), Tuple.typedata(_2), Tuple.typedata(_3), Tuple.typedata(_4), Tuple.typedata(_5), Tuple.typedata(_6), Tuple.typedata(_7), Tuple.typedata(_8))
    override def toString = s"""Tupled8[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}, ${_8}]"""
  }
  class Tupled9[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7, _8: C8, _9: C9) extends scala.Tuple9[C1, C2, C3, C4, C5, C6, C7, C8, C9](_1, _2, _3, _4, _5, _6, _7, _8, _9) with Container[Tupled9[C1, C2, C3, C4, C5, C6, C7, C8, C9]] {
    def factory = Tuple
    def +(that: Tupled9[C1, C2, C3, C4, C5, C6, C7, C8, C9]) = new Tupled9[C1, C2, C3, C4, C5, C6, C7, C8, C9](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7, this._8 + that._8, this._9 + that._9)
    def +[DATUM](that: Tupling9[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9]) = new Tupling9[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7, this._8 + that._8, this._9 + that._9)
    def toJsonFragment = JsonArray(Tuple.typedata(_1), Tuple.typedata(_2), Tuple.typedata(_3), Tuple.typedata(_4), Tuple.typedata(_5), Tuple.typedata(_6), Tuple.typedata(_7), Tuple.typedata(_8), Tuple.typedata(_9))
    override def toString = s"""Tupled9[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}, ${_8}, ${_9}]"""
  }
  class Tupled10[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], C10 <: Container[C10]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7, _8: C8, _9: C9, _10: C10) extends scala.Tuple10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10](_1, _2, _3, _4, _5, _6, _7, _8, _9, _10) with Container[Tupled10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]] {
    def factory = Tuple
    def +(that: Tupled10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]) = new Tupled10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7, this._8 + that._8, this._9 + that._9, this._10 + that._10)
    def +[DATUM](that: Tupling10[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]) = new Tupling10[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7, this._8 + that._8, this._9 + that._9, this._10 + that._10)
    def toJsonFragment = JsonArray(Tuple.typedata(_1), Tuple.typedata(_2), Tuple.typedata(_3), Tuple.typedata(_4), Tuple.typedata(_5), Tuple.typedata(_6), Tuple.typedata(_7), Tuple.typedata(_8), Tuple.typedata(_9), Tuple.typedata(_10))
    override def toString = s"""Tupled10[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}, ${_8}, ${_9}, ${_10}]"""
  }

  class Tupling2[DATUM, C1 <: Container[C1], C2 <: Container[C2]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2]) extends scala.Tuple2[Aggregator[DATUM, C1], Aggregator[DATUM, C2]](_1, _2) with Aggregator[DATUM, Tupled2[C1, C2]] {
    def factory = Tuple
    def +(that: Tupled2[C1, C2]) = new Tupling2[DATUM, C1, C2](this._1 + that._1, this._2 + that._2)
    def +(that: Tupling2[DATUM, C1, C2]) = new Tupling2[DATUM, C1, C2](this._1 + that._1, this._2 + that._2)
    def fill(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.fill(x)
        _2.fill(x)
      }
    }
    def toContainer = new Tupled2[C1, C2](_1.toContainer, _2.toContainer)
    override def toString = s"""Tupling2[${_1}, ${_2}]"""
  }
  class Tupling3[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3]) extends scala.Tuple3[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3]](_1, _2, _3) with Aggregator[DATUM, Tupled3[C1, C2, C3]] {
    def factory = Tuple
    def +(that: Tupled3[C1, C2, C3]) = new Tupling3[DATUM, C1, C2, C3](this._1 + that._1, this._2 + that._2, this._3 + that._3)
    def +(that: Tupling3[DATUM, C1, C2, C3]) = new Tupling10[DATUM, C1, C2, C3](this._1 + that._1, this._2 + that._2, this._3 + that._3)
    def fill(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.fill(x)
        _2.fill(x)
        _3.fill(x)
      }
    }
    def toContainer = new Tupled3[C1, C2, C3](_1.toContainer, _2.toContainer, _3.toContainer)
    override def toString = s"""Tupling3[${_1}, ${_2}, ${_3}]"""
  }
  class Tupling4[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4]) extends scala.Tuple4[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3], Aggregator[DATUM, C4]](_1, _2, _3, _4) with Aggregator[DATUM, Tupled4[C1, C2, C3, C4]] {
    def factory = Tuple
    def +(that: Tupled4[C1, C2, C3, C4]) = new Tupling4[DATUM, C1, C2, C3, C4](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4)
    def +(that: Tupling4[DATUM, C1, C2, C3, C4]) = new Tupling4[DATUM, C1, C2, C3, C4](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4)
    def fill(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.fill(x)
        _2.fill(x)
        _3.fill(x)
        _4.fill(x)
      }
    }
    def toContainer = new Tupled4[C1, C2, C3, C4](_1.toContainer, _2.toContainer, _3.toContainer, _4.toContainer)
    override def toString = s"""Tupling4[${_1}, ${_2}, ${_3}, ${_4}]"""
  }
  class Tupling5[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5]) extends scala.Tuple5[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3], Aggregator[DATUM, C4], Aggregator[DATUM, C5]](_1, _2, _3, _4, _5) with Aggregator[DATUM, Tupled5[C1, C2, C3, C4, C5]] {
    def factory = Tuple
    def +(that: Tupled5[C1, C2, C3, C4, C5]) = new Tupling5[DATUM, C1, C2, C3, C4, C5](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5)
    def +(that: Tupling5[DATUM, C1, C2, C3, C4, C5]) = new Tupling5[DATUM, C1, C2, C3, C4, C5](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5)
    def fill(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.fill(x)
        _2.fill(x)
        _3.fill(x)
        _4.fill(x)
        _5.fill(x)
      }
    }
    def toContainer = new Tupled5[C1, C2, C3, C4, C5](_1.toContainer, _2.toContainer, _3.toContainer, _4.toContainer, _5.toContainer)
    override def toString = s"""Tupling5[${_1}, ${_2}, ${_3}, ${_4}, ${_5}]"""
  }
  class Tupling6[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6]) extends scala.Tuple6[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3], Aggregator[DATUM, C4], Aggregator[DATUM, C5], Aggregator[DATUM, C6]](_1, _2, _3, _4, _5, _6) with Aggregator[DATUM, Tupled6[C1, C2, C3, C4, C5, C6]] {
    def factory = Tuple
    def +(that: Tupled6[C1, C2, C3, C4, C5, C6]) = new Tupling6[DATUM, C1, C2, C3, C4, C5, C6](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6)
    def +(that: Tupling6[DATUM, C1, C2, C3, C4, C5, C6]) = new Tupling6[DATUM, C1, C2, C3, C4, C5, C6](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6)
    def fill(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.fill(x)
        _2.fill(x)
        _3.fill(x)
        _4.fill(x)
        _5.fill(x)
        _6.fill(x)
      }
    }
    def toContainer = new Tupled6[C1, C2, C3, C4, C5, C6](_1.toContainer, _2.toContainer, _3.toContainer, _4.toContainer, _5.toContainer, _6.toContainer)
    override def toString = s"""Tupling6[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}]"""
  }
  class Tupling7[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6], _7: Aggregator[DATUM, C7]) extends scala.Tuple7[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3], Aggregator[DATUM, C4], Aggregator[DATUM, C5], Aggregator[DATUM, C6], Aggregator[DATUM, C7]](_1, _2, _3, _4, _5, _6, _7) with Aggregator[DATUM, Tupled7[C1, C2, C3, C4, C5, C6, C7]] {
    def factory = Tuple
    def +(that: Tupled7[C1, C2, C3, C4, C5, C6, C7]) = new Tupling7[DATUM, C1, C2, C3, C4, C5, C6, C7](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7)
    def +(that: Tupling7[DATUM, C1, C2, C3, C4, C5, C6, C7]) = new Tupling7[DATUM, C1, C2, C3, C4, C5, C6, C7](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7)
    def fill(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.fill(x)
        _2.fill(x)
        _3.fill(x)
        _4.fill(x)
        _5.fill(x)
        _6.fill(x)
        _7.fill(x)
      }
    }
    def toContainer = new Tupled7[C1, C2, C3, C4, C5, C6, C7](_1.toContainer, _2.toContainer, _3.toContainer, _4.toContainer, _5.toContainer, _6.toContainer, _7.toContainer)
    override def toString = s"""Tupling7[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}]"""
  }
  class Tupling8[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6], _7: Aggregator[DATUM, C7], _8: Aggregator[DATUM, C8]) extends scala.Tuple8[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3], Aggregator[DATUM, C4], Aggregator[DATUM, C5], Aggregator[DATUM, C6], Aggregator[DATUM, C7], Aggregator[DATUM, C8]](_1, _2, _3, _4, _5, _6, _7, _8) with Aggregator[DATUM, Tupled8[C1, C2, C3, C4, C5, C6, C7, C8]] {
    def factory = Tuple
    def +(that: Tupled8[C1, C2, C3, C4, C5, C6, C7, C8]) = new Tupling8[DATUM, C1, C2, C3, C4, C5, C6, C7, C8](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7, this._8 + that._8)
    def +(that: Tupling8[DATUM, C1, C2, C3, C4, C5, C6, C7, C8]) = new Tupling8[DATUM, C1, C2, C3, C4, C5, C6, C7, C8](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7, this._8 + that._8)
    def fill(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.fill(x)
        _2.fill(x)
        _3.fill(x)
        _4.fill(x)
        _5.fill(x)
        _6.fill(x)
        _7.fill(x)
        _8.fill(x)
      }
    }
    def toContainer = new Tupled8[C1, C2, C3, C4, C5, C6, C7, C8](_1.toContainer, _2.toContainer, _3.toContainer, _4.toContainer, _5.toContainer, _6.toContainer, _7.toContainer, _8.toContainer)
    override def toString = s"""Tupling8[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}, ${_8}]"""
  }
  class Tupling9[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6], _7: Aggregator[DATUM, C7], _8: Aggregator[DATUM, C8], _9: Aggregator[DATUM, C9]) extends scala.Tuple9[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3], Aggregator[DATUM, C4], Aggregator[DATUM, C5], Aggregator[DATUM, C6], Aggregator[DATUM, C7], Aggregator[DATUM, C8], Aggregator[DATUM, C9]](_1, _2, _3, _4, _5, _6, _7, _8, _9) with Aggregator[DATUM, Tupled9[C1, C2, C3, C4, C5, C6, C7, C8, C9]] {
    def factory = Tuple
    def +(that: Tupled9[C1, C2, C3, C4, C5, C6, C7, C8, C9]) = new Tupling9[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7, this._8 + that._8, this._9 + that._9)
    def +(that: Tupling9[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9]) = new Tupling9[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7, this._8 + that._8, this._9 + that._9)
    def fill(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.fill(x)
        _2.fill(x)
        _3.fill(x)
        _4.fill(x)
        _5.fill(x)
        _6.fill(x)
        _7.fill(x)
        _8.fill(x)
        _9.fill(x)
      }
    }
    def toContainer = new Tupled9[C1, C2, C3, C4, C5, C6, C7, C8, C9](_1.toContainer, _2.toContainer, _3.toContainer, _4.toContainer, _5.toContainer, _6.toContainer, _7.toContainer, _8.toContainer, _9.toContainer)
    override def toString = s"""Tupling9[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}, ${_8}, ${_9}]"""
  }
  class Tupling10[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], C10 <: Container[C10]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6], _7: Aggregator[DATUM, C7], _8: Aggregator[DATUM, C8], _9: Aggregator[DATUM, C9], _10: Aggregator[DATUM, C10]) extends scala.Tuple10[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3], Aggregator[DATUM, C4], Aggregator[DATUM, C5], Aggregator[DATUM, C6], Aggregator[DATUM, C7], Aggregator[DATUM, C8], Aggregator[DATUM, C9], Aggregator[DATUM, C10]](_1, _2, _3, _4, _5, _6, _7, _8, _9, _10) with Aggregator[DATUM, Tupled10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]] {
    def factory = Tuple
    def +(that: Tupled10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]) = new Tupling10[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7, this._8 + that._8, this._9 + that._9, this._10 + that._10)
    def +(that: Tupling10[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]) = new Tupling10[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7, this._8 + that._8, this._9 + that._9, this._10 + that._10)
    def fill(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.fill(x)
        _2.fill(x)
        _3.fill(x)
        _4.fill(x)
        _5.fill(x)
        _6.fill(x)
        _7.fill(x)
        _8.fill(x)
        _9.fill(x)
        _10.fill(x)
      }
    }
    def toContainer = new Tupled10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10](_1.toContainer, _2.toContainer, _3.toContainer, _4.toContainer, _5.toContainer, _6.toContainer, _7.toContainer, _8.toContainer, _9.toContainer, _10.toContainer)
    override def toString = s"""Tupling10[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}, ${_8}, ${_9}, ${_10}]"""
  }
}
