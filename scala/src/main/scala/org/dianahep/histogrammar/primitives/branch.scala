package org.dianahep

import org.dianahep.histogrammar.json._

package histogrammar {
  //////////////////////////////////////////////////////////////// Branched/Branching

  // this is a *type-safe* tuple, with explicit cases for 2 through ?? branches
  object Branched extends ContainerFactory {
    val name = "Branched"

    def apply[C1 <: Container[C1], C2 <: Container[C2]](_1: C1, _2: C2) = new Branched2[C1, C2](_1, _2)
    def apply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](_1: C1, _2: C2, _3: C3) = new Branched3[C1, C2, C3](_1, _2, _3)
    def apply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](_1: C1, _2: C2, _3: C3, _4: C4) = new Branched4[C1, C2, C3, C4](_1, _2, _3, _4)
    def apply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5) = new Branched5[C1, C2, C3, C4, C5](_1, _2, _3, _4, _5)
    def apply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6) = new Branched6[C1, C2, C3, C4, C5, C6](_1, _2, _3, _4, _5, _6)
    def apply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7) = new Branched7[C1, C2, C3, C4, C5, C6, C7](_1, _2, _3, _4, _5, _6, _7)
    def apply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7, _8: C8) = new Branched8[C1, C2, C3, C4, C5, C6, C7, C8](_1, _2, _3, _4, _5, _6, _7, _8)
    def apply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7, _8: C8, _9: C9) = new Branched9[C1, C2, C3, C4, C5, C6, C7, C8, C9](_1, _2, _3, _4, _5, _6, _7, _8, _9)
    def apply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], C10 <: Container[C10]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7, _8: C8, _9: C9, _10: C10) = new Branched10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10](_1, _2, _3, _4, _5, _6, _7, _8, _9, _10)

    def unapply[C1 <: Container[C1], C2 <: Container[C2]](x: Branched2[C1, C2]) = Some((x._1, x._2))
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](x: Branched3[C1, C2, C3]) = Some((x._1, x._2, x._3))
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](x: Branched4[C1, C2, C3, C4]) = Some((x._1, x._2, x._3, x._4))
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](x: Branched5[C1, C2, C3, C4, C5]) = Some((x._1, x._2, x._3, x._4, x._5))
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](x: Branched6[C1, C2, C3, C4, C5, C6]) = Some((x._1, x._2, x._3, x._4, x._5, x._6))
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](x: Branched7[C1, C2, C3, C4, C5, C6, C7]) = Some((x._1, x._2, x._3, x._4, x._5, x._6, x._7))
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](x: Branched8[C1, C2, C3, C4, C5, C6, C7, C8]) = Some((x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8))
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](x: Branched9[C1, C2, C3, C4, C5, C6, C7, C8, C9]) = Some((x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9))
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], C10 <: Container[C10]](x: Branched10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]) = Some((x._1, x._2, x._3, x._4, x._5, x._6, x._7, x._8, x._9, x._10))

    def fromJsonFragment(json: Json): Container[_] = json match {
      case JsonArray(elements @ _*) if (elements.size >= 2) =>
        val subs = elements.zipWithIndex map {
          case (JsonObject(pairs @ _*), i) if (pairs.keySet == Set("type", "data")) =>
            val get = pairs.toMap
            (get("type"), get("data")) match {
              case (JsonString(factory), sub) => ContainerFactory(factory).fromJsonFragment(sub)
              case _ => throw new JsonFormatException(json, s"Branched item $i")
            }
          case _ => throw new JsonFormatException(json, "Branched item")
        }
        if (subs.size == 2) new Branched2[Container[_], Container[_]](subs(0), subs(1)).asInstanceOf[Container[_]]
        else if (subs.size == 3) new Branched3[Container[_], Container[_], Container[_]](subs(0), subs(1), subs(2)).asInstanceOf[Container[_]]
        else if (subs.size == 4) new Branched4[Container[_], Container[_], Container[_], Container[_]](subs(0), subs(1), subs(2), subs(3)).asInstanceOf[Container[_]]
        else if (subs.size == 5) new Branched5[Container[_], Container[_], Container[_], Container[_], Container[_]](subs(0), subs(1), subs(2), subs(3), subs(4)).asInstanceOf[Container[_]]
        else if (subs.size == 6) new Branched6[Container[_], Container[_], Container[_], Container[_], Container[_], Container[_]](subs(0), subs(1), subs(2), subs(3), subs(4), subs(5)).asInstanceOf[Container[_]]
        else if (subs.size == 7) new Branched7[Container[_], Container[_], Container[_], Container[_], Container[_], Container[_], Container[_]](subs(0), subs(1), subs(2), subs(3), subs(4), subs(5), subs(6)).asInstanceOf[Container[_]]
        else if (subs.size == 8) new Branched8[Container[_], Container[_], Container[_], Container[_], Container[_], Container[_], Container[_], Container[_]](subs(0), subs(1), subs(2), subs(3), subs(4), subs(5), subs(6), subs(7)).asInstanceOf[Container[_]]
        else if (subs.size == 9) new Branched9[Container[_], Container[_], Container[_], Container[_], Container[_], Container[_], Container[_], Container[_], Container[_]](subs(0), subs(1), subs(2), subs(3), subs(4), subs(5), subs(6), subs(7), subs(8)).asInstanceOf[Container[_]]
        else if (subs.size == 10) new Branched10[Container[_], Container[_], Container[_], Container[_], Container[_], Container[_], Container[_], Container[_], Container[_], Container[_]](subs(0), subs(1), subs(2), subs(3), subs(4), subs(5), subs(6), subs(7), subs(8), subs(9)).asInstanceOf[Container[_]]
        else
          throw new JsonFormatException(json, "Branched (too many branches)")

      case _ => throw new JsonFormatException(json, "Branched")
    }

    private[histogrammar] def typedata[CONTAINER <: Container[CONTAINER]](sub: CONTAINER) = JsonObject("type" -> JsonString(sub.factory.name), "data" -> sub.toJsonFragment)
  }
  class Branched2[C1 <: Container[C1], C2 <: Container[C2]](_1: C1, _2: C2) extends Tuple2[C1, C2](_1, _2) with Container[Branched2[C1, C2]] {
    def factory = Branched
    def +(that: Branched2[C1, C2]) = new Branched2[C1, C2](this._1 + that._1, this._2 + that._2)
    def toJsonFragment = JsonArray(Branched.typedata(_1), Branched.typedata(_2))
    override def toString = s"""Branched2[${_1}, ${_2}]"""
  }
  class Branched3[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](_1: C1, _2: C2, _3: C3) extends Tuple3[C1, C2, C3](_1, _2, _3) with Container[Branched3[C1, C2, C3]] {
    def factory = Branched
    def +(that: Branched3[C1, C2, C3]) = new Branched3[C1, C2, C3](this._1 + that._1, this._2 + that._2, this._3 + that._3)
    def toJsonFragment = JsonArray(Branched.typedata(_1), Branched.typedata(_2), Branched.typedata(_3))
    override def toString = s"""Branched3[${_1}, ${_2}, ${_3}]"""
  }
  class Branched4[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](_1: C1, _2: C2, _3: C3, _4: C4) extends Tuple4[C1, C2, C3, C4](_1, _2, _3, _4) with Container[Branched4[C1, C2, C3, C4]] {
    def factory = Branched
    def +(that: Branched4[C1, C2, C3, C4]) = new Branched4[C1, C2, C3, C4](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4)
    def toJsonFragment = JsonArray(Branched.typedata(_1), Branched.typedata(_2), Branched.typedata(_3), Branched.typedata(_4))
    override def toString = s"""Branched4[${_1}, ${_2}, ${_3}, ${_4}]"""
  }
  class Branched5[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5) extends Tuple5[C1, C2, C3, C4, C5](_1, _2, _3, _4, _5) with Container[Branched5[C1, C2, C3, C4, C5]] {
    def factory = Branched
    def +(that: Branched5[C1, C2, C3, C4, C5]) = new Branched5[C1, C2, C3, C4, C5](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5)
    def toJsonFragment = JsonArray(Branched.typedata(_1), Branched.typedata(_2), Branched.typedata(_3), Branched.typedata(_4), Branched.typedata(_5))
    override def toString = s"""Branched5[${_1}, ${_2}, ${_3}, ${_4}, ${_5}]"""
  }
  class Branched6[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6) extends Tuple6[C1, C2, C3, C4, C5, C6](_1, _2, _3, _4, _5, _6) with Container[Branched6[C1, C2, C3, C4, C5, C6]] {
    def factory = Branched
    def +(that: Branched6[C1, C2, C3, C4, C5, C6]) = new Branched6[C1, C2, C3, C4, C5, C6](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6)
    def toJsonFragment = JsonArray(Branched.typedata(_1), Branched.typedata(_2), Branched.typedata(_3), Branched.typedata(_4), Branched.typedata(_5), Branched.typedata(_6))
    override def toString = s"""Branched6[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}]"""
  }
  class Branched7[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7) extends Tuple7[C1, C2, C3, C4, C5, C6, C7](_1, _2, _3, _4, _5, _6, _7) with Container[Branched7[C1, C2, C3, C4, C5, C6, C7]] {
    def factory = Branched
    def +(that: Branched7[C1, C2, C3, C4, C5, C6, C7]) = new Branched7[C1, C2, C3, C4, C5, C6, C7](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7)
    def toJsonFragment = JsonArray(Branched.typedata(_1), Branched.typedata(_2), Branched.typedata(_3), Branched.typedata(_4), Branched.typedata(_5), Branched.typedata(_6), Branched.typedata(_7))
    override def toString = s"""Branched7[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}]"""
  }
  class Branched8[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7, _8: C8) extends Tuple8[C1, C2, C3, C4, C5, C6, C7, C8](_1, _2, _3, _4, _5, _6, _7, _8) with Container[Branched8[C1, C2, C3, C4, C5, C6, C7, C8]] {
    def factory = Branched
    def +(that: Branched8[C1, C2, C3, C4, C5, C6, C7, C8]) = new Branched8[C1, C2, C3, C4, C5, C6, C7, C8](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7, this._8 + that._8)
    def toJsonFragment = JsonArray(Branched.typedata(_1), Branched.typedata(_2), Branched.typedata(_3), Branched.typedata(_4), Branched.typedata(_5), Branched.typedata(_6), Branched.typedata(_7), Branched.typedata(_8))
    override def toString = s"""Branched8[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}, ${_8}]"""
  }
  class Branched9[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7, _8: C8, _9: C9) extends Tuple9[C1, C2, C3, C4, C5, C6, C7, C8, C9](_1, _2, _3, _4, _5, _6, _7, _8, _9) with Container[Branched9[C1, C2, C3, C4, C5, C6, C7, C8, C9]] {
    def factory = Branched
    def +(that: Branched9[C1, C2, C3, C4, C5, C6, C7, C8, C9]) = new Branched9[C1, C2, C3, C4, C5, C6, C7, C8, C9](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7, this._8 + that._8, this._9 + that._9)
    def toJsonFragment = JsonArray(Branched.typedata(_1), Branched.typedata(_2), Branched.typedata(_3), Branched.typedata(_4), Branched.typedata(_5), Branched.typedata(_6), Branched.typedata(_7), Branched.typedata(_8), Branched.typedata(_9))
    override def toString = s"""Branched9[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}, ${_8}, ${_9}]"""
  }
  class Branched10[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], C10 <: Container[C10]](_1: C1, _2: C2, _3: C3, _4: C4, _5: C5, _6: C6, _7: C7, _8: C8, _9: C9, _10: C10) extends Tuple10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10](_1, _2, _3, _4, _5, _6, _7, _8, _9, _10) with Container[Branched10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]] {
    def factory = Branched
    def +(that: Branched10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]) = new Branched10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10](this._1 + that._1, this._2 + that._2, this._3 + that._3, this._4 + that._4, this._5 + that._5, this._6 + that._6, this._7 + that._7, this._8 + that._8, this._9 + that._9, this._10 + that._10)
    def toJsonFragment = JsonArray(Branched.typedata(_1), Branched.typedata(_2), Branched.typedata(_3), Branched.typedata(_4), Branched.typedata(_5), Branched.typedata(_6), Branched.typedata(_7), Branched.typedata(_8), Branched.typedata(_9), Branched.typedata(_10))
    override def toString = s"""Branched10[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}, ${_8}, ${_9}, ${_10}]"""
  }

  object Branching extends AggregatorFactory {
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2]) = new Branching2[DATUM, C1, C2](_1, _2)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3]) = new Branching3[DATUM, C1, C2, C3](_1, _2, _3)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4]) = new Branching4[DATUM, C1, C2, C3, C4](_1, _2, _3, _4)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5]) = new Branching5[DATUM, C1, C2, C3, C4, C5](_1, _2, _3, _4, _5)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6]) = new Branching6[DATUM, C1, C2, C3, C4, C5, C6](_1, _2, _3, _4, _5, _6)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6], _7: Aggregator[DATUM, C7]) = new Branching7[DATUM, C1, C2, C3, C4, C5, C6, C7](_1, _2, _3, _4, _5, _6, _7)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6], _7: Aggregator[DATUM, C7], _8: Aggregator[DATUM, C8]) = new Branching8[DATUM, C1, C2, C3, C4, C5, C6, C7, C8](_1, _2, _3, _4, _5, _6, _7, _8)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6], _7: Aggregator[DATUM, C7], _8: Aggregator[DATUM, C8], _9: Aggregator[DATUM, C9]) = new Branching9[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9](_1, _2, _3, _4, _5, _6, _7, _8, _9)
    def apply[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], C10 <: Container[C10]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6], _7: Aggregator[DATUM, C7], _8: Aggregator[DATUM, C8], _9: Aggregator[DATUM, C9], _10: Aggregator[DATUM, C10]) = new Branching10[DATUM, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10](_1, _2, _3, _4, _5, _6, _7, _8, _9, _10)

    def unapply[C1 <: Container[C1], C2 <: Container[C2]](x: Branched2[C1, C2]) = Some((x._1, x._2))
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](x: Branched3[C1, C2, C3]) = Some((x._1, x._2, x._3))
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](x: Branched4[C1, C2, C3, C4]) = Some((x._1, x._2, x._3, x._4))
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](x: Branched5[C1, C2, C3, C4, C5]) = Some((x._1, x._2, x._3, x._4, x._5))
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](x: Branched6[C1, C2, C3, C4, C5, C6]) = Some((x._1, x._2, x._3, x._4, x._5, x._6))
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](x: Branched7[C1, C2, C3, C4, C5, C6, C7]) = Some((x._1, x._2, x._3, x._4, x._5, x._6), x._7)
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](x: Branched8[C1, C2, C3, C4, C5, C6, C7, C8]) = Some((x._1, x._2, x._3, x._4, x._5, x._6), x._7, x._8)
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](x: Branched9[C1, C2, C3, C4, C5, C6, C7, C8, C9]) = Some((x._1, x._2, x._3, x._4, x._5, x._6), x._7, x._8, x._9)
    def unapply[C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], C10 <: Container[C10]](x: Branched10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]) = Some((x._1, x._2, x._3, x._4, x._5, x._6), x._7, x._8, x._9, x._10)
  }
  class Branching2[DATUM, C1 <: Container[C1], C2 <: Container[C2]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2]) extends Tuple2[Aggregator[DATUM, C1], Aggregator[DATUM, C2]](_1, _2) with Aggregator[DATUM, Branched2[C1, C2]] {
    def fill(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.fill(x)
        _2.fill(x)
      }
    }
    def fix = new Branched2[C1, C2](_1.fix, _2.fix)
    override def toString = s"""Branching2[${_1}, ${_2}]"""
  }
  class Branching3[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3]) extends Tuple3[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3]](_1, _2, _3) with Aggregator[DATUM, Branched3[C1, C2, C3]] {
    def fill(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.fill(x)
        _2.fill(x)
        _3.fill(x)
      }
    }
    def fix = new Branched3[C1, C2, C3](_1.fix, _2.fix, _3.fix)
    override def toString = s"""Branching3[${_1}, ${_2}, ${_3}]"""
  }
  class Branching4[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4]) extends Tuple4[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3], Aggregator[DATUM, C4]](_1, _2, _3, _4) with Aggregator[DATUM, Branched4[C1, C2, C3, C4]] {
    def fill(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.fill(x)
        _2.fill(x)
        _3.fill(x)
        _4.fill(x)
      }
    }
    def fix = new Branched4[C1, C2, C3, C4](_1.fix, _2.fix, _3.fix, _4.fix)
    override def toString = s"""Branching4[${_1}, ${_2}, ${_3}, ${_4}]"""
  }
  class Branching5[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5]) extends Tuple5[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3], Aggregator[DATUM, C4], Aggregator[DATUM, C5]](_1, _2, _3, _4, _5) with Aggregator[DATUM, Branched5[C1, C2, C3, C4, C5]] {
    def fill(x: Weighted[DATUM]) {
      if (x.contributes) {
        _1.fill(x)
        _2.fill(x)
        _3.fill(x)
        _4.fill(x)
        _5.fill(x)
      }
    }
    def fix = new Branched5[C1, C2, C3, C4, C5](_1.fix, _2.fix, _3.fix, _4.fix, _5.fix)
    override def toString = s"""Branching5[${_1}, ${_2}, ${_3}, ${_4}, ${_5}]"""
  }
  class Branching6[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6]) extends Tuple6[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3], Aggregator[DATUM, C4], Aggregator[DATUM, C5], Aggregator[DATUM, C6]](_1, _2, _3, _4, _5, _6) with Aggregator[DATUM, Branched6[C1, C2, C3, C4, C5, C6]] {
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
    def fix = new Branched6[C1, C2, C3, C4, C5, C6](_1.fix, _2.fix, _3.fix, _4.fix, _5.fix, _6.fix)
    override def toString = s"""Branching6[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}]"""
  }
  class Branching7[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6], _7: Aggregator[DATUM, C7]) extends Tuple7[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3], Aggregator[DATUM, C4], Aggregator[DATUM, C5], Aggregator[DATUM, C6], Aggregator[DATUM, C7]](_1, _2, _3, _4, _5, _6, _7) with Aggregator[DATUM, Branched7[C1, C2, C3, C4, C5, C6, C7]] {
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
    def fix = new Branched7[C1, C2, C3, C4, C5, C6, C7](_1.fix, _2.fix, _3.fix, _4.fix, _5.fix, _6.fix, _7.fix)
    override def toString = s"""Branching7[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}]"""
  }
  class Branching8[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6], _7: Aggregator[DATUM, C7], _8: Aggregator[DATUM, C8]) extends Tuple8[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3], Aggregator[DATUM, C4], Aggregator[DATUM, C5], Aggregator[DATUM, C6], Aggregator[DATUM, C7], Aggregator[DATUM, C8]](_1, _2, _3, _4, _5, _6, _7, _8) with Aggregator[DATUM, Branched8[C1, C2, C3, C4, C5, C6, C7, C8]] {
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
    def fix = new Branched8[C1, C2, C3, C4, C5, C6, C7, C8](_1.fix, _2.fix, _3.fix, _4.fix, _5.fix, _6.fix, _7.fix, _8.fix)
    override def toString = s"""Branching8[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}, ${_8}]"""
  }
  class Branching9[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6], _7: Aggregator[DATUM, C7], _8: Aggregator[DATUM, C8], _9: Aggregator[DATUM, C9]) extends Tuple9[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3], Aggregator[DATUM, C4], Aggregator[DATUM, C5], Aggregator[DATUM, C6], Aggregator[DATUM, C7], Aggregator[DATUM, C8], Aggregator[DATUM, C9]](_1, _2, _3, _4, _5, _6, _7, _8, _9) with Aggregator[DATUM, Branched9[C1, C2, C3, C4, C5, C6, C7, C8, C9]] {
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
    def fix = new Branched9[C1, C2, C3, C4, C5, C6, C7, C8, C9](_1.fix, _2.fix, _3.fix, _4.fix, _5.fix, _6.fix, _7.fix, _8.fix, _9.fix)
    override def toString = s"""Branching9[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}, ${_8}, ${_9}]"""
  }
  class Branching10[DATUM, C1 <: Container[C1], C2 <: Container[C2], C3 <: Container[C3], C4 <: Container[C4], C5 <: Container[C5], C6 <: Container[C6], C7 <: Container[C7], C8 <: Container[C8], C9 <: Container[C9], C10 <: Container[C10]](_1: Aggregator[DATUM, C1], _2: Aggregator[DATUM, C2], _3: Aggregator[DATUM, C3], _4: Aggregator[DATUM, C4], _5: Aggregator[DATUM, C5], _6: Aggregator[DATUM, C6], _7: Aggregator[DATUM, C7], _8: Aggregator[DATUM, C8], _9: Aggregator[DATUM, C9], _10: Aggregator[DATUM, C10]) extends Tuple10[Aggregator[DATUM, C1], Aggregator[DATUM, C2], Aggregator[DATUM, C3], Aggregator[DATUM, C4], Aggregator[DATUM, C5], Aggregator[DATUM, C6], Aggregator[DATUM, C7], Aggregator[DATUM, C8], Aggregator[DATUM, C9], Aggregator[DATUM, C10]](_1, _2, _3, _4, _5, _6, _7, _8, _9, _10) with Aggregator[DATUM, Branched10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10]] {
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
    def fix = new Branched10[C1, C2, C3, C4, C5, C6, C7, C8, C9, C10](_1.fix, _2.fix, _3.fix, _4.fix, _5.fix, _6.fix, _7.fix, _8.fix, _9.fix, _10.fix)
    override def toString = s"""Branching10[${_1}, ${_2}, ${_3}, ${_4}, ${_5}, ${_6}, ${_7}, ${_8}, ${_9}, ${_10}]"""
  }
}
