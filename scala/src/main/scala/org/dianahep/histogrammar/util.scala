package org.dianahep.histogrammar

package util {
  case class Cache[DOMAIN, RANGE](f: DOMAIN => RANGE) extends Function1[DOMAIN, RANGE] {
    private var last: Option[(DOMAIN, RANGE)] = None
    def apply(x: DOMAIN): RANGE = (x, last) match {
      case (xref: AnyRef, Some((oldx: AnyRef, oldy))) if (xref eq oldx) => oldy
      case (_,            Some((oldx, oldy)))         if (x == oldx)    => oldy
      case _ =>
        val y = f(x)
        last = Some(x -> y)
        y
    }
    def clear() { last = None }
  }
}
