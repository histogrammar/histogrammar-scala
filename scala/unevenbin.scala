import scala.collection.immutable.TreeSet
import scala.collection.SortedMap

trait MetricOrdering[T] extends Ordering[T] {
  def distance(x: T, y: T): Double
  def compare(x: T, y: T) = {
    val d = distance(x, y)
    if (d > 0.0) 1
    else if (d < 0.0) -1
    else 0
  }
}

class MetricSortedMap[A, B](elems: (A, B)*)(implicit val ordering: MetricOrdering[A]) extends SortedMap[A, B] {
  // when the TreeSet searches for an element, keep track of the best distance it finds
  private val best = new java.lang.ThreadLocal[(Double, A, B)]
  best.set((-1.0, null.asInstanceOf[A], null.asInstanceOf[B]))

  private val ord = new MetricOrdering[(A, B)] {
    def distance(x: (A, B), y: (A, B)) = {
      val diff = ordering.distance(x._1, y._1)
      val absdiff = Math.abs(diff)

      (x, y) match {
        case ((to, null), (pos, obj)) =>
          if (absdiff < best.get._1)
            best.set((absdiff, pos, obj))

        case ((pos, obj), (to, null)) =>
          if (absdiff < best.get._1)
            best.set((absdiff, pos, obj))

        case _ =>
      }

      diff
    }
  }

  // use a TreeSet as a backing (not TreeMap because we need to get the whole pair back when we query it)
  private val treeSet = TreeSet[(A, B)](elems: _*)(ord)

  // find the closest key and return: (distance to key, the key, its associated value)
  def closest(to: A): (Double, A, B) = {
    treeSet.headOption match {
      case Some((pos, obj)) => best.set((ordering.distance(to, pos), pos, obj))
      case None =>
        throw new java.util.NoSuchElementException("SortedMap has no elements, and hence no closest element")
    }

    treeSet((to, null.asInstanceOf[B]))  // called for its side effect on "best"

    best.get
  }

  // satisfy the contract (I don't care about these methods; could throw UnsupportedOperationException)
  def +[B1 >: B](kv: (A, B1)): SortedMap[A, B1] = new MetricSortedMap[A, B](elems :+ (kv._1, kv._2.asInstanceOf[B]): _*)
  def -(key: A): SortedMap[A, B] = new MetricSortedMap[A, B](elems.filter(_._1 != key): _*)
  def get(key: A): Option[B] = treeSet.find(_._1 == key).map(_._2)
  def iterator: Iterator[(A, B)] = treeSet.iterator
  def rangeImpl(from: Option[A], until: Option[A]): SortedMap[A, B] = new MetricSortedMap[A, B](treeSet.rangeImpl(from.map((_, null.asInstanceOf[B])), until.map((_, null.asInstanceOf[B]))).toSeq: _*)
}

implicit val doubleOrdering: MetricOrdering[Double] = new MetricOrdering[Double] {
  def distance(x: Double, y: Double) = x - y
}

val stuff = new MetricSortedMap[Double, String](3.3 -> "three", 1.1 -> "one", 5.5 -> "five", 4.4 -> "four", 2.2 -> "two")


stuff.closest(1.5)
stuff.closest(1000)
stuff.closest(-1000)
stuff.closest(3.3)
stuff.closest(3.4)
stuff.closest(3.2)

