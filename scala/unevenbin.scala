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

trait MetricSortedMap[A, B] extends SortedMap[A, B] {
  def closest(to: A): (A, B)   // distance A and object B
  override def ordering: MetricOrdering[A]
}

class MetricSortedMapDouble[B](elems: (Double, B)*) extends MetricSortedMap[Double, B] {
  private val best = new java.lang.ThreadLocal[(Double, B)]
  best.set((-1.0, null.asInstanceOf[B]))

  val ord = new MetricOrdering[(Double, B)] {
    def distance(x: (Double, B), y: (Double, B)) = {
      val diff = x._1 - y._1
      val absdiff = Math.abs(diff)

      (x, y) match {
        case ((to, null), (pos, obj)) =>
          if (absdiff < best.get._1)
            best.set((absdiff, obj))

        case ((pos, obj), (to, null)) =>
          if (absdiff < best.get._1)
            best.set((absdiff, obj))

        case _ =>
      }

      diff
    }
  }

  private val treeSet = TreeSet[(Double, B)](elems: _*)(ord)

  // satisfy the contract (I don't care about these methods; could throw UnsupportedOperationException)
  def +[B1 >: B](kv: (Double, B1)): SortedMap[Double, B1] = new MetricSortedMapDouble[B](elems :+ (kv._1, kv._2.asInstanceOf[B]): _*)
  def -(key: Double): SortedMap[Double, B] = new MetricSortedMapDouble[B](elems.filter(_._1 != key): _*)
  def get(key: Double): Option[B] = treeSet.find(_._1 == key).map(_._2)
  def iterator: Iterator[(Double, B)] = treeSet.iterator
  def ordering: MetricOrdering[Double] = new MetricOrdering[Double] { def distance(x: Double, y: Double) = x - y }
  def rangeImpl(from: Option[Double], until: Option[Double]): SortedMap[Double, B] = new MetricSortedMapDouble[B](treeSet.rangeImpl(from.map((_, null.asInstanceOf[B])), until.map((_, null.asInstanceOf[B]))).toSeq: _*)

  // find the closest key and return it, along with its value
  def closest(to: Double): (Double, B) = {
    treeSet.headOption match {
      case Some((pos, obj)) => best.set((Math.abs(pos - to), obj))
      case None =>
        throw new java.util.NoSuchElementException("SortedMap has no elements, and hence no closest element")
    }

    treeSet((to, null.asInstanceOf[B]))  // called for its side effect on "best"

    best.get
  }
}


scala> val stuff = new MetricSortedMapDouble[String](3.3 -> "three", 1.1 -> "one", 5.5 -> "five", 4.4 -> "four", 2.2 -> "two")
stuff: MetricSortedMapDouble[String] = Map(1.1 -> one, 2.2 -> two, 3.3 -> three, 4.4 -> four, 5.5 -> five)

scala> stuff.closest(1.5)
res0: (Double, String) = (0.3999999999999999,one)

scala> stuff.closest(1000)
res1: (Double, String) = (994.5,five)

scala> stuff.closest(-1000)
res2: (Double, String) = (1001.1,one)

scala> stuff.closest(3.3)
res3: (Double, String) = (0.0,three)

scala> stuff.closest(3.4)
res4: (Double, String) = (0.10000000000000009,three)

scala> stuff.closest(3.2)
res5: (Double, String) = (0.09999999999999964,three)
