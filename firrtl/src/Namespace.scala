package firrtl

import scala.collection.mutable
import firrtl.ir._

class Namespace private {
  private val tempNamePrefix: String = "_GEN"
  // Begin with a tempNamePrefix in namespace so we always have a number suffix
  private val namespace = mutable.HashSet[String](tempNamePrefix)
  // Memoize where we were on a given prefix
  private val indices = mutable.HashMap[String, Int]()

  def tryName(value: String): Boolean = {
    val unused = !contains(value)
    if (unused) namespace += value
    unused
  }

  def contains(value: String): Boolean = namespace.contains(value)

  def newName(value: String): String = {
    // First try, just check
    if (tryName(value)) value
    else {
      var idx = indices.getOrElse(value, 0)
      var str = value
      do {
        str = s"${value}_$idx"
        idx += 1
      }
      while (!(tryName(str)))
      indices(value) = idx
      str
    }
  }
}

object Namespace {
  def apply(names: Seq[String] = Nil): Namespace = {
    val namespace = new Namespace
    namespace.namespace ++= names
    namespace
  }
}
