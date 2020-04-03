// play-chisel/firrtl/src/graph/DiGraph.scala
package firrtl.graph

import scala.collection.{Set, Map}
import scala.collection.mutable
import scala.collection.mutable.{LinkedHashSet, LinkedHashMap}

class CyclicException(val node: Any) extends Exception(s"No valid linearization for cyclic graph, found at $node")

object DiGraph {
  def apply[T](mdg: MutableDiGraph[T]): DiGraph[T] = mdg
}

class DiGraph[T] private[graph] (private[graph] val edges: LinkedHashMap[T, LinkedHashSet[T]]) {
  def contains(v: T): Boolean = edges.contains(v)

  def getVertices: Set[T] = new LinkedHashSet ++ edges.map({ case (k, _) => k })

  def seededLinearize(seed: Option[Seq[T]] = None): Seq[T] = {
    val order = new mutable.ArrayBuffer[T]
    val unmarked = new mutable.LinkedHashSet[T]
    val tempMarked = new mutable.LinkedHashSet[T]

    case class LinearizeFrame[A](v: A, expanded: Boolean)
    val callStack = mutable.Stack[LinearizeFrame[T]]()

    unmarked ++= seed.getOrElse(getVertices)
    while (unmarked.nonEmpty) {
      callStack.push(LinearizeFrame(unmarked.head, false))
      while (callStack.nonEmpty) {
        val LinearizeFrame(n, expanded) = callStack.pop()
        if (!expanded) {
          if (tempMarked.contains(n)) {
            throw new CyclicException(n)
          }
          if (unmarked.contains(n)) {
            tempMarked += n
            unmarked -= n
            callStack.push(LinearizeFrame(n, true))
            // We want to visit the first edge first (so push it last)
            for (m <- edges.getOrElse(n, Set.empty).toSeq.reverse) {
              callStack.push(LinearizeFrame(m, false))
            }
          }
        } else {
          tempMarked -= n
          order.append(n)
        }
      }
    }

    order.reverse.toSeq
  }

  def linearize: Seq[T] = seededLinearize(None)

  def transformNodes[Q](f: (T) => Q): DiGraph[Q] = {
    val eprime = edges.map({ case (k, _) => (f(k), new LinkedHashSet[Q]) })
    edges.foreach({ case (k, v) => eprime(f(k)) ++= v.map(f(_)) })
    new DiGraph(eprime)
  }
}

class MutableDiGraph[T] extends DiGraph[T](new LinkedHashMap[T, LinkedHashSet[T]]) {
  def addVertex(v: T): T = {
    edges.getOrElseUpdate(v, new LinkedHashSet[T])
    v
  }
  def addEdge(u: T, v: T): Unit = {
    require(contains(u))
    require(contains(v))
    edges(u) += v
  }
}