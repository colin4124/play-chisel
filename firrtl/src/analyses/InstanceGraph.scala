// play-chisel/firrtl/src/analyses/InstanceGraph.scala
package firrtl.analyses

import scala.collection.mutable

import firrtl._
import firrtl.ir._
import firrtl.graph._
import firrtl.Utils._
import firrtl.traversals.Foreachers._

class InstanceGraph(c: Circuit) {
  val moduleMap = c.modules.map({m => (m.name,m) }).toMap

  private val instantiated = new mutable.LinkedHashSet[String]
  private val childInstances =
    new mutable.LinkedHashMap[String, mutable.LinkedHashSet[WDefInstance]]
  for (m <- c.modules) {
    childInstances(m.name) = new mutable.LinkedHashSet[WDefInstance]
    m.foreach(InstanceGraph.collectInstances(childInstances(m.name)))
    instantiated ++= childInstances(m.name).map(i => i.module)
  }

  private val instanceGraph = new MutableDiGraph[WDefInstance]
  private val instanceQueue = new mutable.Queue[WDefInstance]

  for (subTop <- c.modules.view.map(_.name).filterNot(instantiated)) {
    val topInstance = WDefInstance(subTop,subTop)
    instanceQueue.enqueue(topInstance)
    while (instanceQueue.nonEmpty) {
      val current = instanceQueue.dequeue
      instanceGraph.addVertex(current)
      for (child <- childInstances(current.module)) {
        if (!instanceGraph.contains(child)) {
          instanceQueue.enqueue(child)
          instanceGraph.addVertex(child)
        }
        instanceGraph.addEdge(current,child)
      }
    }
  }

  lazy val graph = DiGraph(instanceGraph)

  def moduleOrder: Seq[DefModule] = {
    graph.transformNodes(_.module).linearize.map(moduleMap(_))
  }
}

object InstanceGraph {
  def collectInstances(insts: mutable.Set[WDefInstance])
                      (s: Statement): Unit = s match {
    case i: WDefInstance => insts += i
    case i: DefInstance => throwInternalError("Expecting WDefInstance, found a DefInstance!")
    case _ => s.foreach(collectInstances(insts))
  }
}
