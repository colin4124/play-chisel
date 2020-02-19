// play-chisel/firrtl/src/passes/Resolves.scala
package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.Mappers._

object ResolveKinds extends Pass {
  type KindMap = collection.mutable.LinkedHashMap[String, Kind]

  def find_port(kinds: KindMap)(p: Port): Port = {
    kinds(p.name) = PortKind ; p
  }

  def find_stmt(kinds: KindMap)(s: Statement):Statement = {
    s match {
      case sx: DefNode => kinds(sx.name) = NodeKind
      case _ =>
    }
    s map find_stmt(kinds)
  }

  def resolve_expr(kinds: KindMap)(e: Expression): Expression = e match {
    case ex: WRef => ex copy (kind = kinds(ex.name))
    case _ => e map resolve_expr(kinds)
  }

  def resolve_stmt(kinds: KindMap)(s: Statement): Statement =
    s map resolve_stmt(kinds) map resolve_expr(kinds)

  def resolve_kinds(m: DefModule): DefModule = {
    val kinds = new KindMap
    (m map find_port(kinds)
       map find_stmt(kinds)
       map resolve_stmt(kinds))
  }

  def run(c: Circuit): Circuit =
    c copy (modules = c.modules map resolve_kinds)
}

object ResolveFlows extends Pass {
  def resolve_e(g: Flow)(e: Expression): Expression = e match {
    case ex: WRef => ex copy (flow = g)
    case _ => e map resolve_e(g)
  }

  def resolve_s(s: Statement): Statement = s match {
    case Connect(loc, expr) =>
      Connect(resolve_e(SinkFlow)(loc), resolve_e(SourceFlow)(expr))
    case sx => sx map resolve_e(SourceFlow) map resolve_s
  }

  def resolve_flow(m: DefModule): DefModule = m map resolve_s

  def run(c: Circuit): Circuit =
    c copy (modules = c.modules map resolve_flow)
}
