// play-chisel/src/internal/firrtl/Emitter.scala

package chisel3.internal.firrtl
import chisel3._

object Emitter {
  def emit(circuit: Circuit): String = new Emitter(circuit).toString
}

private class Emitter(circuit: Circuit) {
  private val res = new StringBuilder()
  override def toString: String = res.toString

  private var indentLevel = 0
  private def newline = "\n" + ("  " * indentLevel)
  private def indent(): Unit = indentLevel += 1
  private def unindent() { require(indentLevel > 0); indentLevel -= 1 }
  private def withIndent(f: => Unit) { indent(); f; unindent() }

  res ++= s"circuit ${circuit.name} : "
  withIndent { circuit.components.foreach(c => res ++= emit(c)) }
  res ++= newline

  private def emit(m: Component): String = {
    val sb = new StringBuilder
    sb.append(moduleDecl(m))
    sb.append(moduleDefn(m))
    sb.result
  }

  private def moduleDecl(m: Component): String = m.id match {
    case _: RawModule => newline + s"module ${m.name} : "
  }

  private def moduleDefn(m: Component): String = {
    val body = new StringBuilder
    withIndent {
      for (p <- m.ports) {
        val portDef = m match {
          case mod: DefModule => emitPort(p)
        }
        body ++= newline + portDef
      }
      body ++= newline

      m match {
        case mod: DefModule => {
          val procMod = mod
          for (cmd <- procMod.commands) { body ++= newline + emit(cmd, procMod)}
        }
      }
      body ++= newline
    }
    body.toString()
  }

  private def emitPort(e: Port, topDir: SpecifiedDirection=SpecifiedDirection.Unspecified): String = {
    val resolvedDir = SpecifiedDirection.fromParent(topDir, e.dir)
    val dirString = resolvedDir match {
      case SpecifiedDirection.Unspecified | SpecifiedDirection.Output => "output"
      case SpecifiedDirection.Flip | SpecifiedDirection.Input => "input"
    }
    val clearDir = resolvedDir match {
      case SpecifiedDirection.Input | SpecifiedDirection.Output => true
      case SpecifiedDirection.Unspecified | SpecifiedDirection.Flip => false
    }
    s"$dirString ${e.id.getRef.name} : ${emitType(e.id, clearDir)}"
  }

  private def emitType(d: Data, clearDir: Boolean = false): String = d match {
    case d: UInt => s"UInt${d.width}"
  }

  private def emit(e: Command, ctx: Component): String = { // scalastyle:ignore cyclomatic.complexity
    val firrtlLine = e match {
      case e: DefPrim[_] => s"node ${e.name} = ${e.op.name}(${e.args.map(_.fullName(ctx)).mkString(", ")})"
      case e: Connect => s"${e.loc.fullName(ctx)} <= ${e.exp.fullName(ctx)}"
      case e: DefInstance => s"inst ${e.name} of ${e.id.name}"
    }
    firrtlLine
  }
}
