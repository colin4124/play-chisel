// play-chisel/chiselFrontend/src/internal/firrtl/Converter.scala
package chisel3.internal.firrtl

import scala.annotation.tailrec
import scala.collection.immutable.Queue

import chisel3._
import firrtl.{ir => fir}

object Converter {
  def convert(circuit: Circuit): fir.Circuit =
    fir.Circuit(circuit.components.map(convert), circuit.name)

  def convert(component: Component): fir.DefModule = component match {
    case ctx @ DefModule(_, name, ports, cmds) =>
      fir.Module(name, ports.map(p => convert(p)), convert(cmds.toList))
  }

  def convert(port: Port, topDir: SpecifiedDirection = SpecifiedDirection.Unspecified): fir.Port = {
    val resolvedDir = SpecifiedDirection.fromParent(topDir, port.dir)
    val dir = resolvedDir match {
      case SpecifiedDirection.Unspecified | SpecifiedDirection.Output => fir.Output
      case SpecifiedDirection.Flip | SpecifiedDirection.Input => fir.Input
    }
    val tpe = extractType(port.id)
    fir.Port(port.id.getRef.name, dir, tpe)
  }

  def extractType(data: Data): fir.Type = data match {
    case d: UInt => fir.UIntType(convert(d.width))
  }

  def convert(width: Width): fir.Width = width match {
    case UnknownWidth() => fir.UnknownWidth
    case KnownWidth(value) => fir.IntWidth(value)
  }

  def convert(cmds: Seq[Command]): fir.Statement = {
    @tailrec
    def rec(acc: Queue[fir.Statement])(cmds: Seq[Command]): Seq[fir.Statement] = {
      if (cmds.isEmpty) {
        acc
      } else convertSimpleCommand(cmds.head) match {
        case Some(stmt) =>
          rec(acc :+ stmt)(cmds.tail)
        case None => rec(acc)(cmds)
      }
    }
    fir.Block(rec(Queue.empty)(cmds))
  }

  def convertSimpleCommand(cmd: Command): Option[fir.Statement] = cmd match {
    case e: DefPrim[_] =>
      val args = e.args map convert
      val expr = fir.DoPrim(convert(e.op), args, Seq(), fir.UnknownType)
      Some(fir.DefNode(e.name, expr))
    case Connect(loc, exp) =>
      Some(fir.Connect(convert(loc), convert(exp)))
    case _ => None
  }

  def convert(op: PrimOp): fir.PrimOp = firrtl.PrimOps.fromString(op.name)

  def convert(arg: Arg): fir.Expression = arg match {
    case Node(id) =>
      convert(id.getRef)
    case Ref(name) =>
      fir.Reference(name, fir.UnknownType)
    case ModuleIO(mod, name) =>
      fir.Reference(name, fir.UnknownType)
  }
}

