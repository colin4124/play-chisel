// play-chisel/chiselFrontend/src/PrintIR.scala
package chisel3.debug

import scala.annotation.tailrec
import scala.collection.immutable.Queue

import chisel3._
import chisel3.internal.firrtl._
import chisel3.internal.throwException

object PrintIR {

  def print_ir(c: Circuit) = {
    println(s"Circuit: ${c.name}")
    c.components foreach { m =>
      print_module(m)
    }
  }

  def print_module(m: Component) = {
    m match {
      case DefModule(_, name, ports, cmds) =>
        val name_str = m_str(name, 1)
        println(name_str)

        ports foreach { p =>
          println(p_str(p, 2))
        }

        println(cmds_str(cmds, 2))
      case _ => throwException(s"PrintIR: Unknown $m")
    }
  }

  def tab(level: Int) = (" " * 2) * level

  def m_str(name: String, l: Int) = s"${tab(l)}Module: ${name}"

  def p_str(p: Port, l: Int) = s"${tab(l)}Port: ${p.id.getRef.name} ${dir_str(p.dir)} ${type_str(p.id)}"
  def dir_str(d: SpecifiedDirection) = {
    import SpecifiedDirection._
    d match {
      case Unspecified => "Unspecifed"
      case Output      => "Output"
      case Input       => "Input"
      case Flip        => "Flip"
   }
  }
  def type_str(d: Data) = d match {
    case u: UInt => s"UInt<${w_str(u.width)}>"
    case _       => "UnknownType"
  }
  def w_str(w: Width) = w match {
    case UnknownWidth() => "UnknownWidth"
    case KnownWidth(i)  => s"$i"
  }

  def cmds_str(cmds: Seq[Command], l: Int) = {
    @tailrec
    def rec(acc: Queue[String])(cmds: Seq[Command]): Seq[String] = {
      if (cmds.isEmpty) {
        acc
      } else cmd_str(cmds.head, l) match {
        case Some(stmt) =>
          rec(acc :+ stmt)(cmds.tail)
        case None => rec(acc)(cmds)
      }
    }
    rec(Queue.empty)(cmds) mkString "\n"
  }

  def cmd_str(cmd: Command, l: Int): Option[String] = cmd match {
    case e: DefPrim[_] =>
      val consts = e.args.collect { case ILit(i) => s"$i" } mkString ", "
      val args = e.args.flatMap {
        case _: ILit => None
        case other => Some(arg_str(other))
      } mkString ", "
      val op   = e.op.name
      Some(s"${tab(l)}DefPrim: ${e.name} $op $args $consts")
    case Connect(loc, exp) =>
      val lhs = arg_str(loc)
      val rhs = arg_str(exp)
      Some(s"${tab(l)}Connect: $lhs <- $rhs")
    case e @ DefInstance(id, _) =>
      Some(s"${tab(l)}DefInstance: ${e.name} of ${id.name}")
    case _ =>
      throwException(s"Unknown Command: $cmd")
  }

  def arg_str(arg: Arg): String = arg match {
    case Node(id) =>
      arg_str(id.getRef)
    case Ref(name) =>
      s"Reference: $name"
    case ModuleIO(mod, name) =>
      s"ModuleIO: ${mod.getRef.name} $name"
    case lit: ILit =>
      throwException(s"Internal Error! Unexpected ILit: $lit")
  }
}
