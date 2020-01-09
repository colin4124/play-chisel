// play-chisel/src/internal/firrtl/IR.scala
package chisel3.internal.firrtl

import chisel3._
import chisel3.internal._

case class PrimOp(name: String) {
  override def toString: String = name
}

object PrimOp {
  val BitAndOp = PrimOp("and")
  val BitOrOp  = PrimOp("or")
  val BitNotOp = PrimOp("not")
}

abstract class Arg {
  def fullName(ctx: Component): String = name
  def name: String
}

case class Node(id: HasId) extends Arg {
  override def fullName(ctx: Component): String = id.getOptionRef match {
    case Some(arg) => arg.fullName(ctx)
    case None => id.suggestedName.getOrElse("??")
  }
  def name: String = id.getOptionRef match {
    case Some(arg) => arg.name
    case None => id.suggestedName.getOrElse("??")
  }
}

case class Ref(name: String) extends Arg
case class ModuleIO(mod: BaseModule, name: String) extends Arg

sealed abstract class Width {
  type W = Int
  def max(that: Width): Width = this.op(that, _ max _)

  protected def op(that: Width, f: (W, W) => W): Width
}
sealed case class UnknownWidth() extends Width {
  def op(that: Width, f: (W, W) => W): Width = this
  override def toString: String = ""
}
sealed case class KnownWidth(value: Int) extends Width {
  def op(that: Width, f: (W, W) => W): Width = that match {
    case KnownWidth(x) => KnownWidth(f(value, x))
    case _ => that
  }
  override def toString: String = s"<${value.toString}>"
}

object Width {
  def apply(x: Int): Width = KnownWidth(x)
  def apply(): Width = UnknownWidth()
}

abstract class Command
abstract class Definition extends Command  {
  def id: HasId
  def name: String = id.getRef.name
}
case class DefPrim[T <: Data](id: T, op: PrimOp, args: Arg*) extends Definition
case class Connect(loc: Node, exp: Arg) extends Command

case class Port(id: Data, dir: SpecifiedDirection)

abstract class Component extends Arg {
  def id: BaseModule
  def name: String
  def ports: Seq[Port]
}

case class DefModule(id: RawModule, name: String, ports: Seq[Port], commands: Seq[Command]) extends Component

case class Circuit(name: String, components: Seq[Component])
