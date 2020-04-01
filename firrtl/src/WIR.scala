// play-chisel/firrtl/src/WIR.scala
package firrtl

import Utils._
import firrtl.ir._

trait Kind
case object PortKind extends Kind
case object NodeKind extends Kind
case object UnknownKind extends Kind

trait Flow
case object SourceFlow extends Flow
case object SinkFlow extends Flow
case object UnknownFlow extends Flow

case class WRef(name: String, tpe: Type, kind: Kind, flow: Flow) extends Expression {
  def serialize: String = name
  def mapExpr(f: Expression => Expression): Expression = this
  def mapType(f: Type => Type): Expression = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width): Expression = this
}

class WrappedExpression(val e1: Expression)

private[firrtl] sealed trait HasMapWidth {
  def mapWidth(f: Width => Width): Width
}

case class PlusWidth(arg1: Width, arg2: Width) extends Width with HasMapWidth {
  def mapWidth(f: Width => Width): Width = PlusWidth(f(arg1), f(arg2))
}
case class MinusWidth(arg1: Width, arg2: Width) extends Width with HasMapWidth {
  def mapWidth(f: Width => Width): Width = MinusWidth(f(arg1), f(arg2))
}
case class MaxWidth(args: Seq[Width]) extends Width with HasMapWidth {
  def mapWidth(f: Width => Width): Width = MaxWidth(args map f)
}
