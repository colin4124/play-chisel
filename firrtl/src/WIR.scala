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

private[firrtl] trait GenderFromFlow { this: Expression =>
  val flow: Flow
}

case class WRef(name: String, tpe: Type, kind: Kind, flow: Flow) extends Expression {
  def serialize: String = name
  def mapExpr(f: Expression => Expression): Expression = this
  def mapType(f: Type => Type): Expression = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width): Expression = this
}

case class WSubField(expr: Expression, name: String, tpe: Type, flow: Flow) extends Expression with GenderFromFlow {
  def mapExpr(f: Expression => Expression): Expression = this.copy(expr = f(expr))
  def mapType(f: Type => Type): Expression = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width): Expression = this
}

case class WDefInstance(name: String, module: String, tpe: Type) extends Statement with IsDeclaration {
  def mapExpr(f: Expression => Expression): Statement = this
  def mapStmt(f: Statement => Statement): Statement = this
  def mapType(f: Type => Type): Statement = this.copy(tpe = f(tpe))
  def mapString(f: String => String): Statement = this.copy(name = f(name))
  def foreachStmt(f: Statement => Unit): Unit = Unit
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = f(tpe)
  def foreachString(f: String => Unit): Unit = f(name)
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
