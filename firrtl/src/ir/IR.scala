// play-chisel/firrtl/src/ir/IR.scala
package firrtl
package ir

abstract class FirrtlNode

trait HasName {
  val name: String
}

trait IsDeclaration extends HasName

/** Primitive Operation
  */
abstract class PrimOp extends FirrtlNode

/** Expression
  */
abstract class Expression extends FirrtlNode {
  def tpe: Type
  def mapExpr(f: Expression => Expression): Expression
  def mapType(f: Type => Type): Expression
  def mapWidth(f: Width => Width): Expression
}

case class Reference(name: String, tpe: Type) extends Expression with HasName {
  def mapExpr(f: Expression => Expression): Expression = this
  def mapType(f: Type => Type): Expression = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width): Expression = this
}

case class SubField(expr: Expression, name: String, tpe: Type) extends Expression with HasName {
  def mapExpr(f: Expression => Expression): Expression = this.copy(expr = f(expr))
  def mapType(f: Type => Type): Expression = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width): Expression = this
}

case class DoPrim(op: PrimOp, args: Seq[Expression], consts: Seq[BigInt], tpe: Type) extends Expression {
  def mapExpr(f: Expression => Expression): Expression = this.copy(args = args map f)
  def mapType(f: Type => Type): Expression = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width): Expression = this
}

/** Statement
  */
abstract class Statement extends FirrtlNode {
  def mapStmt   (f: Statement => Statement)   : Statement
  def mapExpr   (f: Expression => Expression) : Statement
  def mapType   (f: Type => Type)             : Statement
  def mapString (f: String => String)         : Statement

  def foreachStmt(f: Statement => Unit)  : Unit
  def foreachExpr(f: Expression => Unit) : Unit
  def foreachType(f: Type => Unit)       : Unit
  def foreachString(f: String => Unit)   : Unit
}

case class DefInstance(name: String, module: String) extends Statement with IsDeclaration {
  def mapStmt(f: Statement => Statement): Statement = this
  def mapExpr(f: Expression => Expression): Statement = this
  def mapType(f: Type => Type): Statement = this
  def mapString(f: String => String): Statement = DefInstance(f(name), module)
  def foreachStmt(f: Statement => Unit): Unit = Unit
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachString(f: String => Unit): Unit = f(name)
}

case class DefNode(name: String, value: Expression) extends Statement with IsDeclaration {
  def mapStmt(f: Statement => Statement): Statement = this
  def mapExpr(f: Expression => Expression): Statement = DefNode(name, f(value))
  def mapType(f: Type => Type): Statement = this
  def mapString(f: String => String): Statement = DefNode(f(name), value)
  def foreachStmt(f: Statement => Unit): Unit = Unit
  def foreachExpr(f: Expression => Unit): Unit = f(value)
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachString(f: String => Unit): Unit = f(name)
}

case class Block(stmts: Seq[Statement]) extends Statement {
  def mapStmt(f: Statement => Statement): Statement = Block(stmts map f)
  def mapExpr(f: Expression => Expression): Statement = this
  def mapType(f: Type => Type): Statement = this
  def mapString(f: String => String): Statement = this
  def foreachStmt(f: Statement => Unit): Unit = stmts.foreach(f)
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachString(f: String => Unit): Unit = Unit
}

case class Connect(loc: Expression, expr: Expression) extends Statement {
  def mapStmt(f: Statement => Statement): Statement = this
  def mapExpr(f: Expression => Expression): Statement = Connect(f(loc), f(expr))
  def mapType(f: Type => Type): Statement = this
  def mapString(f: String => String): Statement = this
  def foreachStmt(f: Statement => Unit): Unit = Unit
  def foreachExpr(f: Expression => Unit): Unit = { f(loc); f(expr) }
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachString(f: String => Unit): Unit = Unit
}

/** Width
  */
abstract class Width extends FirrtlNode

object IntWidth {
  private val maxCached = 1024
  private val cache = new Array[IntWidth](maxCached + 1)
  def apply(width: BigInt): IntWidth = {
    if (0 <= width && width <= maxCached) {
      val i = width.toInt
      var w = cache(i)
      if (w eq null) {
        w = new IntWidth(width)
        cache(i) = w
      }
      w
    } else new IntWidth(width)
  }
  def unapply(w: IntWidth): Option[BigInt] = Some(w.width)
}

class IntWidth(val width: BigInt) extends Width

case object UnknownWidth extends Width

/** Type
  */
abstract class Type extends FirrtlNode {
  def mapType(f: Type => Type): Type
  def mapWidth(f: Width => Width): Type
}
abstract class GroundType extends Type {
  val width: Width
  def mapType(f: Type => Type): Type = this
}
object GroundType {
  def unapply(ground: GroundType): Option[Width] = Some(ground.width)
}

case class UIntType(width: Width) extends GroundType {
  def mapWidth(f: Width => Width): Type = UIntType(f(width))
}
case object UnknownType extends Type {
  def mapType(f: Type => Type): Type = this
  def mapWidth(f: Width => Width): Type = this
}

/** Direction
  */
sealed abstract class Direction extends FirrtlNode
case object Input extends Direction
case object Output extends Direction

/** Port
  */
case class Port(
  name      : String,
  direction : Direction,
  tpe       : Type
) extends FirrtlNode with IsDeclaration

/** Modules
  */
abstract class DefModule extends FirrtlNode with IsDeclaration {
  val name : String
  val ports : Seq[Port]
  def mapStmt(f: Statement => Statement): DefModule
  def mapPort(f: Port => Port): DefModule
  def mapString(f: String => String): DefModule
}

case class Module(name: String, ports: Seq[Port], body: Statement) extends DefModule {
  def mapStmt(f: Statement => Statement): DefModule = this.copy(body = f(body))
  def mapPort(f: Port => Port): DefModule = this.copy(ports = ports map f)
  def mapString(f: String => String): DefModule = this.copy(name = f(name))
}

/** Circuit
  */
case class Circuit(modules: Seq[DefModule], main: String) extends FirrtlNode {
  def mapModule(f: DefModule => DefModule): Circuit = this.copy(modules = modules map f)
  def mapString(f: String => String): Circuit = this.copy(main = f(main))
}
