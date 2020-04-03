// play-chisel/firrtl/src/ir/IR.scala
package firrtl
package ir

import Utils.indent

abstract class FirrtlNode {
  def serialize: String
}

trait HasName {
  val name: String
}

trait IsDeclaration extends HasName

/** Primitive Operation
  */
abstract class PrimOp extends FirrtlNode {
  def serialize: String = this.toString
}

/** Expression
  */
abstract class Expression extends FirrtlNode {
  def tpe: Type
  def mapExpr(f: Expression => Expression): Expression
  def mapType(f: Type => Type): Expression
  def mapWidth(f: Width => Width): Expression
}

case class Reference(name: String, tpe: Type) extends Expression with HasName {
  def serialize: String = name
  def mapExpr(f: Expression => Expression): Expression = this
  def mapType(f: Type => Type): Expression = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width): Expression = this
}

case class SubField(expr: Expression, name: String, tpe: Type) extends Expression with HasName {
  def serialize: String = s"${expr.serialize}.$name"
  def mapExpr(f: Expression => Expression): Expression = this.copy(expr = f(expr))
  def mapType(f: Type => Type): Expression = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width): Expression = this
}

case class DoPrim(op: PrimOp, args: Seq[Expression], consts: Seq[BigInt], tpe: Type) extends Expression {
  def serialize: String = op.serialize + "(" +
    (args.map(_.serialize) ++ consts.map(_.toString)).mkString(", ") + ")"
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
  def serialize: String = s"inst $name of $module"
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
  def serialize: String = s"node $name = ${value.serialize}"
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
  def serialize: String = stmts map (_.serialize) mkString "\n"
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
  def serialize: String =  s"${loc.serialize} <= ${expr.serialize}"
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

class IntWidth(val width: BigInt) extends Width {
  def serialize: String = s"<$width>"
}

case object UnknownWidth extends Width {
  def serialize: String = ""
}

/** Orientation of [[Field]] */
abstract class Orientation extends FirrtlNode
case object Default extends Orientation {
  def serialize: String = ""
}
case object Flip extends Orientation {
  def serialize: String = "flip "
}

/** Field of [[BundleType]] */
case class Field(name: String, flip: Orientation, tpe: Type) extends FirrtlNode with HasName {
  def serialize: String = flip.serialize + name + " : " + tpe.serialize
}

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

abstract class AggregateType extends Type {
  def mapWidth(f: Width => Width): Type = this
}

case class UIntType(width: Width) extends GroundType {
  def serialize: String = "UInt" + width.serialize
  def mapWidth(f: Width => Width): Type = UIntType(f(width))
}

case class BundleType(fields: Seq[Field]) extends AggregateType {
  def serialize: String = "{ " + (fields map (_.serialize) mkString ", ") + "}"
  def mapType(f: Type => Type): Type =
    BundleType(fields map (x => x.copy(tpe = f(x.tpe))))
}

case object UnknownType extends Type {
  def serialize: String = "?"
  def mapType(f: Type => Type): Type = this
  def mapWidth(f: Width => Width): Type = this
}

/** Direction
  */
sealed abstract class Direction extends FirrtlNode
case object Input extends Direction {
  def serialize: String = "input"
}
case object Output extends Direction {
  def serialize: String = "output"
}

/** Port
  */
case class Port(
  name      : String,
  direction : Direction,
  tpe       : Type
) extends FirrtlNode with IsDeclaration {
  def serialize: String = s"${direction.serialize} $name : ${tpe.serialize}"
}

/** Modules
  */
abstract class DefModule extends FirrtlNode with IsDeclaration {
  val name : String
  val ports : Seq[Port]
  protected def serializeHeader(tpe: String): String =
    s"$tpe $name :${indent(ports.map("\n" + _.serialize).mkString)}\n"
  def mapStmt(f: Statement => Statement): DefModule
  def mapPort(f: Port => Port): DefModule
  def mapString(f: String => String): DefModule
  def foreachStmt(f: Statement => Unit): Unit
  def foreachPort(f: Port => Unit): Unit
  def foreachString(f: String => Unit): Unit
}

case class Module(name: String, ports: Seq[Port], body: Statement) extends DefModule {
  def serialize: String = serializeHeader("module") + indent("\n" + body.serialize)
  def mapStmt(f: Statement => Statement): DefModule = this.copy(body = f(body))
  def mapPort(f: Port => Port): DefModule = this.copy(ports = ports map f)
  def mapString(f: String => String): DefModule = this.copy(name = f(name))
  def foreachStmt(f: Statement => Unit): Unit = f(body)
  def foreachPort(f: Port => Unit): Unit = ports.foreach(f)
  def foreachString(f: String => Unit): Unit = f(name)
}

/** Circuit
  */
case class Circuit(modules: Seq[DefModule], main: String) extends FirrtlNode {
  def serialize: String =
    s"circuit $main :" +
      (modules map ("\n" + _.serialize) map indent mkString "\n") + "\n"
  def mapModule(f: DefModule => DefModule): Circuit = this.copy(modules = modules map f)
  def mapString(f: String => String): Circuit = this.copy(main = f(main))
}
