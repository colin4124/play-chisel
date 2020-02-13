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
abstract class Expression extends FirrtlNode

case class Reference(name: String, tpe: Type) extends Expression with HasName

case class DoPrim(op: PrimOp, args: Seq[Expression], consts: Seq[BigInt], tpe: Type) extends Expression

/** Statement
  */
abstract class Statement extends FirrtlNode

case class DefNode(name: String, value: Expression) extends Statement with IsDeclaration

case class Block(stmts: Seq[Statement]) extends Statement

case class Connect(loc: Expression, expr: Expression) extends Statement

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
}

class IntWidth(val width: BigInt) extends Width

case object UnknownWidth extends Width

/** Type
  */
abstract class Type extends FirrtlNode
abstract class GroundType extends Type

case class UIntType(width: Width) extends GroundType
case object UnknownType extends Type

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
}

case class Module(name: String, ports: Seq[Port], body: Statement) extends DefModule

/** Circuit
  */
case class Circuit(modules: Seq[DefModule], main: String) extends FirrtlNode

