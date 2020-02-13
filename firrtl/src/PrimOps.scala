// play-chisel/firrtl/src/PrimOps.scala
package firrtl

import firrtl.ir._

object PrimOps {
  /** Bitwise Complement */
  case object Not extends PrimOp { override def toString = "not" }
  /** Bitwise And */
  case object And extends PrimOp { override def toString = "and" }
  /** Bitwise Or */
  case object Or extends PrimOp { override def toString = "or" }

  private lazy val builtinPrimOps: Seq[PrimOp] =
    Seq(Not, And, Or)
  private lazy val strToPrimOp: Map[String, PrimOp] = {
    builtinPrimOps.map { case op : PrimOp=> op.toString -> op }.toMap
  }

  def fromString(op: String): PrimOp = strToPrimOp(op)
}
