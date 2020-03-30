// play-chisel/firrtl/src/PrimOps.scala
package firrtl

import firrtl.ir._
import firrtl.Utils.max

object PrimOps {
  /** Bitwise Complement */
  case object Not extends PrimOp { override def toString = "not" }
  /** Bitwise And */
  case object And extends PrimOp { override def toString = "and" }
  /** Bitwise Or */
  case object Or extends PrimOp { override def toString = "or" }
  /** Bit Extraction */
  case object Bits extends PrimOp { override def toString = "bits" }

  private lazy val builtinPrimOps: Seq[PrimOp] =
    Seq(Not, And, Or, Bits)
  private lazy val strToPrimOp: Map[String, PrimOp] = {
    builtinPrimOps.map { case op : PrimOp=> op.toString -> op }.toMap
  }

  def fromString(op: String): PrimOp = strToPrimOp(op)

  def MAX (w1:Width, w2:Width) : Width = (w1, w2) match {
    case (IntWidth(i), IntWidth(j)) => IntWidth(max(i,j))
    case _ => MaxWidth(Seq(w1, w2))
  }

  def set_primop_type (e:DoPrim) : DoPrim = {
    def t1 = e.args.head.tpe
    def t2 = e.args(1).tpe
    def w1 = getWidth(e.args.head.tpe)
    def w2 = getWidth(e.args(1).tpe)
    e copy (tpe = e.op match {
      case Not => t1 match {
        case (_: UIntType) => UIntType(w1)
        case _ => UnknownType
      }
      case And | Or => (t1, t2) match {
        case (_: UIntType, _: UIntType) => UIntType(MAX(w1, w2))
        case _ => UnknownType
      }
    })
  }
}
