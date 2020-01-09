// play-chisel/chiselFrontend/src/Bits.scala
package chisel3

import chisel3.internal._
import chisel3.internal.Builder.pushOp
import chisel3.internal.firrtl._
import chisel3.internal.firrtl.PrimOp._

sealed abstract class Bits(private[chisel3] val width: Width) extends Element {
  private[chisel3] def cloneTypeWidth(width: Width): this.type

  def cloneType: this.type = cloneTypeWidth(width)

  private[chisel3] def unop[T <: Data](dest: T, op: PrimOp): T = {
    requireIsHardware(this, "bits operated on")
    pushOp(DefPrim(dest, op, this.ref))
  }

  private[chisel3] def binop[T <: Data](dest: T, op: PrimOp, other: Bits): T = {
    requireIsHardware(this, "bits operated on")
    requireIsHardware(other, "bits operated on")
    pushOp(DefPrim(dest, op, this.ref, other.ref))
  }
}

sealed class UInt private[chisel3] (width: Width) extends Bits(width) with Num[UInt] {
  private[chisel3] override def cloneTypeWidth(w: Width): this.type =
    new UInt(w).asInstanceOf[this.type]

  final def & (that: UInt): UInt =
    binop(UInt(this.width max that.width), BitAndOp, that)
  final def | (that: UInt): UInt =
    binop(UInt(this.width max that.width), BitOrOp, that)

  def unary_~ (): UInt =
    unop(UInt(width = width), BitNotOp)
}
