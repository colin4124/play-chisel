// play-chisel/chiselFrontend/src/Bits.scala
package chisel3

import chisel3.internal.firrtl._

sealed abstract class Bits(private[chisel3] val width: Width) extends Element

sealed class UInt private[chisel3] (width: Width) extends Bits(width) with Num[UInt] {
  override def toString: String = {
    s"UInt$width"
  }

  final def & (that: UInt): UInt = that
  final def | (that: UInt): UInt = that
  def unary_~ (): UInt = UInt(width = width)
}
