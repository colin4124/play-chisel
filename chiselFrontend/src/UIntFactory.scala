// play-chisel/chiselFrontend/src/UIntFactory.scala
package chisel3

import chisel3.internal.firrtl.Width

trait UIntFactory {
  def apply(): UInt = apply(Width())
  def apply(width: Width): UInt = new UInt(width)
}