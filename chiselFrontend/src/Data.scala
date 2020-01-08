// play-chisel/chiselFrontend/src/Data.scala
package chisel3

import chisel3.internal._
import chisel3.internal.firrtl._

object Input {
  def apply[T<:Data](source: T): T = {
    source
  }
}
object Output {
  def apply[T<:Data](source: T): T = {
    source
  }
}
abstract class Data extends HasId {
  private[chisel3] def connect(that: Data): Unit = {}
  private[chisel3] def width: Width
  final def := (that: Data): Unit = this.connect(that)
}
