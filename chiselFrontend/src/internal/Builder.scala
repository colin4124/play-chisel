// play-chisel/chiselFrontend/src/internal/Builder.scala
package chisel3.internal

import chisel3._
import chisel3.internal.firrtl._

private[chisel3] trait HasId

object Builder {
  def build[T <: RawModule](f: => T): (Circuit, T) = {
    println("Elaborating design...")
    val mod = f
    println("Done elaborating.")
    (Circuit("", Seq()), mod)
  }
}