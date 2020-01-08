// play-chisel/src/internal/firrtl/Emitter.scala

package chisel3.internal.firrtl
import chisel3._

object Emitter {
  def emit(circuit: Circuit): String = new Emitter(circuit).toString
}

private class Emitter(circuit: Circuit) {
  override def toString: String = "Hello, chisel3"
}
