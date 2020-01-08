// play-chisel/src/Main.scala
package playchisel

import chisel3._
import chisel3.internal.Builder
import chisel3.internal.firrtl._

import java.io.{File, FileWriter}

class Mux2 extends RawModule

object Main extends App {
  val (circuit, _) = Builder.build(Module(new Mux2))

  val emitted = Emitter.emit(circuit)

  val file = new File("Mux2.fir")
  val w = new FileWriter(file)
  w.write(emitted)
  w.close()
}
