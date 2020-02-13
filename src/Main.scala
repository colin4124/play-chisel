// play-chisel/src/Main.scala
package playchisel

import chisel3._
import chisel3.internal.Builder
import chisel3.internal.firrtl._

import java.io.{File, FileWriter}

class Mux2 extends RawModule {
  val sel = IO(Input(UInt(1.W)))
  val in0 = IO(Input(UInt(1.W)))
  val in1 = IO(Input(UInt(1.W)))
  val out = IO(Output(UInt(1.W)))
  out := (sel & in1) | (~sel & in0)
}

object Main extends App {
  val (circuit, _) = Builder.build(Module(new Mux2))

  val emitted = Emitter.emit(circuit)

  val file = new File("Mux2.fir")
  val w = new FileWriter(file)
  w.write(emitted)
  w.close()

  val firrtl = Converter.convert(circuit)
  println(firrtl)
}
