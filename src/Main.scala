// play-chisel/src/Main.scala
package playchisel

import chisel3._
import chisel3.internal.Builder
import chisel3.internal.firrtl._
import firrtl.{VerilogCompiler, CircuitState, ChirrtlForm}

import chisel3.debug.PrintIR.print_ir

import java.io.{File, FileWriter}

class Mux2 extends RawModule {
  val sel = IO(Input(UInt(1.W)))
  val in0 = IO(Input(UInt(1.W)))
  val in1 = IO(Input(UInt(1.W)))
  val out = IO(Output(UInt(1.W)))
  out := (sel & in1) | (~sel & in0)
}

class Mux4 extends RawModule {
  val in0 = IO(Input(UInt(1.W)))
  val in1 = IO(Input(UInt(1.W)))
  val in2 = IO(Input(UInt(1.W)))
  val in3 = IO(Input(UInt(1.W)))
  val sel = IO(Input(UInt(2.W)))
  val out = IO(Output(UInt(1.W)))

  val m0 = Module(new Mux2)
  m0.sel := sel(0)
  m0.in0 := in0
  m0.in1 := in1
  val m1 = Module(new Mux2)
  m1.sel := sel(0)
  m1.in0 := in2
  m1.in1 := in3
  val m2 = Module(new Mux2)
  m2.sel := sel(1)
  m2.in0 := m0.out
  m2.in1 := m1.out

  out := m2.out
}

object Main extends App {
  val (circuit, _) = Builder.build(Module(new Mux4))

  val emitted = Emitter.emit(circuit)

  val file = new File("Mux4.fir")
  val w = new FileWriter(file)
  w.write(emitted)
  w.close()

  print_ir(circuit)
  // val firrtl = Converter.convert(circuit)

  // val state = CircuitState(firrtl, ChirrtlForm)
  // val compiler = new VerilogCompiler
  // compiler.compile(state)
}
