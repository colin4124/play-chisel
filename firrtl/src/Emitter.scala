// play-chisel/firrtl/src/Emitter.scala
package firrtl

import java.io.Writer
import scala.collection.mutable

import firrtl.ir._
import firrtl.passes._
import firrtl.traversals.Foreachers._
import firrtl.PrimOps._
import Utils._

case class EmitterException(message: String) extends PassException(message)

class VerilogEmitter extends SeqTransform with Emitter {
  def inputForm = LowForm
  def outputForm = LowForm
  val outputSuffix = ".v"
  def transforms = Seq()

  val tab = "  "
  def stringify(tpe: GroundType): String = tpe match {
    case (_: UIntType) =>
      val wx = bitWidth(tpe) - 1
      if (wx > 0) s"[$wx:0]" else ""
    case _ => throwInternalError(s"trying to write unsupported type in the Verilog Emitter: $tpe")
  }
  def emit(x: Any)(implicit w: Writer): Unit = { emit(x, 0) }
  def emit(x: Any, top: Int)(implicit w: Writer): Unit = {
    def cast(e: Expression): Any = e.tpe match {
      case (t: UIntType) => e
      case _ => throwInternalError(s"unrecognized cast: $e")
    }
    x match {
      case (e: DoPrim) => emit(op_stream(e), top + 1)
      case (e: WRef) => w write e.serialize
      case (t: GroundType) => w write stringify(t)
      case (s: String) => w write s
      case (i: Int) => w write i.toString
      case (i: Long) => w write i.toString
      case (i: BigInt) => w write i.toString
      case (s: Seq[Any]) =>
        s foreach (emit(_, top + 1))
        if (top == 0) w write "\n"
      case x => throwInternalError(s"trying to emit unsupported operator: $x")
    }
  }

  def op_stream(doprim: DoPrim): Seq[Any] = {
    def cast_as(e: Expression): Any = e.tpe match {
      case (t: UIntType) => e
      case _ => throwInternalError(s"cast_as - unrecognized type: $e")
    }
    def a0: Expression = doprim.args.head
    def a1: Expression = doprim.args(1)

    def checkArgumentLegality(e: Expression) = e match {
      case _: WRef =>
      case _ => throw EmitterException(s"Can't emit ${e.getClass.getName} as PrimOp argument")
    }

    doprim.args foreach checkArgumentLegality
    doprim.op match {
      case Not => Seq("~ ", a0)
      case And => Seq(cast_as(a0), " & ", cast_as(a1))
      case Or => Seq(cast_as(a0), " | ", cast_as(a1))
    }
  }
  class VerilogRender(m: Module)(implicit writer: Writer) {
    val netlist = mutable.LinkedHashMap[WrappedExpression, Expression]()
    def build_netlist(s: Statement): Unit = {
      s.foreach(build_netlist)
      s match {
        case sx: Connect => netlist(sx.loc) = sx.expr
        case sx: DefNode =>
          val e = WRef(sx.name, sx.value.tpe, NodeKind, SourceFlow)
          netlist(e) = sx.value
        case _ =>
      }
    }

    val portdefs = mutable.ArrayBuffer[Seq[Any]]()
    val declares = mutable.ArrayBuffer[Seq[Any]]()
    val assigns = mutable.ArrayBuffer[Seq[Any]]()
    def declare(b: String, n: String, t: Type): Unit = t match {
      case tx =>
        declares += Seq(b, " ", tx, " ", n,";")
    }

    def assign(e: Expression, value: Expression): Unit = {
      assigns += Seq("assign ", e, " = ", value, ";")
    }

    // Turn ports into Seq[String] and add to portdefs
    def build_ports(): Unit = {
      def padToMax(strs: Seq[String]): Seq[String] = {
        val len = if (strs.nonEmpty) strs.map(_.length).max else 0
        strs map (_.padTo(len, ' '))
      }

      // Turn directions into strings (and AnalogType into inout)
      val dirs = m.ports map { case Port(name, dir, tpe) =>
        (dir, tpe) match {
          case (Input, _) => "input "
          case (Output, _) => "output"
        }
      }
      // Turn types into strings, all ports must be GroundTypes
      val tpes = m.ports map {
        case Port(_, _, tpe: GroundType) => stringify(tpe)
        case port: Port => error(s"Trying to emit non-GroundType Port $port")
      }

      // dirs are already padded
      (dirs, padToMax(tpes), m.ports).zipped.toSeq.zipWithIndex.foreach {
        case ((dir, tpe, Port(name, _, _)), i) =>
          if (i != m.ports.size - 1) {
            portdefs += Seq(dir, " ", tpe, " ", name, ",")
          } else {
            portdefs += Seq(dir, " ", tpe, " ", name)
          }
      }
    }

    def build_streams(s: Statement): Unit = {
      s.foreach(build_streams)
      s match {
        case sx@Connect(loc@WRef(_, _, PortKind, _), expr) =>
          assign(loc, expr)
        case sx: DefNode =>
          declare("wire", sx.name, sx.value.tpe)
          assign(WRef(sx.name, sx.value.tpe, NodeKind, SourceFlow), sx.value)
        case _ =>
      }
    }

    def emit_streams(): Unit = {
      emit(Seq("module ", m.name, "("))
      for (x <- portdefs) emit(Seq(tab, x))
      emit(Seq(");"))

      if (declares.isEmpty && assigns.isEmpty) emit(Seq(tab, "initial begin end"))
      for (x <- declares) emit(Seq(tab, x))
      for (x <- assigns) emit(Seq(tab, x))
      emit(Seq("endmodule"))
    }

    def emit_verilog(): DefModule = {
      build_netlist(m.body)
      build_ports()
      build_streams(m.body)
      emit_streams()
      m
    }
  }

  def emit(state: CircuitState, writer: Writer): Unit = {
    val circuit = state.circuit
    circuit.modules.foreach {
      case m: Module =>
        val renderer = new VerilogRender(m)(writer)
        renderer.emit_verilog()
        println(writer.toString)
      case _ => // do nothing
    }
  }

  override def execute(state: CircuitState): CircuitState = {
    val writer = new java.io.StringWriter
    emit(state, writer)
    state
  }
}
