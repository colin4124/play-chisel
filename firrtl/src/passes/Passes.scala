// play-chisel/firrtl/src/passes/Passes.scala
package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._

class PassException(message: String) extends FirrtlUserException(message)

trait Pass extends Transform {
  def inputForm: CircuitForm = UnknownForm
  def outputForm: CircuitForm = UnknownForm
  def run(c: Circuit): Circuit
  def execute(state: CircuitState): CircuitState = {
    val result = (state.form, inputForm) match {
      case (_, UnknownForm) => run(state.circuit)
      case (UnknownForm, _) => run(state.circuit)
      case (x, y) if x > y =>
        error(s"[$name]: Input form must be lower or equal to $inputForm. Got ${state.form}")
      case _ => run(state.circuit)
    }
    CircuitState(result, outputForm)
  }
}

object ToWorkingIR extends Pass {
  def toExp(e: Expression): Expression = e map toExp match {
    case ex: Reference => WRef(ex.name, ex.tpe, UnknownKind, UnknownFlow)
    case ex => ex
  }

  def toStmt(s: Statement): Statement = s map toExp match {
    case sx => sx map toStmt
  }

  def run (c:Circuit): Circuit =
    c copy (modules = c.modules map (_ map toStmt))
}
