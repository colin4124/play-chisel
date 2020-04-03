// play-chisel/firrtl/src/Compiler.scala

package firrtl

import firrtl.ir.Circuit
import firrtl.Utils.throwInternalError
import firrtl.options.TransformLike

import debug.PrintIR._

case class CircuitState(
  circuit: Circuit,
  form: CircuitForm,
  )

sealed abstract class CircuitForm(private val value: Int) extends Ordered[CircuitForm] {
  def compare(that: CircuitForm): Int = this.value - that.value
}

final case object ChirrtlForm extends CircuitForm(value = 3)
final case object HighForm extends CircuitForm(2)
final case object MidForm extends CircuitForm(1)
final case object LowForm extends CircuitForm(0)
final case object UnknownForm extends CircuitForm(-1) {
  override def compare(that: CircuitForm): Int = { sys.error("Illegal to compare UnknownForm"); 0 }
}

abstract class Transform extends TransformLike[CircuitState] {
  def name: String = this.getClass.getName
  def inputForm: CircuitForm
  def outputForm: CircuitForm

  protected def execute(state: CircuitState): CircuitState
  def transform(state: CircuitState): CircuitState = execute(state)

  private[firrtl] def prepare(state: CircuitState): CircuitState = state

  final def runTransform(state: CircuitState): CircuitState = {
    val result = execute(prepare(state))

    CircuitState(result.circuit, result.form)
  }
}

trait Emitter extends Transform {
  def outputSuffix: String
}

trait SeqTransformBased {
  def transforms: Seq[Transform]
  protected def runTransforms(state: CircuitState): CircuitState =
    transforms.foldLeft(state) { (in, xform) => xform.runTransform(in) }
}

abstract class SeqTransform extends Transform with SeqTransformBased {
  def execute(state: CircuitState): CircuitState = {
    val ret = runTransforms(state)
    CircuitState(ret.circuit, outputForm)
  }
}

object CompilerUtils {
  def getLoweringTransforms(inputForm: CircuitForm, outputForm: CircuitForm): Seq[Transform] = {
    if (outputForm >= inputForm) {
      Seq.empty
    } else {
      inputForm match {
        case ChirrtlForm =>
          Seq(new ChirrtlToHighFirrtl) ++ getLoweringTransforms(HighForm, outputForm)
        case HighForm =>
          Seq(new IRToWorkingIR,
              new ResolveAndCheck,
              new transforms.DedupModules,
              new HighFirrtlToMiddleFirrtl) ++
            getLoweringTransforms(MidForm, outputForm)
        case MidForm => Seq(new MiddleFirrtlToLowFirrtl) ++ getLoweringTransforms(LowForm, outputForm)
        case LowForm => throwInternalError("getLoweringTransforms - LowForm")
        case UnknownForm => throwInternalError("getLoweringTransforms - UnknownForm")
      }
    }
  }
}

trait Compiler {
  def emitter: Emitter
  def transforms: Seq[Transform]

  require(transforms.size >= 1,
          s"Compiler transforms for '${this.getClass.getName}' must have at least ONE Transform! " +
            "Use IdentityTransform if you need an identity/no-op transform.")

  def compile(state: CircuitState): CircuitState = {
    val finalState = transforms.foldLeft(state) { (in, xform) =>
      xform.runTransform(in)
    }
    finalState
  }
}
