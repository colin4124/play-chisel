// play-chisel/firrtl/src/LoweringCompiler.scala

package firrtl

import CompilerUtils.getLoweringTransforms

sealed abstract class CoreTransform extends SeqTransform

class ChirrtlToHighFirrtl extends CoreTransform {
  def inputForm = ChirrtlForm
  def outputForm = HighForm
  def transforms = Seq(
    passes.CInferTypes,
  )
}
class IRToWorkingIR extends CoreTransform {
  def inputForm = HighForm
  def outputForm = HighForm
  def transforms = Seq(passes.ToWorkingIR)
}
class ResolveAndCheck extends CoreTransform {
  def inputForm = HighForm
  def outputForm = HighForm
  def transforms = Seq(
    passes.ResolveKinds,
    passes.ResolveFlows,
  )
}
class HighFirrtlToMiddleFirrtl extends CoreTransform {
  def inputForm = HighForm
  def outputForm = MidForm
  def transforms = Seq()
}
class MiddleFirrtlToLowFirrtl extends CoreTransform {
  def inputForm = MidForm
  def outputForm = LowForm
  def transforms = Seq()
}
class LowFirrtlOptimization extends CoreTransform {
  def inputForm = LowForm
  def outputForm = LowForm
  def transforms = Seq()
}

class VerilogCompiler extends Compiler {
  val emitter = new VerilogEmitter
  def transforms: Seq[Transform] = getLoweringTransforms(ChirrtlForm, LowForm) ++
    Seq(new LowFirrtlOptimization) ++
    Seq(emitter)
}
