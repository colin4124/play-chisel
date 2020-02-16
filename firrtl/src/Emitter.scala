// play-chisel/firrtl/src/Emitter.scala
package firrtl

class VerilogEmitter extends SeqTransform with Emitter {
  def inputForm = LowForm
  def outputForm = LowForm
  val outputSuffix = ".v"
  def transforms = Seq()
}
