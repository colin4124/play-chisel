// play-chisel/chiselFrontend/src/internal/MonoConnect.scala
package chisel3.internal

import chisel3._
import chisel3.internal.Builder.pushCommand
import chisel3.internal.firrtl.Connect

private[chisel3] object MonoConnect {
  def UnwritableSinkException =
    MonoConnectException(": Sink is unwriteable by current module.")
  def UnknownRelationException =
    MonoConnectException(": Sink or source unavailable to current module.")
  def MismatchedException(sink: String, source: String) =
    MonoConnectException(s": Sink ($sink) and Source ($source) have different types.")

  def connect(
    sink: Data,
    source: Data,
    context_mod: RawModule): Unit =
    (sink, source) match {
      case (sink_e: UInt, source_e: UInt) =>
        elemConnect(sink_e, source_e, context_mod)
      case (sink, source) => throw MismatchedException(sink.toString, source.toString)
    }

  def elemConnect(sink: Element, source: Element, context_mod: RawModule): Unit = {
    import BindingDirection.{Internal, Input, Output}
    val sink_mod: BaseModule   = sink.topBinding.location.getOrElse(throw UnwritableSinkException)
    val source_mod: BaseModule = source.topBinding.location.getOrElse(context_mod)

    val sink_direction = BindingDirection.from(sink.topBinding, sink.direction)
    val source_direction = BindingDirection.from(source.topBinding, source.direction)

    // CASE: Context is same module that both left node and right node are in
    if( (context_mod == sink_mod) && (context_mod == source_mod) ) {
      ((sink_direction, source_direction): @unchecked) match {
        //    SINK          SOURCE
        //    CURRENT MOD   CURRENT MOD
        case (Output,       _) => issueConnect(sink, source)
        case (Internal,     _) => issueConnect(sink, source)
        case (Input,        _) => throw UnwritableSinkException
      }
    }
    else throw UnknownRelationException
  }

  private def issueConnect(sink: Element, source: Element): Unit = {
    source.topBinding match {
      case _ => pushCommand(Connect(sink.lref, source.ref))
    }
  }
}
