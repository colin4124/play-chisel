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
    // CASE: Context is same module as sink node and right node is in a child module
    else if( (sink_mod == context_mod) &&
              (source_mod._parent.map(_ == context_mod).getOrElse(false)) ) {
      // Thus, right node better be a port node and thus have a direction
      ((sink_direction, source_direction): @unchecked) match {
        //    SINK        SOURCE
        //    CURRENT MOD CHILD MOD
        case (Internal,   Output)   => issueConnect(sink, source)
        case (Internal,   Input)    => issueConnect(sink, source)
        case (Output,     Output)   => issueConnect(sink, source)
        case (Output,     Input)    => issueConnect(sink, source)
        case (Input,      _)        => throw UnwritableSinkException
      }
    }
    // CASE: Context is same module as source node and sink node is in child module
    else if( (source_mod == context_mod) &&
              (sink_mod._parent.map(_ == context_mod).getOrElse(false)) ) {
      // Thus, left node better be a port node and thus have a direction
      ((sink_direction, source_direction): @unchecked) match {
        //    SINK          SOURCE
        //    CHILD MOD     CURRENT MOD
        case (Input,        _) => issueConnect(sink, source)
        case (Output,       _) => throw UnwritableSinkException
        case (Internal,     _) => throw UnwritableSinkException
      }
    }
    // CASE: Context is the parent module of both the module containing sink node
    //                                        and the module containing source node
    //   Note: This includes case when sink and source in same module but in parent
    else if( (sink_mod._parent.map(_ == context_mod).getOrElse(false)) &&
              (source_mod._parent.map(_ == context_mod).getOrElse(false))
    ) {
      // Thus both nodes must be ports and have a direction
      ((sink_direction, source_direction): @unchecked) match {
        //    SINK      SOURCE
        //    CHILD MOD CHILD MOD
        case (Input,    _) => issueConnect(sink, source)
        case (Output,   _) => throw UnwritableSinkException
        case (Internal, _) => throw UnwritableSinkException
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
