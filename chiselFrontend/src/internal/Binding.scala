// play-chisel/chiselFrontend/src/internal/Binding.scala
package chisel3.internal

import chisel3._

object requireIsHardware {
  def apply(node: Data, msg: String = ""): Unit = {
    if (!node.isSynthesizable) {
      val prefix = if (msg.nonEmpty) s"$msg " else ""
      throw ExpectedHardwareException(s"$prefix'$node' must be hardware, " +
        "not a bare Chisel type. Perhaps you forgot to wrap it in Wire(_) or IO(_)?")
    }
  }
}

object requireIsChiselType {
  def apply(node: Data, msg: String = ""): Unit = if (node.isSynthesizable) {
    val prefix = if (msg.nonEmpty) s"$msg " else ""
    throw ExpectedChiselTypeException(s"$prefix'$node' must be a Chisel type, not hardware")
  }
}

private[chisel3] sealed abstract class BindingDirection
private[chisel3] object BindingDirection {
  case object Internal extends BindingDirection
  case object Output   extends BindingDirection
  case object Input    extends BindingDirection

  def from(binding: TopBinding, direction: ActualDirection): BindingDirection = {
    binding match {
      case PortBinding(_) => direction match {
        case ActualDirection.Output => Output
        case ActualDirection.Input => Input
        case dir => throw new RuntimeException(s"Unexpected port element direction '$dir'")
      }
      case _ => Internal
    }
  }
}

sealed trait Binding {
  def location: Option[BaseModule]
}
sealed trait TopBinding extends Binding

sealed trait ConstrainedBinding extends TopBinding {
  def enclosure: BaseModule
  def location: Option[BaseModule] = Some(enclosure)
}

sealed trait ReadOnlyBinding extends TopBinding

case class OpBinding(enclosure: RawModule) extends ConstrainedBinding with ReadOnlyBinding
case class PortBinding(enclosure: BaseModule) extends ConstrainedBinding
