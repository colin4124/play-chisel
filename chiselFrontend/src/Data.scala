// play-chisel/chiselFrontend/src/Data.scala
package chisel3

import chisel3.internal._
import chisel3.internal.firrtl._

sealed abstract class SpecifiedDirection
object SpecifiedDirection {
  case object Unspecified extends SpecifiedDirection
  case object Output      extends SpecifiedDirection
  case object Input       extends SpecifiedDirection
  case object Flip        extends SpecifiedDirection

  def flip(dir: SpecifiedDirection): SpecifiedDirection = dir match {
    case Unspecified => Flip
    case Flip        => Unspecified
    case Output      => Input
    case Input       => Output
  }

  def fromParent(parentDirection: SpecifiedDirection, thisDirection: SpecifiedDirection): SpecifiedDirection =
    (parentDirection, thisDirection) match {
      case (SpecifiedDirection.Output, _) => SpecifiedDirection.Output
      case (SpecifiedDirection.Input, _) => SpecifiedDirection.Input
      case (SpecifiedDirection.Unspecified, thisDirection) => thisDirection
      case (SpecifiedDirection.Flip, thisDirection) => SpecifiedDirection.flip(thisDirection)
    }

  private[chisel3] def specifiedDirection[T<:Data](source: T)(dir: SpecifiedDirection): T = {
    val out = source.cloneType.asInstanceOf[T]
    out.specifiedDirection = dir
    out
  }
}

object Input {
  def apply[T<:Data](source: T): T = {
    SpecifiedDirection.specifiedDirection(source)(SpecifiedDirection.Input)
  }
}
object Output {
  def apply[T<:Data](source: T): T = {
    SpecifiedDirection.specifiedDirection(source)(SpecifiedDirection.Output)
  }
}

sealed abstract class ActualDirection
object ActualDirection {
  case object Empty       extends ActualDirection
  case object Unspecified extends ActualDirection
  case object Output      extends ActualDirection
  case object Input       extends ActualDirection

  def fromSpecified(direction: SpecifiedDirection): ActualDirection = direction match {
    case SpecifiedDirection.Unspecified | SpecifiedDirection.Flip => ActualDirection.Unspecified
    case SpecifiedDirection.Output => ActualDirection.Output
    case SpecifiedDirection.Input => ActualDirection.Input
  }
}

abstract class Data extends HasId {
  private var _specifiedDirection: SpecifiedDirection = SpecifiedDirection.Unspecified

  def specifiedDirection: SpecifiedDirection = _specifiedDirection
  def specifiedDirection_=(direction: SpecifiedDirection) = {
    if (_specifiedDirection != SpecifiedDirection.Unspecified) {
      throw RebindingException(s"Attempted reassignment of user-specified direction to $this")
    }
    _specifiedDirection = direction
  }

  private var _binding: Option[Binding] = None
  protected[chisel3] def binding: Option[Binding] = _binding
  protected def binding_=(target: Binding) {
    if (_binding.isDefined) {
      throw RebindingException(s"Attempted reassignment of binding to $this")
    }
    _binding = Some(target)
  }

  private[chisel3] def bind(target: Binding, parentDirection: SpecifiedDirection = SpecifiedDirection.Unspecified)

  private[chisel3] final def isSynthesizable: Boolean = _binding.map {
    case _: TopBinding => true
  }.getOrElse(false)

  private[chisel3] def topBindingOpt: Option[TopBinding] = _binding.flatMap {
    case bindingVal: TopBinding => Some(bindingVal)
  }

  private[chisel3] def topBinding: TopBinding = topBindingOpt.get

  private var _direction: Option[ActualDirection] = None

  private[chisel3] def direction: ActualDirection = _direction.get
  private[chisel3] def direction_=(actualDirection: ActualDirection) {
    if (_direction.isDefined) {
      throw RebindingException(s"Attempted reassignment of resolved direction to $this")
    }
    _direction = Some(actualDirection)
  }

  def cloneType: this.type

  private[chisel3] def cloneTypeFull: this.type = {
    // get a fresh object, without bindings
    val clone = this.cloneType.asInstanceOf[this.type]
    // Only the top-level direction needs to be fixed up, cloneType should do the rest
    clone.specifiedDirection = specifiedDirection
    clone
  }

  private[chisel3] def lref: Node = {
    requireIsHardware(this)
    topBindingOpt match {
      case Some(binding: ReadOnlyBinding) => throwException(s"internal error: attempted to generate LHS ref to ReadOnlyBinding $binding")
      case Some(binding: TopBinding) => Node(this)
      case opt => throwException(s"internal error: unknown binding $opt in generating LHS ref")
    }
  }

  private[chisel3] def ref: Arg = {
    requireIsHardware(this)
    topBindingOpt match {
      case Some(binding: TopBinding) => Node(this)
      case opt => throwException(s"internal error: unknown binding $opt in generating LHS ref")
    }
  }

  private[chisel3] def connect(that: Data): Unit = {
    requireIsHardware(this, "data to be connected")
    requireIsHardware(that, "data to be connected")
    this.topBinding match {
      case _: ReadOnlyBinding => throwException(s"Cannot reassign to read-only $this")
      case _ =>  // fine
    }
    try {
      MonoConnect.connect(this, that, Builder.referenceUserModule)
    } catch {
      case MonoConnectException(message) =>
        throwException(
          s"Connection between sink ($this) and source ($that) failed @$message"
        )
    }
  }

  private[chisel3] def width: Width

  final def := (that: Data): Unit = this.connect(that)
}
