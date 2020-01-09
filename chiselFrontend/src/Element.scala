// play-chisel/chiselFrontend/src/Element.scala
package chisel3

import chisel3.internal._

abstract class Element extends Data {
  private[chisel3] override def bind(target: Binding, parentDirection: SpecifiedDirection) {
    binding = target
    val resolvedDirection = SpecifiedDirection.fromParent(parentDirection, specifiedDirection)
    direction = ActualDirection.fromSpecified(resolvedDirection)
  }
}
