// play-chisel/src/internal/firrtl/IR.scala
package chisel3.internal.firrtl

sealed abstract class Width
sealed case class UnknownWidth() extends Width
sealed case class KnownWidth(value: Int) extends Width

object Width {
  def apply(x: Int): Width = KnownWidth(x)
  def apply(): Width = UnknownWidth()
}

abstract class Component
case class Circuit(name: String, components: Seq[Component])
