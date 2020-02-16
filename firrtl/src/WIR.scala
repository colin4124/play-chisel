// play-chisel/firrtl/src/WIR.scala
package firrtl

import Utils._
import firrtl.ir._

private[firrtl] sealed trait HasMapWidth {
  def mapWidth(f: Width => Width): Width
}

case class MaxWidth(args: Seq[Width]) extends Width with HasMapWidth {
  def mapWidth(f: Width => Width): Width = MaxWidth(args map f)
}
