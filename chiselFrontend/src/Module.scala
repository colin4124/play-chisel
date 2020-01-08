// play-chisel/chiselFrontend/src/Module.scala
package chisel3

import chisel3.internal._

object Module {
  def apply[T <: BaseModule](bc: => T): T = {
    val module: T = bc
    module
  }
}

package experimental {
  object IO {
    def apply[T<:Data](iodef: T): T = {
      iodef
    }
  }
}

abstract class BaseModule extends HasId