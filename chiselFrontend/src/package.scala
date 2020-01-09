// play-chisel/chiselFrontend/src/package.scala
package object chisel3 {
  import internal.firrtl.{Width}

  import scala.language.implicitConversions

  implicit class fromIntToWidth(int: Int) {
    def W: Width = Width(int)
  }

  object UInt extends UIntFactory

  type ChiselException = internal.ChiselException
  class BindingException(message: String) extends ChiselException(message)
  case class RebindingException(message: String) extends BindingException(message)
  case class ExpectedChiselTypeException(message: String) extends BindingException(message)
  case class ExpectedHardwareException(message: String) extends BindingException(message)
  case class MonoConnectException(message: String) extends ChiselException(message)
}
