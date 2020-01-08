// play-chisel/chiselFrontend/src/package.scala
package object chisel3 {
  import internal.firrtl.{Width}

  import scala.language.implicitConversions

  implicit class fromIntToWidth(int: Int) {
    def W: Width = Width(int)
  }

  object UInt extends UIntFactory
}
