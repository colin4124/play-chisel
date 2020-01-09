// play-chisel/chiselFrontend/src/internal/Error.scala
package chisel3.internal

class ChiselException(message: String, cause: Throwable = null) extends Exception(message, cause)

private[chisel3] object throwException {
  def apply(s: String, t: Throwable = null): Nothing =
    throw new ChiselException(s, t)
}
