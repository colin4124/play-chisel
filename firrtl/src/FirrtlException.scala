// play-chisel/firrtl/src/FirrtlException.scala
package firrtl

private[firrtl] class FirrtlInternalException(message: String, cause: Throwable = null)
    extends Exception(message, cause)
