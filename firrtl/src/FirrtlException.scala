// play-chisel/firrtl/src/FirrtlException.scala
package firrtl

import scala.util.control.NoStackTrace

private[firrtl] class FirrtlInternalException(message: String, cause: Throwable = null)
    extends Exception(message, cause)

class FirrtlUserException(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause) with NoStackTrace
