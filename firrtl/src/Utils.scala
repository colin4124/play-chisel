// play-chisel/firrtl/src/Utils.scala

package firrtl

import language.implicitConversions

import firrtl.ir._

object Utils {
  def getThrowable(maybeException: Option[Throwable], first: Boolean): Throwable = {
    maybeException match {
      case Some(e: Throwable) => {
        val t = e.getCause
        if (t != null) {
          if (first) {
            getThrowable(Some(t), first)
          } else {
            t
          }
        } else {
          e
        }
      }
      case None | null => null
    }
  }

  def throwInternalError(message: String = "", exception: Option[Exception] = None) = {
    val throwable = getThrowable(exception, true)
    val string = if (message.nonEmpty) message + "\n" else message
    error("Internal Error! %sPlease file an issue at https://github.com/ucb-bar/firrtl/issues".format(string), throwable)
  }

  implicit def toWrappedExpression (x:Expression): WrappedExpression = new WrappedExpression(x)

  def max(a: BigInt, b: BigInt): BigInt = if (a >= b) a else b

  def error(str: String, cause: Throwable = null) = throw new FirrtlInternalException(str, cause)
}

object getWidth {
  def apply(t: Type): Width = t match {
    case t: GroundType => t.width
    case _ => Utils.error(s"No width: $t")
  }
  def apply(e: Expression): Width = apply(e.tpe)
}

object bitWidth {
  def apply(dt: Type): BigInt = widthOf(dt)
  private def widthOf(dt: Type): BigInt = dt match {
    case GroundType(IntWidth(width)) => width
    case t => Utils.error(s"Unknown type encountered in bitWidth: $dt")
  }
}
