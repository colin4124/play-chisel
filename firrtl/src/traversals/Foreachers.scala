// play-chisel/firrtl/src/traversals/Foreachers.scala
package firrtl.traversals

import firrtl.ir._
import language.implicitConversions

object Foreachers {

  /** Statement Foreachers */
  private trait StmtForMagnet {
    def foreach(stmt: Statement): Unit
  }
  private object StmtForMagnet {
    implicit def forStmt(f: Statement => Unit): StmtForMagnet = new StmtForMagnet {
      def foreach(stmt: Statement): Unit = stmt foreachStmt f
    }
    implicit def forExp(f: Expression => Unit): StmtForMagnet = new StmtForMagnet {
      def foreach(stmt: Statement): Unit = stmt foreachExpr f
    }
    implicit def forType(f: Type => Unit): StmtForMagnet = new StmtForMagnet {
      def foreach(stmt: Statement) : Unit = stmt foreachType f
    }
    implicit def forString(f: String => Unit): StmtForMagnet = new StmtForMagnet {
      def foreach(stmt: Statement): Unit = stmt foreachString f
    }
  }
  implicit class StmtForeach(val _stmt: Statement) extends AnyVal {
    // Using implicit types to allow overloading of function type to foreach, see StmtForMagnet above
    def foreach[T](f: T => Unit)(implicit magnet: (T => Unit) => StmtForMagnet): Unit = magnet(f).foreach(_stmt)
  }
}

