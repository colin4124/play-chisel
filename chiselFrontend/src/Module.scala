// play-chisel/chiselFrontend/src/Module.scala
package chisel3

import scala.collection.mutable.{ArrayBuffer, HashMap}

import chisel3.internal._
import chisel3.internal.Builder._
import chisel3.internal.firrtl._

object Module {
  def apply[T <: BaseModule](bc: => T): T = {
    val parent = Builder.currentModule
    val module: T = bc
    Builder.currentModule = parent

    val component = module.generateComponent()
    Builder.components += component

    if(!Builder.currentModule.isEmpty) {
      pushCommand(DefInstance(module, component.ports))
    }

    module
  }
  def currentModule: Option[BaseModule] = Builder.currentModule
}

package experimental {
  object IO {
    def apply[T<:Data](iodef: T): T = {
      val module = Module.currentModule.get
      require(!module.isClosed, "Can't add more ports after module close")
      requireIsChiselType(iodef, "io type")

      val iodefClone = iodef.cloneTypeFull
      module.bindIoInPlace(iodefClone)
      iodefClone
    }
  }
}

abstract class BaseModule extends HasId {
  Builder.currentModule = Some(this)

  protected var _closed = false
  private[chisel3] def isClosed = _closed

  private[chisel3] val _namespace = Namespace.empty

  private val _ids = ArrayBuffer[HasId]()
  private[chisel3] def addId(d: HasId) {
    require(!_closed, "Can't write to module after module close")
    _ids += d
  }
  protected def getIds = {
    require(_closed, "Can't get ids before module close")
    _ids.toSeq
  }

  private val _ports = new ArrayBuffer[Data]()

  protected[chisel3] def getModulePorts = {
    require(_closed, "Can't get ports before module close")
    _ports.toSeq
  }

  private[chisel3] def generateComponent(): Component

  def desiredName: String = this.getClass.getName.split('.').last

  final lazy val name = Builder.globalNamespace.name(desiredName)

  protected def nameIds(rootClass: Class[_]): HashMap[HasId, String] = {
    val names = new HashMap[HasId, String]()
    def name(node: HasId, name: String) {
      if (!names.contains(node)) {
        names.put(node, name)
      }
    }
    for (m <- getPublicFields(rootClass)) {
      Builder.nameRecursively(m.getName, m.invoke(this), name)
    }
    names
  }

  protected def _bindIoInPlace(iodef: Data): Unit = {
    iodef.bind(PortBinding(this))
    _ports += iodef
  }

  private[chisel3] def bindIoInPlace(iodef: Data): Unit = _bindIoInPlace(iodef)

  protected def IO[T <: Data](iodef: T): T = chisel3.experimental.IO.apply(iodef)
}
