// play-chisel/chiselFrontend/src/RawModule.scala
package chisel3

import scala.collection.mutable.{ArrayBuffer, HashMap}

import chisel3.internal._
import chisel3.internal.firrtl._

abstract class RawModule extends BaseModule {
  val _commands = ArrayBuffer[Command]()

  def addCommand(c: Command) {
    require(!_closed, "Can't write to module after module close")
    _commands += c
  }

  def getCommands = {
    require(_closed, "Can't get commands before module close")
    _commands.toSeq
  }

  private[chisel3] def namePorts(names: HashMap[HasId, String]): Unit = {
    for (port <- getModulePorts) {
      port.suggestedName.orElse(names.get(port)) match {
        case Some(name) =>
          if (_namespace.contains(name)) {
            throwException(s"""Unable to name port $port to "$name" in $this,""" +
              " name is already taken by another port!")
          }
          port.setRef(ModuleIO(this, _namespace.name(name)))
        case None => throwException(s"Unable to name port $port in $this, " +
          "try making it a public field of the Module")
      }
    }
  }

  def generateComponent(): Component = {
    require(!_closed, "Can't generate module more than once")
    _closed = true

    val names = nameIds(classOf[RawModule])

    namePorts(names)

    for ((node, name) <- names) {
      node.suggestName(name)
    }

    // All suggestions are in, force names to every node.
    for (id <- getIds) {
      id match {
        case id: BaseModule => id.forceName(default=id.desiredName, _namespace)
        case id: Data  =>
          if (id.isSynthesizable) {
            id.topBinding match {
              case OpBinding(_) | PortBinding(_) =>
                id.forceName(default="_T", _namespace)
              case _ =>  // don't name literals
            }
          } // else, don't name unbound types
      }
    }

    val firrtlPorts = getModulePorts map { port: Data =>
      val direction = port.specifiedDirection
      Port(port, direction)
    }

    DefModule(this, name, firrtlPorts, getCommands)
  }
}
