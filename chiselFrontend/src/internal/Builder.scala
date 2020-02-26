// play-chisel/chiselFrontend/src/internal/Builder.scala
package chisel3.internal

import scala.collection.mutable.ArrayBuffer

import chisel3._
import chisel3.internal.firrtl._

private[chisel3] class Namespace(keywords: Set[String]) {
  private val names = collection.mutable.HashMap[String, Long]()
  for (keyword <- keywords)
    names(keyword) = 1

  private def rename(n: String): String = {
    val index = names(n)
    val tryName = s"${n}_${index}"
    names(n) = index + 1
    if (this contains tryName) rename(n) else tryName
  }

  def contains(elem: String): Boolean = names.contains(elem)

  def name(elem: String): String = {
    if (this contains elem) {
      name(rename(elem))
    } else {
      names(elem) = 1
      elem
    }
  }
}

private[chisel3] object Namespace {
  /** Constructs an empty Namespace */
  def empty: Namespace = new Namespace(Set.empty[String])
}

private[chisel3] trait HasId {
  private[chisel3] val _parent: Option[BaseModule] = Builder.currentModule
  _parent.foreach(_.addId(this))

  private var suggested_name: Option[String] = None

  def suggestName(name: =>String): this.type = {
    if(suggested_name.isEmpty) suggested_name = Some(name)
    this
  }
  private[chisel3] def suggestedName: Option[String] = suggested_name

  private[chisel3] def forceName(default: =>String, namespace: Namespace): Unit =
    if(_ref.isEmpty) {
      val candidate_name = suggested_name.getOrElse(default)
      val available_name = namespace.name(candidate_name)
      setRef(Ref(available_name))
    }

  var _ref: Option[Arg] = None
  private[chisel3] def setRef(imm: Arg): Unit = {
    _ref = Some(imm)
  }
  private[chisel3] def getRef: Arg = _ref.get
  private[chisel3] def getOptionRef: Option[Arg] = _ref

  private[chisel3] def getPublicFields(rootClass: Class[_]): Seq[java.lang.reflect.Method] = {
    // Suggest names to nodes using runtime reflection
    def getValNames(c: Class[_]): Set[String] = {
      if (c == rootClass) {
        Set()
      } else {
        getValNames(c.getSuperclass) ++ c.getDeclaredFields.map(_.getName)
      }
    }
    val valNames = getValNames(this.getClass)
    def isPublicVal(m: java.lang.reflect.Method) =
      m.getParameterTypes.isEmpty && valNames.contains(m.getName) && !m.getDeclaringClass.isAssignableFrom(rootClass)

    this.getClass.getMethods.sortWith(_.getName < _.getName).filter(isPublicVal(_))
  }

}

object Builder {
  val components: ArrayBuffer[Component] = ArrayBuffer[Component]()
  var currentModule: Option[BaseModule] = None

  val globalNamespace: Namespace = Namespace.empty

  def forcedUserModule: RawModule = currentModule match {
    case Some(module: RawModule) => module
    case _ => throwException(
      "Error: Not in a UserModule. Likely cause: Missed Module() wrap, bare chisel API call, or attempting to construct hardware inside a BlackBox."
    )
  }

  def pushCommand[T <: Command](c: T): T = {
    forcedUserModule.addCommand(c)
    c
  }

  def pushOp[T <: Data](cmd: DefPrim[T]): T = {
    // Bind each element of the returned Data to being a Op
    cmd.id.bind(OpBinding(forcedUserModule))
    pushCommand(cmd).id
  }

  def referenceUserModule: RawModule = {
    currentModule match {
      case Some(module: RawModule) => module
      case _ => throwException(
        "Error: Not in a RawModule. Likely cause: Missed Module() wrap, bare chisel API call, or attempting to construct hardware inside a BlackBox."
      )
    }
  }

  def nameRecursively(prefix: String, nameMe: Any, namer: (HasId, String) => Unit): Unit = nameMe match {
    case (id: HasId) => namer(id, prefix)
    case Some(elt) => nameRecursively(prefix, elt, namer)
    case m: Map[_, _] =>
      m foreach { case (k, v) =>
        nameRecursively(s"${k}", v, namer)
      }
    case (iter: Iterable[_]) if iter.hasDefiniteSize =>
      for ((elt, i) <- iter.zipWithIndex) {
        nameRecursively(s"${prefix}_${i}", elt, namer)
      }

    case _ => // Do nothing
  }

  def build[T <: RawModule](f: => T): (Circuit, T) = {
    println("Elaborating design...")
    val mod = f
    mod.forceName(mod.name, globalNamespace)
    println("Done elaborating.")

    (Circuit(components.last.name, components), mod)
  }
}
