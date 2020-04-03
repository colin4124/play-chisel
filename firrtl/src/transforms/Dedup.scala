// play-chisel/firrtl/src/transforms/Dedup.scala
package firrtl
package transforms

import scala.collection.mutable

import firrtl.ir._
import firrtl.Mappers._
import firrtl.analyses.InstanceGraph
import firrtl.Utils.throwInternalError

import debug.PrintIR.print_fir

class DedupModules extends Transform { def inputForm: CircuitForm = HighForm
  def outputForm: CircuitForm = HighForm

  def execute(state: CircuitState): CircuitState = {
    println("Run Dedup...")
    val newC = run(state.circuit)
    println("Done Dedup.")
    print_fir(newC)
    state.copy(circuit = newC)
  }

  def run(c: Circuit): Circuit = {
    // Maps module name to corresponding dedup module
    val dedupMap = DedupModules.deduplicate(c)

    // Use old module list to preserve ordering
    val dedupedModules = c.modules.map(m => dedupMap(m.name)).distinct

    c.copy(modules = dedupedModules)
  }
}

object DedupModules {
  def changeInternals(rename: String=>String,
                      retype: String=>Type=>Type,
                      renameOfModule: (String, String)=>String,
                      renameExps: Boolean = true
                     )(module: DefModule): DefModule = {
    def onPort(p: Port): Port = Port(rename(p.name), p.direction, retype(p.name)(p.tpe))
    def onExp(e: Expression): Expression = e match {
      case WRef(n, t, k, g) => WRef(rename(n), retype(n)(t), k, g)
      case WSubField(expr, n, tpe, kind) =>
        val fieldIndex = expr.tpe.asInstanceOf[BundleType].fields.indexWhere(f => f.name == n)
        val newExpr = onExp(expr)
        val newField = newExpr.tpe.asInstanceOf[BundleType].fields(fieldIndex)
        val finalExpr = WSubField(newExpr, newField.name, newField.tpe, kind)
        finalExpr
      case other => other map onExp
    }
    def onStmt(s: Statement): Statement = s match {
      case DefNode(name, value) =>
        if(renameExps) DefNode(rename(name), onExp(value))
        else DefNode(rename(name), value)
      case WDefInstance(n, m, t) =>
        val newmod = renameOfModule(n, m)
        WDefInstance(rename(n), newmod, retype(n)(t))
      case DefInstance(n, m) => DefInstance(rename(n), renameOfModule(n, m))
      case h: IsDeclaration =>
        val temp = h map rename map retype(h.name)
        if(renameExps) temp map onExp else temp
      case other =>
        val temp = other map onStmt
        if(renameExps) temp map onExp else temp
    }
    module map onPort map onStmt
  }

  def buildRTLTags(moduleLinearization: Seq[DefModule]): (collection.Map[String, collection.Set[String]], mutable.HashMap[String, String]) = {
    // Maps a module name to its agnostic name
    val tagMap = mutable.HashMap.empty[String, String]

    // Maps a tag to all matching module names
    val tag2all = mutable.HashMap.empty[String, mutable.HashSet[String]]

    def fastSerializedHash(s: Statement): Int ={
      def serialize(builder: StringBuilder, nindent: Int)(s: Statement): Unit = s match {
        case Block(stmts) => stmts.map {
          val x = serialize(builder, nindent)(_)
          builder ++= "\n"
          x
        }
        case other: Statement =>
          builder ++= ("  " * nindent)
          builder ++= other.serialize
      }
      val builder = new mutable.StringBuilder()
      serialize(builder, 0)(s)
      builder.hashCode()
    }

    moduleLinearization.foreach { originalModule =>
      // Build name-agnostic module
      val agnosticModule = originalModule

      // Build tag
      val builder = new mutable.ArrayBuffer[Any]()
      agnosticModule.ports.foreach { builder ++= _.serialize }
      agnosticModule match {
        case Module(_, _, b) => builder ++= fastSerializedHash(b).toString()//.serialize
      }
      val tag = builder.hashCode().toString

      tagMap += originalModule.name -> tag

      // Set tag's module to be the first matching module
      val all = tag2all.getOrElseUpdate(tag, mutable.HashSet.empty[String])
      all += originalModule.name
    }
    (tag2all, tagMap)
  }

  def dedupInstances(originalModule: String,
                     moduleMap: Map[String, DefModule],
                     name2name: Map[String, String]): DefModule = {
    val module = moduleMap(originalModule)

    // Get all instances to know what to rename in the module
    val instances = mutable.Set[WDefInstance]()
    InstanceGraph.collectInstances(instances)(module.asInstanceOf[Module].body)
    val instanceModuleMap = instances.map(i => i.name -> i.module).toMap

    def getNewModule(old: String): DefModule = {
      moduleMap(name2name(old))
    }
    // Define rename functions
    def renameOfModule(instance: String, ofModule: String): String = {
      name2name(ofModule)
    }
    val typeMap = mutable.HashMap[String, Type]()
    def retype(name: String)(tpe: Type): Type = {
      if (typeMap.contains(name)) typeMap(name) else {
        if (instanceModuleMap.contains(name)) {
          val newType = Utils.module_type(getNewModule(instanceModuleMap(name)))
          typeMap(name) = newType
          newType
        } else {
          tpe
        }
      }
    }

    // Change module internals
    changeInternals({n => n}, retype, renameOfModule)(module)
  }
  def deduplicate(circuit: Circuit): Map[String, DefModule] = {
    val (moduleMap, moduleLinearization) = {
      val iGraph = new InstanceGraph(circuit)
      (iGraph.moduleMap, iGraph.moduleOrder.reverse)
    }

    val main = circuit.main
    val (tag2all, tagMap) = buildRTLTags(moduleLinearization)

    // Set tag2name to be the best dedup module name
    val moduleIndex = circuit.modules.zipWithIndex.map{case (m, i) => m.name -> i}.toMap
    def order(l: String, r: String): String = {
     if (l == main) l
     else if (r == main) r
     else if (moduleIndex(l) < moduleIndex(r)) l else r
    }

    // Maps a module's tag to its deduplicated module
    val tag2name = mutable.HashMap.empty[String, String]
    tag2all.foreach { case (tag, all) => tag2name(tag) = all.reduce(order)}


    // Create map from original to dedup name
    val name2name = moduleMap.keysIterator.map{ originalModule =>
      tagMap.get(originalModule) match {
        case Some(tag) => originalModule -> tag2name(tag)
        case None => originalModule -> originalModule
        case other => throwInternalError(other.toString)
      }
    }.toMap

    val dedupedName2module = tag2name.map({ case (_, name) => name -> DedupModules.dedupInstances(name, moduleMap, name2name) })

    // Build map from original name to corresponding deduped module
    val name2module = tag2all.flatMap({ case (tag, names) => names.map(n => n -> dedupedName2module(tag2name(tag))) })

    name2module.toMap
  }
}
