// play-chisel/firrtl/src/passes/InferTypes.scala
package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._

import debug.PrintIR.print_fir

object InferTypes extends Pass {
  type TypeMap = collection.mutable.LinkedHashMap[String, Type]

  def run(c: Circuit): Circuit = {
    val namespace = Namespace()
    val mtypes = (c.modules map (m => m.name -> module_type(m))).toMap

    def remove_unknowns_w(w: Width): Width = w match {
      case UnknownWidth => VarWidth(namespace.newName("w"))
      case wx => wx
    }

    def remove_unknowns(t: Type): Type =
      t map remove_unknowns map remove_unknowns_w

    def infer_types_e(types: TypeMap)(e: Expression): Expression =
      e map infer_types_e(types) match {
        case e: WRef => e copy (tpe = types(e.name))
        case e: WSubField => e copy (tpe = field_type(e.expr.tpe, e.name))
        case e: DoPrim => PrimOps.set_primop_type(e)
      }

    def infer_types_s(types: TypeMap)(s: Statement): Statement = s match {
      case sx: WDefInstance =>
        val t = mtypes(sx.module)
        types(sx.name) = t
        sx copy (tpe = t)
      case sx: DefNode =>
        val sxx = (sx map infer_types_e(types)).asInstanceOf[DefNode]
        val t = remove_unknowns(sxx.value.tpe)
        types(sx.name) = t
        sxx map infer_types_e(types)
      case sx => sx map infer_types_s(types) map infer_types_e(types)
    }

    def infer_types_p(types: TypeMap)(p: Port): Port = {
      val t = remove_unknowns(p.tpe)
      types(p.name) = t
      p copy (tpe = t)
    }

    def infer_types(m: DefModule): DefModule = {
      val types = new TypeMap
      m map infer_types_p(types) map infer_types_s(types)
    }

    println("Run InferTypes ......")
    val res = c copy (modules = c.modules map infer_types)
    println("Done InferTypes.")
    print_fir(res)
    res
  }
}

object CInferTypes extends Pass {
  type TypeMap = collection.mutable.LinkedHashMap[String, Type]

  def run(c: Circuit): Circuit = {
    val mtypes = (c.modules map (m => m.name -> module_type(m))).toMap

    def infer_types_e(types: TypeMap)(e: Expression) : Expression =
      e map infer_types_e(types) match {
        case (e: Reference) => e copy (tpe = types.getOrElse(e.name, UnknownType))
        case (e: SubField) => e copy (tpe = field_type(e.expr.tpe, e.name))
        case (e: DoPrim) => PrimOps.set_primop_type(e)
      }

    def infer_types_s(types: TypeMap)(s: Statement): Statement = s match {
      case sx: DefNode =>
        val sxx = (sx map infer_types_e(types)).asInstanceOf[DefNode]
        types(sxx.name) = sxx.value.tpe
        sxx
      case sx: DefInstance =>
        types(sx.name) = mtypes(sx.module)
        sx
      case sx => sx map infer_types_s(types) map infer_types_e(types)
    }

    def infer_types_p(types: TypeMap)(p: Port): Port = {
      types(p.name) = p.tpe
      p
    }

    def infer_types(m: DefModule): DefModule = {
      val types = new TypeMap
      m map infer_types_p(types) map infer_types_s(types)
    }

    c copy (modules = c.modules map infer_types)
  }
}

