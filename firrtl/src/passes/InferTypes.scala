// play-chisel/firrtl/src/passes/InferTypes.scala
package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._

import debug.PrintIR.print_fir

object CInferTypes extends Pass {
  type TypeMap = collection.mutable.LinkedHashMap[String, Type]

  def run(c: Circuit): Circuit = {
    println("Run CInferTypes ......")
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

    val res = c copy (modules = c.modules map infer_types)
    println("Done CInferTypes")
    print_fir(res)
    res
  }
}

