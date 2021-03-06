// play-chisel/firrtl/src/PrintIR.scala
package firrtl.debug

import firrtl._
import firrtl.{ir => fir}

object PrintIR {
  def tab(l: Int) = (" " * 2) * l
  def m_str(name: String) = s"Module: ${name}"
  def p_str(p: fir.Port) = s"Port: ${p.name} ${dir_str(p.direction)} ${type_str(p.tpe)}"
  def dir_str(d: fir.Direction) = d match {
    case fir.Input  => "Input"
    case fir.Output => "Output"
  }
  def bundle_str(b: fir.BundleType) = {
    b.fields map field_str mkString ", "
  }
  def field_str(f: fir.Field) = {
    s"${f.name}: ${ori_str(f.flip)} ${type_str(f.tpe)}"
  }
  def ori_str(o: fir.Orientation) = o match {
    case fir.Default => "Default"
    case fir.Flip    => "Flip"
  }
  def type_str(d: fir.Type): String = d match {
    case fir.UIntType(w)  => s"UInt(${w_str(w)})"
    case bundle: fir.BundleType  => s"Bundle( ${bundle_str(bundle)} )"
    case fir.UnknownType => "UnknownType"
  }
  def w_str(w: fir.Width) = w match {
    case fir.IntWidth(i) => s"$i"
    case fir.UnknownWidth => "UnknownWidth"
  }
  def stmt_str(s: fir.Statement, l: Int): String = {
    s match {
      case fir.DefNode(n, v) => s"${tab(l)}DefNode: $n\n${tab(l+1)}${e_str(v, l+1)}"
      case fir.Block(s) => s"${tab(l)}Block\n" + (s map { x => stmt_str(x, l+1) } mkString "\n")
      case fir.Connect(left, right) =>
        s"${tab(l)}Connect\n${tab(l+1)}${e_str(left, l+1)}\n${tab(l+1)}${e_str(right, l+1)}"
      case fir.DefInstance(n, m) => s"${tab(l)}DefInstance: inst $n of $m"
      case WDefInstance(n, m, t) => s"${tab(l)}WDefInstance($n, $m, ${type_str(t)})"
    }
  }
  def e_str(e: fir.Expression, l: Int): String = {
    e match {
      case fir.Reference(n, t) => s"Reference(${n}: ${type_str(t)})"
      case fir.SubField(e, n, t) => s"SubField(${e_str(e, l)}.$n, ${type_str(t)})"
      case fir.DoPrim(op, args, const, tpe) =>
        s"DoPrim\n${tab(l+1)}${op}\n${tab(l+1)}"      +
        (args map {x => e_str(x, l+1)} mkString ", ") +
        s"\n${tab(l+1)}${type_str(tpe)}"
      case WRef(n, t, k, f) => s"WRef(${n}: ${type_str(t)} ${k_str(k)} ${f_str(f)})"
      case WSubField(e, n, t, f) => s"WSubField(${e_str(e, l)}.${n}: ${type_str(t)} ${f_str(f)})"
    }
  }
  def k_str(k: Kind): String = k match {
    case InstanceKind => "InstanceKind"
    case PortKind    => "PortKind"
    case NodeKind    => "NodeKind"
    case UnknownKind => "UnknownKind"
  }
  def f_str(f: Flow): String = f match {
    case SourceFlow  => "SourceFlow"
    case SinkFlow    => "SinkFlow"
    case UnknownFlow => "UnknownFlow"
  }

  def print_fir(ast: fir.Circuit) = {
    println(s"Circuit:")
    println(s"  modules: [${ast.modules map { _.name } mkString ", "}]")
    println(s"  main: ${ast.main}\n")
    ast.modules foreach { m =>
      print(tab(1))
      println(m_str(m.name))
      m.ports foreach { p =>
        print(tab(2))
        println(p_str(p))
      }
      m match {
        case fir.Module(_, _, body) => println(stmt_str(body, 2))
        case _ =>
      }
    }
  }
}
