//  play-chisel/firrtl/src/options/Phase.scala
package firrtl.options

trait TransformLike[A] extends {
  def name: String

  def transform(a: A): A
}
