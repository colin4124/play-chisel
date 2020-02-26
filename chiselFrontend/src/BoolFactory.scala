// play-chisel/chiselFrontend/src/BoolFactory.scala
package chisel3

trait BoolFactory {
  def apply(): Bool = new Bool()
}
