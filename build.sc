// play-chisel/build.sc
import ammonite.ops._
import mill._
import mill.scalalib._

trait CommonChiselModule extends ScalaModule {
  def scalaVersion = "2.12.10"
  def scalacOptions = Seq(
    "-deprecation",  // Emit warning and location for usages of deprecated APIs.
    "-explaintypes", // Explain type errors in more detail.
    "-feature",      // Emit warning and location for usages of features that should
    "-unchecked",    // Enable additional warnings where generated code depends on assumptions.
  )
}

object chisel3 extends CommonChiselModule {
  object firrtl extends CommonChiselModule
  object chiselFrontend extends CommonChiselModule {
    def moduleDeps = Seq(firrtl)
  }
  def moduleDeps = Seq(firrtl, chiselFrontend)
  def millSourcePath = super.millSourcePath / ammonite.ops.up
}