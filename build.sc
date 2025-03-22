package build
import mill._, scalalib._

object `package` extends RootModule with ScalaModule {
  def scalaVersion = "3.6.4"

  // def ivyDeps = Agg(
  // ivy"org.typelevel::cats-parse::1.0.0"
  // )
}
