package build
import mill._, scalalib._

object `package` extends RootModule with ScalaModule {
  def scalaVersion = "3.6.4"

  def ivyDeps = Agg(
    ivy"ch.qos.logback:logback-classic::1.3.5",
    ivy"com.typesafe.scala-logging::scala-logging::3.9.5",
    ivy"com.lihaoyi::sourcecode:0.4.2"
    // ivy"org.typelevel::cats-parse::1.0.0"
  )
}
