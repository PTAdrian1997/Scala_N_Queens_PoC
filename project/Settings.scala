import sbt._
import sbt.Keys._
import org.apache.ivy.util.url.URLHandlerRegistry

object Settings {

  private lazy val compilerSettings = scalacOptions ++= Seq(
    "-unchecked",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-deprecation",
    "-encoding",
    "utf8"
  )

  lazy val commonSettings: Seq[Def.Setting[_]] = compilerSettings


}
