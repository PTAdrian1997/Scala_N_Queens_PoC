import sbt._

object Dependencies {

  lazy val akkaVersion = "2.6.12"
  lazy val logbackVersion = "1.2.3"
  lazy val scalatestVersion = "3.2.9"

  object AkkaDependencies {
    lazy val akkaTypedActor = "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion
  }

  object LoggingDependencies {
    lazy val logback = "ch.qos.logback" % "logback-classic" % logbackVersion
  }

  object TestDependencies {
    lazy val scalaTest = "org.scalatest" %% "scalatest" % scalatestVersion % Test
    lazy val akkaTypedTestkit = "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test
  }

}
