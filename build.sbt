name := "Scala_N_Queens_PoC"

version := "0.1"

scalaVersion := "2.13.5"

// https://mvnrepository.com/artifact/com.typesafe/config
libraryDependencies += "com.typesafe" % "config" % "1.4.1"

// https://mvnrepository.com/artifact/com.typesafe.akka/akka-actor
libraryDependencies += "com.typesafe.akka" %% "akka-actor-typed" % "2.6.13"

libraryDependencies += Dependencies.LoggingDependencies.logback

libraryDependencies += Dependencies.TestDependencies.scalaTest
libraryDependencies += Dependencies.TestDependencies.akkaTypedTestkit