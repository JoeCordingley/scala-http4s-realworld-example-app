enablePlugins(JavaAppPackaging)

val http4sVersion = "1.0.0-M38"
val circeVersion = "0.14.6"
val doobieVersion = "1.0.0-M5"
val tSecVersion = "0.5.0"
val hashidsVersion = "1.0.3"
val pureConfigVersion = "0.17.4"
val logbackVersion = "1.2.3"
val pgEmbededVersion = "0.13.3"
val flywayVersion = "6.2.0"
val uTestVersion = "0.8.1"

ThisBuild / scalaVersion := "3.3.1"
ThisBuild / testFrameworks += new TestFramework("utest.runner.Framework")

lazy val `typed-json` = (project in file("typed-json"))
  .settings(
    organization := "com.joecordingley",
    name := "typed-json",
    version := "0.0.1-SNAPSHOT",
    libraryDependencies ++= List(
      "org.http4s" %% "http4s-core" % "1.0.0-M38",
      "com.lihaoyi" %% "utest" % "0.8.1" % Test,
      "org.typelevel" %% "cats-core" % "2.12.0",
      "io.circe" %% "circe-core" % "0.14.9",
      "io.circe" %% "circe-parser" % "0.14.9",
      "io.circe" %% "circe-literal" % "0.14.9",
      "org.scalacheck" %% "scalacheck" % "1.17.0" % Test
    )
  )

lazy val root = (project in file("."))
  .dependsOn(`typed-json`)
  .settings(
    version := "0.0.1",
    organization := "io.rw.app",
    name := "scala-http4s-realworld",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-blaze-server" % http4sVersion,
      "org.http4s" %% "http4s-blaze-client" % http4sVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion,
      "org.http4s" %% "http4s-dsl" % http4sVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-literal" % circeVersion,
      "org.tpolecat" %% "doobie-core" % doobieVersion,
      "org.tpolecat" %% "doobie-postgres" % doobieVersion,
      "org.tpolecat" %% "doobie-hikari" % doobieVersion,
      "io.github.jmcardon" %% "tsec-common" % tSecVersion,
      "io.github.jmcardon" %% "tsec-password" % tSecVersion,
      "io.github.jmcardon" %% "tsec-jwt-mac" % tSecVersion,
      "org.hashids" % "hashids" % hashidsVersion,
      "com.github.pureconfig" %% "pureconfig-core" % pureConfigVersion,
      "ch.qos.logback" % "logback-classic" % logbackVersion,
      "com.lihaoyi" %% "utest" % uTestVersion % Test,
      "com.opentable.components" % "otj-pg-embedded" % pgEmbededVersion % Test,
      "org.scalacheck" %% "scalacheck" % "1.17.0" % Test,
      "org.flywaydb" % "flyway-core" % flywayVersion % Test
    ),
    dockerBaseImage := "openjdk:11-jre-slim"
  )

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-language:higherKinds",
  "-language:postfixOps",
  "-feature",
  "-source:future",
  "-Xfatal-warnings"
)
