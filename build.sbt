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

lazy val root = (project in file("."))
  .settings(
    organization := "io.rw.app",
    name := "scala-http4s-realworld",
    version := "0.0.1",
    scalaVersion := "3.3.1",
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
      "org.flywaydb" % "flyway-core" % flywayVersion % Test
    ),
    testFrameworks += new TestFramework("utest.runner.Framework"),
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
