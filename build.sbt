enablePlugins(JavaAppPackaging)

val http4sVersion = "0.21.1"
val circeVersion = "0.13.0"
val doobieVersion = "0.8.8"
val tSecVersion = "0.2.0"
val hashidsVersion = "1.0.3"
val pureConfigVersion = "0.12.3"
val logbackVersion = "1.2.3"
val uTestVertion = "0.7.4"
val pgEmbededVersion = "0.13.3"
val flywayVersion = "6.2.0"

lazy val root = (project in file("."))
  .settings(
    organization := "io.rw.app",
    name := "scala-http4s-realworld",
    version := "0.0.1",
    scalaVersion := "2.13.1",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-blaze-server" % http4sVersion,
      "org.http4s" %% "http4s-blaze-client" % http4sVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion,
      "org.http4s" %% "http4s-dsl" % http4sVersion,

      "io.circe" %% "circe-generic" % circeVersion,

      "org.tpolecat" %% "doobie-core" % doobieVersion,
      "org.tpolecat" %% "doobie-postgres" % doobieVersion,
      "org.tpolecat" %% "doobie-hikari" % doobieVersion,

      "io.github.jmcardon" %% "tsec-common" % tSecVersion,
      "io.github.jmcardon" %% "tsec-password" % tSecVersion,
      "io.github.jmcardon" %% "tsec-jwt-mac" % tSecVersion,

      "org.hashids" % "hashids" % hashidsVersion,

      "com.github.pureconfig" %% "pureconfig" % pureConfigVersion,

      "ch.qos.logback" % "logback-classic" % logbackVersion,

      "com.lihaoyi" %% "utest" % uTestVertion % Test,
      "com.opentable.components" % "otj-pg-embedded" % pgEmbededVersion % Test,
      "org.flywaydb" % "flyway-core" % flywayVersion % Test
    ),
    testFrameworks += new TestFramework("utest.runner.Framework"),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),

    dockerBaseImage := "openjdk:11-jre-slim"
  )

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-language:higherKinds",
  "-language:postfixOps",
  "-feature",
  "-Xfatal-warnings"
)
