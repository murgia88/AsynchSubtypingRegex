val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "AsynchronousSubtyping",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq("org.scalameta" %% "munit" % "0.7.29" % Test,
     "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0"),
     
     libraryDependencies += "com.lihaoyi" %% "mainargs" % "0.7.6",
     
     scalacOptions += "-deprecation"
  )
