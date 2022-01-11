name := "parsel"

ThisBuild / organization := "io.github.jeremyrsmith"
ThisBuild / version := "0.1.0"
ThisBuild / scalaVersion := "2.11.12"
ThisBuild / crossScalaVersions := Seq("2.11.12", "2.12.15", "2.13.6")

val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.2" % "test",
    "org.scalatestplus" %% "scalacheck-1-14" % "3.2.2.0" % "test",
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.5" % "test"
  ),
  scalacOptions ++= Seq(
    "-language:experimental.macros"
  ),
  publishMavenStyle := true,
  homepage := Some(url("https://jeremyrsmith.github.io/parsel")),
  licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/jeremyrsmith/parsel"),
      "scm:git@github.com:jeremyrsmith/parsel.git"
    )
  ),
  developers := List(
    Developer(id = "jeremyrsmith", name = "Jeremy Smith", email = "", url = url("https://github.com/jeremyrsmith"))),
  publishTo := sonatypePublishToBundle.value
)

val `parsel-ast` = project.settings(commonSettings)
val `parsel-parser` = project.settings(commonSettings).dependsOn(`parsel-ast` % "compile->compile;test->test")
val `parsel-quotes` = project
  .settings(
    commonSettings ++ Seq(
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
      )
    )
  )
  .dependsOn(`parsel-parser`)
val `parsel` = project.in(file("."))
  .settings(commonSettings)
  .settings(publishArtifact := false)
  .aggregate(`parsel-ast`, `parsel-parser`, `parsel-quotes`)

