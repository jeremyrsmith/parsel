name := "parsel"

ThisBuild / organization := "org.polynote"
ThisBuild / version := "0.1.0-SNAPSHOT"
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
  )
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
val `parsel` = project.in(file(".")).aggregate(`parsel-ast`, `parsel-parser`, `parsel-quotes`)

