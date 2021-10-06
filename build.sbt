name := "parsel"

ThisBuild / organization := "org.polynote"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.11.12"
ThisBuild / crossScalaVersions := Seq("2.11.12", "2.12.15", "2.13.6")

val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.2.9" % "test"
  ),
  scalacOptions ++= Seq(
    "-language:experimental.macros"
  )
)

val `parsel-ast` = project.settings(commonSettings)
val `parsel-parser` = project.settings(commonSettings).dependsOn(`parsel-ast`)
val `parsel-quotes` = project
  .settings(
    commonSettings ++ Seq(
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
      )
    )
  )
  .dependsOn(`parsel-parser`)
val `parsel` = project.in(file(".")).aggregate(`parsel-ast`, `parsel-parser`)

