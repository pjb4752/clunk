import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.2",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "clunk",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "mysql" % "mysql-connector-java" % "6.0.6"
  )
