name := "scalax15-interpreters"

version := "1.0"

lazy val commonSettings = Seq(
  scalaVersion := "2.11.7",
  libraryDependencies += "org.spire-math" %% "cats" % "0.3.0",
  scalacOptions ++= Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-unchecked",
    "-feature",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-Ywarn-dead-code",
    "-Xlint",
    "-Xfatal-warnings"
  )
)

lazy val untyped = project.settings(commonSettings: _*)
lazy val gadt = project.settings(commonSettings: _*)
lazy val free = project.settings(commonSettings: _*)
