name := "scalax15-interpreters"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.5"

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
