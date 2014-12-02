name := "scalafirst"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies += "junit" % "junit" % "4.11" % "test"

val scalazVersion = "7.2.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.scalaz" % "scalaz-core_2.10" % scalazVersion,
  "org.scalaz" % "scalaz-effect_2.10" % scalazVersion,
  //"org.scalaz" % "scalaz-typelevel_2.10" % scalazVersion,
  "org.scalaz" % "scalaz-scalacheck-binding_2.10" % scalazVersion % "test"
)

scalacOptions += "-feature"

initialCommands in console := "import scalaz._, Scalaz._"

initialCommands in console in Test := "import scalaz._, Scalaz._, scalacheck.ScalazProperties._, scalacheck.ScalazArbitrary._,scalacheck.ScalaCheckBinding._"
