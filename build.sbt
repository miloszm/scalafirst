name := "scalafirst"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies += "junit" % "junit" % "4.11" % "test"

val scalazVersion = "7.1.0"

resolvers += "Typesafe releases" at "http://repo.ttypesafe.com/typesafe/releases"

libraryDependencies ++= Seq(
  "org.scalaz" % "scalaz-core_2.10" % scalazVersion withSources(),
  "org.scalaz" % "scalaz-effect_2.10" % scalazVersion withSources(),
  //"org.scalaz" % "scalaz-typelevel_2.10" % scalazVersion,
  "org.scalaz" % "scalaz-scalacheck-binding_2.10" % scalazVersion % "test",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test",
  "com.typesafe.akka" %% "akka-actor" % "2.3.15"
)

scalacOptions += "-feature"

initialCommands in console := "import scalaz._, Scalaz._"

initialCommands in console in Test := "import scalaz._, Scalaz._, scalacheck.ScalazProperties._, scalacheck.ScalazArbitrary._,scalacheck.ScalaCheckBinding._"

lazy val hello = taskKey[Unit]("An example task MM")

lazy val root = (project in file(".")).
  settings(
    hello := { println("Hello example task MM!")}
  )

//
//
// All keys consist of both a name and a scope (where the scope has 3 axes: Projects, Configurations, Tasks
//
// cheat sheet (commands to be entered in sbt interactive session):
//
//  reload
//  fullClasspath
//  inspect fullClasspath
//  inspect test:fullClasspath
//  sourceDirectories
//  baseDirectory
//  !:
//  inspect compile
//  unmanagedBase
//  unmanagedJars
//  unmanagedJars in Compile := Seq.empty[sbt.Attributed[java.io.File]]
//  libraryDependencies
//  resolvers  // only added non-default resolvers
//  externalResolvers   // all resolvers including default resolvers
//
// Aggregation means that running a task on the aggregate project will also run it on the
// aggregated projects.