scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

scalacOptions += "-feature"

javacOptions ++= Seq("-parameters", "-Xlint:unchecked")
