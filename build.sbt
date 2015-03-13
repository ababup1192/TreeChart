name := """TreeChart"""

version := "1.0"

scalaVersion := "2.11.5"

// Set the main Scala source directory to be <base>/src
scalaSource in Compile <<= baseDirectory(_ / "src/main/scala")

resourceDirectory in Compile <<= baseDirectory(_ / "src/main/resource")

// Append -deprecation to the options passed to the Scala compiler
scalacOptions += "-deprecation"

// Point to location of a snapshot repository for ScalaFX
resolvers += Opts.resolver.sonatypeSnapshots

libraryDependencies ++= Seq(
  "org.scalafx" %% "scalafx" % "8.0.31-R7",
  "org.scalatest" %% "scalatest" % "2.2.3" % "test" //http://www.scalatest.org/download
)

// Set the prompt (for this build) to include the project id.
shellPrompt := { state => System.getProperty("user.name") + ":" + Project.extract(state).currentRef.project + "> "}


// Fork a new JVM for 'run' and 'test:run'
fork := true
