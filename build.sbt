import AssemblyKeys._ // put this at the top of the file

assemblySettings

jarName in assembly := "codegen.jar"

test in assembly := {}

assemblyOption in assembly ~= { _.copy(includeScala = false) }

organization := "com.adal"

name := "codegen"

version := "1.0"

scalaVersion := "2.11.2"

javaHome := Some(file("C:/Java/jdk1.7.0_45"))

fork := true

javacOptions ++= Seq("-source", "1.7")

// set the main Scala source directory to be <base>/src
scalaSource in Compile <<= baseDirectory(_ / "src")

// set the Scala test source directory to be <base>/test
scalaSource in Test <<= baseDirectory(_ / "test")

// add a test dependency on ScalaCheck
libraryDependencies ++= Seq(
	"org.specs2" %% "specs2" % "2.3.11" % "test",
	"org.scala-lang" % "scala-compiler" % "2.11.2" % "provided"
)

libraryDependencies += "junit" % "junit" % "4.11" % "test"
