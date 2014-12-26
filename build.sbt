import _root_.sbt.Keys._
import AssemblyKeys._

name := "BioNER"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq("edu.arizona.sista" % "processors" % "3.2",
  //"edu.arizona.sista" % "processors" % "3.2" classifier "models",
  "org.scalatest" % "scalatest_2.10" % "2.1.0" % "test"
)

unmanagedClasspath in Runtime += baseDirectory.value / "resources"

javaOptions += "-Xmx512m"

assemblySettings

assemblyOption in assembly ~= { _.copy(prependShellScript = Some(defaultShellScript)) }

jarName in assembly := { s"pc-ner" }

//mainClass in assembly := Some("com.example.Main"

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) =>
{
  case x if x startsWith "java_cup/runtime/" => MergeStrategy.first
  case x if x startsWith "nu/xom/" => MergeStrategy.first
  case x if x startsWith "org/w3c/dom/" => MergeStrategy.first
  case x => old(x)
}
}