name := "sdpn.testapps"

version := "1.0"

organization := "de.wwu"

//scalaVersion := "2.9.1"

parallelExecution in Test := false

crossPaths := false

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))
