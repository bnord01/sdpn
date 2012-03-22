name := "sdpn"

version := "1.0"

organization := "de.wwu"

scalaVersion := "2.9.1"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test"
