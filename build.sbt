name := "sdpn"

version := "1.0"

organization := "de.wwu"

scalaVersion := "2.9.1"

fork in (Test,run) := true

javaOptions in (Test,run) += "-Xmx2G"

// sets the working directory for `test:run` and `test:run-main` only
// doesn't work as expected
//baseDirectory in (Test,run) <<= baseDirectory( _ /"de.wwu.sdpn.core")


resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test"

