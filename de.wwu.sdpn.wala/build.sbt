name := "sDPN wala"

version := "1.0"

organization := "bnord"

scalaVersion := "2.9.1"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

//libraryDependencies += "org.scala-lang" % "scala-swing" % "2.9.1"

libraryDependencies += "org.eclipse.core" % "runtime" % "[3.5.0,)"