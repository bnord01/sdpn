name := "sDPN"

version := "1.0"

organization := "bnord"

scalaVersion := "2.9.1"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test"
