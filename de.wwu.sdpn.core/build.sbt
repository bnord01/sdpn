name := "sDPN core"

version := "1.0"

organization := "bnord"

scalaVersion := "2.9.1"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.9.1"

//addCompilerPlugin("org.scala-tools.sxr" %% "sxr" % "0.2.7")

//scalacOptions <+= scalaSource in Compile map { "-P:sxr:base-directory:" + _.getAbsolutePath }

//externalPom()

libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test"

