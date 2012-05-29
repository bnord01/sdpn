name := "sdpn.core"

version := "1.0-SNAPSHOT"

organization := "de.wwu"

scalaVersion := "2.9.2"

fork in (Test,run) := true

parallelExecution in Test := false

javaOptions in (Test,run) += "-Xmx2G"

javaOptions in (Test,run) += "-XX:MaxPermSize=521M"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.9.2"

libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test"

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

//addCompilerPlugin("org.scala-tools.sxr" %% "sxr" % "0.2.7")

//scalacOptions <+= scalaSource in Compile map { "-P:sxr:base-directory:" + _.getAbsolutePath }



