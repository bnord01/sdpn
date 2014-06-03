name := "sdpn.core"

version := "1.0-SNAPSHOT"

organization := "de.wwu"

scalaVersion := "2.10.4"

fork in (Test,run) := true

parallelExecution in Test := false

javaOptions in (Test,run) += "-Xmx2G"

javaOptions in (Test,run) += "-XX:MaxPermSize=521M"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

unmanagedBase := baseDirectory.value / "custom_lib"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10.3"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.10.3"

libraryDependencies += "org.scala-lang" % "scala-actors" % "2.10.3"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test"

libraryDependencies += "com.declarativa" % "interprolog" % "2.1.2a4"

libraryDependencies += "com.typesafe" %% "scalalogging-slf4j" % "1.0.1" exclude("org.scala-lang","scala-reflect")

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.2" % "test"

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

//addCompilerPlugin("org.scala-tools.sxr" %% "sxr" % "0.2.7")

//scalacOptions <+= scalaSource in Compile map { "-P:sxr:base-directory:" + _.getAbsolutePath }

//net.virtualvoid.sbt.graph.Plugin.graphSettings

