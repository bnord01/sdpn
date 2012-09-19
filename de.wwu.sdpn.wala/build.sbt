name := "sdpn.wala"

version := "1.0-SNAPSHOT"

organization := "de.wwu"

scalaVersion := "2.9.2"

parallelExecution in Test := false

fork in (Test,run) := true

javaOptions in (Test,run) += "-Xmx2G"

javaOptions in (Test,run) += "-XX:MaxPermSize=521M"

baseDirectory in test := file("de.wwu.sdpn.wala")

baseDirectory in (Test,run) := file("de.wwu.sdpn.wala")

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

resolvers += "repo.codahale.com" at "http://repo.codahale.com"

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

//libraryDependencies += "org.eclipse.core" % "runtime" % "[3.5.0,)"

libraryDependencies += "de.wwu" %% "sdpn.core" % "1.0-SNAPSHOT"

libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test"

libraryDependencies += "com.codahale" % "logula_2.9.1" % "2.1.3"

