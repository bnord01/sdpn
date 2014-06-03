name := "sdpn.wala"

version := "1.0-SNAPSHOT"

organization := "de.wwu"

scalaVersion := "2.10.4"

parallelExecution in Test := false

fork in (Test,run) := true

javaOptions in (Test,run) += "-Xmx2G"

javaOptions in (Test,run) += "-XX:MaxPermSize=521M"

baseDirectory in test := file("de.wwu.sdpn.wala")

baseDirectory in (Test,run) := file("de.wwu.sdpn.wala")

unmanagedBase := baseDirectory.value / "custom_lib"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

resolvers += "Local Ivy Repository" at "file://"+Path.userHome.absolutePath+"/.ivy/local"

//resolvers += "repo.codahale.com" at "http://repo.codahale.com"

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

//libraryDependencies += "org.eclipse.core" % "runtime" % "[3.5.0,)"

libraryDependencies += "de.wwu" %% "sdpn-core" % "1.0-SNAPSHOT"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10" % "test"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.2" % "test"

libraryDependencies += "com.ibm.wala" % "com.ibm.wala.core" % "1.3.4-SNAPSHOT"

libraryDependencies += "com.ibm.wala" % "com.ibm.wala.shrike" % "1.3.4-SNAPSHOT"

libraryDependencies += "com.ibm.wala" % "com.ibm.wala.util" % "1.3.4-SNAPSHOT"



