name := "sdpn.eclipse"

version := "1.0"

organization := "de.wwu"

scalaVersion := "2.9.2"

parallelExecution in Test := false

fork in (Test,run) := true

javaOptions in (Test,run) += "-Xmx2G"

javaOptions in (Test,run) += "-XX:MaxPermSize=521M"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

libraryDependencies += "org.eclipse.core" % "runtime" % "[3.5.0,)"

libraryDependencies += "org.eclipse" % "ui" % "[3.5.0,)"

libraryDependencies += "org.eclipse.ui" % "editors" % "[3.5.0,)"

libraryDependencies += "org.eclipse.debug" % "ui" % "[3.5.0,)"

libraryDependencies += "org.eclipse.jdt" % "launching" % "[3.5.0,)"

libraryDependencies += "org.eclipse.debug" % "core" % "[3.5.0,)"

libraryDependencies += "org.eclipse.jdt" % "core" % "[3.5.0,)"

libraryDependencies += "org.eclipse.jdt" % "ui" % "[3.5.0,)"

libraryDependencies += "org.eclipse.jdt.debug" % "ui" % "[3.5.0,)"

libraryDependencies += "org.eclipse" % "text" % "[3.5.0,)"

libraryDependencies += "org.eclipse" % "help" % "[3.5.0,)"

libraryDependencies += "org.eclipse" % "swt" % "[3.5.0,)"

libraryDependencies += "org.eclipse" % "draw2d" % "[3.5.0,)"

libraryDependencies += "org.eclipse.zest" % "core" % "[2.0.0,)"

libraryDependencies += "org.eclipse.zest" % "layouts" % "[2.0.0,)"

libraryDependencies += "com.novocode" % "junit-interface" % "0.8" % "test"