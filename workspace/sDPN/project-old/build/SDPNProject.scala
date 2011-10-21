import sbt._
import de.element34.sbteclipsify._

class SDPNProject(info: ProjectInfo) extends DefaultProject(info) with Eclipsify{

val mavenLocal = "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"

  // val scalaToolsReppo = "Scala-Tools Maven2 Repository" at 
  // "http://scala-tools.org/repo-releases"
  
  val junit = "junit" % "junit" % "4.4" % "test"

  val jline = "jline" % "jline" %"1.0"

  val walaTest = "com.ibm.wala" % "core.tests" % "1.3.2-SNAPSHOT"

  val bddbddb = "net.sf"%"bddbddb"%"1.0"
  
  val eclipseRuntime = "org.eclipse.core"%"runtime"%"3.5.0"

  val scalaSwing = "org.scala-lang"%"scala-swing"%"2.9.0"
/*
		<!-- <dependency>
			<groupId>org.scala-tools.testing</groupId>
			<artifactId>specs_${scala.version}</artifactId>
			<version>1.6.6</version>
			<scope>test</scope>
		</dependency> -->
		<!--
			<dependency> <groupId>edu.kit</groupId>
			<artifactId>jsdg-full</artifactId> <version>1.0</version>
			</dependency>
		-->
*/
		/*
                 * <dependency>
    		<groupId>net.sf.jopt-simple</groupId>
    		<artifactId>jopt-simple</artifactId>
    		<version>3.2</version>
  		</dependency>
		*/
	
  


  
  
}
