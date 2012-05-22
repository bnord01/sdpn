import sbt._
import Keys._

object SDPNBuild extends Build {
    lazy val root = Project(id = "root",
                            base = file(".")) aggregate(sdpnCore, sdpnWala, sdpnPFG, sdpnTestapps)

    lazy val sdpnCore = Project(id = "core",
                           base = file("de.wwu.sdpn.core"))
    lazy val sdpnWala = Project(id = "wala",
                           base = file("de.wwu.sdpn.wala")) dependsOn(sdpnCore)
    lazy val sdpnPFG = Project(id = "pfg",
                           base = file("de.wwu.sdpn.pfg")) dependsOn(sdpnCore,sdpnWala)
//    lazy val sdpnEclipse = Project(id = "eclipse",
//                           base = file("de.wwu.sdpn.eclipse")) dependsOn(sdpnCore,sdpnWala)
    lazy val sdpnTestapps = Project(id = "testapps",
                           base = file("de.wwu.sdpn.testapps")) 
}
