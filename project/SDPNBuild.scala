import sbt._
import Keys._

object SDPNBuild extends Build {
    lazy val root = Project(id = "root",
                            base = file(".")) aggregate(sdpnCore, sdpnWala)

    lazy val sdpnCore = Project(id = "core",
                           base = file("de.wwu.sdpn.core"))
    lazy val sdpnWala = Project(id = "wala",
                           base = file("de.wwu.sdpn.wala")) dependsOn(sdpnCore)
    lazy val sdpnPFG = Project(id = "pfg",
                           base = file("de.wwu.sdpn.pfg")) dependsOn(sdpnCore,sdpnWala)
}
