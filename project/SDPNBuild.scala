import sbt._
import Keys._

object SDPNBuild extends Build {
    lazy val root = Project(id = "root",
                            base = file(".")) aggregate(sdpnCore, sdpnWala)

    lazy val sdpnCore = Project(id = "sdpn-core",
                           base = file("de.wwu.sdpn.core"))
    lazy val sdpnWala = Project(id = "sdpn-wala",
                           base = file("de.wwu.sdpn.wala")) dependsOn(sdpnCore)
}
