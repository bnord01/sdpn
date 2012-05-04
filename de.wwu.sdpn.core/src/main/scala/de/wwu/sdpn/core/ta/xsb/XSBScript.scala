package de.wwu.sdpn.core.ta.xsb

trait XSBScript {
	def genScript:String
	def boundNames:Set[String]
}