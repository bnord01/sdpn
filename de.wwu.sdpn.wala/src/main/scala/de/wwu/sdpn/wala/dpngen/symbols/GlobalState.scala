package de.wwu.sdpn.wala.dpngen.symbols
import de.wwu.sdpn.core.ta.xsb.HasTermRepresentation

sealed trait GlobalState extends HasTermRepresentation {
	def num:Int
	def toTerm = num.toString()
}

case object NState extends GlobalState{val num = 0}
case object EState extends GlobalState{val num = 1}