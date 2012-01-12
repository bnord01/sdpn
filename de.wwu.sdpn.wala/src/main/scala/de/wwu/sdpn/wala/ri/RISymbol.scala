package de.wwu.sdpn.wala.ri

import de.wwu.sdpn.core.ta.xsb.{HasTermRepresentation => HTR}

//TODO The view bound makes this very ugly to use. Change this! 
sealed abstract class RISymbol [I<%HTR,S<%HTR] extends HTR {
	def oldSymbol : S
	def instance: Option[I]
}
case class Isolated[I<%HTR,S<%HTR](i:I,s:S) extends RISymbol[I,S] {
    def oldSymbol=s
    def instance = Some(i)
    def toTerm = "ri(" + i.toTerm + ", " + s.toTerm  + ")" 
}
case class NotIsolated[I<%HTR,S<%HTR](s:S) extends RISymbol[I,S] {
    def oldSymbol=s
    def instance = None
    def toTerm = "nri(" + s.toTerm  + ")"
}
case class Summary[I<%HTR,S<%HTR](s:S) extends RISymbol[I,S] {
    def oldSymbol=s
    def instance = None
    def toTerm = "rs(" + s.toTerm  + ")"
}