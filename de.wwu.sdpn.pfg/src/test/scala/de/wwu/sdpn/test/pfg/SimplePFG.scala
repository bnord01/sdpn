package de.wwu.sdpn.test.pfg
import de.wwu.sdpn.pfg._

trait SPFG extends ParFlowGraph[SProc,SN,SimpleAction,Unit,SimpleEdge] {
	def edges : Set[SimpleEdge]
	def retNodes : Map[SProc,Map[Unit,SN]]
	def nodes: Set[SN]
	def procs: Set[SProc]
	def procOf : SN => SProc
	def entryNode : Map[SProc,SN]
	def mainProc: SProc
}

sealed trait SimpleEdge extends Edge[SProc,SN,SimpleAction,Unit] 

case class SBase(src:SN,ba:SimpleAction,snk:SN) extends SimpleEdge with BaseEdge[SProc,SN,SimpleAction,Unit] 
 

case class SCall(src:SN,proc:SProc,snk:SN) extends SimpleEdge with CallEdge[SProc,SN,SimpleAction,Unit]  {    
    def returns = Map(() -> snk)
    def ba = Skip
}

case class SSpawn(src:SN,proc:SProc,snk:SN) extends SimpleEdge with SpawnEdge[SProc,SN,SimpleAction,Unit] {
    def ba = Skip
}

case class SProc(name:String,start:SN,ret:SN)
case class SN(name:String)
sealed trait SimpleAction
case object Skip extends SimpleAction
case class Write(field:String) extends SimpleAction