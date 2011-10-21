package sdpn.mnfs

import com.ibm.wala.util.graph.Graph
import scala.collection.Set

/**
 * A generalized M nondeterministic finite state machine
 * @tparam P control state
 * @tparam G stack symbols
 * @tparam S states from which there are only P edges 
 * @tparam T states from which there
 *  
 * @deprecated
 * @author Benedikt Nordhoff
 */
class MNFS[P,G,S,T] (pset: Set[P],gset: Set[G]) {
	private var sptSet = Set[(S,P,G,T)]()
	private var spsSet = Set[(S,P,S)]()
	private var tsSet = Set[(T,S)]()
	private var ttSet = Set[(T,G,T)]()
	private var sSet = Set[S]()
	private var tSet = Set[T]()
	private var finalSPSet = Set[(S,P)]()
	private var finalTSet = Set[T]()
	private var start : Option[S] = None 
	
	private var startConf : Option[(P,G)] = None
	
	def getPSet = pset
	def getGSet = gset	
	def getTSet = tSet
	def getSSet = sSet
	def getSPTSet = sptSet
	def getSPSSet = spsSet
	def getTSSet = tsSet
	def getTTSet = ttSet
	def getFinalSPSet = finalSPSet
	def getFinalTSet = finalTSet
	def getStart = start	
	def getStartConf = startConf
	
	
	
	def addSPTEdge(s:S,p:P,g:G,t:T) {
		assert(pset(p))
		assert(gset(g))
		
		sptSet += ((s,p,g,t))
		sSet += s
		tSet += t
	}
	
	def addSPSEdge(s1:S,p:P,s2:S) {
		assert(pset(p))
		
		spsSet += ((s1,p,s2))
		sSet ++= Set(s1,s2)
	}
	
	def addTSEdge(t:T,s:S) {
		tsSet += ((t,s))
		tSet += t
		sSet += s
	}
	
	def addTTEdge(t1:T,g:G,t2:T) {
		assert(gset(g))
		
		ttSet += ((t1,g,t2))
		tSet ++= Set(t1,t2)
	}
	
	def setFinalSP(s:S,p:P){
		finalSPSet += ((s,p))
	}
	def setFinalT(t:T){
		finalTSet += t
	}
	
	def setStart(s:S){
		start = Some(s)
	}
	
	def setStartConf(p:P,g:G){
		startConf = Some((p,g))
	}
	
	
	
}

