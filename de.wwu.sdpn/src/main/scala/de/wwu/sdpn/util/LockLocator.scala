package de.wwu.sdpn.util

import scala.collection.JavaConversions.asScalaIterator
import scala.collection.JavaConversions.iterableAsScalaIterable
import scala.collection.Set
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ssa.SSAMonitorInstruction
import com.ibm.wala.util.graph.traverse.BFSIterator
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.callgraph.CGNode

/**
 * Trait to be mixed into a PreAnalysis
 * defines the set locks as the set of instance keys on which a lock operation is contained in the call graph.
 * @author Benedikt Nordhoff
 */
trait LockLocator {
	def cg:CallGraph
	def pa:PointerAnalysis
	def entryNode: CGNode
	
	lazy val locks = locateLocks
	
	private def locateLocks = {
		var l = Set[InstanceKey]()
		for (node <- new BFSIterator(cg, entryNode) if node.getIR != null){
			//Instance keys for synchronized methods
			if(node.getMethod().isSynchronized){
				if(node.getMethod().isStatic){
					val ref = node.getMethod.getDeclaringClass.getReference;
					l += pa.getHeapModel.getInstanceKeyForClassObject(ref);					
				}
				else {
				    val thisidx = node.getIR.getSymbolTable.getParameter(0)
					val pk = pa.getHeapModel.getPointerKeyForLocal (node, thisidx)
					l ++= pa.getPointsToSet(pk)
				}
				
			}
				
			//Instance keys for monitor instructions
			for(statement <- node.getIR.iterateNormalInstructions){
				statement match {
					case st: SSAMonitorInstruction =>
					val pk = pa.getHeapModel.getPointerKeyForLocal (node, st.getRef)
					l ++= pa.getPointsToSet(pk)					
					case _ =>
				}
			}
		}
		l	
	}
	
}

object LockLocator {
  def instances(cg1:CallGraph, pa1:PointerAnalysis) : Set[InstanceKey] = {
    val ll = new AnyRef() with LockLocator {def cg = cg1; def pa = pa1; def entryNode = cg1.getFakeRootNode()}
    return ll.locks
  }
}