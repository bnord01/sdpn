package de.wwu.sdpn.wala.util

import scala.collection.JavaConversions.asScalaIterator
import scala.collection.JavaConversions.iterableAsScalaIterable
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ssa.SSAMonitorInstruction
import com.ibm.wala.util.graph.traverse.BFSIterator
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ipa.callgraph.propagation.ConstantKey

/**
 * Trait to be mixed into a PreAnalysis
 * defines the set locks as the set of instance keys on which a lock operation is contained in the call graph.
 * @author Benedikt Nordhoff
 */
trait LockWithOriginLocator {
	def cg:CallGraph
	def pa:PointerAnalysis[InstanceKey]
	def entryNode: CGNode
	
	lazy val locks:Map[InstanceKey,Set[CGNode]] = locateLocks
	
	private def locateLocks = {
		var l:Map[InstanceKey,Set[CGNode]] = Map().withDefaultValue(Set())
		for (node <- new BFSIterator(cg, entryNode) if node.getIR != null){		  
			//Instance keys for synchronized methods
			if(node.getMethod().isSynchronized){
				if(node.getMethod().isStatic){
					val ref = node.getMethod.getDeclaringClass.getReference;
					val ik = pa.getHeapModel.getInstanceKeyForClassObject(ref)
					val ls = l(ik)
					l += ik -> (ls + node);					
				}
				else {
				    val thisidx = node.getIR.getSymbolTable.getParameter(0)
					val pk = pa.getHeapModel.getPointerKeyForLocal (node, thisidx)
					for (ik <- pa.getPointsToSet(pk)) {
					  l += ik -> (l(ik) + node);
					}					 
				}				
			}
				
			//Instance keys for monitor instructions
			for(statement <- node.getIR.iterateNormalInstructions){
				statement match {
					case st: SSAMonitorInstruction =>
					val pk = pa.getHeapModel.getPointerKeyForLocal (node, st.getRef)
					for (ik <- pa.getPointsToSet(pk)) {
					  l += ik -> (l(ik) + node);
					}						
					case _ =>
				}
			}
		}
		l	
	}
	
}

object LockWithOriginLocator {
  def instances(cg1:CallGraph, pa1:PointerAnalysis[InstanceKey]) : Map[InstanceKey,Set[CGNode]] = {
    val ll = new LockWithOriginLocator {def cg = cg1; def pa = pa1; def entryNode = cg1.getFakeRootNode()}
    return ll.locks
  }
  
  def uniqueLocks(cg1:CallGraph, pa1:PointerAnalysis[InstanceKey]) : Set[InstanceKey] = {
      return uniqueLocksWithOrigin(cg1,pa1).keySet
  }
  def uniqueLocksWithOrigin(cg1:CallGraph, pa1:PointerAnalysis[InstanceKey]) : Map[InstanceKey,Set[CGNode]] = {
      val ui = UniqueInstanceLocator.instances(cg1, pa1)
      val loi = instances(cg1, pa1)
      return loi.filterKeys(x => ui(x)||x.isInstanceOf[ConstantKey[_]])
  }
}