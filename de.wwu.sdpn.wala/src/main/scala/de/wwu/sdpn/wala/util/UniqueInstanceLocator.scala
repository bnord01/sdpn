package de.wwu.sdpn.wala.util

import scala.collection.JavaConversions.iterableAsScalaIterable
import com.ibm.wala.ipa.callgraph.propagation.AllocationSiteInNode
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ssa.SSANewInstruction
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ipa.callgraph.propagation.ConstantKey

/**
 * Trait to be mixed into a PreAnalysis.
 * Defines uniqueInstances as the set of instances 
 * which are known to to contain only one concrete object at runtime.
 * Based on [[de.wwu.sdpn.util.CGCycleCounter]].
 * @author Benedikt Nordhoff
 */
trait UniqueInstanceLocator {
    def cg:CallGraph; 
    def pa:PointerAnalysis
    
    lazy val uniqueInstances = calcUniqueInstances
    	
    private def calcUniqueInstances: Set[InstanceKey] = {
        var ui = Set[InstanceKey]()
        val ucnodes = new CGCycleCounter(cg).getUCNodes
        for (node <- ucnodes if node.getIR != null) {
            val cfg = node.getIR.getControlFlowGraph
            val start = cfg.entry
            val infBB = new GraphCycleFinder(cfg) // CGCycleCounter.findInftyNodes(cfg, start)
            infBB.solve(null)
            for (bb <- cfg if (!infBB.inCycle(bb))) {
                for (instr <- bb) {
                    instr match {
                        case ssanew: SSANewInstruction =>
                            val ik = pa.getHeapModel().getInstanceKeyForAllocation(node, ssanew.getNewSite)
                            //TODO Check whether any other InstanceKey type can be allowed 
                            // We only allow AllocationSiteInNode Keys  
                            //TODO Check whether AllocationSites share Instance Keys
                            if (ik != null && ik.isInstanceOf[AllocationSiteInNode])
                                ui += ik
                        case _ =>
                    }
                }
            }
        }
        ui
    }

}

object UniqueInstanceLocator {
  def instances(cg1: CallGraph, pa1: PointerAnalysis):Set[InstanceKey] = {
    val uic = new UniqueInstanceLocator {def cg = cg1;def pa=pa1};
    return uic.uniqueInstances
    
  }
}