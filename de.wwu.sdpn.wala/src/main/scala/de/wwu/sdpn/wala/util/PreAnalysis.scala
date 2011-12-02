package de.wwu.sdpn.wala.util

import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.callgraph.AnalysisCache
import com.ibm.wala.ipa.callgraph.AnalysisOptions
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ipa.cha.IClassHierarchy

/**
 * This trait contains everything needed to generate a MonitorDPN 
 * @author Benedikt Nordhoff
 */
trait PreAnalysis {
    /**
     * The class hierarchy to analyze
     */
    def cha: IClassHierarchy
    /**
     * The call graph representing the program
     */
    def cg: CallGraph
    /**
     * the corresponding pointer analysis
     */
    def pa: PointerAnalysis

    /**
     * Does the node represent something like java.lang.Thread.start() ?
     * @return true iff any call of node should be interpreted as a new spawned thread
     */
    def isThreadStart(cgnode: CGNode): Boolean

    /**
     * The entry node for the DPN generation
     */
    def entryNode : CGNode

    /**
     * should this node be considered for the DPN generation
     */
    def isInteresting(n: CGNode):Boolean

    /**
     * Is it safe to use ik as a lock in node?
     * @param ik the instance key
     * @param node the node which contains a monitor instruction on ik or is synchronized on ik
     * @return true iff the monitor on ik in node should be contained in the generated DPN
     */
    def safeLock(ik: InstanceKey, node: CGNode):Boolean

}
