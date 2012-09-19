package de.wwu.sdpn.wala.util

import scala.collection.JavaConversions.asScalaIterator
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ipa.callgraph.CallGraph

/**
 * Trait to be mixed in with a PreAnalysis.
 * Redefines isInteresting so that every node 
 * from which a node in initialSet can be reached is interesting.
 * 
 * @author Benedikt Nordhoff
 */
trait BackwardSliceFilter {
    this: PreAnalysis =>
    def initialSet: Set[CGNode]

    lazy val accessibleNodes = calcAccessibleNodes

    def calcAccessibleNodes: Set[CGNode] = {
        val taNodes = scala.collection.mutable.Set[CGNode]()
        //TODO convert to worklist algorithm or make tail recursive
        def addNode(node: CGNode) {
            if (!taNodes(node)) {
                taNodes += node
                cg.getPredNodes(node).foreach(addNode(_))
            }
        }
        initialSet.foreach(addNode(_))
        return taNodes.toSet
    }

    override def isInteresting(node: CGNode) = accessibleNodes(node)

}

object BackwardSliceFilter {
    def backwardSlice(cg0:CallGraph, init:Set[CGNode]): Set[CGNode] = {
        val bsf = new EmptyPA with BackwardSliceFilter {
            val initialSet = init
            override val cg = cg0            
        }
        bsf.accessibleNodes
    }
}