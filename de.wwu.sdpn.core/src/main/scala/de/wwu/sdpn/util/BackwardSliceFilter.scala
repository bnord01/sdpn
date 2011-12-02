package de.wwu.sdpn.util

import scala.collection.JavaConversions.asScalaIterator
import scala.collection.Set

import com.ibm.wala.ipa.callgraph.CGNode

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

    private lazy val accessibleNodes = calcAccessibleNodes

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
        return taNodes

    }

    override def isInteresting(node: CGNode) = accessibleNodes(node)

}