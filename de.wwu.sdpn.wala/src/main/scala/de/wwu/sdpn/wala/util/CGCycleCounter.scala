package de.wwu.sdpn.wala.util

import com.ibm.wala.ssa.SSAAbstractInvokeInstruction
import com.ibm.wala.util.graph.Graph
import scala.collection.mutable.Queue
import com.ibm.wala.ipa.callgraph.CGNode
import scala.collection.mutable.HashSet
import com.ibm.wala.ipa.callgraph.CallGraph
import scala.collection.JavaConversions._

/**
 * Helper class to identify unique instance keys
 * @author Benedikt Nordhoff
 */
class CGCycleCounter(val cg: CallGraph) {
  private var inftyNodes: Set[CGNode] = null
  private var ucNodes: Set[CGNode] = null

  def getInftyNodes() = {
    if (inftyNodes != null)
      inftyNodes
    else {
      val infnodes = calcInftyNodes
      inftyNodes = Set()
      inftyNodes ++= infnodes
      inftyNodes
    }
  }
  def getUCNodes() = {
    if (ucNodes != null)
      ucNodes
    else {
      getInftyNodes()
      ucNodes = Set()
      for (x <- cg if (!inftyNodes.contains(x))) {
        ucNodes += x
      }
      ucNodes
    }
  }

  private def calcInftyNodes() = {
    //TODO Check if two call sites call the same successor
    import CGCycleCounter.findInftyNodes
    //find cycles in call graph
    val cginfty = findInftyNodes(cg, cg.getFakeRootNode)
    //find cycles within controll flow graphs of non infty nodes
    for (node <- cg) {
      if (!cginfty.contains(node)) {
        if (node.getIR != null) {
          val cfg = node.getIR.getControlFlowGraph
          val start = cfg.entry
          //val bbinfty = findInftyNodes(cfg, start)
          val bbinfity = new GraphCycleFinder(cfg)
          bbinfity.solve(null)
          for (bb <- cfg if bbinfity.inCycle(bb); instr <- bb) {
            instr match {
              case call: SSAAbstractInvokeInstruction =>
                cginfty ++= cg.getPossibleTargets(node, call.getCallSite)
              case _ =>
            }
          }
        }
      }
    }

    val cgtoVisit = Queue[CGNode]()
    cgtoVisit ++= cginfty
    while (!cgtoVisit.isEmpty) {
      val node = cgtoVisit.dequeue
      for (cnode <- cg.getSuccNodes(node)) {
        if (!cginfty.contains(cnode)) {
          cginfty += cnode
          cgtoVisit += cnode
        }
      }
    }
    cginfty
  }

}

/**
 * Helper to find nodes to which there exists more than one path from start
 * @author Benedikt Nordhoff
 */
object CGCycleCounter {
  def findInftyNodes(cg: CallGraph, start: CGNode) = {
    val visited = HashSet[CGNode](start)
    val infty = HashSet[CGNode]()
    val childrenToVisit = Queue[CGNode](start)

    //Find cycles in Graph
    while (!childrenToVisit.isEmpty) {
      addChildren(childrenToVisit.dequeue)
    }
    //visit all infty nodes again adding 
    //everything to infty what's reachable from there 
    childrenToVisit ++= infty
    while (!childrenToVisit.isEmpty) {
      addChildren(childrenToVisit.dequeue)
    }

    def addChildren(node: CGNode) {
      val it: Iterator[CGNode] = cg.getSuccNodes(node)
      while (it.hasNext) {
        val cnode = it.next
        if (visited.contains(cnode)) {
          infty += cnode
        } else if (cg.getPossibleSites(node, cnode).size > 1) {
          visited += cnode
          infty += cnode
          childrenToVisit += cnode
        } else {
          childrenToVisit += cnode
          visited += cnode
        }
      }
    }

    infty //return infty set    
  }
}