package de.wwu.sdpn.util

import de.wwu.sdpn.util.proxies.dataflow.SetDataflowSolver
import com.ibm.wala.dataflow.graph.DataflowSolver
import com.ibm.wala.dataflow.graph.IKilldallFramework
import com.ibm.wala.util.graph.impl.InvertedGraph
import com.ibm.wala.fixpoint.UnaryOperator
import com.ibm.wala.fixpoint.AbstractVariable
import com.ibm.wala.dataflow.graph.AbstractMeetOperator
import com.ibm.wala.fixpoint.IVariable
import com.ibm.wala.dataflow.graph.ITransferFunctionProvider
import com.ibm.wala.types.MethodReference
import com.ibm.wala.ssa.SSAAbstractInvokeInstruction
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ssa.SSAMonitorInstruction
import com.ibm.wala.ipa.callgraph.CallGraph
import scala.collection.immutable.Map
import scala.collection.JavaConversions._
import scala.collection.Set
import de.wwu.sdpn.util.proxies.dataflow._

//TODO maybe change to BitVectors instead of Scala sets.


/**
 * Generates a wait map which maps any CGNode to the set of instance keys on which this 
 * or any called method might call wait().
 * 
 * This uses the KilldallFramework
 * @author Benedikt Nordhoff
 */
class WaitMap (analysis:PreAnalysis, locks:InstanceKey => Boolean){
	import analysis. {cg,pa,isThreadStart}
	
    //on which instance keys does a method call wait directly 
    private lazy val directWaitMap = {
        var wm = Map[CGNode, Set[InstanceKey]]().withDefaultValue(Set())
        for (node <- cg if node.getIR != null && !isWait(node.getMethod().getReference)) {
            for (statement <- node.getIR.iterateNormalInstructions) {
                statement match {
                    case st: SSAAbstractInvokeInstruction if isWait(st.getDeclaredTarget) =>
                        val pk = pa.getHeapModel.getPointerKeyForLocal(node, st.getReceiver)
                        wm += node -> (wm(node) ++ pa.getPointsToSet(pk).filter(locks))
                    case _ =>
                }
            }
        }
        wm
    }

    /**
     * Represents a method reference one of the java.lang.Object.wait methods?
     * @param mr a method reference
     * @return true iff mr represents java.lang.Object.wait()
     */
    def isWait(mr: MethodReference) = {
        val sig = mr.getSignature
        sig == "java.lang.Object.wait()V" ||
            sig == "java.lang.Object.wait(J)V" ||
            sig == "java.lang.Object.wait(JI)V"
    }

    /**
     * 
     * @param node any CGNode 
     * @return the set of InstanceKeys on which a wait() may be called   
     */
    def apply(node:CGNode) =  waitMap.getOrElse(node,Set())

    /**
     * This lazy val holds the result of the flow analysis as an immutable map.
     * The map contains only keys for which there exists an non empty wait set.
     * 
     * apply(node:CGNode) returns an empty Set for keys not contained.   
     */
    lazy val waitMap : Map[CGNode,Set[InstanceKey]] = {
    	//setup the transfer functions
        val transFun = new SetTransferFunctionProvider {
            def getNodeTransferFunction(node: CGNode): SetUnaryOperatorProxy = {
                if (isThreadStart(node)) 
                    EmptySetOperator //we don't propagate over threadStarts
                else
                    new SetAddOperator(directWaitMap(node))
            }

            def hasNodeTransferFunctions() = true

            def getEdgeTransferFunction(from: CGNode, to: CGNode) = null

            def hasEdgeTransferFunctions() = false

            def getMeetOperator() = SetUnion
        }
        
        //invert the graph for backwards propagation
        val cgInv = new InvertedGraph(cg)

        //setup the problem
        val problem = new SetKilldallFramework {
            def getFlowGraph = cgInv
            def getTransferFunctionProvider = transFun
        }

        val solver = new SetDataflowSolver(problem)

        solver.solve(null);
        
        //copy results into immutable map
        var wm = Map[CGNode, Set[InstanceKey]]().withDefaultValue(Set())

        for (x <- cg) {
            val set = solver.getOut(x).getSet
            if (!set.isEmpty)
                wm += x -> set
        }

        wm
    }

}


/**
 * A variable used by the data flow solver representing a set of InstanceKeys
 * @author Benedikt Nordhoff
 */
class SetVariable(private var set: Set[InstanceKey]) extends SetVariableProxy[InstanceKey] {
    def this() = {
        this(Set())
    }

    def getSet = set
    def setSet(set2: Set[InstanceKey]) { set = set2 }
    def copyState(other: SetVariableProxy[InstanceKey]) {
        set = other.getSet
    }

}

/**
 * An operator adding a fixed set of InstanceKeys to the given SetVariable
 * @author Benedikt Nordhoff
 */
class SetAddOperator(aSet: Set[InstanceKey]) extends SetUnaryOperatorProxy {
    def evaluate(lhs: SetVariable, rhs: SetVariable): Byte = {

        val set = rhs.getSet ++ aSet
        val notchanged = set == lhs.getSet
        lhs.setSet(set)
        if (notchanged)
            return com.ibm.wala.fixpoint.FixedPointConstants.NOT_CHANGED
        else
            return com.ibm.wala.fixpoint.FixedPointConstants.CHANGED

    }

    override lazy val hashCode = aSet.hashCode()
}

/**
 * An operator always returning the empty set.
 * @author Benedikt Nordhoff
 */
object EmptySetOperator extends SetUnaryOperatorProxy {
    def evaluate(lhs: SetVariable, rhs: SetVariable): Byte = {
        val notchanged = lhs.getSet.isEmpty
        lhs.setSet(Set.empty)
        if (notchanged)
            return com.ibm.wala.fixpoint.FixedPointConstants.NOT_CHANGED
        else
            return com.ibm.wala.fixpoint.FixedPointConstants.CHANGED

    }

    override lazy val hashCode = 2367 //randomly chosen by fair cat
}

/**
 * A meet operator representing the union of to SetVariables
 * @author Benedikt Nordhoff
 */
object SetUnion extends SetMeetOperatorProxy {
    def evaluate(lhs: SetVariable, rhs: Array[SetVariable]): Byte = {
        var set = lhs.getSet
        for (sv <- rhs) {
            set ++= sv.asInstanceOf[SetVariable].getSet
        }
        val notchanged = set == lhs.getSet
        lhs.setSet(set)
        if (notchanged)
            return com.ibm.wala.fixpoint.FixedPointConstants.NOT_CHANGED
        else
            return com.ibm.wala.fixpoint.FixedPointConstants.CHANGED
    }

    override def hashCode() = 42;
}

