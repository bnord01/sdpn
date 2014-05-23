package de.wwu.sdpn.wala.util

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
import com.ibm.wala.ipa.cha.IClassHierarchy

//TODO maybe change to BitVectors instead of Scala sets.

/**
 * Generates a wait map which maps any CGNode to the set of instance keys on which this
 * or any called method might call wait().
 *
 * This uses the KilldallFramework
 * @author Benedikt Nordhoff
 */
class WaitMap(analysis: PreAnalysis, locks: InstanceKey => Boolean) {
    import analysis.{ cg, pa, isThreadStart }

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
    def apply(node: CGNode) = waitMap.getOrElse(node, Set())

    /**
     * This lazy val holds the result of the flow analysis as an immutable map.
     * The map contains only keys for which there exists an non empty wait set.
     *
     * apply(node:CGNode) returns an empty Set for keys not contained.
     */
    lazy val waitMap: Map[CGNode, Set[InstanceKey]] = {
        //setup the transfer functions
        val transFun = new ITransferFunctionProvider[CGNode, SetVariable] {
            def getNodeTransferFunction(node: CGNode): UnaryOperator[SetVariable] = {
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
        val problem = new IKilldallFramework[CGNode, SetVariable] {
            def getFlowGraph = cgInv
            def getTransferFunctionProvider = transFun
        }

        val solver = new DataflowSolver[CGNode, SetVariable](problem) {
            def makeEdgeVariable(src: CGNode, dst: CGNode): SetVariable = {
                // TODO Auto-generated method stub
                throw new UnsupportedOperationException("No");

            }

            def makeNodeVariable(n: CGNode, in: Boolean): SetVariable = {
                // TODO Auto-generated method stub
                return new SetVariable();
            }

            def makeStmtRHS(size: Int): Array[SetVariable] = {
                val v = new Array[SetVariable](size);
                for (i <- 0 until size)
                    v(i) = new SetVariable();
                return v;
            }
        }

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
class SetVariable(private var set: Set[InstanceKey]) extends AbstractVariable[SetVariable] {
    def this() = {
        this(Set())
    }

    def getSet = set
    def setSet(set2: Set[InstanceKey]) { set = set2 }
    def copyState(other: SetVariable) {
        set = other.getSet
    }

}

/**
 * An operator adding a fixed set of InstanceKeys to the given SetVariable
 * @author Benedikt Nordhoff
 */
class SetAddOperator(aSet: Set[InstanceKey]) extends UnaryOperator[SetVariable] {
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
    override def toString():String= "SetAddOperator("+aSet+")"
    override def equals(other:Any) = other match {case x:SetAddOperator => x.eq(this) case _ => false}
}

/**
 * An operator always returning the empty set.
 * @author Benedikt Nordhoff
 */
object EmptySetOperator extends UnaryOperator[SetVariable] {
    def evaluate(lhs: SetVariable, rhs: SetVariable): Byte = {
        val notchanged = lhs.getSet.isEmpty
        lhs.setSet(Set.empty)
        if (notchanged)
            return com.ibm.wala.fixpoint.FixedPointConstants.NOT_CHANGED
        else
            return com.ibm.wala.fixpoint.FixedPointConstants.CHANGED

    }

    override lazy val hashCode = 2367 //randomly chosen by fair cat
    
    override def toString():String= "EmptySetOperator"
    override def equals(other:Any) = other match {case EmptySetOperator => true case _ => false}
}

/**
 * A meet operator representing the union of to SetVariables
 * @author Benedikt Nordhoff
 */
object SetUnion extends AbstractMeetOperator[SetVariable] {
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
    override def toString():String= "SetUnion"
    override def equals(other:Any) = other match {case SetUnion => true case _ => false}
}

object WaitMap {
    private val emptyPA = new PreAnalysis {
        def cha: IClassHierarchy = null
        def cg: CallGraph = null

        def pa: PointerAnalysis[InstanceKey] = null
        def isThreadStart(cgnode: CGNode): Boolean = false

        def entryNode: CGNode = null

        def isInteresting(n: CGNode): Boolean = false

        def safeLock(ik: InstanceKey, node: CGNode): Boolean = false

    }
    lazy val empty: WaitMap = new WaitMap(emptyPA, _ => false) {
        override def apply(x: CGNode) = Set[InstanceKey]()
    }
}
