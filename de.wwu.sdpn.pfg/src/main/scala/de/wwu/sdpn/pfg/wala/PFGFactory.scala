package de.wwu.sdpn.pfg.wala

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.asScalaIterator
import scala.collection.JavaConversions.asScalaSet
import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.collection.JavaConversions.iterableAsScalaIterable
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ssa.SSAAbstractInvokeInstruction
import com.ibm.wala.ssa.SSACFG
import com.ibm.wala.ssa.SSAInstruction
import com.ibm.wala.util.graph.traverse.BFSIterator
import de.wwu.sdpn.wala.util.PreAnalysis
import de.wwu.sdpn.pfg.ParFlowGraph
import com.ibm.wala.ipa.callgraph.CallGraph

/**
 * Factory to create a MonitorDPN using the given PreAnalysis
 *
 * @author Benedikt Nordhoff
 */
class PFGFactory(cg: CallGraph, isInteresting: CGNode => Boolean = _ => true, entryNodeOption: Option[CGNode] = None) {

    type Proc = CGNode

    private val entryNode = entryNodeOption getOrElse (cg getFakeRootNode)
    private var tmainProc: Proc = entryNode
    private var tedges: Set[Edge] = Set()
    private var tretNodes: Map[Proc, Map[State, Node]] = Map().withDefaultValue(Map())
    private var tnodes: Set[Node] = Set()
    private var tprocs: Set[Proc] = Set()

    private var tentryNode: Map[Proc, Node] = Map()

    def procOf: Node => Proc = _.proc

    def isThreadStart(cgnode: CGNode) = {
        cgnode != null &&
            cgnode.getMethod != null &&
            "java.lang.Thread.start()V".equals(cgnode.getMethod.getSignature)
    }

    private lazy val pfg: WalaPFG = {
        genPFG;
        new WalaPFG {
            val edges = tedges
            val retNodes = tretNodes
            val nodes = tnodes
            val procs = tprocs
            val entryNode = tentryNode
            val mainProc = tmainProc
            def procOf: Node => Proc = _.proc
        }
    }

    /**
     * @return the generated parallel flow graph
     */
    def getPFG = pfg

    private var isGenerated = false

    /**
     * Generate the MonitorDPN
     */
    private def genPFG {
        this.synchronized {
            if (isGenerated) return //maybe some generated the dpn while we where waiting.
            //We iterate BFS over the CallGraph beginning at the given entrypoint node
            val bfsi = new BFSIterator(cg, entryNode)
            for (node <- bfsi if isInteresting(node)) {
                tprocs += node
                tentryNode += node -> Node(N, CFGPoint(node, 0, 0))

                createRules4CGNode(node)
            }
            isGenerated = true
        }
    }

    /**
     * Handle one CGNode
     * @param cgnode
     */
    private def createRules4CGNode(cgnode: CGNode) = {
        val ir = cgnode.getIR
        if (ir != null) {
            val cfg = ir.getControlFlowGraph
            //Iterate basic blocks
            for (bb <- cfg) {
                //Handle Basic Blocks
                createRules4BB(bb.asInstanceOf[SSACFG#BasicBlock], cgnode, cfg)
            }
        } else {
            createRule4Return(cgnode, 0)
            //System.err.println("Got null IR for method: " + cgnode.getMethod.getSignature)
        }

    }

    /**
     * Create Rules for instructions within a BasicBlock bb and rules to
     * reach following BasicBlocks.
     * @param bb current BasicBlock
     * @param context
     * @param method
     * @param cgnode
     * @param cfg
     */
    private def createRules4BB(bb: SSACFG#BasicBlock, cgnode: CGNode, cfg: SSACFG) = {
        val bbnr = bb.getNumber
        var index = 0

        //Rules for phi instructions first
        for (instruction <- bb.iteratePhis) {
            createRule4Instruction(instruction, bbnr, index, bbnr, index + 1, cgnode)
            index += 1
        }
        //Rules for catch instructions 
        if (bb.isCatchBlock) {
            val instruction = bb.asInstanceOf[SSACFG#ExceptionHandlerBasicBlock].getCatchInstruction
            createRule4Instruction(instruction, bbnr, index, bbnr, index + 1, cgnode)
            index += 1
        }
        //Rules for normal instructions
        for (instruction <- bb.iterateNormalInstructions) {
            createRule4Instruction(instruction, bbnr, index, bbnr, index + 1, cgnode)
            index += 1
        }
        //rules for pi instructions
        for (instruction <- bb.iteratePis) {
            createRule4Instruction(instruction, bbnr, index, bbnr, index + 1, cgnode)
            index += 1
        }
        //create rules for successor basic blocks
        for (bb2 <- cfg.getNormalSuccessors(bb)) {
            addBaseEdge(
                Node(N, CFGPoint(cgnode, bb.getNumber, index)),
                Skip,
                Node(N, CFGPoint(cgnode, bb2.getNumber, 0)))
        }
        var i = 0;
        for (bb2 <- cfg.getExceptionalSuccessors(bb)) {
            i += 1;
            addBaseEdge(
                Node(E, CFGPoint(cgnode, bb.getNumber, index)),
                Skip,
                Node(if (bb2.isCatchBlock) N else E, CFGPoint(cgnode, bb2.getNumber, 0)))
            //falls bb2 exception fangen kann in NState uebergehen.
            //TODO Hier immer EState verwenden und erst bei der Catch instruction wechseln.
        }
        //create rule for exit/return block
        if (bb.isExitBlock)
            createRule4Return(cgnode, bbnr)
    }

    /**
     * Create rule for one SSSAInstruction.
     * @param action Executed SSAInstruction or null for NoAction.
     * @param bbnr1 BasicBlock number where SSAInstruction is executed.
     * @param index1 Index within basic block where SSAInstruction is executed.
     * @param bbnr2 number of the next basic block.
     * @param index2 Index of the next instruction.
     * @param cgnode Containing CGNode.
     */
    private def createRule4Instruction(action: SSAInstruction, bbnr1: Int, index1: Int, bbnr2: Int, index2: Int, cgnode: CGNode) = {
        action match {
            //TODO Maybe handle CatchInstruction here.
            // Methodinvokation
            case a: SSAAbstractInvokeInstruction => {
                val targets = cg.getPossibleTargets(cgnode, a.getCallSite)
                if (targets.isEmpty) {
                    //System.err.println("No targets for call to: " + a.getDeclaredTarget.getSignature);
                    addBaseEdge(
                        Node(N, CFGPoint(cgnode, bbnr1, index1)),
                        SSAAction(action),
                        Node(N, CFGPoint(cgnode, bbnr2, index2)))
                }

                for (target <- targets) {
                    if (!isInteresting(target)) {
                        addBaseEdge(
                            Node(N, CFGPoint(cgnode, bbnr1, index1)),
                            SSAAction(action),
                            Node(N, CFGPoint(cgnode, bbnr2, index2)))
                        //there is always an exception action for method invocations!
                        //addBaseRule(
                        //        NState, StackSymbol(cgnode, bbnr1, index1),
                        //        SkipAction(SSAAction(action)),
                        //        EState, StackSymbol(cgnode, bbnr2, index2))
                    } else if (isThreadStart(target)) {
                        addSpawnEdge(
                            Node(N, CFGPoint(cgnode, bbnr1, index1)),
                            target,
                            Node(N, CFGPoint(cgnode, bbnr2, index2)))
                        addSpawnEdge(
                            Node(N, CFGPoint(cgnode, bbnr1, index1)),
                            target,
                            Node(E, CFGPoint(cgnode, bbnr2, index2)))
                    } else {
                        addCallEdge(
                            Node(N, CFGPoint(cgnode, bbnr1, index1)),
                            target,
                            Map(
                                N -> Node(N, CFGPoint(cgnode, bbnr2, index2)),
                                E -> Node(E, CFGPoint(cgnode, bbnr2, index2))))
                    }
                }
                addBaseEdge(
                    Node(N, CFGPoint(cgnode, bbnr1, index1)),
                    SSAAction(action),
                    Node(E, CFGPoint(cgnode, bbnr2, index2)))

            }

            // Compute
            case _ =>
                addBaseEdge(
                    Node(N, CFGPoint(cgnode, bbnr1, index1)),
                    if (action != null) SSAAction(action) else Skip,
                    Node(N, CFGPoint(cgnode, bbnr2, index2)))
                //if isPEI add exceptional rule
                if (action != null && !action.getExceptionTypes.isEmpty)
                    addBaseEdge(
                        Node(N, CFGPoint(cgnode, bbnr1, index1)),
                        if (action != null) SSAAction(action) else Skip,
                        Node(E, CFGPoint(cgnode, bbnr2, index2)))
        }
    }

    
    /**
     * Create rule for method return.
     * (context,method,bbnr,0) ---Return---> ()
     * @param context Context of returning method.
     * @param method Returning method.
     * @param bbnr Exitblock number of returning method.
     */
    private def createRule4Return(cgnode: CGNode, bbnr: Int) = {
        var map = tretNodes(cgnode)
        for (s <- List(N, E)) {
            val node = Node(s, CFGPoint(cgnode, bbnr, 0))
            assert(map.get(s) == None)
            map += s -> node
            tnodes += node
        }
        tretNodes += cgnode -> map

    }

    def addBaseEdge(src: Node, ba: BaseAction, snk: Node) {
        tedges += BaseEdge(src, ba, snk)
        tnodes += src
        tnodes += snk
    }

    def addCallEdge(src: Node, proc: CGNode, returns: Map[State, Node]) {
        tedges += CallEdge(src, proc, returns)
        tnodes += src
        tnodes ++= returns.values
    }
    def addSpawnEdge(src: Node, proc: CGNode, snk: Node) {
        tedges += SpawnEdge(src, proc, snk)
        tnodes += src
        tnodes += snk
    }

}

object PFGFactory {
    def getPFG(cg: CallGraph, isInteresting: CGNode => Boolean = _ => true, entryNodeOption: Option[CGNode] = None): WalaPFG = {
        new PFGFactory(cg, isInteresting, entryNodeOption).getPFG
    }

    def getPFG(pa: PreAnalysis): WalaPFG = {
        new PFGFactory(pa.cg, pa.isInteresting, Some(pa.entryNode)).getPFG
    }

    /**
     * Obtains the CFGPoint representing the point just ''before'' the
     * instruction corresponding to the given instructionIndex.
     *
     * @param node A CGNode
     * @param instructionIndex An index of an instruction within the array node.getIR.getInstructions
     * @return A corresponding stack symbol
     */
    def getCFGPoint4NodeAndIndex(node: CGNode, instructionIndex: Int): CFGPoint = {
        val ir = node.getIR
        val cfg = ir.getControlFlowGraph

        val bb = cfg.getBlockForInstruction(instructionIndex)
        //val bb = cfg.filter(x => x.getFirstInstructionIndex <= instructionIndex && instructionIndex <= x.getLastInstructionIndex).first

        var index = 0
        for (instr <- bb.iteratePhis()) {
            index += 1
        }
        if (bb.isCatchBlock())
            index += 1
        val start = bb.getFirstInstructionIndex

        val instrArr = ir.getInstructions
        for (i <- start until instructionIndex) {
            if (instrArr(i) != null) {
                index += 1
            }
        }
        return CFGPoint(node, bb.getNumber, index)
    }
}