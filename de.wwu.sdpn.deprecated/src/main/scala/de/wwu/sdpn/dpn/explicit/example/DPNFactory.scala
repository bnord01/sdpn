package de.wwu.sdpn.dpn.explicit.example

import com.ibm.wala.ipa.cha.IClassHierarchy
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.callgraph.propagation.rta.CallSite
import com.ibm.wala.cfg.ControlFlowGraph
import com.ibm.wala.ssa.SSAMonitorInstruction
import com.ibm.wala.ssa.SSAAbstractInvokeInstruction
import com.ibm.wala.ssa.SSAInvokeInstruction
import com.ibm.wala.ssa.IR
import com.ibm.wala.ssa.SSACFG
import com.ibm.wala.ssa.ISSABasicBlock
import com.ibm.wala.classLoader.IMethod
import com.ibm.wala.ipa.callgraph.Context
import com.ibm.wala.ssa.SSAInstruction
import com.ibm.wala.util.graph.traverse.BFSIterator
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ipa.callgraph.CallGraph
import scala.collection.JavaConversions._
import de.wwu.sdpn.dpn.explicit._
import de.wwu.sdpn.util.PreAnalysis

/**
 * Old Factory to create a DPN, this has been partially replaced by MonitorDPNFactory but CutTA still depends on this.
 * 
 * @author Benedikt Nordhoff
 * @deprecated
 */
trait DPNFactory {
    this: PreAnalysis =>

    private val dpn = new DPN[GlobalState, StackSymbol, DPNAction](
        NState, StackSymbol(entryNode, 0, 0))
    private var isGenerated = false

    private def genDPN {
        this.synchronized {
            if (isGenerated) return //maybe some generated the dpn while we where waiting.
            //We iterate BFS over the CallGraph beginning at the given entrypoint node
            val bfsi = new BFSIterator(cg, entryNode)
            for (node <- bfsi if isInteresting(node)) {
                if (isThreadStart(node)) {
                    // handle ThreadStart as CFG isn't consitent with CG.
                    createRules4ThreadStart(node)
                } else {
                    createRules4CGNode(node)
                }
            }
            isGenerated = true
        }
    }

    //Handle one CGNode
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
            dpn.addBaseRule(
                NState, StackSymbol(cgnode, bb.getNumber, index),
                NoAction,
                NState, StackSymbol(cgnode, bb2.getNumber, 0))

        }
        var i = 0;
        for (bb2 <- cfg.getExceptionalSuccessors(bb)) {
            i += 1;
            dpn.addBaseRule(
                EState, StackSymbol(cgnode, bb.getNumber, index),
                ExceptionCatch(i),
                if (bb2.isCatchBlock) NState else EState, StackSymbol(cgnode, bb2.getNumber, 0))
            //falls bb2 exception fangen kann in NState uebergehen.
            //TODO Hier immer EState verwenden und erst bei der Catch instruction wechseln.
        }
        //create rule for exit/return block
        if (bb.isExitBlock)
            createRule4Return(cgnode, bbnr)
    }

    /**
     * Create rule for SSSAInstruction execution.
     * @param action Executed SSAInstruction or null for NoAction.
     * @param bbnr1 BasicBlock number where SSAInstruction is executed.
     * @param index1 Index within basic block where SSAInstruction is executed.
     * @param bbnr2 number of the next basic block.
     * @param index2 Index of the next instruction.
     * @param cgnode Containing CGNode.
     */
    private def createRule4Instruction(action: SSAInstruction, bbnr1: Int, index1: Int, bbnr2: Int, index2: Int, cgnode: CGNode) = {
        action match {
            //TODO Fall fuer CatchInstruction hinzufuegen.
            // Methodinvokation
            case a: SSAAbstractInvokeInstruction => {
                val targets = cg.getPossibleTargets(cgnode, a.getCallSite)
                if (targets.isEmpty) {
                    //System.err.println("No targets for call to: " + a.getDeclaredTarget.getSignature);
                    dpn.addBaseRule(
                        NState, StackSymbol(cgnode, bbnr1, index1),
                        SSAAction(action),
                        NState, StackSymbol(cgnode, bbnr2, index2))
                    dpn.addBaseRule(
                        NState, StackSymbol(cgnode, bbnr1, index1),
                        SSAAction(action),
                        EState, StackSymbol(cgnode, bbnr2, index2))
                }

                for (target <- targets) {
                    if (!isInteresting(target)) {
                        dpn.addBaseRule(
                            NState, StackSymbol(cgnode, bbnr1, index1),
                            SkipAction(SSAAction(action)),
                            NState, StackSymbol(cgnode, bbnr2, index2))
                        dpn.addBaseRule(
                            NState, StackSymbol(cgnode, bbnr1, index1),
                            SkipAction(SSAAction(action)),
                            EState, StackSymbol(cgnode, bbnr2, index2))
                    } else if (!target.getMethod.isSynchronized) {
                        dpn.addPushRule(
                            NState, StackSymbol(cgnode, bbnr1, index1),
                            SSAAction(action),
                            NState, StackSymbol(target, 0, 0), StackSymbol(cgnode, bbnr2, index2))
                    } else if (target.getMethod.isStatic) {
                        val ik = pa.getHeapModel().getInstanceKeyForClassObject(target.getMethod().getDeclaringClass().getReference);
                        dpn.addPushRule(
                            NState, StackSymbol(cgnode, bbnr1, index1),
                            SyncMethodEnter(a, ik),
                            NState, StackSymbol(target, 0, 0), StackSymbol(cgnode, bbnr2, index2))
                    } else if (!target.getMethod.isStatic) {
                        val thisidx = target.getIR().getSymbolTable().getParameter(0)
                        val ptk = pa.getHeapModel.getPointerKeyForLocal(target, thisidx)
                        val iks = pa.getPointsToSet(ptk)
                        if (iks.isEmpty)
                            println("DPNFAC empty iks for thispointer")
                        for (ik <- iks) {
                            dpn.addPushRule(
                                NState, StackSymbol(cgnode, bbnr1, index1),
                                SyncMethodEnter(a, ik),
                                NState, StackSymbol(target, 0, 0), StackSymbol(cgnode, bbnr2, index2))
                        }

                    } else { // TODO this shouldn't happen?

                        dpn.addPushRule(
                            NState, StackSymbol(cgnode, bbnr1, index1),
                            SSAAction(a),
                            NState, StackSymbol(target, 0, 0), StackSymbol(cgnode, bbnr2, index2))
                    }

                    //TODO: check whether this criteria is exhaustive (i don't think so!)
                    //                    if (!a.getExceptionTypes.isEmpty)
                    dpn.addBaseRule(
                        NState, StackSymbol(cgnode, bbnr1, index1),
                        ExceptionAction(SSAAction(action)),
                        EState, StackSymbol(cgnode, bbnr2, index2))
                }

            }
            // Monitor Action
            case a: SSAMonitorInstruction => {
                val ptk = pa.getHeapModel.getPointerKeyForLocal(cgnode, a.getRef)
                val iks = pa.getPointsToSet(ptk)
                for (target <- iks) {
                    dpn.addBaseRule(
                        NState, StackSymbol(cgnode, bbnr1, index1),
                        if (a.isMonitorEnter) MonitorEnter(a, target) else MonitorExit(a, target),
                        NState, StackSymbol(cgnode, bbnr2, index2))

                    //add exceptional rule
                    dpn.addBaseRule(
                        NState, StackSymbol(cgnode, bbnr1, index1),
                        ExceptionAction(if (a.isMonitorEnter) MonitorEnter(a, target) else MonitorExit(a, target)),
                        EState, StackSymbol(cgnode, bbnr2, index2))
                }
            }

            // Compute
            case _ =>
                dpn.addBaseRule(
                    NState, StackSymbol(cgnode, bbnr1, index1),
                    if (action != null) SSAAction(action) else NoAction,
                    NState, StackSymbol(cgnode, bbnr2, index2))
                //if isPEI add exceptional rule
                if (action != null && !action.getExceptionTypes.isEmpty)
                    dpn.addBaseRule(
                        NState, StackSymbol(cgnode, bbnr1, index1),
                        ExceptionAction(SSAAction(action)),
                        EState, StackSymbol(cgnode, bbnr2, index2))
        }
    }

    /**
     * Create rules for thread start.
     * This creates a SpawnRule where we model the Thread.start() method as.
     * (0,0) --- target.run() ---> (1,0) --- return ---> ()
     * @param cgnode Spawning Thread.start() node.
     */
    private def createRules4ThreadStart(cgnode: CGNode) = {
        assert(isThreadStart(cgnode))
        for (target <- cg.getSuccNodes(cgnode)) {
            createRule4Spawn(cgnode, target)
        }
        createRule4Return(cgnode, 1)
    }

    /**
     * Create a SpawnRule where cgnode spawns target.
     * cgnode should to refer to a Thread.start() as we
     * the rule transfers the spawning cgnode from
     * (0,0) ---> (1,0).
     */
    private def createRule4Spawn(cgnode: CGNode, target: CGNode) = {
        dpn.addSpawnRule(
            NState, StackSymbol(cgnode, 0, 0),
            Spawn,
            NState, StackSymbol(cgnode, 1, 0), NState, StackSymbol(target, 0, 0))
        dpn.addBaseRule(
            NState, StackSymbol(cgnode, 0, 0),
            ExceptionAction(Spawn),
            EState, StackSymbol(cgnode, 1, 0))
    }

    /**
     * Create rule for method return.
     * (context,method,bbnr,0) ---Return---> ()
     * @param context Context of returning method.
     * @param method Returning method.
     * @param bbnr Exitblock number of returning method.
     */
    private def createRule4Return(cgnode: CGNode, bbnr: Int) = {
        if (!cgnode.getMethod.isSynchronized) {
            dpn.addPopRule(
                NState, StackSymbol(cgnode, bbnr, 0),
                Return,
                NState)
            dpn.addPopRule(
                EState, StackSymbol(cgnode, bbnr, 0),
                Return,
                EState)
        } else if (cgnode.getMethod.isStatic) {
            val ik = pa.getHeapModel().getInstanceKeyForClassObject(cgnode.getMethod().getDeclaringClass().getReference);
            dpn.addPopRule(
                NState, StackSymbol(cgnode, bbnr, 0),
                SyncMethodExit(ik),
                NState)
            dpn.addPopRule(
                EState, StackSymbol(cgnode, bbnr, 0),
                SyncMethodExit(ik),
                EState)
        } else {
            val ptk = pa.getHeapModel.getPointerKeyForLocal(cgnode, 1)
            val iks = pa.getPointsToSet(ptk)
            for (ik <- iks) {
                dpn.addPopRule(
                    NState, StackSymbol(cgnode, bbnr, 0),
                    SyncMethodExit(ik),
                    NState)
                dpn.addPopRule(
                    EState, StackSymbol(cgnode, bbnr, 0),
                    SyncMethodExit(ik),
                    EState)
            }
        }

    }

    def getDPN = { if (isGenerated) dpn else { genDPN; dpn } }

}