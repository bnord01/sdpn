package de.wwu.sdpn.wala.dpngen

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.asScalaIterator
import scala.collection.JavaConversions.asScalaSet
import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.collection.JavaConversions.iterableAsScalaIterable
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ssa.SSAAbstractInvokeInstruction
import com.ibm.wala.ssa.SSACFG
import com.ibm.wala.ssa.SSAInstruction
import com.ibm.wala.ssa.SSAMonitorInstruction
import com.ibm.wala.util.graph.traverse.BFSIterator
import de.wwu.sdpn.core.dpn.monitor.MonitorDPN
import de.wwu.sdpn.core.dpn.monitor.BaseRule
import de.wwu.sdpn.core.dpn.monitor.DPNRule
import de.wwu.sdpn.core.dpn.monitor.PopRule
import de.wwu.sdpn.core.dpn.monitor.PushRule
import de.wwu.sdpn.core.dpn.monitor.SpawnRule
import de.wwu.sdpn.wala.dpngen.symbols.DPNAction
import de.wwu.sdpn.wala.dpngen.symbols.EState
import de.wwu.sdpn.wala.dpngen.symbols.ExceptionAction
import de.wwu.sdpn.wala.dpngen.symbols.ExceptionCatch
import de.wwu.sdpn.wala.dpngen.symbols.ExceptionInMonitor
import de.wwu.sdpn.wala.dpngen.symbols.GlobalState
import de.wwu.sdpn.wala.dpngen.symbols.MonitorEnter
import de.wwu.sdpn.wala.dpngen.symbols.MonitorExit
import de.wwu.sdpn.wala.dpngen.symbols.NState
import de.wwu.sdpn.wala.dpngen.symbols.NoAction
import de.wwu.sdpn.wala.dpngen.symbols.Return
import de.wwu.sdpn.wala.dpngen.symbols.SSAAction
import de.wwu.sdpn.wala.dpngen.symbols.SkipAction
import de.wwu.sdpn.wala.dpngen.symbols.Spawn
import de.wwu.sdpn.wala.dpngen.symbols.StackSymbol
import de.wwu.sdpn.wala.dpngen.symbols.SyncMethodEnter
import de.wwu.sdpn.wala.dpngen.symbols.SyncMethodExit
import de.wwu.sdpn.wala.util.MonitorMatcher
import de.wwu.sdpn.wala.util.PreAnalysis
import de.wwu.sdpn.wala.dpngen.symbols.MExitState

/**
 * Factory to create a MonitorDPN using the given PreAnalysis
 *
 * @author Benedikt Nordhoff
 */
class MonitorDPNFactory(analysis: PreAnalysis) {
  import analysis._

  //private val dpn = new DPN[GlobalState, StackSymbol, DPNAction](
  //   NState, StackSymbol(entryNode, 0, 0))
  private val tinitial = (NState, StackSymbol(entryNode, 0, 0))
  private var tactions = Set[DPNAction]()
  private var tcontrolSymbols = Set[GlobalState]()
  private var tstackSymbols = Set[StackSymbol]()
  private var ttransitions = Set[DPNRule[GlobalState, StackSymbol, DPNAction]]()
  private var ttransmap = Map[(GlobalState, StackSymbol), Set[DPNRule[GlobalState, StackSymbol, DPNAction]]]()
  private var tlocks = Set[InstanceKey]()

  private lazy val dpn: MonitorDPN[GlobalState, StackSymbol, DPNAction, InstanceKey] = {
    genDPN;
    new MonitorDPN[GlobalState, StackSymbol, DPNAction, InstanceKey] {
      val initialState = tinitial._1
      val initialStack = tinitial._2
      val actions = tactions
      val controlSymbols = tcontrolSymbols
      val stackSymbols = tstackSymbols
      val transitions = ttransitions
      val transmap = ttransmap
      val locks = tlocks
      def usedLock(rule: DPNRule[GlobalState, StackSymbol, DPNAction]) = {
        rule match {
          case PushRule(_, _, a, _, _, _) =>
            a match {
              case SyncMethodEnter(_, ik) => Some(ik)
              case MonitorEnter(_, ik) => Some(ik)
              case _ => None
            }
          case _ => None
        }
      }
    }
  }

  /**
   * @return the generated MonitorDPN
   */
  def getDPN = dpn

  private var isGenerated = false

  /**
   * Generate the MonitorDPN
   */
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
      addBaseRule(
        NState, StackSymbol(cgnode, bb.getNumber, index),
        NoAction,
        NState, StackSymbol(cgnode, bb2.getNumber, 0))

    }
    var i = 0;
    for (bb2 <- cfg.getExceptionalSuccessors(bb)) {
      i += 1;
      addBaseRule(
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
          addBaseRule(
            NState, StackSymbol(cgnode, bbnr1, index1),
            SSAAction(action),
            NState, StackSymbol(cgnode, bbnr2, index2))
          addBaseRule(
            NState, StackSymbol(cgnode, bbnr1, index1),
            SSAAction(action),
            EState, StackSymbol(cgnode, bbnr2, index2))
        }

        for (target <- targets) {
          if (!isInteresting(target)) {
            addBaseRule(
              NState, StackSymbol(cgnode, bbnr1, index1),
              SkipAction(SSAAction(action)),
              NState, StackSymbol(cgnode, bbnr2, index2))
            //there is always an exception action for method invocations!
            //addBaseRule(
            //        NState, StackSymbol(cgnode, bbnr1, index1),
            //        SkipAction(SSAAction(action)),
            //        EState, StackSymbol(cgnode, bbnr2, index2))
          } else if (!target.getMethod.isSynchronized) {
            addPushRule(
              NState, StackSymbol(cgnode, bbnr1, index1),
              SSAAction(action),
              NState, StackSymbol(target, 0, 0), StackSymbol(cgnode, bbnr2, index2))
          } else if (target.getMethod.isStatic) {
            val ik = pa.getHeapModel().getInstanceKeyForClassObject(target.getMethod().getDeclaringClass().getReference);
            if (safeLock(ik, cgnode)) {
                addPushRule(
                  NState, StackSymbol(cgnode, bbnr1, index1),
                  SyncMethodEnter(a, ik),
                  NState, StackSymbol(target, 0, 0), StackSymbol(cgnode, bbnr2, index2))
                tlocks += ik
              } else {
                addPushRule(
                  NState, StackSymbol(cgnode, bbnr1, index1),
                  SkipAction(SyncMethodEnter(a, ik)),
                  NState, StackSymbol(target, 0, 0), StackSymbol(cgnode, bbnr2, index2))
              }
          } else {
            val thisidx = target.getIR().getSymbolTable().getParameter(0)
            val ptk = pa.getHeapModel.getPointerKeyForLocal(target, thisidx)
            val iks = pa.getPointsToSet(ptk)
            if (iks.isEmpty) {
              println("DPNFAC empty iks for thispointer")
              addPushRule(
                  NState, StackSymbol(cgnode, bbnr1, index1),
                  SSAAction(a),
                  NState, StackSymbol(target, 0, 0), StackSymbol(cgnode, bbnr2, index2))
            }
            for (ik <- iks) {
              if (safeLock(ik, cgnode)) {
                addPushRule(
                  NState, StackSymbol(cgnode, bbnr1, index1),
                  SyncMethodEnter(a, ik),
                  NState, StackSymbol(target, 0, 0), StackSymbol(cgnode, bbnr2, index2))
                tlocks += ik
              } else {
                addPushRule(
                  NState, StackSymbol(cgnode, bbnr1, index1),
                  SkipAction(SyncMethodEnter(a, ik)),
                  NState, StackSymbol(target, 0, 0), StackSymbol(cgnode, bbnr2, index2))
              }
            }
          }

          addBaseRule(
            NState, StackSymbol(cgnode, bbnr1, index1),
            ExceptionAction(SSAAction(action)),
            EState, StackSymbol(cgnode, bbnr2, index2))
        }

      }
      // Monitor Action
      case a: SSAMonitorInstruction => {
        val cfg = cgnode.getIR.getControlFlowGraph
        if (a.isMonitorEnter) {
          val ptk = pa.getHeapModel.getPointerKeyForLocal(cgnode, a.getRef)
          val iks = pa.getPointsToSet(ptk)

          /*
           * We obtain the possible monitor exits corresponding to this enter  
           * For each exit we introduce a special global state
           * We introduce a fake index within this basic block (-1)
           * The enter will push the fake index onto the stack
           * Instead of the exit we perform a pop operation which moves to the 
           * special global state.  From this special global state at the fake index 
           * which was pushed before we continue to the point after the exit.
           */
          val fakeIndex = -1
          val exits = MonitorMatcher.getExits(cfg,bbnr1).zipWithIndex
          for(((exitBlock,idxBeforeMonitorExit),num) <- exits) {
              addPopRule(
            		  NState, StackSymbol(cgnode, exitBlock.getGraphNodeId(), idxBeforeMonitorExit),
            		  MonitorExit(a),
            		  MExitState(num)) 
              addBaseRule(
            		  MExitState(num), StackSymbol(cgnode, bbnr1, fakeIndex),
            		  NoAction,
            		  NState,
            		  StackSymbol(cgnode, exitBlock.getGraphNodeId, idxBeforeMonitorExit + 1))
          }          

          for (target <- iks) {
            //we use (CGNode,bbnr1,fakeIndex) as a fake return point for the fake method call
            //for the monitor             

            if (safeLock(target, cgnode)) {
              addPushRule(
                NState, StackSymbol(cgnode, bbnr1, index1),
                MonitorEnter(a, target),
                NState, StackSymbol(cgnode, bbnr2, index2),
                StackSymbol(cgnode, bbnr1, fakeIndex))
              tlocks += target
            } else {
              addPushRule(
                NState, StackSymbol(cgnode, bbnr1, index1),
                SkipAction(MonitorEnter(a, target)),
                NState, StackSymbol(cgnode, bbnr2, index2),
                StackSymbol(cgnode, bbnr1, fakeIndex))
            }
          }
          if (iks.isEmpty()) {
            //This shouldn't happen
            println("DPNFAC empty iks for lock pointer in CGNode: " + cgnode + " , BBNr: " + bbnr1)
            addPushRule(
              NState, StackSymbol(cgnode, bbnr1, index1),
              SkipAction(MonitorEnter(a, null)),
              NState, StackSymbol(cgnode, bbnr2, index2),
              StackSymbol(cgnode, bbnr1, fakeIndex))
          }
        } else { 
            //monitor exit // Only the exceptional case.
            // The normal case is already handled at the enter.
            addBaseRule(
              NState, StackSymbol(cgnode, bbnr1, index1),
              ExceptionAction(SSAAction(action)),
              EState, StackSymbol(cgnode, bbnr2, index2))
        }

      }

      // Compute
      case _ =>
        addBaseRule(
          NState, StackSymbol(cgnode, bbnr1, index1),
          if (action != null) SSAAction(action) else NoAction,
          NState, StackSymbol(cgnode, bbnr2, index2))
        //if isPEI add exceptional rule
        if (action != null && !action.getExceptionTypes.isEmpty)
          addBaseRule(
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
    //TODO Only spawn Runnable.run() here not exception.init if using jSDG stubs.
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
    addSpawnRule(
      NState, StackSymbol(cgnode, 0, 0),
      Spawn,
      NState, StackSymbol(cgnode, 1, 0), NState, StackSymbol(target, 0, 0))
    //Not sure if this is necessary: Spawn and Exception maybe add a Flag.
    addSpawnRule(
      NState, StackSymbol(cgnode, 0, 0),
      ExceptionAction(Spawn),
      EState, StackSymbol(cgnode, 1, 0), NState, StackSymbol(target, 0, 0))
    addBaseRule(
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
      addPopRule(
        NState, StackSymbol(cgnode, bbnr, 0),
        Return,
        NState)
      addPopRule(
        EState, StackSymbol(cgnode, bbnr, 0),
        Return,
        EState)
    } else if (cgnode.getMethod.isStatic) {
      val ik = pa.getHeapModel().getInstanceKeyForClassObject(cgnode.getMethod().getDeclaringClass().getReference);
      addPopRule(
        NState, StackSymbol(cgnode, bbnr, 0),
        SyncMethodExit(ik),
        NState)
      addPopRule(
        EState, StackSymbol(cgnode, bbnr, 0),
        SyncMethodExit(ik),
        EState)
    } else {
      val ptk = pa.getHeapModel.getPointerKeyForLocal(cgnode, 1)
      val iks = pa.getPointsToSet(ptk)
      for (ik <- iks) {
        addPopRule(
          NState, StackSymbol(cgnode, bbnr, 0),
          SyncMethodExit(ik),
          NState)
        addPopRule(
          EState, StackSymbol(cgnode, bbnr, 0),
          SyncMethodExit(ik),
          EState)
      }
      if(iks.isEmpty()) {
        addPopRule(
          NState, StackSymbol(cgnode, bbnr, 0),
          Return,
          NState)
        addPopRule(
          EState, StackSymbol(cgnode, bbnr, 0),
          Return,
          EState)
      }
    }

  }

  /**
   * Add a Transition to the DPN
   * @param rule the DPN rule to add to the DPN
   */
  private def addTransition(rule: DPNRule[GlobalState, StackSymbol, DPNAction]) {
    ttransitions += rule
    val (p, s) = (rule.inState, rule.inSymbol)
    if (!ttransmap.contains((p, s)))
      ttransmap += (p, s) -> Set(rule)
    else
      ttransmap += (p, s) -> (ttransmap((p, s)) + rule)
    rule match {
      case SpawnRule(p, s, a, p1, w1, p2, w2) =>
        tactions += a
        tstackSymbols ++= Set(s, w1, w2)
        tcontrolSymbols ++= Set(p, p1, p2)
      case PushRule(p, s, a, p1, w1, w2) =>
        tactions += a
        tstackSymbols ++= Set(s, w1, w2)
        tcontrolSymbols ++= Set(p, p1)
      case PopRule(p, s, a, p1) =>
        tactions += a
        tstackSymbols += s
        tcontrolSymbols ++= Set(p, p1)
      case BaseRule(p, s, a, p1, s1) =>
        tactions += a
        tstackSymbols ++= Set(s, s1)
        tcontrolSymbols ++= Set(p, p1)
    }

  }

  private def addSpawnRule(p: GlobalState, s: StackSymbol, a: DPNAction, p1: GlobalState, w1: StackSymbol, p2: GlobalState, w2: StackSymbol) {
    addTransition(SpawnRule(p, s, a, p1, w1, p2, w2))
  }
  private def addPushRule(p: GlobalState, s: StackSymbol, a: DPNAction, p1: GlobalState, w1: StackSymbol, w2: StackSymbol) {
    addTransition(PushRule(p, s, a, p1, w1, w2))
  }
  private def addPopRule(p: GlobalState, s: StackSymbol, a: DPNAction, p1: GlobalState) {
    addTransition(PopRule(p, s, a, p1))
  }
  private def addBaseRule(p: GlobalState, s: StackSymbol, a: DPNAction, p1: GlobalState, w1: StackSymbol) {
    addTransition(BaseRule(p, s, a, p1, w1))
  }

}