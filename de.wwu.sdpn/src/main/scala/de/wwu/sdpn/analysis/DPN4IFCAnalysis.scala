package de.wwu.sdpn.analysis
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ipa.callgraph.CGNode
import de.wwu.sdpn.dpn.explicit.StackSymbol
import scala.collection.JavaConversions._
import com.ibm.wala.ssa.SSACFG
import de.wwu.sdpn.util.LockWithOriginLocator
import de.wwu.sdpn.util.UniqueInstanceLocator
import de.wwu.sdpn.util.WaitMap
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.NullProgressMonitor
import com.ibm.wala.types.ClassLoaderReference
import de.wwu.sdpn.dpn.explicit.monitor.MonitorDPN
import de.wwu.sdpn.dpn.explicit.DPNAction
import de.wwu.sdpn.dpn.explicit.GlobalState
import de.wwu.sdpn.util.BackwardSliceFilter
import de.wwu.sdpn.dpn.explicit.monitor.MonitorDPNFactory
import de.wwu.sdpn.ta.prolog.cuts.FwdCutLockSet
import de.wwu.sdpn.ta.IntersectionTA
import de.wwu.sdpn.ta.prolog.cuts.MDPN2CutTA
import de.wwu.sdpn.ta.prolog.cuts.IFlowReading
import de.wwu.sdpn.ta.prolog.cuts.IFlowWriting
import de.wwu.sdpn.ta.prolog.cuts.CutWellFormed
import de.wwu.sdpn.ta.prolog.cuts.CutReleaseStructTA
import de.wwu.sdpn.ta.prolog.cuts.CutAcqStructComplTA
import de.wwu.sdpn.ta.prolog.cuts.CutAcqStructPrecutTA
import de.wwu.sdpn.ta.ScriptTreeAutomata
import de.wwu.sdpn.ta.IntersectionEmptinessCheck

/**
 * Interface class to use for integration of sDPN with Joana.
 *
 * @author Benedikt Nordhoff
 */
class DPN4IFCAnalysis(cg: CallGraph, pa: PointerAnalysis) {
  type MDPN = MonitorDPN[GlobalState, StackSymbol, DPNAction, InstanceKey]
  var possibleLocks: Set[InstanceKey] = null
  var lockOrigins: Map[InstanceKey, Set[CGNode]] = null
  var waitMap: Map[CGNode, scala.collection.Set[InstanceKey]] = null
  var includeLockLocations = true
  var uniqueInstances: Set[InstanceKey] = null

  var lockFilter: InstanceKey => Boolean = {
    x: InstanceKey =>
      ClassLoaderReference.Application.equals(x.getConcreteType().getClassLoader().getReference())
  }

  /**
   * Initialize this analysis by calculating possible locks and the wait map.
   * @throws OperationCanceledException if pm.isCanceled.
   */
  def init(pm0: IProgressMonitor = null) {
    var pm = pm0
    if (pm == null)
      pm = new NullProgressMonitor()
    try {
      pm.beginTask("Initialyizing DPN analysis", 5)

      check(pm)
      pm.subTask("Identifying unique instances")
      val ui = UniqueInstanceLocator.instances(cg, pa)
      uniqueInstances = ui
      pm.worked(1)

      check(pm)
      pm.subTask("Identifying lock usages")
      val loi = LockWithOriginLocator.instances(cg, pa)
      lockOrigins = loi.filterKeys(ui)
      possibleLocks = lockOrigins.keySet
      pm.worked(1)

      check(pm)
      pm.subTask("Locating wait() calls")
      val wmc = new WaitMap(new MyPreAnalysis(cg, pa), possibleLocks)
      waitMap = wmc.waitMap
      pm.worked(1)
    } finally { pm.done }
  }

  /**
   * Generate a Monitor DPN which models all nodes from which a node of pruneSet can be reached.
   * @param pruneSet A set of interesting nodes
   * @return a Monitor DPN
   */
  def genMDPN(pruneSet: Set[CGNode]): MDPN = {
    var ss0 = pruneSet
    if (includeLockLocations) {
      for ((ik, nodes) <- lockOrigins; node <- nodes)
        if (lockFilter(ik) && !waitMap(node)(ik))
          ss0 += node
    }
    val prea = new MyPreAnalysis(cg, pa) with BackwardSliceFilter {
      override def initialSet = ss0
      override def safeLock(ik: InstanceKey, node: CGNode) = possibleLocks(ik) && lockFilter(ik) && !waitMap(node)(ik)
    }
    val dpnFac = new MonitorDPNFactory(prea)
    return dpnFac.getDPN
  }

  /**
   * @param writePos
   * @param readPos
   * @param pm0
   * @return
   */
  def runWeakCheck(writePos: StackSymbol, readPos: StackSymbol, pm0: IProgressMonitor = null): Boolean = {
    val (td, bu) = genWeakAutomata(writePos, readPos, pm0)
    val icheck = new IntersectionEmptinessCheck(td, bu) { override val name = "ifccheck" }
    return XSBRunner.runCheck(icheck,pm0)
  }

  /**
   * @param writePos
   * @param readPos
   * @param pm0
   * @return
   */
  def genWeakAutomata(writePos: StackSymbol, readPos: StackSymbol, pm0: IProgressMonitor = null): (ScriptTreeAutomata, ScriptTreeAutomata) = {
    var pm = pm0
    if (pm == null)
      pm = new NullProgressMonitor()
    try {
      pm beginTask ("Generating tree automata for emptiness check", 2)

      check(pm)
      pm subTask "Generating Monitor DPN"
      val dpn = genMDPN(Set(readPos.node, writePos.node))
      pm worked 1

      check(pm)
      pm subTask "Setting up tree automata"

      //The automata representing the lock insensitive  control flow of the DPN 
      val cflow = new MDPN2CutTA(dpn)

      //An top down automata calculating lock sets to identify reentrant operations
      val fwdLS = new FwdCutLockSet("fwdLS", dpn.locks.size)

      //The control flow and the lock sets are evaluated top down
      val topDown = new IntersectionTA(cflow, fwdLS) {
        override val name = "flowls"
      }

      //Now we build an bottom up tree automata which checks for conflicts and
      //assures that the execution tree can be scheduled lock sensitive

      //the stack symbols where a variable is read/written
      var writeStack = Set(writePos)
      var readStack = Set(readPos)

      //Automatons which check for a conflict      
      val ifwrite = new IFlowWriting("ifwrite", writeStack)
      val ifread = new IFlowReading("ifread", readStack)

      val conflict = new IntersectionTA(ifwrite, ifread) { override val name = "ifconf" }

      //A automata which ensures that the tree is cut well formed
      val cwf = new CutWellFormed("cwf")
      val cwfc = new IntersectionTA(conflict, cwf) {
        override val name = "cwfc"
      }

      //Automatons which ensure lock sensitive schedulability      

      val relstr = new CutReleaseStructTA("crs", dpn.locks.size)
      val inter1 = new IntersectionTA(cwfc, relstr) {
        override val name = "crf"
      }

      val lockTA = new CutAcqStructComplTA("compacq", dpn.locks.size)
      val inter2 = new IntersectionTA(inter1, lockTA) {
        override val name = "craf"
      }

      val lockPreTA = new CutAcqStructPrecutTA("precutacq", dpn.locks.size)
      val bottomUp = new IntersectionTA(inter2, lockPreTA)

      pm worked 1

      return (topDown, bottomUp)

    } finally { pm done }

  }

  /**
   * Convert a CGNode plus BasicBlock index into the StackSymbol(node,bbNr,0) representing
   * the entry point of the basic block within the node.
   *
   * @param node A node from the call graph
   * @param bbNr A basic block number from the control flow graph corresponding to node
   * @return StackSymbol(node,bbNr,0)
   */
  def getSS4NodeAndBB(node: CGNode, bbNr: Int) = StackSymbol(node, bbNr, 0)

  /**
   * Convert a CGNode into the StackSymbol(node,0,0) representing the entry point of that method.
   *
   * @param node A node from the call graph
   * @return StackSymbol(node,0,0)
   */
  def getSS4Node(node: CGNode) = StackSymbol(node, 0, 0)

  /**
   * Obtains the StackSymbol representing the point just *before* the given
   * instruction in the corresponding basic block
   *
   * @param node A CGNode
   * @param instructionIndex An index of an instruction within the array node.getIR.getInstructions
   * @return A corresponding stack symbol
   */
  def getSS4NodeAndIndex(node: CGNode, instructionIndex: Int): StackSymbol = {
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
    return StackSymbol(node, bb.getNumber, index)
  }

  private def check(pm: IProgressMonitor) {
    if (pm.isCanceled())
      throw new org.eclipse.core.runtime.OperationCanceledException();
  }

}