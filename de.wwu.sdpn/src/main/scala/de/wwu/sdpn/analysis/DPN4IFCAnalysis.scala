package de.wwu.sdpn.analysis
import scala.collection.JavaConversions.asScalaIterator

import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.types.ClassLoaderReference
import com.ibm.wala.util.MonitorUtil.IProgressMonitor
import com.ibm.wala.util.MonitorUtil.beginTask
import com.ibm.wala.util.MonitorUtil.done
import com.ibm.wala.util.MonitorUtil.worked
import com.ibm.wala.util.CancelException

import de.wwu.sdpn.dpn.explicit.monitor.MonitorDPN
import de.wwu.sdpn.dpn.explicit.monitor.MonitorDPNFactory
import de.wwu.sdpn.dpn.explicit.DPNAction
import de.wwu.sdpn.dpn.explicit.GlobalState
import de.wwu.sdpn.dpn.explicit.StackSymbol
import de.wwu.sdpn.ta.prolog.cuts.CutAcqStructComplTA
import de.wwu.sdpn.ta.prolog.cuts.CutAcqStructPrecutTA
import de.wwu.sdpn.ta.prolog.cuts.CutReleaseStructTA
import de.wwu.sdpn.ta.prolog.cuts.CutWellFormed
import de.wwu.sdpn.ta.prolog.cuts.FwdCutLockSet
import de.wwu.sdpn.ta.prolog.cuts.IFlowReading
import de.wwu.sdpn.ta.prolog.cuts.IFlowWriting
import de.wwu.sdpn.ta.prolog.cuts.MDPN2CutTA
import de.wwu.sdpn.ta.IntersectionEmptinessCheck
import de.wwu.sdpn.ta.IntersectionTA
import de.wwu.sdpn.ta.ScriptTreeAutomata
import de.wwu.sdpn.util.wala.SubProgressMonitor
import de.wwu.sdpn.util.BackwardSliceFilter
import de.wwu.sdpn.util.LockWithOriginLocator
import de.wwu.sdpn.util.UniqueInstanceLocator
import de.wwu.sdpn.util.WaitMap

/**
 * Interface class to use for integration of sDPN with Joana.
 *
 * After creating an instance of this class the `init` method should be called to
 * perform preanalyses which will compute information used in subsequent interference
 * analyses.
 *
 * An interference analysis can than be performed by using the `runWeakCheck` method.
 *
 *
 * Some facts:
 *
 *  - The generated DPN uses `getSS4Node(cg.getFakeRootNode)` as starting configuration.
 *  - Calls to `java.lang.Thread.start()` are modeled as new processes in the DPN.
 *  - The call graph is pruned for every new analysis by only considering interesting nodes.
 *   - The nodes containing read and write positions are ''interesting''.
 *   - If `includeLockLocations` is true than all nodes containing possible lock usages are ''interesting'' too.
 *   - All nodes calling ''interesting'' nodes are ''interesting''.
 *  - The set of used locks can be configured by setting the variable `lockFilter`.
 *  The default accepts only locks corresponding to the class loader ''Application''.
 *   - Currently at most eight locks may be used on a 64bit System as the acquisition graph is modeled as
 *   a bit string.
 *  - The XSB-executable (obtainable from [[http://xsb.sf.net XSB web site]]) and a directory with write permission
 *  where temporary files will be stored need to be specified in the file `sdpn.properties`
 *  which needs to be on class path during execution.
 *
 *
 * @param cg A call graph representing the control flow of a program
 * @param pa The associated pointer analysis.
 *
 * @author Benedikt Nordhoff
 */
class DPN4IFCAnalysis(cg: CallGraph, pa: PointerAnalysis) {
  type MDPN = MonitorDPN[GlobalState, StackSymbol, DPNAction, InstanceKey]

  protected var possibleLocks: Set[InstanceKey] = null
  protected var lockUsages: Map[InstanceKey, Set[CGNode]] = null
  protected var waitMap: Map[CGNode, scala.collection.Set[InstanceKey]] = null
  protected var includeLockLocations = true
  protected var uniqueInstances: Set[InstanceKey] = null
  protected var lockFilter: InstanceKey => Boolean = {
    x: InstanceKey =>
      ClassLoaderReference.Application.equals(x.getConcreteType().getClassLoader().getReference())
  }

  /**
   * Should locations of used locks be factored in when pruning the call graph for DPN generation.
   * This increases precision and cost of the analysis.
   * @param value The new value for includeLockLocations.
   */
  def setIncludeLockLocations(value: Boolean) { includeLockLocations = value }

  /**
   * Set a filter which decides for abstractable locks whether they should
   * be considered for DPN generation. The default value is
   * {{{
   * x: InstanceKey =>
   *   ClassLoaderReference.Application.equals(x.getConcreteType().getClassLoader().getReference())
   * }}}
   * This can be implemented in Java by extending `scala.Function1<InstanceKey,Boolean>` and
   * implementing the `Boolean apply(InstanceKey ik)` method.
   *
   * @param value A new filter used to select locks.
   */
  def setLockFilter(value: InstanceKey => Boolean) { lockFilter = value }

  /**
   * @return The set of instance keys which have been identified to be unique
   * in the sense that there exists at most one concrete instance
   */
  def getUniqueInstances = uniqueInstances

  /**
   * @return The set of possible locks.
   */
  def getPossibleLocks = possibleLocks

  /**
   * @return The wait map, mapping a `CGNode` to a set of instance keys on which a `wait()` call may occur within.
   */
  def getWaitMap = waitMap

  /**
   * @return A map which maps `InstanceKeys` to a set of `CGNodes` where they are used as locks.
   * If includeLockLocations is set to true these will be included for pruning.
   */
  def getLockUsages = lockUsages

  /**
   * @return the value of includeLockLocations
   */
  def getIncludeLockLocations = includeLockLocations

  /**
   * @return the currently used lock filter
   */
  def getLockFilter = lockFilter

  /**
   * Initialize this analysis by calculating possible locks and the wait map.
   * @param pm0 The progress monitor used to report progress, with default value null.
   */
  def init(pm: IProgressMonitor = null) {

    try {
      beginTask(pm, "Initialyizing DPN-based analyses", 3)

      subTask(pm, "Identifying unique instances")
      val ui = UniqueInstanceLocator.instances(cg, pa)
      uniqueInstances = ui
      worked(pm, 1)

      subTask(pm, "Identifying lock usages")
      val loi = LockWithOriginLocator.instances(cg, pa)
      lockUsages = loi.filterKeys(ui)
      possibleLocks = lockUsages.keySet
      worked(pm, 1)

      subTask(pm, "Locating wait() calls")
      val wmc = new WaitMap(new MyPreAnalysis(cg, pa), possibleLocks)
      waitMap = wmc.waitMap
      worked(pm, 1)
    } finally { done(pm) }
  }

  /**
   * Generate a Monitor DPN which models all nodes from which a node of pruneSet can be reached.
   * @param pruneSet A set of interesting nodes
   * @return a Monitor DPN
   */
  def genMDPN(pruneSet: Set[CGNode]): MDPN = {
    var ss0 = pruneSet
    if (includeLockLocations) {
      for ((ik, nodes) <- lockUsages; node <- nodes)
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
   * Run an interference check between '''writePos''' and '''readPos''' assuming weak updates or the absence of killing definitions.
   * This means we check if it is possible to reach '''readPos''' after reaching '''writePos'''.
   *
   * Throws an '''IOException''' if an error occurs while interacting with the XSB process
   * (e.g. more than five locks on a 32bit system)
   *
   * Throws an  '''IllegalArgumentException''' if more than eight locks are used.
   *
   * Throws an '''OperationCanceledException''' if `pm0.isCanceled` is true.
   *
   * @param writePos A stack symbol representing a point where a variable is written.
   * @param readPos A stack symbol representing a point where the written variable is read.
   * @param pm0 A progress monitor used to report progress with default value null.
   * @return True if '''readPos''' can be reached after reaching '''writePos''' in the DPN-model
   *
   */
  def runWeakCheck(writePos: StackSymbol, readPos: StackSymbol, pm: IProgressMonitor = null): Boolean = {
    beginTask(pm, "Running DPN-based interference check", 3)
    try {
      val pm1 = new SubProgressMonitor(pm, 1)
      val (td, bu) = genWeakAutomata(writePos, readPos, pm1)
      val icheck = new IntersectionEmptinessCheck(td, bu) { override val name = "ifccheck" }
      val pm2 = new SubProgressMonitor(pm, 2)
      return !XSBRunner.runCheck(icheck, pm2)
    } finally {
      done(pm)
    }

  }

  /**
   * Helper method used by runWeakCheck.
   *
   * @param writePos A stack symbol representing a point where a variable is written.
   * @param readPos A stack symbol representing a point where the written variable is read.
   * @param pm0 A progress monitor used to report progress.
   * @return Two [[de.wwu.sdpn.ta.ScriptTreeAutomata]] the first one to be evaluated top down the second one to be evaluated bottom up
   * by an intersection emptiness test.
   */
  protected def genWeakAutomata(writePos: StackSymbol, readPos: StackSymbol, pm: IProgressMonitor = null): (ScriptTreeAutomata, ScriptTreeAutomata) = {

    try {
      beginTask(pm, "Generating automata for interference check", 2)

      subTask(pm, "Generating Monitor DPN")
      val dpn = genMDPN(Set(readPos.node, writePos.node))
      worked(pm, 1)

      subTask(pm, "Generating tree automata")

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

      worked(pm, 1)

      return (topDown, bottomUp)

    } finally { done(pm) }

  }

  /**
   * Convert a CGNode plus BasicBlock index into the StackSymbol(node,bbNr,0) representing
   * the entry point of the basic block within the control flow graph corresponding to the node.
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
   * Obtains the StackSymbol representing the point just ''before'' the
   * instruction corresponding to the given instructionIndex.
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

  def subTask(monitor: IProgressMonitor, task: String) {
    if (monitor != null) {
      monitor.subTask(task);
      if (monitor.isCanceled()) {
        throw CancelException.make("cancelled in " + task);
      }
    }
  }

}