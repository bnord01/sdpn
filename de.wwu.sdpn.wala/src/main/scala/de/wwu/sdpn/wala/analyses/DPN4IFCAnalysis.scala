package de.wwu.sdpn.wala.analyses
import java.io.IOException
import scala.annotation.implicitNotFound
import scala.collection.JavaConversions.asScalaIterator
import com.ibm.wala.classLoader.IClass
import com.ibm.wala.ipa.callgraph.propagation.ConstantKey
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.types.ClassLoaderReference
import com.ibm.wala.util.CancelException
import de.wwu.sdpn.core.analyses.TwoSetReachability
import de.wwu.sdpn.core.dpn.monitor.MonitorDPN
import de.wwu.sdpn.core.ta.xsb.cuts.CutAcqStructComplTA
import de.wwu.sdpn.core.ta.xsb.cuts.CutAcqStructPrecutTA
import de.wwu.sdpn.core.ta.xsb.cuts.CutReleaseStructTA
import de.wwu.sdpn.core.ta.xsb.cuts.CutWellFormed
import de.wwu.sdpn.core.ta.xsb.cuts.FwdCutLockSet
import de.wwu.sdpn.core.ta.xsb.cuts.IFlowReading
import de.wwu.sdpn.core.ta.xsb.cuts.IFlowWriting
import de.wwu.sdpn.core.ta.xsb.cuts.MDPN2CutTA
import de.wwu.sdpn.core.ta.xsb.IntersectionEmptinessCheck
import de.wwu.sdpn.core.ta.xsb.IntersectionTA
import de.wwu.sdpn.core.ta.xsb.ScriptTreeAutomata
import de.wwu.sdpn.core.ta.xsb.XSBInterRunner
import de.wwu.sdpn.wala.dpngen.symbols.DPNAction
import de.wwu.sdpn.wala.dpngen.symbols.GlobalState
import de.wwu.sdpn.wala.dpngen.symbols.StackSymbol
import de.wwu.sdpn.wala.dpngen.MonitorDPNFactory
import de.wwu.sdpn.wala.util.BackwardSliceFilter
import de.wwu.sdpn.wala.util.LockWithOriginLocator
import de.wwu.sdpn.wala.util.UniqueInstanceLocator
import de.wwu.sdpn.wala.util.WaitMap
import com.ibm.wala.ssa.SSAPutInstruction
import com.ibm.wala.ssa.SSAGetInstruction
import de.wwu.sdpn.wala.util.FieldUtil
import de.wwu.sdpn.core.ta.xsb.cuts.DPNAnnotater
import de.wwu.sdpn.core.dpn.monitor._
import de.wwu.sdpn.wala.dpngen.symbols.SSAAction
import de.wwu.sdpn.core.ta.xsb.cuts.IFlowNoOverwrite
import de.wwu.sdpn.core.util.IProgressMonitor
import de.wwu.sdpn.core.util.SubProgressMonitor
import de.wwu.sdpn.core.util.ProgressMonitorUtil._
import de.wwu.sdpn.core.ta.xsb.HasTermRepresentation
import com.codahale.logula.Logging
import com.ibm.wala.classLoader.IField
import de.wwu.sdpn.wala.ri.Isolated
import de.wwu.sdpn.wala.ri.RISymbol
import de.wwu.sdpn.wala.ri.NotIsolated
import de.wwu.sdpn.wala.ri.Summary
import de.wwu.sdpn.wala.ri.RIDPN
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
class DPN4IFCAnalysis(cg: CallGraph, pa: PointerAnalysis) extends Logging {
    type MDPN = MonitorDPN[GlobalState, StackSymbol, DPNAction, InstanceKey]

    protected var possibleLocks: Set[InstanceKey] = null
    protected var lockUsages: Map[InstanceKey, Set[CGNode]] = null
    protected var unsafeLockUsages: Map[InstanceKey, Set[CGNode]] = null
    protected var waitMap: Map[CGNode, scala.collection.Set[InstanceKey]] = null
    protected var includeLockLocations = true
    protected var includeOverwriteLocations = true
    protected var uniqueInstances: Set[InstanceKey] = null
    protected var randomIsolation = false

    protected var lockFilter: InstanceKey => Boolean = defaultLockFilter

    protected lazy val cha = cg.getClassHierarchy()

    protected var wm: WaitMap = null

    protected def defaultLockFilter: InstanceKey => Boolean = {
        case x: ConstantKey[_] =>
            x.getValue() match {
                case x: IClass => ClassLoaderReference.Application.equals(x.getClassLoader().getReference())
                case _         => false
            }
        case x: InstanceKey =>
            ClassLoaderReference.Application.equals(x.getConcreteType().getClassLoader().getReference())
        case _ => false
    }

    /**
     * Should locations of used locks be factored in when pruning the call graph for DPN generation.
     * This increases precision and cost of the analysis. This doesn't introduce new locks to the DPN.
     * @param value The new value for includeLockLocations.
     */
    def setIncludeLockLocations(value: Boolean) { includeLockLocations = value }

    /**
     * Should locations of other writes be factored in when pruning the call graph for DPN generation.
     * This increases precision and cost of the analysis.
     * @param value The new value for includeOverwriteLocations.
     */
    def setIncludeOverwriteLocations(value: Boolean) { includeOverwriteLocations = value }

    /**
     * Set a filter which decides for abstractable locks whether they should
     * be considered for DPN generation.  This doesn't effect the identification of possible locks.
     * The default value is
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
     * Reset the lock filter to it's default value.
     */
    def resetLockFilter() {
        lockFilter = defaultLockFilter
    }

    /**
     * Set the lock filter to ignore all locks.
     */
    def setLockInsensitive() {
        lockFilter = _ => false
    }

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
     * @return the value of includeOverwriteLocations
     */
    def getIncludeOverwriteLocations = includeOverwriteLocations

    /**
     * @return the currently used lock filter
     */
    def getLockFilter = lockFilter

    /**
     * Set whether random isolation should be applied to the object corresponding to the field.
     */
    def setRandomIsolation(value: Boolean) {
        randomIsolation = value
    }

    def getRandomIsolation = randomIsolation

    /**
     * Initialize this analysis by calculating possible locks and the wait map.
     * @param pm0 The progress monitor used to report progress, with default value null.
     */
    def init(pm: IProgressMonitor = null) {
        log.trace("Entered init")
        try {
            log.info("Initializing DPN-based analysis")
            log.debug("""Settings are
! lockLocations:     %s
! randomIsolation:   %s
! defaultLockFilter: %s""", includeLockLocations, randomIsolation, lockFilter == defaultLockFilter)
            beginTask(pm, "Initialyizing DPN-based analyses", 3)

            log.info("Identifying unique instances")
            subTask(pm, "Identifying unique instances")
            val ui = UniqueInstanceLocator.instances(cg, pa)
            uniqueInstances = ui
            log.info("Identified %d unique instances", ui.size)
            if (log.isDebugEnabled)
                log.debug("Uniques instances are: %n! %s", ui.mkString(",\n! "))

            worked(pm, 1)

            log.info("Identifying lock usages")
            subTask(pm, "Identifying lock usages")
            unsafeLockUsages = LockWithOriginLocator.instances(cg, pa)
            lockUsages = unsafeLockUsages.filterKeys(x => UniqueInstanceLocator.isConstantKey(x) || ui(x))
            possibleLocks = lockUsages.keySet
            log.info("Identified %d lock usages (also unsafe)", unsafeLockUsages.size)
            if (log.isDebugEnabled)
                log.debug("Lock usages are: %n! %s", unsafeLockUsages.mkString(",\n! "))
            log.info("Identified %d possible locks", possibleLocks.size)
            if (log.isDebugEnabled)
                log.debug("Possible locks are: %n! %s", possibleLocks.mkString(",! \n"))
            worked(pm, 1)

            log.info("Identifying wait() calls")
            subTask(pm, "Locating wait() calls")
            val wmLocks = if (randomIsolation) unsafeLockUsages.keySet else possibleLocks
            wm = new WaitMap(new MyPreAnalysis(cg, pa), wmLocks)
            waitMap = wm.waitMap
            log.info("Done identifying wait() calls")
            worked(pm, 1)
        } finally { done(pm) }
        log.trace("Exiting init")
    }

    /**
     * Generate a Monitor DPN which models all nodes from which a node of pruneSet can be reached.
     * @param pruneSet A set of interesting nodes
     * @return a Monitor DPN
     */
    def genMDPN(pruneSet: Set[CGNode]): MDPN = {
        log.trace("Entered genMDPN")
        var ss0 = pruneSet
        if (includeLockLocations) {
            log.info("Identifying relevant locks")
            // Nodes from which the pruning criteria is reachable
            ss0 = BackwardSliceFilter.backwardSlice(cg, pruneSet)
            // Collect locks which are used in these nodes and add all nodes in which they are also used 
            val lockNodes = for {
                (ik, nodes) <- lockUsages if !(nodes intersect ss0 isEmpty); // a lock with is used in an interesting node
                node <- nodes if (lockFilter(ik) && !waitMap(node)(ik)) // without an wati
            } yield node
            ss0 ++= lockNodes
        }

        val prea = new MyPreAnalysis(cg, pa) with BackwardSliceFilter {
            override def initialSet = ss0
            override def safeLock(ik: InstanceKey, node: CGNode) = possibleLocks(ik) && lockFilter(ik) && !waitMap(node)(ik)
        }
        val dpnFac = new MonitorDPNFactory(prea, false)
        val dpn = dpnFac.getDPN
        log.debug("Generated MDPN size: %s, locks %s", dpn.transitions.size, dpn.locks.size)
        log.trace("Exiting genMDPN")
        return dpn
    }

    /**
     * Run an interference check between '''writePos''' and '''readPos''' assuming weak updates or the absence of killing definitions.
     * This means we check if it is possible to reach '''readPos''' after reaching '''writePos'''.
     *
     * Throws an '''IOException''' if an error occurs while interacting with the XSB process
     * (e.g. more than five locks on a 32bit system)
     *
     * Throws an '''IllegalArgumentException''' if more than eight locks are used.
     *
     * Throws a '''CancelException''' or '''RuntimeException''' if `pm0.isCanceled` is set to true during the execution.
     *
     * @param writePos A stack symbol representing a point where a variable is written.
     * @param readPos A stack symbol representing a point where the written variable is read.
     * @param pm0 A progress monitor used to report progress with default value null.
     * @return True if '''readPos''' can be reached after reaching '''writePos''' in the DPN-model.
     *
     */
    @throws(classOf[IOException])
    @throws(classOf[IllegalArgumentException])
    @throws(classOf[RuntimeException])
    @throws(classOf[CancelException])
    def mayHappenSuccessively(writePos: StackSymbol, readPos: StackSymbol, pm: IProgressMonitor = null, timeout: Long = 0): Boolean = {
        log.trace("Entered mayHappenSuccessively")
        beginTask(pm, "Running DPN-based interference check", 3)
        try {
            val pm1 = new SubProgressMonitor(pm, 1)
            val (td, bu) = genWeakAutomata(writePos, readPos, pm1)
            val icheck = new IntersectionEmptinessCheck(td, bu) { override val name = "ifccheck" }
            val pm2 = new SubProgressMonitor(pm, 2)
            log.debug("Calling emptiness check")
            val res = !XSBInterRunner.runCheck(icheck, pm2, timeout)
            log.debug("Empiness check returned %s (%sflow possible)", !res, if (res) "" else "no ")
            log.trace("Exiting mayHappenSuccessively")
            return res
        } finally {
            done(pm)
        }

    }

    /**
     * Run a may happen in parallel check between '''posOne''' and '''posTwo'''.
     * Note: If '''posOne == posTwo''' two processes must reach '''posOne'''.
     *
     * Throws an '''IOException''' if an error occurs while interacting with the XSB process
     * (e.g. more than five locks on a 32bit system)
     *
     * Throws an '''IllegalArgumentException''' if more than eight locks are used.
     *
     * Throws a '''CancelException''' or '''RuntimeException''' if `pm0.isCanceled` is set to true during the execution.
     *
     * @param posOne A stack symbol representing a point where a variable is written.
     * @param posTwo A stack symbol representing a point where the written variable is read.
     * @param pm0 A progress monitor used to report progress with default value null.
     * @return True if '''posOne''' and '''posTwo''' may happen in parallel in the DPN-model.
     *
     */
    @throws(classOf[IOException])
    @throws(classOf[IllegalArgumentException])
    @throws(classOf[RuntimeException])
    @throws(classOf[CancelException])
    def mayHappenInParallel(posOne: StackSymbol, posTwo: StackSymbol, pm: IProgressMonitor = null, timeout: Long = 0): Boolean = {
        log.trace("Entered mayHappenInParallel")
        beginTask(pm, "Running DPN-based MHP check", 5)
        try {
            subTask(pm, "Generating MonitorDPN")
            val dpn = genMDPN(Set(posOne.node, posTwo.node))
            worked(pm, 1)
            val lockSens = !dpn.locks.isEmpty

            subTask(pm, "Generating tree automata")
            val (td, bu) = TwoSetReachability.genAutomata(dpn, Set(posOne), Set(posTwo), lockSens)
            val icheck = new IntersectionEmptinessCheck(td, bu) { override val name = "mhpcheck" }
            worked(pm, 1)
            val pm2 = new SubProgressMonitor(pm, 3)
            log.debug("Calling emptienss check")
            val res = !XSBInterRunner.runCheck(icheck, pm2, timeout)
            log.debug("Emptiness check returned %s (may %shappen in parellel)", !res, if (res) "" else "not ")
            log.trace("Exiting mayHappenInParallel")
            return res
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
            beginTask(pm, "Generating weak automata for interference check", 2)
            log.info("Generating MonitorDPN for interference check")
            subTask(pm, "Generating MonitorDPN")
            val dpn = genMDPN(Set(readPos.node, writePos.node))
            worked(pm, 1)

            log.info("DPN generated size: %d, locks: %d", dpn.getTransitions.size, dpn.locks.size)

            val lockSens = !dpn.locks.isEmpty

            subTask(pm, "Generating tree automata")
            log.info("Generating tree automata")

            //The automata representing the lock insensitive  control flow of the DPN 
            val cflow = new MDPN2CutTA(dpn)

            val topDown: ScriptTreeAutomata = if (lockSens) {
                //An top down automata calculating lock sets to identify reentrant operations
                val fwdLS = new FwdCutLockSet("fwdLS", dpn.locks.size)

                //The control flow and the lock sets are evaluated top down
                new IntersectionTA(cflow, fwdLS) {
                    override val name = "flowls"
                }
            } else {
                cflow
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

            val bottomUp: ScriptTreeAutomata = if (lockSens) {

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
                new IntersectionTA(inter2, lockPreTA)
            } else { cwfc }

            worked(pm, 1)

            log.info("Tree automata generated")

            return (topDown, bottomUp)

        } finally { done(pm) }

    }

    def mayFlowFromTo(writeNode: CGNode, writeIdx: Int, readNode: CGNode, readIdx: Int, pm: IProgressMonitor = null, timeout: Long = 0): Boolean = {
        log.debug("Entered mayFlowFromTo")
        log.debug("writeNode: (%s,%d), readNode: (%s,%d), timeout: %d", writeNode, writeIdx, readNode, readIdx, timeout)
        beginTask(pm, "Running DPN-based interference check", 6)
        try {
            log.info("Running DPN-based interference check")
            subTask(pm, "Identifying fields")
            val writePos = getSS4NodeAndIndex(writeNode, writeIdx, true)
            val readPos = getSS4NodeAndIndex(readNode, readIdx)

            val wi = writeNode.getIR().getInstructions()(writeIdx)
            val ri = readNode.getIR().getInstructions()(readIdx)

            val isRegularFieldAccess = wi.isInstanceOf[SSAPutInstruction] && ri.isInstanceOf[SSAGetInstruction]

            // this must be some kind of array field or something
            if (!isRegularFieldAccess) {
                log.info("Non regular field, running weak check")
                val pm1 = new SubProgressMonitor(pm, 4)
                val result = mayHappenSuccessively(writePos, readPos, pm1, timeout)
                return result
            }
            log.info("Regular field, identifiying field")
            require(wi.isInstanceOf[SSAPutInstruction], "Write instruction isn't instance of SSAPutInstruction: " + wi)
            require(ri.isInstanceOf[SSAGetInstruction], "Read instruction isn't instance of SSAGetInstruction: " + ri)

            val writeInstr = wi.asInstanceOf[SSAPutInstruction]
            val readInstr = ri.asInstanceOf[SSAGetInstruction]

            val readField = cha.resolveField(readInstr.getDeclaredField())
            val writeField = cha.resolveField(writeInstr.getDeclaredField())

            require(readField == writeField, "Instructions refer to differently named fields read: " + readField + " write: " + writeField)
            val field = readField
            log.info("Field: %s", field)

            val writeObs = FieldUtil.getIKS4FieldInstr(pa, writeNode, writeInstr)
            val readObs = FieldUtil.getIKS4FieldInstr(pa, readNode, readInstr)

            val interObs = writeObs intersect readObs
            log.debug("Abstract objects for write:       %s", writeObs)
            log.debug("Abstract objects for read:        %s", readObs)
            log.debug("Abstract objects in intersection: %s", interObs)

            //no shared instance keys means no flow possible, but we would wan't joana to check for this!
            require(!(interObs isEmpty), "No shared instance keys for field found!")

            val uniqueInterObs = interObs filter (key => uniqueInstances(key) || UniqueInstanceLocator.isConstantKey(key))
            if ((interObs size) > 1 || (uniqueInterObs isEmpty)) {
                if (log.isDebugEnabled)
                    log.debug("No safe killings, running weak check. %n! interObs: %s %n! uniqueInterObs: %s", interObs.mkString("\n!\t", "\n!\t", ""),
                        uniqueInterObs.mkString("\n!\t", "\n!\t", ""))

                // Check if random isolation can be used
                val wvn = writeNode.getIR().getSymbolTable().getParameter(0)
                val rvn = readNode.getIR().getSymbolTable().getParameter(0)
                if (!readNode.getMethod().isStatic() && !writeNode.getMethod().isStatic() &&
                    wvn == writeInstr.getRef() && rvn == readInstr.getRef()) { // read and write on thispointer
                    log.info("Applying random isolation for field accesses to this-pointer")
                    for (fieldObj <- interObs) {
                        if (this.runRICheckOnInstanceKey(writePos, readPos, fieldObj, field, uniqueInterObs(fieldObj), pm, timeout))
                            return true
                    }
                    return false
                }
                val pm1 = new SubProgressMonitor(pm, 4)

                // There may be multiple instances which correspond to this interference we can't interpret any killing definitions
                log.info("No single unique object in intersection and no possibility for random isolation -> running weak check")
                return mayHappenSuccessively(writePos, readPos, pm1, timeout)
            }

            assert(uniqueInterObs.size == 1, "I'm with stupid. I've written rubbish above.")

            val fieldObj = uniqueInterObs head

            log.info("Single unique object running strong check")
            return runStrongCheckOnInstanceKey(writePos, readPos, fieldObj, field, pm, timeout)

        } finally {
            done(pm)
        }
    }

    /**
     * Helper method used by mayFlowFromTo
     *
     * @param writePos A stack symbol representing a point where a variable is written.
     * @param readPos A stack symbol representing a point where the written variable is read.
     * @param pm0 A progress monitor used to report progress.
     * @return Two [[de.wwu.sdpn.ta.ScriptTreeAutomata]] the first one to be evaluated top down the second one to be evaluated bottom up
     * by an intersection emptiness test.
     */
    protected def genStrongAutomata[SS <% HasTermRepresentation, A](dpn: MonitorDPN[GlobalState, SS, A, InstanceKey], writePos: SS, readPos: SS, annotater: DPNAnnotater[GlobalState, SS, A], pm: IProgressMonitor = null): (ScriptTreeAutomata, ScriptTreeAutomata) = {

        try {
            beginTask(pm, "Generating automata for interference check", 1)

            if (log.isTraceEnabled) {
                val rs = for (r <- dpn.transitions; if r.inSymbol == readPos) yield r.action
                val ws = dpn.transitions.collect { case r @ BaseRule(_, _, a, _, s) if s == writePos => a }
                log.trace("Incoming transitions for write sack symbol: %s", ws)
                log.trace("Outgoing transitions for read sack symbol:  %s", rs)
            }

            val lockSens = !dpn.locks.isEmpty

            subTask(pm, "Generating tree automata")

            //The automata representing the lock insensitive  control flow of the DPN 
            val cflow = new MDPN2CutTA(dpn, annotater = annotater)

            val topDown: ScriptTreeAutomata = if (lockSens) {
                //An top down automata calculating lock sets to identify reentrant operations
                val fwdLS = new FwdCutLockSet("fwdLS", dpn.locks.size)

                //The control flow and the lock sets are evaluated top down
                new IntersectionTA(cflow, fwdLS) {
                    override val name = "flowls"
                }
            } else {
                cflow
            }

            //Now we build an bottom up tree automata which checks for conflicts and
            //assures that the execution tree can be scheduled lock sensitive

            //the stack symbols where a variable is read/written
            var writeStack = Set(writePos)
            var readStack = Set(readPos)

            //Automatons which check for a conflict      
            val ifwrite = new IFlowWriting("ifwrite", writeStack)
            val ifread = new IFlowReading("ifread", readStack)
            val ifowr = new IFlowNoOverwrite("ifowr")

            val confl1 = new IntersectionTA(ifwrite, ifread, "ifreadwrite")
            val conflict = new IntersectionTA(ifowr, confl1, "ifconfl")

            //A automata which ensures that the tree is cut well formed
            val cwf = new CutWellFormed("cwf")
            val cwfc = new IntersectionTA(conflict, cwf) {
                override val name = "cwfc"
            }

            val bottomUp: ScriptTreeAutomata = if (lockSens) {

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
                new IntersectionTA(inter2, lockPreTA)
            } else { cwfc }

            worked(pm, 1)

            return (topDown, bottomUp)

        } finally { done(pm) }

    }

    protected def runRICheckOnInstanceKey(writePos: StackSymbol, readPos: StackSymbol, fieldObj: InstanceKey, field: IField, isUniqueField: Boolean = false, pm: IProgressMonitor = null, timeout: Long = 0): Boolean = {
        log.debug("Running random isolation based check on object: %s", fieldObj)

        log.debug("Identifying overwrites")

        val writes = FieldUtil.getFieldWrites(cg, pa, fieldObj, field) filter {
            case (node, instr) =>
                var res = false
                if (isUniqueField) { // It's a unique field, check whether it's referred to by this write
                    val owrObs = FieldUtil.getIKS4FieldInstr(pa, node, instr)
                    if (owrObs.size == 1 &&
                        owrObs(fieldObj) &&
                        field == cha.resolveField(instr.getDeclaredField()))
                        res = true
                }
                if (!node.getMethod().isStatic()) { // Some field check for random isolation.
                    val vn = node.getIR.getSymbolTable().getParameter(0)
                    if (vn == instr.getRef())
                        res = true
                }
                res
        }
        log.debug("Identified %d possible overwrites: %s", writes.size, writes)

        worked(pm, 1)

        subTask(pm, "Generating MonitorDPN")
        val dpn = genMDPN(writes.map(_._1) ++ Set(readPos.node, writePos.node))
        //            //Debug code explore DPN 
        //                        de.wwu.sdpn.core.gui.MonitorDPNView.show(dpn, true);
        //                        Thread.sleep(10000000)
        //            //End debug code

        val ridpn = new RIDPN(dpn, fieldObj, "fieldObj", pa, wm)
//        //Debug code explore RIDPN 
//        de.wwu.sdpn.core.gui.MonitorDPNView.show(ridpn, true);
//        Thread.sleep(10000000)
//        //End debug code
        log.trace("Generated RIDPN of size: %d, locks: %d", ridpn.transitions.size, ridpn.locks.size)
        worked(pm, 1)

        val pm1 = new SubProgressMonitor(pm, 1)

        val annot = getRIOverwriteAnnotator(fieldObj, field, isUniqueField)
        val iWritePos = ridpn.getIsolated(writePos)
        val iReadPos = ridpn.getIsolated(readPos)
        log.trace("New stack symbol for write: %s", iWritePos)
        log.trace("New stack symbol for read:  %s", iReadPos)

        val (td, bu) = genStrongAutomata(ridpn, iWritePos, iReadPos, annot, pm1)
        val icheck = new IntersectionEmptinessCheck(td, bu) { override val name = "ifccheck" }
        val pm2 = new SubProgressMonitor(pm, 3)

        log.info("Calling emptiness check")
        val res = !XSBInterRunner.runCheck(icheck, pm2, timeout)
        log.info("Emptiness check returned %s (%sflow possible)", !res, if (res) "" else "no ")
        return res
    }

    /**
     * Runs a strong may flow check on a given field on a given instance key.
     *
     * It's assumed that the instance key represents a unique object.
     * It checks whether writePos can be reached before readPos
     * without an intervening write to the given field.
     *
     */
    protected def runStrongCheckOnInstanceKey(writePos: StackSymbol, readPos: StackSymbol, fieldObj: InstanceKey, field: IField, pm: IProgressMonitor = null, timeout: Long = 0): Boolean = {
        log.debug("Running strong check on unique object: %s", fieldObj)

        log.debug("Identifying overwrites")

        val writes = FieldUtil.getFieldWrites(cg, pa, fieldObj, field) filter {
            case (node, instr) =>
                val owrObs = FieldUtil.getIKS4FieldInstr(pa, node, instr)
                (owrObs.size == 1 &&
                    owrObs(fieldObj) &&
                    field == cha.resolveField(instr.getDeclaredField()))
        }
        log.debug("Identified %d possible overwrites: %s", writes.size, writes)

        worked(pm, 1)

        subTask(pm, "Generating MonitorDPN")
        val dpn = genMDPN(writes.map(_._1) ++ Set(readPos.node, writePos.node))
        //            //Debug code explore DPN 
        //                        de.wwu.sdpn.core.gui.MonitorDPNView.show(dpn, true);
        //                        Thread.sleep(10000000)
        //            //End debug code
        worked(pm, 1)

        val pm1 = new SubProgressMonitor(pm, 1)

        val annot = getRegularOverwriteAnnotator(fieldObj, field)

        val (td, bu) = genStrongAutomata(dpn, writePos, readPos, annot, pm1)
        val icheck = new IntersectionEmptinessCheck(td, bu) { override val name = "ifccheck" }
        val pm2 = new SubProgressMonitor(pm, 3)

        log.info("Calling emptiness check")
        val res = !XSBInterRunner.runCheck(icheck, pm2, timeout)
        log.info("Emptiness check returned %s (%sflow possible)", !res, if (res) "" else "no ")
        return res

        /* Alternative using the new iterable analysis  
             	val pruneSet = Set(writeNode, readNode)
                val dpn = genMDPN(pruneSet)
                val owTrans = dpn.transitions.filter({
                    case BaseRule(_, StackSymbol(node, _, _), SSAAction(instr: SSAPutInstruction), _, _) =>
                        val owrObs = FieldUtil.getIKS4FieldInstr(pa, node, instr)
                        if (owrObs.size == 1 && owrObs(fieldObj) && instr.getDeclaredField().getName() == fieldName)
                            //TODO this is unsound when there are different fields with the same name as juergen talked about on the mailing list
                            true
                        else
                            false
                    case _ =>
                        false
                })

                val firstConf = Set(writePos)
                val confs = List((owTrans, Set(readPos)))
                val lockSens = !dpn.locks.isEmpty

                return de.wwu.sdpn.core.analyses.DPNReachability.runAIRCheck(dpn, firstConf, confs, true)
             */
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
     * instruction corresponding to the given instructionIndex or ''after'' if ''afterInstruction'' is true.
     *
     * @param node A CGNode
     * @param instructionIndex An index of an instruction within the array node.getIR.getInstructions
     * @return A corresponding stack symbol
     */
    def getSS4NodeAndIndex(node: CGNode, instructionIndex: Int): StackSymbol = getSS4NodeAndIndex(node, instructionIndex, false)

    /**
     * Obtains the StackSymbol representing the point just ''before'' the
     * instruction corresponding to the given instructionIndex or ''after'' if ''afterInstruction'' is true.
     *
     * @param node A CGNode
     * @param instructionIndex An index of an instruction within the array node.getIR.getInstructions
     * @return A corresponding stack symbol
     */
    def getSS4NodeAndIndex(node: CGNode, instructionIndex: Int, afterInstruction: Boolean): StackSymbol = {
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
        if (afterInstruction)
            index += 1
        return StackSymbol(node, bb.getNumber, index)
    }

    def getRegularOverwriteAnnotator(fieldObj: InstanceKey, field: IField) = {
        object FieldOverwriteAnnotater extends DPNAnnotater[GlobalState, StackSymbol, DPNAction] {
            override type RuleAnnotation = String
            override def annotateRule(rule: DPNRule[GlobalState, StackSymbol, DPNAction]): String = {
                rule match {
                    case BaseRule(_, StackSymbol(node, _, _), SSAAction(instr: SSAPutInstruction), _, _) =>
                        val owrObs = FieldUtil.getIKS4FieldInstr(pa, node, instr)
                        if (owrObs.size == 1 &&
                            owrObs(fieldObj) &&
                            field == cha.resolveField(instr.getDeclaredField()))
                            return "write"
                        else
                            return "none"
                    case _ =>
                        return "none"
                }
            }
        }
        FieldOverwriteAnnotater
    }
    def getRIOverwriteAnnotator(fieldObj: InstanceKey, field: IField, isUniqueField: Boolean = false) = {
        object FieldRIOverwriteAnnotater extends DPNAnnotater[GlobalState, RISymbol[InstanceKey, StackSymbol], DPNAction] {
            override type RuleAnnotation = String
            override def annotateRule(rule: DPNRule[GlobalState, RISymbol[InstanceKey, StackSymbol], DPNAction]): String = {
                val THEKEY = fieldObj // upper case identifyier for pattern matching 
                rule match {
                    case BaseRule(_, Isolated(THEKEY, StackSymbol(node, _, _)), SSAAction(instr: SSAPutInstruction), _, _) =>
                        val vn = node.getIR.getSymbolTable().getParameter(0)
                        if (!node.getMethod().isStatic() && vn == instr.getRef()) {
                            log.trace("Added write annotation for transition on isolated object in node %s and instruction %s", node, instr)
                            return "write"
                        } else if (isUniqueField) {
                            val owrObs = FieldUtil.getIKS4FieldInstr(pa, node, instr)
                            if (owrObs.size == 1 &&
                                owrObs(fieldObj) &&
                                field == cha.resolveField(instr.getDeclaredField())) {
                                log.trace("Added write annotation for transition on isolated, unique object in node %s and instruction %s", node, instr)
                                return "write"
                            } else return "none"
                        } else
                            return "none"
                    case BaseRule(_, NotIsolated(StackSymbol(node, _, _)), SSAAction(instr: SSAPutInstruction), _, _) =>
                        if (isUniqueField) {
                            val owrObs = FieldUtil.getIKS4FieldInstr(pa, node, instr)
                            if (owrObs.size == 1 &&
                                owrObs(fieldObj) &&
                                field == cha.resolveField(instr.getDeclaredField())) {
                                log.trace("Added write annotation for transition on not-isolated object in node %s and instruction %s", node, instr)
                                return "write"
                            } else return "none"
                        } else
                            return "none"
                    case BaseRule(_, Summary(StackSymbol(node, _, _)), SSAAction(instr: SSAPutInstruction), _, _) =>
                        if (isUniqueField) {
                            val owrObs = FieldUtil.getIKS4FieldInstr(pa, node, instr)
                            if (owrObs.size == 1 &&
                                owrObs(fieldObj) &&
                                field == cha.resolveField(instr.getDeclaredField()))
                                return "write"
                            else return "none"
                        } else
                            return "none"
                    case _ =>
                        return "none"
                }
            }
        }
        FieldRIOverwriteAnnotater
    }

    def shutdown() {
        XSBInterRunner.shutdown();
    }

}