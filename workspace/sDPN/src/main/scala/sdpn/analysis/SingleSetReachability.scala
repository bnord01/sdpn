package sdpn.analysis
import sdpn.dpn.explicit.monitor.MonitorDPN
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import sdpn.dpn.explicit.GlobalState
import sdpn.dpn.explicit.DPNAction
import sdpn.dpn.explicit.StackSymbol
import sdpn.ta.prolog.reachability.MDPN2TA
import sdpn.ta.prolog.reachability.FwdLockSet
import sdpn.ta.IntersectionTA
import sdpn.ta.prolog.reachability.SingleSetConflictTA
import sdpn.ta.prolog.reachability.IntLockTA
import sdpn.ta.WitnessIntersectionEmptinessCheck
import sdpn.ta.IntersectionEmptinessCheck
import java.io.PrintWriter
import java.io.BufferedWriter
import java.io.FileWriter
import sdpn.ta.ScriptTreeAutomata
import sdpn.util.PreAnalysis
import sdpn.dpn.explicit.monitor.MonitorDPNFactory
import com.ibm.wala.util.strings.StringStuff
import com.ibm.wala.ipa.callgraph.CGNode
import scala.collection.JavaConversions._
import com.ibm.wala.types.TypeName
import sdpn.util.LockLocator
import sdpn.util.UniqueInstanceLocator
import sdpn.util.BackwardSliceFilter
import sdpn.util.WaitMap
import scala.collection.Set

/**
 * Helper functions to run single set reachability analyses
 * the genAutomata functions return tow ScriptTreeAutomata,
 * one to be evaluated top down and one to be evaluated bottom up.
 *
 * @author Benedikt Nordhoff
 */
object SingleSetReachability {
    import SSRProps.get.debug
    import System.{ currentTimeMillis => now }
    type MDPN = MonitorDPN[GlobalState, StackSymbol, DPNAction, InstanceKey]

    /**
     * Generate tree automata based on an arbitrary monitor dpn and conflict set
     * @param dpn the MonitorDPN to analyze
     * @param cset the conflict set of stack symbols
     * @param lockSens should this analysis be lock sensitive
     * @return (td-automata,bu-automata)
     */
    def genAutomata(
        dpn: MDPN,
        cset: Set[StackSymbol],
        lockSens: Boolean): (ScriptTreeAutomata, ScriptTreeAutomata) = {

        require(dpn != null, "Can't analyze null-DPN")
        require(!lockSens || (dpn.locks.size <= 8 && 0 < dpn.locks.size),
            "Can't run locksensitive analysis with " + dpn.locks.size + " locks.")

        val cflow: ScriptTreeAutomata = if (lockSens) {
            val flow = new MDPN2TA(dpn, "cflow")
            val fwdLS = new FwdLockSet("fwdLS", dpn.locks.size)

            new IntersectionTA(flow, fwdLS) {
                override val name = "flowls"
            }
        } else { new MDPN2TA(dpn, "cflow") }

        val conflict: ScriptTreeAutomata = if (lockSens) {
            val c1 = new SingleSetConflictTA("conflict", cset)
            val lockTA = new IntLockTA("acq", dpn.locks.size)
            new IntersectionTA(lockTA, c1) {
                override val name = "acq_conf"
            }
        } else {
            new SingleSetConflictTA("conflict", cset)
        }
        return (cflow, conflict)
    }

    /**
     * Generate a WitnessIntersectionEmptinessCheck for two automata
     * @param topdown
     * @param bottomup
     * @return WitnessIntersectionEmptinessCheck named wcheck
     */
    def genWitnessCheck(topdown: ScriptTreeAutomata, bottomup: ScriptTreeAutomata) = {
        new WitnessIntersectionEmptinessCheck(topdown, bottomup) {
            override val name = "wcheck"
        }
    }

    /**
     * Generate a IntersectionEmptinessCheck for two automata
     * @param topdown
     * @param bottomup
     * @return the IntersectionEmptinessCheck named check
     */
    def genCheck(topdown: ScriptTreeAutomata, bottomup: ScriptTreeAutomata) = {
        new IntersectionEmptinessCheck(topdown, bottomup) {
            override val name = "check"
        }
    }

    /**
     * Generate tree automata based on an arbitrary monitor dpn and conflict set
     * which is locksensitive iff the dpn contains 1 to 8 locks
     * @param dpn the MonitorDPN to analyze
     * @param cset the conflict set of stack symbols
     * @return (td-automata,bu-automata)
     */
    def genAutomata(mdpn: MDPN, cset: Set[StackSymbol]): (ScriptTreeAutomata, ScriptTreeAutomata) = {
        val lockSens = mdpn.locks.size > 0 && mdpn.locks.size <= 8
        genAutomata(mdpn, cset, lockSens)
    }

    /**
     * Generate a MonitorDPN and conflict set for the given parameters
     * @param cp the ClassPath to analyze
     * @param mainClass ClassName containing the main-Method e.g. Lcom/package/Class
     * @param methodSig the signature of the method to check for mutual exclusion e.g. com.package.Class.method()V
     * @param lockFilter the filter used to filter interesting locks
     * @param lockSens should the MonitorDPN contain any locks
     * @param slicing only consider method which lead to methodSig
     * @return (the MonitorDPN, the conflict Set)
     */
    def genDPNandCset(cp: String, mainClass: String, methodSig: String, lockFilter: InstanceKey => Boolean, lockSens: Boolean, slicing: Boolean): (MDPN, Set[StackSymbol]) = {
        val tstart = now
        var analysis = MyPreAnalysis.getStd(cp, mainClass)
        val tinit = now
        if (debug)
            println("Wala Init:\t\t" + (tinit - tstart) + "ms")
        val mr = StringStuff.makeMethodReference(methodSig)
        val confSet: Set[CGNode] = Set() ++ analysis.cg.getNodes(mr)

        val analysis1 = if (slicing) {
            new MyPreAnalysis(analysis) with BackwardSliceFilter {
                override val initialSet = confSet
            }
        } else
            analysis

        val analysis2 = if (lockSens) {
            val at1 = new MyPreAnalysis(analysis1) with LockLocator with UniqueInstanceLocator {

                val safeLocks = locks.intersect(uniqueInstances).filter(lockFilter)

                override def safeLock(lock: InstanceKey, node: CGNode) = safeLocks(lock)
            }
            if (debug)
                println("Safe Locks:\t\t" + at1.safeLocks)

            val wm = new WaitMap(at1, at1.safeLocks.contains _)
            if (debug)
                println("Wait Map:\t\t" + wm.waitMap)

            new MyPreAnalysis(at1) {
                override def safeLock(lock: InstanceKey, node: CGNode) = at1.safeLock(lock, node) && !wm(node)(lock)
            }

        } else
            analysis1
        val tpre = now
        if (debug)
            println("Pre Analysis:\t\t" + (tpre - tinit) + "ms")
        val mdpn = (new MonitorDPNFactory(analysis2)).getDPN
        val tgen = now
        if (debug) {
            println("Generated Rules:\t" + mdpn.transitions.size)
            println("Generation Time:\t" + (tgen - tpre) + "ms/" + (tgen - tstart) + "ms")
        }

        val cset = confSet.map((x: CGNode) => StackSymbol(x, 0, 0))
        if (debug)
            println("ConflictSet:\t\t" + cset)
        return (mdpn, cset)
    }

    /**
     * Generate a lock-filter based on a TypeName
     * @param lockTypeName the type of all used locks e.g. Lcom/package/Lock
     * @return the filter
     */
    def lockTypeFilter(lockTypeName: String): InstanceKey => Boolean = {
        val lockType = TypeName.findOrCreate(lockTypeName)
        lock => (lock != null &&
            lock.getConcreteType() != null &&
            lockType.equals(lock.getConcreteType().getName()))
    }

    /**
     * Generate a MonitorDPN and conflict set using the settings from [[sdpn.analysis.SSRProps]].
     * @return the MonitorDPN and the conflict Set
     */
    def genDPNandCset: (MDPN, Set[StackSymbol]) = {
        val props = SSRProps.get
        import props._
        val lockFilter = if (lockSens) lockTypeFilter(lockType) else { x: InstanceKey => false }
        genDPNandCset(classPath, mainClass, exclusiveMethod, lockFilter, lockSens, slicing)
    }

    /**
     * Generate an EmptinessCheck using the settings from [[sdpn.analysis.SSRProps]].
     * @return the corresponding IntersectionEmptinessCheck
     */
    def genCheck: IntersectionEmptinessCheck = {
        val (dpn, cset) = genDPNandCset
        val (td, bu) = genAutomata(dpn, cset)
        return genCheck(td, bu)
    }

    /**
     * Generate an WitnessEmptinessCheck using the settings from [[sdpn.analysis.SSRProps]].
     * @return the corresponding WitnessIntersectionEmptinessCheck
     */
    def genWitnessCheck: WitnessIntersectionEmptinessCheck = {
        val (dpn, cset) = genDPNandCset
        val (td, bu) = genAutomata(dpn, cset)
        return genWitnessCheck(td, bu)
    }

} 