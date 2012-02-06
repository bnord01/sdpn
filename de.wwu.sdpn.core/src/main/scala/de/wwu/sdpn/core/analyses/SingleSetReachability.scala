package de.wwu.sdpn.core.analyses
import scala.collection.Set
import de.wwu.sdpn.core.dpn.monitor.MonitorDPN
import de.wwu.sdpn.core.ta.xsb.reachability.FwdLockSet
import de.wwu.sdpn.core.ta.xsb.reachability.IntLockTA
import de.wwu.sdpn.core.ta.xsb.reachability.MDPN2TA
import de.wwu.sdpn.core.ta.xsb.reachability.SingleSetConflictTA
import de.wwu.sdpn.core.ta.xsb.{HasTermRepresentation => HTR}
import de.wwu.sdpn.core.ta.xsb.IntersectionEmptinessCheck
import de.wwu.sdpn.core.ta.xsb.IntersectionTA
import de.wwu.sdpn.core.ta.xsb.ScriptTreeAutomata
import de.wwu.sdpn.core.ta.xsb.WitnessIntersectionEmptinessCheck
import de.wwu.sdpn.core.ta.xsb.reachability.LibFwdLockSet
import de.wwu.sdpn.core.ta.xsb.StdLibLockOperations
import de.wwu.sdpn.core.ta.xsb.reachability.LibLockTA

/**
 * Helper functions to run single set reachability analyses
 * the genAutomata functions return tow ScriptTreeAutomata,
 * one to be evaluated top down and one to be evaluated bottom up.
 *
 * @author Benedikt Nordhoff
 */
object SingleSetReachability {
  private def debug = SDPNProps.get.debug
  import System.{ currentTimeMillis => now }

  /**
   * Generate tree automata based on an arbitrary monitor dpn and conflict set
   * @param dpn the MonitorDPN to analyze
   * @param cset the conflict set of stack symbols
   * @param lockSens should this analysis be lock sensitive
   * @return (td-automata,bu-automata)
   */
  def genAutomata[GlobalState <% HTR, StackSymbol <% HTR, DPNAction, Lock](
    dpn: MonitorDPN[GlobalState, StackSymbol, DPNAction, Lock],
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
   * Generate tree automata based on an arbitrary monitor dpn and conflict set
   * @param dpn the MonitorDPN to analyze
   * @param cset the conflict set of stack symbols
   * @param lockSens should this analysis be lock sensitive
   * @return (td-automata,bu-automata)
   */
  def genStdLibAutomata[GlobalState <% HTR, StackSymbol <% HTR, DPNAction, Lock](
    dpn: MonitorDPN[GlobalState, StackSymbol, DPNAction, Lock],
    cset: Set[StackSymbol]): (ScriptTreeAutomata, ScriptTreeAutomata) = {

    require(dpn != null, "Can't analyze null-DPN")
    
    val cflow: ScriptTreeAutomata = {
      val flow = new MDPN2TA(dpn, "cflow")
      val fwdLS = new LibFwdLockSet("fwdLS", new StdLibLockOperations())

      new IntersectionTA(flow, fwdLS) {
        override val name = "flowls"
      }
    }

    val conflict: ScriptTreeAutomata = {
      val c1 = new SingleSetConflictTA("conflict", cset)
      val lockTA = new LibLockTA("acq", new StdLibLockOperations())
      new IntersectionTA(lockTA, c1) {
        override val name = "acq_conf"
      }
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
  def genAutomata[GlobalState <% HTR, StackSymbol <% HTR, DPNAction , Lock ](mdpn: MonitorDPN[GlobalState, StackSymbol, DPNAction, Lock], cset: Set[StackSymbol]): (ScriptTreeAutomata, ScriptTreeAutomata) = {
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
  /*
    def genDPNandCset(cp: String, mainClass: String, methodSig: String, lockFilter: Lock => Boolean, lockSens: Boolean, slicing: Boolean): (MDPN, Set[StackSymbol]) = {
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

                override def safeLock(lock: Lock, node: CGNode) = safeLocks(lock)
            }
            if (debug)
                println("Safe Locks:\t\t" + at1.safeLocks)

            val wm = new WaitMap(at1, at1.safeLocks.contains _)
            if (debug)
                println("Wait Map:\t\t" + wm.waitMap)

            new MyPreAnalysis(at1) {
                override def safeLock(lock: Lock, node: CGNode) = at1.safeLock(lock, node) && !wm(node)(lock)
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
    def lockTypeFilter(lockTypeName: String): Lock => Boolean = {
        val lockType = TypeName.findOrCreate(lockTypeName)
        lock => (lock != null &&
            lock.getConcreteType() != null &&
            lockType.equals(lock.getConcreteType().getName()))
    }

    /**
     * Generate a MonitorDPN and conflict set using the settings from [[de.wwu.sdpn.analysis.SDPNProps]].
     * @return the MonitorDPN and the conflict Set
     */
    def genDPNandCset: (MonitorDPN[GlobalState, StackSymbol, DPNAction, Lock], Set[StackSymbol]) = {
        val props = SDPNProps.get
        import props._
        val lockFilter = if (lockSens) lockTypeFilter(lockType) else { x: Lock => false }
        genDPNandCset(classPath, mainClass, exclusiveMethod, lockFilter, lockSens, slicing)
    }

    /**
     * Generate an EmptinessCheck using the settings from [[de.wwu.sdpn.analysis.SDPNProps]].
     * @return the corresponding IntersectionEmptinessCheck
     */
    def genCheck: IntersectionEmptinessCheck = {
        val (dpn, cset) = genDPNandCset
        val (td, bu) = genAutomata(dpn, cset)
        return genCheck(td, bu)
    }
    

    /**
     * Generate an WitnessEmptinessCheck using the settings from [[de.wwu.sdpn.analysis.SDPNProps]].
     * @return the corresponding WitnessIntersectionEmptinessCheck
     */
    def genWitnessCheck: WitnessIntersectionEmptinessCheck = {
        val (dpn, cset) = genDPNandCset
        val (td, bu) = genAutomata(dpn, cset)
        return genWitnessCheck(td, bu)
    }
    */

} 