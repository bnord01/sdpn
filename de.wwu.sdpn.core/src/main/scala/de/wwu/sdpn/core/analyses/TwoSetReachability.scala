package de.wwu.sdpn.core.analyses

import de.wwu.sdpn.core.dpn.monitor.MonitorDPN
import de.wwu.sdpn.core.ta.xsb.reachability.FwdLockSet
import de.wwu.sdpn.core.ta.xsb.reachability.IntLockTA
import de.wwu.sdpn.core.ta.xsb.reachability.MDPN2TA
import de.wwu.sdpn.core.ta.xsb.reachability.SingleSetConflictTA
import de.wwu.sdpn.core.ta.xsb.{ HasTermRepresentation => HTR }
import de.wwu.sdpn.core.ta.xsb.IntersectionEmptinessCheck
import de.wwu.sdpn.core.ta.xsb.IntersectionTA
import de.wwu.sdpn.core.ta.xsb.ScriptTreeAutomata
import de.wwu.sdpn.core.ta.xsb.WitnessIntersectionEmptinessCheck
import java.io.PrintWriter
import java.io.BufferedWriter
import java.io.FileWriter
import scala.collection.JavaConversions._
import scala.collection.Set
import de.wwu.sdpn.core.ta.xsb.reachability.TwoSetConflictTA
import de.wwu.sdpn.core.ta.xsb.XSBInterRunner
import de.wwu.sdpn.core.ta.xsb.XSBCommandRunner
import de.wwu.sdpn.core.ta.xsb.XSBRunner

object TwoSetReachability {
    val runner:XSBRunner = if(isWindows) XSBCommandRunner else XSBInterRunner
    
    def isWindows = System.getProperty("os.name").toLowerCase.indexOf("win") >= 0

  /**
   * Generate tree automata based on an arbitrary monitor dpn and conflict set
   * @param dpn the MonitorDPN to analyze
   * @param cset the conflict set of stack symbols
   * @param lockSens should this analysis be lock sensitive
   * @return (td-automata,bu-automata)
   */
  def genAutomata[GlobalState <% HTR, StackSymbol <% HTR, DPNAction, Lock](
    dpn: MonitorDPN[GlobalState, StackSymbol, DPNAction, Lock],
    cset1: Set[StackSymbol],
    cset2: Set[StackSymbol],
    lockSens: Boolean): (ScriptTreeAutomata, ScriptTreeAutomata) = {

    require(dpn != null, "Can't analyze null-DPN")
    require(!lockSens || (dpn.locks.size <= 8),
      "Can't run locksensitive analysis with " + dpn.locks.size + " locks.")

    val doLockSens = lockSens && dpn.locks.size > 0
    

    val cflow: ScriptTreeAutomata = if (doLockSens) {
      val flow = new MDPN2TA(dpn, "cflow")
      val fwdLS = new FwdLockSet("fwdLS", dpn.locks.size)

      new IntersectionTA(flow, fwdLS) {
        override val name = "flowls"
      }
    } else { new MDPN2TA(dpn, "cflow") }

    val conflict: ScriptTreeAutomata = if (doLockSens) {
      val c1 = new TwoSetConflictTA("conflict", cset1, cset2)
      val lockTA = new IntLockTA("acq", dpn.locks.size)
      new IntersectionTA(lockTA, c1) {
        override val name = "acq_conf"
      }
    } else {
      new TwoSetConflictTA("conflict", cset1, cset2)
    }
    return (cflow, conflict)
  }
    
    /**
     * Method to run a TwoSetReachability analysis and return the result of the
     * conflict check.
     *
     * Here there must exist two processes where the first reaches a stack symbol
     * of confSet1 and the second a stack symbol of confSet2.
     *
     * If one want's to check for data races on a variable  confSet1 could be all positions where
     * a variable is written and confSet2 all positions where the variable is written or read.
     *
     *
     * @param dpn A MonitorDPN to be checked.
     * @param confSet1 A set of StackSymbols which should be checked for mutual exclusion with confSet2.
     * @param confSet2 A set of StackSymbols which should be checked for mutual exclusion with confSet1.
     * @param lockSens A flag which decides if this analysis should be lock sensitive.
     * @return true iff a conflict could exist
     */
    def runConflictCheck[C<%HTR,S<%HTR,A<%HTR,L](dpn:MonitorDPN[C,S,A,L], confSet1: Set[S], confSet2: Set[S], lockSens:Boolean = true): Boolean = {
    	val ss = dpn.getStackSymbols
        require(confSet1.subsetOf(ss), "Some symbols of confSet1 are not contained in the DPN!")
        require(confSet2.subsetOf(ss), "Some symbols of confSet2 are not contained in the DPN!")
        val (td, bu) = genAutomata(dpn, confSet1, confSet2, lockSens)
        val check = new IntersectionEmptinessCheck(td, bu)
        return !runner.runCheck(check)
    }
    def runConflictCheck[C<%HTR,S<%HTR,A<%HTR,L](task: TSRTask[C,S,A,L]): Boolean = {    	
        return runConflictCheck(task.dpn,task.confSet1,task.confSet2,task.lockSens)
    }
    

    
}
case class TSRTask[C<%HTR,S<%HTR,A<%HTR,L](dpn:MonitorDPN[C,S,A,L], confSet1: Set[S], confSet2: Set[S], lockSens:Boolean){
    def run : Boolean = {TwoSetReachability.runConflictCheck(this)}
}