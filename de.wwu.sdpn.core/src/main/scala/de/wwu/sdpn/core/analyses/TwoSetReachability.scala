package de.wwu.sdpn.core.analyses

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
import java.io.PrintWriter
import java.io.BufferedWriter
import java.io.FileWriter
import scala.collection.JavaConversions._
import scala.collection.Set
import de.wwu.sdpn.core.ta.xsb.reachability.TwoSetConflictTA

object TwoSetReachability {
  
    
  /**
     * Generate tree automata based on an arbitrary monitor dpn and conflict set
     * @param dpn the MonitorDPN to analyze
     * @param cset the conflict set of stack symbols
     * @param lockSens should this analysis be lock sensitive
     * @return (td-automata,bu-automata)
     */
    def genAutomata[GlobalState<%HTR, StackSymbol<%HTR, DPNAction, Lock](
        dpn: MonitorDPN[GlobalState, StackSymbol, DPNAction, Lock],
        cset1: Set[StackSymbol],
        cset2: Set[StackSymbol],
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
            val c1 = new TwoSetConflictTA("conflict", cset1,cset2)
            val lockTA = new IntLockTA("acq", dpn.locks.size)
            new IntersectionTA(lockTA, c1) {
                override val name = "acq_conf"
            }
        } else {
            new TwoSetConflictTA("conflict", cset1,cset2)
        }
        return (cflow, conflict)
    }

}