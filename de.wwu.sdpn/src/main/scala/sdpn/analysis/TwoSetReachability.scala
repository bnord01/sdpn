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
import sdpn.ta.prolog.reachability.TwoSetConflictTA

object TwoSetReachability {
  
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