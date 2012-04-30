package de.wwu.sdpn.core.analyses
import de.wwu.sdpn.core.dpn.monitor.MonitorDPN
import de.wwu.sdpn.core.ta.xsb.{ HasTermRepresentation => HTR }
import de.wwu.sdpn.core.ta.xsb.IntersectionEmptinessCheck
import de.wwu.sdpn.core.ta.xsb.IntersectionTA
import de.wwu.sdpn.core.ta.xsb.ScriptTreeAutomata
import de.wwu.sdpn.core.ta.xsb.FullWitnessIntersectionEmptinessCheck
import de.wwu.sdpn.core.ta.xsb.iterable._
import de.wwu.sdpn.core.ta.xsb.witness.iterable._
import java.io.PrintWriter
import java.io.BufferedWriter
import java.io.FileWriter
import scala.collection.JavaConversions._
import de.wwu.sdpn.core.ta.xsb.reachability.TwoSetConflictTA
import de.wwu.sdpn.core.ta.xsb.XSBInterRunner

object IteratedReachability {
    val runner = XSBInterRunner

    /**
     * Generate tree automata based on an arbitrary monitor dpn and conflict set
     * @param dpn the MonitorDPN to analyze
     * @param cset the conflict set of stack symbols
     * @param lockSens should this analysis be lock sensitive
     * @return (td-automata,bu-automata)
     */
    def genAutomata[GlobalState <% HTR, StackSymbol <% HTR, DPNAction, Lock](
        dpn: MonitorDPN[GlobalState, StackSymbol, DPNAction, Lock],
        confs: List[Iterable[StackSymbol]],
        lockSens: Boolean): (ScriptTreeAutomata, ScriptTreeAutomata) = {

        require(!lockSens, "No lock sens at the moment!")
        require(dpn != null, "Can't analyze null-DPN")
        require(!lockSens || (dpn.locks.size <= 8),
            "Can't run locksensitive analysis with " + dpn.locks.size + " locks.")
        require(!confs.isEmpty, "Empty list of stack symbols can't be checked for iterated reachability!")

        val doLockSens = lockSens && dpn.locks.size > 0

        val dpnTA = new MDPN2IterableTA(dpn)
        val s0 :: st = confs
        var confl: ScriptTreeAutomata = new TopOfStackTA("reach0", Set() ++ s0)

        for ((sym, i) <- st.zipWithIndex) {
            val reach1Cut = new CutTransducer(i, confl, "cut" + i + "confl")
            val reach = new TopOfStackTA("reach" + (i + 1), Set() ++ sym)
            val conf0 = new IntersectionTA(reach1Cut, reach, "confl" + i)
            val cwf = new CutWellFormed("cwf" + i, i)
            confl = new IntersectionTA(conf0, cwf, "conflwf" + i)
        }

        //TODO This doesn't terminate. Check why!?
        //        for ((sym, i) <- st.zipWithIndex) {
        //            val reach1Cut = new CutTransducer(i, confl, "cut" + i + "confl")
        //            val cwf = new CutWellFormed("cwf" + i, i)            
        //            val conf0 = new IntersectionTA(cwf,reach1Cut, "conflwf" + i)
        //            
        //            val reach = new TopOfStackTA("reach" + (i + 1), Set(sym))
        //            confl = new IntersectionTA(conf0, reach, "reach" + (i +1))
        //        }
        return (dpnTA, confl)
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
    def runConflictCheck[C <% HTR, S <% HTR, A <% HTR, L](dpn: MonitorDPN[C, S, A, L], confs: List[Iterable[S]], lockSens: Boolean = true): Boolean = {
        val ss = dpn.getStackSymbols
        require(ss.containsAll(confs.flatMap(x => x)), "Some symbols are not contained in the DPN!")
        val (td, bu) = genAutomata(dpn, confs, lockSens)

        val check = new IntersectionEmptinessCheck(td, bu, "check")
        return !runner.runCheck(check)
    }
    def runConflictCheck[C <% HTR, S <% HTR, A <% HTR, L](task: IRTask[C, S, A, L]): Boolean = {
        return runConflictCheck(task.dpn, task.confs, task.lockSens)
    }
    def runWitnessCheck[C <% HTR, S <% HTR, A <% HTR, L](dpn: MonitorDPN[C, S, A, L], confs: List[Iterable[S]], lockSens: Boolean = true): Option[WitnessTree] = {
        val ss = dpn.getStackSymbols
        require(ss.containsAll(confs.flatMap(x => x)), "Some symbols are not contained in the DPN!")
        val (td, bu) = genAutomata(dpn, confs, lockSens)

        val check = new FullWitnessIntersectionEmptinessCheck(td, bu, "check")
        runner.runFullWitnessCheck(check) match {
            case None => None
            case Some(w) =>
                val pr = AbstractFullWitnessParser.parseTree(w)
                assert(pr.successful, "Couldn't parse witness!\n" + pr)
                Some(pr.get)
        }
    }

}

case class IRTask[C <% HTR, S <% HTR, A <% HTR, L](dpn: MonitorDPN[C, S, A, L], confs: List[Iterable[S]], lockSens: Boolean) {
    def run: Boolean = IteratedReachability.runConflictCheck(this)
    def runWitness = IteratedReachability.runWitnessCheck(dpn, confs, lockSens)
}
