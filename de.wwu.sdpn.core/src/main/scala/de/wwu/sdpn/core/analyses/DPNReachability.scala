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
import de.wwu.sdpn.core.ta.xsb.XSBInterRunner
import de.wwu.sdpn.core.ta.xsb.XSBCommandRunner
import de.wwu.sdpn.core.ta.xsb.IntLockOperations
import de.wwu.sdpn.core.ta.xsb.XSBScript
import de.wwu.sdpn.core.ta.xsb.XSBRunner
import de.wwu.sdpn.core.dpn.monitor.DPNRule

/**
 * Class to run an iterated reachability analysis and return the result of the
 * conflict check.
 *
 * Given a list of sets of stack symbols this checks whether it is possible that these
 * can be reached by arbitrary processes in the given order.
 *
 * E.g. if some definition can reach some use (ignoring killing definitions) when
 * the list has two elements: the point of the definition and the point of the use. *
 *
 * @param dpn A MonitorDPN to be checked.
 * @param confs A list of sets of StackSymbols
 * @param lockSens A flag which decides if this analysis should be lock sensitive.
 */
case class IRTask[C <% HTR, S <% HTR, A <% HTR, L](dpn: MonitorDPN[C, S, A, L], confs: List[Iterable[S]], lockSens: Boolean) {
    /**
     * Run the check and return true iff a 'conflict' can exist.
     */
    def run: Boolean = DPNReachability.runIRCheck(dpn, confs, lockSens)
    /**
     * Run the check and return the generated witness iff a 'conflict' can exist.
     */
    def runWitness = DPNReachability.runWIRCheck(dpn, confs, lockSens)
}

case class TSRTask[C <% HTR, S <% HTR, A <% HTR, L](dpn: MonitorDPN[C, S, A, L], confSet1: Set[S], confSet2: Set[S], lockSens: Boolean) {
    def run: Boolean =  DPNReachability.runTSRCheck(dpn, confSet1, confSet2, lockSens) 
    def runWitness =  DPNReachability.runWTSRCheck(dpn, confSet1, confSet2, lockSens) 
}

object DPNReachability {
    val runner: XSBRunner = if (isWindows) XSBCommandRunner else XSBInterRunner

    def isWindows = System.getProperty("os.name").toLowerCase.indexOf("win") >= 0

    /*
     * ############ Plain iterable reachability based on stack symbols #############
     */

    def runIRCheck[C <% HTR, S <% HTR, A <% HTR, L](dpn: MonitorDPN[C, S, A, L], confs: List[Iterable[S]], lockSens: Boolean = true): Boolean = {
        val ss = dpn.getStackSymbols
        require(ss.containsAll(confs.flatMap(x => x)), "Some symbols are not contained in the DPN!")
        val (td, bu, lo) = genIRAutomata(dpn, confs, lockSens, false)

        val check = new IntersectionEmptinessCheck(td, bu, lo, "check")
        return !runner.runCheck(check)
    }
    def runWIRCheck[C <% HTR, S <% HTR, A <% HTR, L](dpn: MonitorDPN[C, S, A, L], confs: List[Iterable[S]], lockSens: Boolean = true): Option[WitnessTree] = {
        val ss = dpn.getStackSymbols
        require(ss.containsAll(confs.flatMap(x => x)), "Some symbols are not contained in the DPN!")
        val (td, bu, lo) = genIRAutomata(dpn, confs, lockSens, true)

        val check = new FullWitnessIntersectionEmptinessCheck(td, bu, lo, "check")
        runner.runFullWitnessCheck(check) match {
            case None => None
            case Some(w) =>
                val pr = AbstractFullWitnessParser.parseTree(w)
                assert(pr.successful, "Couldn't parse witness!\n" + pr)
                Some(pr.get)
        }
    }

    /**
     * Generate tree automata based on an arbitrary monitor dpn and conflict set
     * @param dpn the MonitorDPN to analyze
     * @param confs the conflict sets of stack symbols in the given order
     * @param lockSens should this analysis be lock sensitive
     * @param annotateActions should the actions of the dpn be annotated to the rules? This is useful for witness generation.
     * @return (td-automata,bu-automata,additional scripts)
     */
    def genIRAutomata[G <% HTR, S <% HTR, A <% HTR, L](
        dpn: MonitorDPN[G, S, A, L],
        confs: List[Iterable[S]],
        lockSens: Boolean, annotateActions: Boolean): (ScriptTreeAutomata, ScriptTreeAutomata, XSBScript) = {

        require(dpn != null, "Can't analyze null-DPN")
        require(!lockSens || (dpn.locks.size <= 8),
            "Can't run locksensitive analysis with " + dpn.locks.size + " locks.")
        require(!confs.isEmpty, "Empty list of stack symbols can't be checked for iterated reachability!")

        val doLockSens = lockSens && dpn.locks.size > 0

        if (doLockSens) {
            val numLocks = dpn.locks.size
            val lo = new IntLockOperations(numLocks, "ilo")

            val annotator = if (annotateActions) { x: DPNRule[G, S, A] => x.action.toTerm } else null

            val dpnTA = new MDPN2IterableTA(dpn, "dpn", annotator)
            val fwdls = new LibFwdLockSet("fwdls", lo)
            val lsdpn = new IntersectionTA(dpnTA, fwdls, "lsdpn")

            val s0 :: st = confs
            var confl: ScriptTreeAutomata = new TopOfStackTA("reach0", Set() ++ s0)

            val ah = new LibLockTA("ah", lo)
            confl = new IntersectionTA(confl, ah, "reach0ls")

            for ((sym, i) <- st.zipWithIndex) {
                val reach1Cut = new CutTransducer(i, confl, "cut" + i + "confl")
                val reach = new TopOfStackTA("reach" + (i + 1), Set() ++ sym)
                val conf0 = new IntersectionTA(reach1Cut, reach, "confl" + i)
                val cwf = new CutWellFormed("cwf" + i, i)
                val conflwf = new IntersectionTA(conf0, cwf, "conflwf" + i)

                val rs = new LibRelTA(i, "rs" + i, lo)
                val ht = new LocksHeldTroughoutTA(i, "ht" + i, lo)
                val rsht = new IntersectionTA(rs, ht, "rs_ht" + i)
                val ah = new LibLockTA("ah" + i, lo)
                val ls = new IntersectionTA(ah, rsht, "ls" + i)
                confl = new IntersectionTA(conflwf, ls, "conflwfls" + i)
            }
            return (lsdpn, confl, lo)
        } else {
            val annotator = if (annotateActions) { x: DPNRule[G, S, A] => x.action.toTerm } else null

            val dpnTA = new MDPN2IterableTA(dpn, "dpn", annotator)
            val s0 :: st = confs
            var confl: ScriptTreeAutomata = new TopOfStackTA("reach0", Set() ++ s0)

            for ((sym, i) <- st.zipWithIndex) {
                val reach1Cut = new CutTransducer(i, confl, "cut" + i + "confl")
                val reach = new TopOfStackTA("reach" + (i + 1), Set() ++ sym)
                val conf0 = new IntersectionTA(reach1Cut, reach, "confl" + i)
                val cwf = new CutWellFormed("cwf" + i, i)
                confl = new IntersectionTA(conf0, cwf, "conflwf" + i)
            }
            return (dpnTA, confl, null)
        }
    }

    /*
     * ############ Two set reachability e.g. for data races #############
     */

    def runTSRCheck[C <% HTR, S <% HTR, A <% HTR, L](dpn: MonitorDPN[C, S, A, L], cset1: Set[S], cset2: Set[S], lockSens: Boolean = true): Boolean = {
        val ss = dpn.getStackSymbols
        require(ss.containsAll(cset1 union cset2), "Some symbols are not contained in the DPN!")
        val (td, bu, lo) = genTSRAutomata(dpn, cset1, cset2, lockSens, false)

        val check = new IntersectionEmptinessCheck(td, bu, lo, "check")
        return !runner.runCheck(check)
    }
    def runWTSRCheck[C <% HTR, S <% HTR, A <% HTR, L](dpn: MonitorDPN[C, S, A, L], cset1: Set[S], cset2: Set[S], lockSens: Boolean = true): Option[WitnessTree] = {
        val ss = dpn.getStackSymbols
        require(ss.containsAll(cset1 union cset2), "Some symbols are not contained in the DPN!")
        val (td, bu, lo) = genTSRAutomata(dpn, cset1, cset2, lockSens, true)

        val check = new FullWitnessIntersectionEmptinessCheck(td, bu, lo, "check")
        runner.runFullWitnessCheck(check) match {
            case None => None
            case Some(w) =>
                val pr = AbstractFullWitnessParser.parseTree(w)
                assert(pr.successful, "Couldn't parse witness!\n" + pr)
                Some(pr.get)
        }
    }

    /**
     * Generate tree automata based on an arbitrary monitor dpn and conflict set
     * @param dpn the MonitorDPN to analyze
     * @param confs the conflict sets of stack symbols in the given order
     * @param lockSens should this analysis be lock sensitive
     * @param annotateActions should the actions of the dpn be annotated to the rules? This is useful for witness generation.
     * @return (td-automata,bu-automata,additional scripts)
     */
    def genTSRAutomata[G <% HTR, S <% HTR, A <% HTR, L](
        dpn: MonitorDPN[G, S, A, L],
        cset1: Set[S],
        cset2: Set[S],
        lockSens: Boolean, annotateActions: Boolean): (ScriptTreeAutomata, ScriptTreeAutomata, XSBScript) = {

        require(dpn != null, "Can't analyze null-DPN")
        require(!lockSens || (dpn.locks.size <= 8),
            "Can't run locksensitive analysis with " + dpn.locks.size + " locks.")
        require(!cset1.isEmpty, "Empty list of stack symbols can't be checked for iterated reachability!")
        require(!cset2.isEmpty, "Empty list of stack symbols can't be checked for iterated reachability!")

        val doLockSens = lockSens && dpn.locks.size > 0

        val annotator = if (annotateActions) { x: DPNRule[G, S, A] => x.action.toTerm } else null

        var dpnTa: ScriptTreeAutomata = new MDPN2IterableTA(dpn, "dpn", annotator)
        var confl: ScriptTreeAutomata = new TwoSetConflictTA("confl", cset1, cset2)

        if (doLockSens) {
            val numLocks = dpn.locks.size
            val lo = new IntLockOperations(numLocks, "ilo")

            val fwdls = new LibFwdLockSet("fwdls", lo)
            val td = new IntersectionTA(dpnTa, fwdls, "ls_dpn")

            val ah = new LibLockTA("ah", lo)
            val bu = new IntersectionTA(confl, ah, "ls_confl")

            return (td, bu, lo)
        } else {
            return (dpnTa, confl, null)
        }
    }

}


