package de.wwu.sdpn.tests.analyses

import de.wwu.sdpn.core.dpn.monitor.MonitorDPN
import de.wwu.sdpn.core.ta.xsb.{ HasTermRepresentation => HTR }
import de.wwu.sdpn.core.ta.xsb.IntersectionEmptinessCheck
import de.wwu.sdpn.core.ta.xsb.IntersectionTA
import de.wwu.sdpn.core.ta.xsb.ScriptTreeAutomata
import de.wwu.sdpn.core.ta.xsb.iterable._
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
import org.junit.BeforeClass
import org.junit.AfterClass
import org.junit.Test
import com.ibm.wala.util.strings.StringStuff
import scala.collection.JavaConversions._
import org.junit.Assert._
import com.ibm.wala.types.MethodReference
import de.wwu.sdpn.wala.analyses.SDPNTestProps
import junit.framework.JUnit4TestAdapter
import org.junit.runners.Parameterized.Parameters
import org.junit.runners.Parameterized
import org.junit.runner.RunWith
import de.wwu.sdpn.wala.analyses.SimpleAnalyses
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.callgraph.CallGraph
import de.wwu.sdpn.wala.dpngen.symbols.StackSymbol
import de.wwu.sdpn.wala.dpngen.symbols.GlobalState
import de.wwu.sdpn.wala.dpngen.symbols.DPNAction
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey

object DPNTrimCompareTest {

    var stuff: Map[Int, (CallGraph, PointerAnalysis[InstanceKey], MethodReference)] = Map()

    @BeforeClass
    def setUp() {
        for (i <- 1 to 7) {
            val (cg, pa) = SimpleAnalyses.getCGandPAfromCP(SDPNTestProps.get.classPath, "Lbnord/unittests/simpleAnalyses/BSP0" + i)
            val mr = StringStuff.makeMethodReference("bnord.unittests.simpleAnalyses.BSP0" + i + ".excludeMe()V")
            stuff += i -> (cg, pa, mr)
        }
    }

    @AfterClass
    def tearDown() {
        stuff = null
        XSBInterRunner.shutdown();
    }
    @Parameters
    def data(): java.util.List[Array[Object]] = {
        val list = new java.util.ArrayList[Array[Object]]()
        list.add(Array[Object](true: java.lang.Boolean))
        list.add(Array[Object](false: java.lang.Boolean))
        return list
    }
}

@RunWith(classOf[Parameterized])
class DPNTrimCompareTest(trim: Boolean) {
    import DPNTrimCompareTest.stuff

    val runner = XSBInterRunner

    @Test
    def test01 = runOn(1) 
    @Test
    def test02 = runOn(2)
    @Test
    def test03 = runOn(3)
    @Test
    def test04 = runOn(4)
    @Test
    def test05 = runOn(5)
    @Test
    def test06 = runOn(6)
    @Test
    def test07 = runOn(7)
    
    def runOn(i: Int) {
        val (cg, pa, mr) = stuff(i)
        val cgnset = cg.getNodes(mr)
        val sliceSet = null
        val lockSens = true
        val dpn = SimpleAnalyses.getMDPN(cg, pa, sliceSet, lockSens)
        
        val cset = Set() ++ cgnset.map {StackSymbol(_,0,0)}
       
        implicit def a2htr(a: DPNAction) = new HTR { def toTerm = "0" }

        val result = runTSRCheck(dpn,cset,cset,lockSens)
        
    }

    /*
     * ############ Two set reachability e.g. for data races #############
     */

    def runTSRCheck[C <% HTR, S <% HTR, A <% HTR, L](dpn: MonitorDPN[C, S, A, L], cset1: Set[S], cset2: Set[S], lockSens: Boolean = true): Boolean = {
        val ss = dpn.getStackSymbols
        require(ss.containsAll(cset1 union cset2), "Some symbols are not contained in the DPN!")
        val (td, bu, lo) = genTSRAutomata(dpn, cset1, cset2, lockSens, false)

        val check = new IntersectionEmptinessCheck(td, bu, lo, "check")
        println(check.emptiness)
        return !runner.runCheck(check)
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

        var dpnTa: ScriptTreeAutomata =
            if (trim) {
                new BaseTrimmedTA(new MDPN2IterableTA(dpn, "dpn", annotator), cset1 ++ cset2)
            } else {
                new MDPN2IterableTA(dpn, "dpn", annotator)
            }
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

