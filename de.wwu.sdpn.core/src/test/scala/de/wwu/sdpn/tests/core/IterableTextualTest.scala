package de.wwu.sdpn.tests.core

import de.wwu.sdpn.core.textual._
import de.wwu.sdpn.core.analyses.IRTask
import de.wwu.sdpn.core.ta.xsb.iterable._
import de.wwu.sdpn.core.dpn.monitor.MonitorDPN
import org.junit.Assert._
import de.wwu.sdpn.core.ta.xsb.IntersectionEmptinessCheck
import de.wwu.sdpn.core.ta.xsb.IntersectionTA
import de.wwu.sdpn.core.ta.xsb.XSBInterRunner
import de.wwu.sdpn.core.ta.xsb.FullWitnessIntersectionEmptinessCheck
import de.wwu.sdpn.core.ta.xsb.witness.iterable._
import org.junit.Test
import de.wwu.sdpn.core.ta.xsb.ScriptTreeAutomata

class IterableTextualTest {

    import TextualTestDPNs._

    @Test
    def testDPN1_1_2_true() {
        testOnceIterated(true, "1", "2", dpn1)
    }
    @Test
    def testDPN1_2_1_false() {
        testOnceIterated(false, "2", "1", dpn1)
    }
    @Test
    def testDPN1_1_2_3_true() {
        testDoubleIterated(true, "1", "2", "3", dpn1)
    }
    @Test
    def testDPN1_3_2_1_false() {
        testDoubleIterated(false, "3", "2", "1", dpn1)
    }
    @Test
    def testDPN2_1_2_3_true() {
        testDoubleIterated(true, "1", "2", "3", dpn2)
    }
    @Test
    def testDPN2_3_2_1_false() {
        testDoubleIterated(false, "3", "2", "1", dpn2)
    }
    @Test
    def testDPN2_l_1_2_3_true() {
        testIterated(true, List("1", "2", "3"), dpn2)
    }
    @Test
    def testDPN2_l_3_2_1_false() {
        testIterated(false, List("3", "2", "1"), dpn2)
    }
    @Test
    def destDPN2_1_2_3_4() {
        testIterated(true, List("1", "2", "3", "4"), dpn2)
    }
    @Test
    def destDPN3_a1_a2_a3_a4() {
        testIterated(true, List("a1", "a2", "a3", "a4"), dpn3)
    }
    @Test
    def destDPN4_a1_a4() {
        testIterated(true, List("a1", "a4"), dpn4)
    }
    @Test
    def destDPN4_a2_a4() {
        testIterated(true, List("a2", "a4"), dpn4)
    }
    @Test
    def destDPN4_a2_a3() {
        testIterated(true, List("a2", "a3"), dpn4)
    }
    @Test
    def destDPN4_a1_a2_a3() {
        testIterated(true, List("a1","a2", "a3"), dpn4)
    }
    @Test
    def destDPN4_a1_a2_a4() {
        testIterated(true, List("a1","a2", "a4"), dpn4)
    }
    @Test
    def destDPN4_a2_a3_a4() {
        testIterated(true, List("a2","a3", "a4"), dpn4)
    }
    @Test
    def destDPN4_a1_a2_a3_a4() {
        testIterated(true, List("a1", "a2", "a3", "a4"), dpn4)
    }
    @Test
    def destDPN4_a1_a3_a4() {
        testIterated(true, List("a1", "a3", "a4"), dpn4)
    }
    @Test
    def destDPN5_a1_a4_a4() {
        testIterated(true, List("a1", "a4", "a4"), dpn5)
    }
    @Test
    def destDPN5_a1_a3_a4() {
        testIterated(true, List("a1", "a3", "a4"), dpn5)
    }
    @Test
    def destDPN5_a3_a4_1() {
        testOnceIterated(true, "a3", "a4", dpn5)
    }
    @Test
    def destDPN5_a3_a4() {
        testIterated(true, List("a3", "a4"), dpn5)
    }
    @Test
    def destDPN5_a3_a4_2() {
        testOnceIterated2(true, "a3", "a4", dpn5)
    }
    @Test
    def destDPN6_a2_a3() {
        testIterated(true, List("a2", "a3"), dpn6)
    }
    @Test
    def destDPN6_a2_a3_2() {
        testOnceIterated2(true, "a2", "a3", dpn6)
    }
    @Test
    def destDPN6_a2_a3_3() {
        testIterated2(true, List("a2", "a3"), dpn6)
    }
    
    

    /**
     * Parse the given DPN and check whether sym2 can be reached after reaching sym1 and compare the result to expected.
     */
    def testOnceIterated(expected: Boolean, sym1: String, sym2: String, dpnString: String) {
        val (dpn, naming) = DPNParser.parseDPN(dpnString).get

        val s1 = naming.sname(sym1)
        val s2 = naming.sname(sym2)

        assert(dpn.stackSymbols.contains(s1), "Stack symbol 1 not found in DPN " + sym1)
        assert(dpn.stackSymbols.contains(s2), "Stack symbol 2 not found in DPN " + sym2)

        val dpnTA = new MDPN2IterableTA(dpn)
        val reach1 = new TopOfStackTA("reach1", Set(s1))
        val reach1Cut = new CutTransducer(0, reach1, "cutReach1")

        val reach2 = new TopOfStackTA("reach2", Set(s2))

        val conf = new IntersectionTA(reach1Cut, reach2)

        val cwf = new CutWellFormed("cwf1", 0)
        val confwf = new IntersectionTA(conf, cwf)

        val check = new IntersectionEmptinessCheck(dpnTA, confwf, "check")

        val result = !XSBInterRunner.runCheck(check)

        if (result) {
            val wcheck = new FullWitnessIntersectionEmptinessCheck(dpnTA, confwf)
            val w = XSBInterRunner.runFullWitnessCheck(wcheck)
            assert(w.isDefined, "Check said not empty but no witness found! This is bad!")
            val wtr = AbstractFullWitnessParser.parseTree(w.get)
            if (!wtr.successful)
                println(wtr)
            assert(wtr.successful, "Couldn't parse witness")
            println("witness: \n " + wtr.get.printTree)
        }
        assertEquals("Check didn't yield expected result!", expected, result)
    }

    /**
     * Parse the given DPN and check whether sym2 can be reached after reaching sym1 and compare the result to expected.
     */
    def testDoubleIterated(expected: Boolean, sym1: String, sym2: String, sym3: String, dpnString: String) {
        val (dpn, naming) = DPNParser.parseDPN(dpnString).get

        val s1 = naming.sname(sym1)
        val s2 = naming.sname(sym2)
        val s3 = naming.sname(sym3)

        assert(dpn.stackSymbols.contains(s1), "Stack symbol 1 not found in DPN " + sym1)
        assert(dpn.stackSymbols.contains(s2), "Stack symbol 2 not found in DPN " + sym2)
        assert(dpn.stackSymbols.contains(s3), "Stack symbol 3 not found in DPN " + sym3)

        val dpnTA = new MDPN2IterableTA(dpn)
        val reach1 = new TopOfStackTA("reach1", Set(s1))
        val reach1Cut = new CutTransducer(0, reach1, "cut0Reach1")

        val reach2 = new TopOfStackTA("reach2", Set(s2))

        val conf0 = new IntersectionTA(reach1Cut, reach2, "conf0")

        val cwf = new CutWellFormed("cwf0", 0)
        val confwf = new IntersectionTA(conf0, cwf, "confwf0")

        val cut1conf0 = new CutTransducer(1, confwf, "cut1conf0")

        val reach3 = new TopOfStackTA("reach3", Set(s3))
        val cut1conf = new IntersectionTA(cut1conf0, reach3, "cut1conf")

        val cwf1 = new CutWellFormed("cwf1", 1)
        val confwf2 = new IntersectionTA(cut1conf, cwf1, "confwf2")

        val check = new IntersectionEmptinessCheck(dpnTA, confwf2, "check")

        val result = !XSBInterRunner.runCheck(check)
        if (result) {
            val wcheck = new FullWitnessIntersectionEmptinessCheck(dpnTA, confwf2)
            val w = XSBInterRunner.runFullWitnessCheck(wcheck)
            assert(w.isDefined, "Check said not empty but no witness found! This is bad!")
            val wtr = AbstractFullWitnessParser.parseTree(w.get)
            if (!wtr.successful)
                println(wtr)
            assert(wtr.successful, "Couldn't parse witness")
            println("witness: \n" + wtr.get.printTree)
        }
        assertEquals("Check didn't yield expected result!", expected, result)
    }
    
    /**
     * Parse the given DPN and check whether sym2 can be reached after reaching sym1 and compare the result to expected.
     */
    def testIterated(expected: Boolean, symsS: List[String], dpnString: String,lockSens:Boolean = false) {
        val (dpn, naming) = DPNParser.parseDPN(dpnString).get

        assert(!symsS.isEmpty, "Empty list of stack symbols!")

        val syms = symsS.map(x => Set(naming.sname(x)))
        
        val task = IRTask(dpn,syms,lockSens)

        val result = task.run
        if (result) {            
            println("witness: \n" + task.runWitness.get)
        }
        assertEquals("Check didn't yield expected result!", expected, result)
    }

    /**
     * Parse the given DPN and check whether sym2 can be reached after reaching sym1 and compare the result to expected.
     */
    def testIterated2(expected: Boolean, symsS: List[String], dpnString: String) {
        val (dpn, naming) = DPNParser.parseDPN(dpnString).get

        assert(!symsS.isEmpty, "Empty list of stack symbols!")

        val syms = symsS.map(naming.sname(_))

        assert(syms.forall(dpn.stackSymbols.contains(_)), "Not all stack symbols contained in DPN " + syms)

        val dpnTA = new MDPN2IterableTA(dpn)
        val s0 :: st = syms
        var confl: ScriptTreeAutomata = new TopOfStackTA("reach0", Set(s0))

        // Other order than in IRTask here, problem found and fixed ;)
        for ((sym, i) <- st.zipWithIndex) {
            val reach1Cut = new CutTransducer(i, confl, "cut" + i + "confl")
            val cwf = new CutWellFormed("cwf" + i, i)            
            val conf0 = new IntersectionTA(cwf,reach1Cut, "conflwf" + i)
            
            val reach = new TopOfStackTA("reach" + (i + 1), Set(sym))
            confl = new IntersectionTA(conf0, reach, "reachconfl" + (i +1))
        }
        val check = new IntersectionEmptinessCheck(dpnTA, confl, "check")

        val result = !XSBInterRunner.runCheck(check)
        if (result) {
            val wcheck = new FullWitnessIntersectionEmptinessCheck(dpnTA, confl)
            val w = XSBInterRunner.runFullWitnessCheck(wcheck)
            assert(w.isDefined, "Check said not empty but no witness found! This is bad!")
            val wtr = AbstractFullWitnessParser.parseTree(w.get)
            if (!wtr.successful)
                println(wtr)
            assert(wtr.successful, "Couldn't parse witness")
            println("witness: \n" + wtr.get.printTree)
        }
        assertEquals("Check didn't yield expected result!", expected, result)
    }
    
    /**
     * Parse the given DPN and check whether sym2 can be reached after reaching sym1 and compare the result to expected.
     */
    def testOnceIterated2(expected: Boolean, sym1: String, sym2: String, dpnString: String) {
        val (dpn, naming) = DPNParser.parseDPN(dpnString).get

        val s1 = naming.sname(sym1)
        val s2 = naming.sname(sym2)

        assert(dpn.stackSymbols.contains(s1), "Stack symbol 1 not found in DPN " + sym1)
        assert(dpn.stackSymbols.contains(s2), "Stack symbol 2 not found in DPN " + sym2)

        val dpnTA = new MDPN2IterableTA(dpn)
        val reach1 = new AtCutTopOfStackTA(0,"reach1", Set(s1))

        val reach2 = new TopOfStackTA("reach2", Set(s2))

        val conf = new IntersectionTA(reach1, reach2)

        val cwf = new CutWellFormed("cwf", 0)
        val confwf = new IntersectionTA(conf, cwf)

        val check = new IntersectionEmptinessCheck(dpnTA, confwf, "check")

        val result = !XSBInterRunner.runCheck(check)

        if (result) {
            val wcheck = new FullWitnessIntersectionEmptinessCheck(dpnTA, confwf)
            val w = XSBInterRunner.runFullWitnessCheck(wcheck)
            assert(w.isDefined, "Check said not empty but no witness found! This is bad!")
            val wtr = AbstractFullWitnessParser.parseTree(w.get)
            if (!wtr.successful)
                println(wtr)
            assert(wtr.successful, "Couldn't parse witness")
            println("witness: \n " + wtr.get.printTree)
        }
        assertEquals("Check didn't yield expected result!", expected, result)
    }
    

    //    @Test
    //    def testPrintTree {
    //        val a = UnparsedAnnot("a")
    //        val s = UnparsedState("s")
    //        val tree: WitnessTree = BaseTree(a, s, Call2Tree(s, Call2Tree(s, BaseTree(a, s, RetTree(s)), NilTree(a, s)), Call2Tree(s, RetTree(s), NilTree(a, s))))
    //        println(tree.printTree)
    //    }
}
