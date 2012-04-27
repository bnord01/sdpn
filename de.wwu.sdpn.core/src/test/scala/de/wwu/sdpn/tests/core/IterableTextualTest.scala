package de.wwu.sdpn.tests.core

import de.wwu.sdpn.core.textual._
import de.wwu.sdpn.core.ta.xsb.iterable._
import de.wwu.sdpn.core.dpn.monitor.MonitorDPN
import org.junit.Assert._
import de.wwu.sdpn.core.ta.xsb.IntersectionEmptinessCheck
import de.wwu.sdpn.core.ta.xsb.IntersectionTA
import de.wwu.sdpn.core.ta.xsb.XSBInterRunner
import de.wwu.sdpn.core.ta.xsb.FullWitnessIntersectionEmptinessCheck
import org.junit.Test

class IterableTextualTest {

    import TextualTestDPNs._
    
    @Test
    def testDPN1_1_2_true() {
        testOnceIterated(true,"1","2",dpn1)
    }
    @Test
    def testDPN1_2_1_false() {
        testOnceIterated(false,"2","1",dpn1)
    }
    @Test
    def testDPN1_1_2_3_true() {
        testDoubleIterated(true,"1","2","3",dpn1)
    }
    @Test
    def testDPN1_3_2_1_false() {
        testDoubleIterated(false,"3","2","1",dpn1)
    }
    @Test
    def testDPN2_1_2_3_true() {
        testDoubleIterated(true,"1","2","3",dpn2)
    }
    @Test
    def testDPN2_3_2_1_false() {
        testDoubleIterated(false,"3","2","1",dpn2)
    }
    
    /**
     * Parse the given DPN and check whether sym2 can be reached after reaching sym1 and compare the result to expected.
     */
    def testOnceIterated(expected:Boolean,sym1:String,sym2:String,dpnString:String){
        val (dpn,naming) = DPNParser.parseDPN(dpnString).get
        
        val s1 = naming.sname(sym1)
        val s2 = naming.sname(sym2)
        
        assert(dpn.stackSymbols.contains(s1),"Stack symbol 1 not found in DPN " + sym1)
        assert(dpn.stackSymbols.contains(s2),"Stack symbol 2 not found in DPN " + sym2)
        
        
        val dpnTA = new MDPN2IterableTA(dpn)
        val reach1 = new TopOfStackTA("reach1",Set(s1))
        val reach1Cut = new CutTransducer(0,reach1,"cutReach1") 
        
        val reach2 = new TopOfStackTA("reach2",Set(s2))
        
        val conf = new IntersectionTA(reach1Cut,reach2)
        
        val cwf = new CutWellFormed("cwf1",0)
        val confwf = new IntersectionTA(conf,cwf)
        
        val check = new IntersectionEmptinessCheck(dpnTA,confwf,"check")
        
        val result = !XSBInterRunner.runCheck(check)
        if(result != expected){
            val wcheck = new FullWitnessIntersectionEmptinessCheck(dpnTA,confwf)
            val w = XSBInterRunner.runFullWitnessCheck(wcheck)
            println("witness: " + w)
        }
        assertEquals("Check didn't yield expected result!",expected,result)
    }
    
    
    /**
     * Parse the given DPN and check whether sym2 can be reached after reaching sym1 and compare the result to expected.
     */
    def testDoubleIterated(expected:Boolean,sym1:String,sym2:String,sym3:String,dpnString:String){
        val (dpn,naming) = DPNParser.parseDPN(dpnString).get
        
        val s1 = naming.sname(sym1)
        val s2 = naming.sname(sym2)
        val s3 = naming.sname(sym3)
        
        assert(dpn.stackSymbols.contains(s1),"Stack symbol 1 not found in DPN " + sym1)
        assert(dpn.stackSymbols.contains(s2),"Stack symbol 2 not found in DPN " + sym2)
        assert(dpn.stackSymbols.contains(s3),"Stack symbol 3 not found in DPN " + sym2)
        
        
        val dpnTA = new MDPN2IterableTA(dpn)
        val reach1 = new TopOfStackTA("reach1",Set(s1))
        val reach1Cut = new CutTransducer(0,reach1,"cut0Reach1") 
        
        val reach2 = new TopOfStackTA("reach2",Set(s2))
        
        val conf0 = new IntersectionTA(reach1Cut,reach2,"conf0")
        
        val cwf = new CutWellFormed("cwf0",0)
        val confwf = new IntersectionTA(conf0,cwf,"confwf0")
        
        val cut1conf0 = new CutTransducer(1,confwf,"cut1conf0")
        
        
        val reach3 = new TopOfStackTA("reach3",Set(s3))
        val cut1conf = new IntersectionTA(cut1conf0,reach3,"cut1conf")
        
        val cwf1 = new CutWellFormed("cwf1",1)
        val confwf2 = new IntersectionTA(cut1conf,cwf1,"confwf2")
        
        
        val check = new IntersectionEmptinessCheck(dpnTA,confwf2,"check")
        
        val result = !XSBInterRunner.runCheck(check)
        if(result != expected){
            val wcheck = new FullWitnessIntersectionEmptinessCheck(dpnTA,confwf2)
            val w = XSBInterRunner.runFullWitnessCheck(wcheck)
            println("witness: " + w)
        }
        assertEquals("Check didn't yield expected result!",expected,result)
    }
    
}
