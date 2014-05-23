package de.wwu.sdpn.tests.analyses

import com.ibm.wala.types.MethodReference
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import org.junit.BeforeClass
import de.wwu.sdpn.wala.analyses.SimpleAnalyses
import de.wwu.sdpn.wala.analyses.SDPNTestProps
import com.ibm.wala.util.strings.StringStuff
import org.junit.AfterClass
import scala.collection.JavaConversions._
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import org.junit.Test
import org.junit.Assert._
import de.wwu.sdpn.wala.ri.RIDPN
import de.wwu.sdpn.core.gui.MonitorDPNView
import de.wwu.sdpn.core.analyses.SingleSetReachability
import de.wwu.sdpn.core.ta.xsb.XSBInterRunner
import de.wwu.sdpn.wala.ri.Isolated
import de.wwu.sdpn.wala.ri.RISymbol
import de.wwu.sdpn.core.ta.xsb.HasTermRepresentation
import de.wwu.sdpn.wala.dpngen.symbols.StackSymbol
import junit.framework.JUnit4TestAdapter
import com.ibm.wala.ipa.callgraph.CGNode
import de.wwu.sdpn.wala.dpngen.symbols.DPNAction

object DPNReachabilityTest {

    var stuff: Map[Int, (CallGraph, PointerAnalysis[InstanceKey], CGNode)] = Map()

    @BeforeClass
    def setUp() {
        for (i <- 1 to 1) {
            val (cg, pa) = SimpleAnalyses.getCGandPAfromCP(SDPNTestProps.get.classPath, "Lbnord/unittests/iterable/Test0" + i)
            val mr = StringStuff.makeMethodReference("bnord.unittests.iterable.Test0" + i + ".excludeMe()V")
            val nodes = cg.getNodes(mr)
            assert(nodes.size == 1, "Found " + nodes.size + " nodes representing the excludeMe-Method, expected 1.")
            val node = nodes.head
            stuff += i -> (cg, pa, node)
        }
    }

    @AfterClass
    def tearDown() {
        stuff = null
        XSBInterRunner.shutdown();
    }
}

class DPNReachabilityTest {
    import DPNReachabilityTest.stuff

    @Test
    def test01() {
        runCheck(1,true)
    }
    
    def runCheck(nr: Int, expectedResult: Boolean) {
        val (cg, pa, cgnode) = stuff(nr)
        val dpn = SimpleAnalyses.getMDPN(cg, pa, Set(cgnode), true)

        
        val aa = Map() ++ dpn.actions.zipWithIndex
        val la = Map() ++ dpn.locks.zipWithIndex
        implicit def a2htr(c:DPNAction) = new HasTermRepresentation{ def toTerm = aa(c).toString}
        implicit def l2htr(c:InstanceKey) = new HasTermRepresentation{ def toTerm = la(c).toString}
        
//        implicit def ikToTerm(iks: DPNAction) = new HasTermRepresentation {
//            def toTerm = "0"
//        }

        val confSet =  Set(StackSymbol(cgnode, 0, 0))

        require(confSet.subsetOf(dpn.stackSymbols), "Some symbols of confSet are not contained in the DPN!")
        
        val res = de.wwu.sdpn.core.analyses.DPNReachability.runIRCheck(dpn, List(confSet), true)
        
        for(r <- dpn.getTransitions)
            println(r)
            
        import java.io._
        val ot = new PrintWriter(new FileWriter("/tmp/DPNReachTest" + nr + ".txt"))
        try {ot.println(de.wwu.sdpn.core.dpn.monitor.DPNUtil.printMDPN(dpn))}
        finally {ot.close}        
        
        assert(res == expectedResult)
        
        
    }


}