package de.wwu.sdpn.tests.randomisolation
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

object SimpleRITest {

    var stuff: Map[Int, (CallGraph, PointerAnalysis, MethodReference, InstanceKey)] = Map()

    @BeforeClass
    def setUp() {
        for (i <- 1 to 2) {
            val (cg, pa) = SimpleAnalyses.getCGandPAfromCP(SDPNTestProps.get.classPath, "Lbnord/unittests/randomisolation/BSP0" + i)
            val mr = StringStuff.makeMethodReference("bnord.unittests.randomisolation.BSP0" + i + ".excludeMe()V")
            val nodes = cg.getNodes(mr)
            assert(nodes.size == 1, "Found " + nodes.size + " nodes representing the excludeMe-Method, expected 1.")
            val node = nodes.head
            val vn = node.getIR.getSymbolTable().getParameter(0)
            val pk = pa.getHeapModel.getPointerKeyForLocal(node, vn)
            val iks = pa.getPointsToSet(pk)
            assert(iks.size == 1, "Found " + iks.size + " instancekeys representing the this pointer of the excludeMe-Method, expected 1.")
            stuff += i -> (cg, pa, mr, iks.head)
        }
    }

    @AfterClass
    def tearDown() {
        stuff = null
        XSBInterRunner.shutdown();
    }

//    /**
//     * test suite for JUnit3 and SBT compatibility
//     */
//    def suite(): junit.framework.Test = new JUnit4TestAdapter(classOf[SimpleRITest])

}

class SimpleRITest {
    import SimpleRITest.stuff

    @Test
    def testRIBsp01() {
        runCheck(1, true)
    }
    @Test
    def testRIBsp02() {
        runCheck(2, false)
    }

    def runCheck(nr: Int, expectedResult: Boolean) {
        val (cg, pa, mr, ik) = stuff(nr)
        val sliceSet = cg.getNodes(mr)
        val cgnode = sliceSet.head
        val mdpn = SimpleAnalyses.getMDPN(cg, pa, sliceSet, false)
        val ridpn = new RIDPN(mdpn, ik, "0", pa)

        val ss = ridpn.getStackSymbols

        implicit def ikToTerm(iks: InstanceKey) = new HasTermRepresentation {
            def toTerm = {
                assert(iks == ik, "Tried to convert instance key other than the one beeing isolated")
                "0"
            }
        }

        val confSet = Set[RISymbol[InstanceKey, StackSymbol]](Isolated(ik, StackSymbol(cgnode, 0, 0)))

        require(confSet.subsetOf(ss), "Some symbols of confSet are not contained in the DPN!")
        val (td, bu) = SingleSetReachability.genAutomata(ridpn, confSet)
        val check = SingleSetReachability.genCheck(td, bu)
        assert(XSBInterRunner.runCheck(check, null) == expectedResult, "There should" + (if (expectedResult) "n't" else "") + " be a conflict.")
    }

    // TODO This doesn't terminate! Check Why! 
    //@Test 
    def testStdLibCheck() {
        val (cg, pa, mr, ik) = stuff(1)
        val sliceSet = cg.getNodes(mr)
        val cgnode = sliceSet.head
        val mdpn = SimpleAnalyses.getMDPN(cg, pa, sliceSet, false)
        val ridpn = new RIDPN(mdpn, ik, "0", pa)

        val ss = ridpn.getStackSymbols

        implicit def ikToTerm(iks: InstanceKey) = new HasTermRepresentation {
            def toTerm = {
                assert(iks == ik, "Tried to convert instance key other than the one beeing isolated")
                "0"
            }
        }

        val confSet = Set[RISymbol[InstanceKey, StackSymbol]](Isolated(ik, StackSymbol(cgnode, 0, 0)))

        require(confSet.subsetOf(ss), "Some symbols of confSet are not contained in the DPN!")
        val (td, bu) = SingleSetReachability.genStdLibAutomata(ridpn, confSet)
        val check = SingleSetReachability.genCheck(td, bu)
        assertTrue("There shouldn't be a conflict.", XSBInterRunner.runCheck(check, null))
    }

}