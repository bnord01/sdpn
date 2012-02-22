package de.wwu.sdpn.tests.analyses
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import org.junit.BeforeClass
import org.junit.AfterClass
import org.junit.Test
import com.ibm.wala.util.strings.StringStuff
import scala.collection.JavaConversions._
import scala.collection.Set
import org.junit.Assert._
import com.ibm.wala.types.MethodReference
import de.wwu.sdpn.wala.analyses.SimpleAnalyses
import de.wwu.sdpn.wala.analyses.SDPNTestProps
import de.wwu.sdpn.wala.dpngen.symbols.StackSymbol
import de.wwu.sdpn.core.util.EPMWrapper
import junit.framework.JUnit4TestAdapter
import de.wwu.sdpn.core.ta.xsb.XSBInterRunner

object SimpleAnalysesTest {

    var stuff: Map[Int, (CallGraph, PointerAnalysis, MethodReference)] = Map()

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

//    /**
//     * test suite for JUnit3 and SBT compatibility
//     */
//    def suite(): junit.framework.Test = new JUnit4TestAdapter(classOf[SimpleAnalysesTest])
}
class SimpleAnalysesTest {
    import SimpleAnalysesTest.stuff
    @Test
    def testPrintCGandStackSymbols() {
        val (cg, pa, mr) = stuff(1)
        println("Number of CGNodes:\t" + cg.getNumberOfNodes())
        println("Identified StackSymbols:\t" + getStackSymbols(cg, mr))
    }

    @Test
    def testUnslicedLockInsensSSR1() {
        val (cg, pa, mr) = stuff(1)
        assertFalse("There should be an Conflict", SimpleAnalyses.runSSRCheck(cg, pa, getStackSymbols(cg, mr), null, false))
    }
    @Test
    def testSlicedLockInsensSSR1() {
        val (cg, pa, mr) = stuff(1)
        val nodes = cg.getNodes(mr)
        assertFalse("There should be an Conflict", SimpleAnalyses.runSSRCheck(cg, pa, getStackSymbols(cg, mr), nodes, false))
    }
    @Test
    def testUnslicedLockInsensTSR1() {
        val (cg, pa, mr) = stuff(1)
        assertFalse("There should be an Conflict", SimpleAnalyses.runTSRCheck(cg, pa, getStackSymbols(cg, mr), getStackSymbols(cg, mr), null, false))
    }

    @Test
    def testSlicedLockInsensWitnessTSR1() {
        val (cg, pa, mr) = stuff(1)
        val nodes = cg.getNodes(mr)
        SimpleAnalyses.runWitnessTSRCheck(cg, pa, getStackSymbols(cg, mr), getStackSymbols(cg, mr), nodes, false) match {
            case None    => fail
            case Some(x) => println("Witness: " + x)
        }
    }

    @Test
    def testSlicedLockSensSSR1() {
        val (cg, pa, mr) = stuff(2)
        val nodes = cg.getNodes(mr)
        assertTrue("There shouldn't be an Conflict", SimpleAnalyses.runSSRCheck(cg, pa, getStackSymbols(cg, mr), nodes, true))
    }
    @Test
    def testStdLibSlicedLockSensSSR1() {
        val (cg, pa, mr) = stuff(2)
        val nodes = cg.getNodes(mr)
        assertTrue("There shouldn't be an Conflict", SimpleAnalyses.runStdLibSSRCheck(cg, pa, getStackSymbols(cg, mr), nodes))
    }

    @Test
    def testSlicedLockSensSSR1LS() {
        val (cg, pa, mr) = stuff(2)
        val nodes = cg.getNodes(mr)
        var locks = SimpleAnalyses.getPossibleLocks(cg, pa)
        locks = SimpleAnalyses.filterByClassLoader(locks)
        println("PossibleLocks: " + locks)
        assertTrue("There shouldn't be an Conflict", SimpleAnalyses.runSSRCheck(cg, pa, getStackSymbols(cg, mr), nodes, locks))
    }

    @Test
    def testSlicedLockSensSSR2() {
        val (cg, pa, mr) = stuff(3)
        val nodes = cg.getNodes(mr)
        assertTrue("There shouldn't be an Conflict", SimpleAnalyses.runSSRCheck(cg, pa, getStackSymbols(cg, mr), nodes, true))
    }
    @Test
    def testSlicedLockSensSSR7() {
        val (cg, pa, mr) = stuff(7)
        val nodes = cg.getNodes(mr)
        assertTrue("There shouldn't be an Conflict", SimpleAnalyses.runSSRCheck(cg, pa, getStackSymbols(cg, mr), nodes, true))
    }
    @Test
    def testSlicedStdLibLockSensSSR2() {
        val (cg, pa, mr) = stuff(3)
        val nodes = cg.getNodes(mr)
        assertTrue("There shouldn't be an Conflict", SimpleAnalyses.runStdLibSSRCheck(cg, pa, getStackSymbols(cg, mr), nodes))
    }

    @Test
    def testSlicedLockSensSSR2LS() {
        val (cg, pa, mr) = stuff(3)
        val nodes = cg.getNodes(mr)
        var locks = SimpleAnalyses.getPossibleLocks(cg, pa)
        locks = SimpleAnalyses.filterByClassLoader(locks)
        println("PossibleLocks: " + locks)
        assertTrue("There shouldn't be an Conflict", SimpleAnalyses.runSSRCheck(cg, pa, getStackSymbols(cg, mr), nodes, locks))
    }

    @Test
    def testSlicedWitnessLockSensTSR2() {
        val (cg, pa, mr) = stuff(3)
        val nodes = cg.getNodes(mr)
        val res = SimpleAnalyses.runWitnessTSRCheck(cg, pa, getStackSymbols(cg, mr), getStackSymbols(cg, mr), nodes, true)

        assertTrue("There shouldn't be an Conflict", res == None)
    }

    @Test
    def testSlicedWitnessLockSensTSR3() {
        val (cg, pa, mr) = stuff(5)
        val nodes = cg.getNodes(mr)
        val res = SimpleAnalyses.runWitnessTSRCheck(cg, pa, getStackSymbols(cg, mr), getStackSymbols(cg, mr), nodes, true)

        assertFalse("There should be an Conflict", res == None)
    }

    @Test
    def testStdLibUnslicedLockSensSR2() {
        val (cg, pa, mr) = stuff(2)
        assertTrue("There shouldn't be an Conflict", SimpleAnalyses.runStdLibSSRCheck(cg, pa, getStackSymbols(cg, mr), null))
    }

    @Test
    def testUnslicedLockSensSR6() {
        val (cg, pa, mr) = stuff(6)
        assertFalse("There should be an Conflict", SimpleAnalyses.runSSRCheck(cg, pa, getStackSymbols(cg, mr), null, true))
    }
    @Test
    def testStdLibUnslicedLockSensSR6() {
        val (cg, pa, mr) = stuff(6)
        assertFalse("There should be an Conflict", SimpleAnalyses.runStdLibSSRCheck(cg, pa, getStackSymbols(cg, mr), null))
    }

    //@Test // This is a bad test because it's not race free
    def testUnslicedLockInsensSSR1Cancel() {
        val (cg, pa, mr) = stuff(1)
        val epm = new PrintingPM()
        val pm = new EPMWrapper(epm)
        new Thread() {
            override def run() {
                Thread.sleep(40)
                pm.setCanceled(true)
            }
        }.start()
        try {
            SimpleAnalyses.runSSRCheck(cg, pa, getStackSymbols(cg, mr), null, false, pm)
        } catch {
            case e: InterruptedException => println("Catched expected exception: " + e)
            case e: RuntimeException     => if (!("Canceled!" == e.getMessage())) throw e else println("Catched expected exception: " + e)
        }
        fail("XSB wasn't canceld! (most likely bad timing)")
    }

    def getStackSymbols(cg: CallGraph, mr: MethodReference): Set[StackSymbol] = {
        val nodes = cg.getNodes(mr)
        var res = Set[StackSymbol]()
        nodes.foreach({ res += StackSymbol(_, 0, 0) })
        return res
    }

}