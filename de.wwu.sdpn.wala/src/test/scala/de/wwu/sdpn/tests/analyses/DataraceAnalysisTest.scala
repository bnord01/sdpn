package de.wwu.sdpn.tests.analyses
import org.junit.Test
import org.junit.BeforeClass
import org.junit.AfterClass
import com.ibm.wala.classLoader.IMethod
import scala.collection.JavaConversions._
import com.ibm.wala.util.strings.StringStuff
import org.junit.Assert._
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.types.MethodReference
import com.ibm.wala.ipa.cha.IClassHierarchy
import de.wwu.sdpn.wala.analyses.SimpleAnalyses
import de.wwu.sdpn.wala.analyses.SDPNTestProps
import de.wwu.sdpn.wala.analyses.DPN4IFCAnalysis
import de.wwu.sdpn.wala.dpngen.symbols.StackSymbol
import de.wwu.sdpn.wala.dpngen.symbols.StackSymbol
import de.wwu.sdpn.wala.analyses.datarace._
import de.wwu.sdpn.core.result._
import junit.framework.JUnit4TestAdapter
import de.wwu.sdpn.core.ta.xsb.XSBInterRunner
import org.junit.runners.Parameterized.Parameters
import org.junit.runners.Parameterized
import org.junit.runner.RunWith
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey

object DataraceAnalysisTest {

    var stuff: Map[Int, (CallGraph, PointerAnalysis[InstanceKey], IClassHierarchy)] = Map()

    @BeforeClass
    def setUp() {
        for (i <- 1 to 3) {
            val (cg, pa) = SimpleAnalyses.getCGandPAfromCP(SDPNTestProps.get.classPath, "Lbnord/unittests/datarace/BSP0" + i)
            stuff += i -> (cg, pa, cg.getClassHierarchy)
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

//    /**
//     * test suite for JUnit3 and SBT compatibility
//     */
//    def suite(): junit.framework.Test = new JUnit4TestAdapter(classOf[DataraceAnalysisTest])
}

@RunWith(classOf[Parameterized])
class DataraceAnalysisTest (useIPL: Boolean) {
    SimpleAnalyses.useInterprolog(useIPL)
    import DataraceAnalysisTest.stuff

    @Test
    def testBSP01 {
        runTest(1, false)
    }

    @Test
    def testBSP02 {
        runTest(2, true)
    }

    @Test
    def testBSP03 {
        runTest(3, false)
    }

    @Test
    def testDetailedBSP01 {
        runDetailedTest(3, Negative)
    }

    def runTest(num: Int, expectedResult: Boolean) {
        val (cg, pa, cha) = stuff(num)
        val dra = new DataraceAnalysis(cg, pa)

        println()
        println("Testing BSP0" + num + " expect " + (if (expectedResult) "a" else "no") + " data race")
        for (m <- dra.fieldMap)
            println(m)
        println("Number of Fields: " + dra.fieldMap.size)
        println("Number of read/writes: " + dra.fieldMap.map((x => x._2._1.size + x._2._2.size)).sum)
        if (expectedResult)
            assertTrue("There should be any race!", dra.anyRacePossible)
        else
            assertFalse("There shouldn't be any race!", dra.anyRacePossible)
    }

    def runDetailedTest(num: Int, expectedResult: ResultValue) {
        val (cg, pa, cha) = stuff(num)
        val dra = new DataraceAnalysis(cg, pa)

        println()
        println("Testing BSP0" + num + " expect " + expectedResult)

        for (m <- dra.fieldMap)
            println(m)
        println("Number of Fields: " + dra.fieldMap.size)
        println("Number of read/writes: " + dra.fieldMap.map((x => x._2._1.size + x._2._2.size)).sum)

        val result = dra.fullDetailedAnalysis()
        println(result)
        assertEquals("Wrong Result!", expectedResult, result.value)
    }

}