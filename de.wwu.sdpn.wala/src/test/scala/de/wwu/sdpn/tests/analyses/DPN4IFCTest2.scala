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
import junit.framework.JUnit4TestAdapter
import de.wwu.sdpn.core.ta.xsb.XSBInterRunner
import de.wwu.sdpn.core.util.PrintProgressMonitor
import com.ibm.wala.ipa.callgraph.CGNode

/**
 * JUnit tests for DPN4IFCAnalysis.mayFlowFromTo
 * Utilizes classes bnord.unittests.dpn4ifc.BSP0x
 * Assumes read in p1()V and write in p2()V
 */
object DPN4IFCTest2 {

    var stuff: Map[Int, (CallGraph, PointerAnalysis, CGNode, CGNode)] = Map()

    @BeforeClass
    def setUp() {
        for (i <- 1 to 3) {
            val (cg, pa) = SimpleAnalyses.getCGandPAfromCP(SDPNTestProps.get.classPath, "Lbnord/unittests/dpn4ifc/BSP0" + i)
            val ip1 = mrr("bnord.unittests.dpn4ifc.BSP0" + i + ".p1()V", cg.getClassHierarchy())
            val ip2 = mrr("bnord.unittests.dpn4ifc.BSP0" + i + ".p2()V", cg.getClassHierarchy())
            val n1s = cg.getNodes(ip1.getReference())
            val n2s = cg.getNodes(ip2.getReference())
            assert(n1s.size == 1 && n2s.size == 1)
            val p1 = n1s.head
            val p2 = n2s.head
            stuff += i -> (cg, pa, p1, p2)
        }
        import com.codahale.logula.Logging
        import org.apache.log4j.Level

        Logging.configure { log =>
            log.registerWithJMX = true

            log.level = Level.TRACE
            //log.loggers("com.myproject.weebits") = Level.OFF

            log.console.enabled = true
            log.console.threshold = Level.TRACE

            log.file.enabled = true
            log.file.filename = "/tmp/sdpn/DPN4IFCTest.log"
            log.file.maxSize = 10 * 1024 // KB
            log.file.retainedFiles = 5 // keep five old logs around

            // syslog integration is always via a network socket
            //log.syslog.enabled = true
            //log.syslog.host = "syslog-001.internal.example.com"
            //log.syslog.facility = "local3"
        }
    }

    @AfterClass
    def tearDown() {
        stuff = null
        XSBInterRunner.shutdown();
    }

    def mrr(methodSig: String, cha: IClassHierarchy): IMethod = {
        val mr = StringStuff.makeMethodReference(methodSig)

        val m = cha.resolveMethod(mr);
        assertNotNull("IMethod Null", m)
        return m

    }

    /**
     * test suite for JUnit3 and SBT compatibility
     */
    def suite(): junit.framework.Test = new JUnit4TestAdapter(classOf[DPN4IFCTest])
}

class DPN4IFCTest2 {
    import DPN4IFCTest2.stuff

    @Test
    def printIR1 = printIR(1)

    def printIR(i: Int) {
        val (cg, pa) = SimpleAnalyses.getCGandPAfromCP(SDPNTestProps.get.classPath, "Lbnord/unittests/dpn4ifc/BSP0" + i)
        val ip1 = mrr("bnord.unittests.dpn4ifc.BSP0" + i + ".p1()V", cg.getClassHierarchy())
        val ip2 = mrr("bnord.unittests.dpn4ifc.BSP0" + i + ".p2()V", cg.getClassHierarchy())
        val n1s = cg.getNodes(ip1.getReference())
        val n2s = cg.getNodes(ip2.getReference())
        assert(n1s.size == 1 && n2s.size == 1)
        val n1 = n1s.head
        val n2 = n2s.head
        println("Instructions for bnord.unittests.dpn4ifc.BSP0" + i + ".p1()V")
        println(n1.getIR().getInstructions().zipWithIndex.mkString("\n"))
        println("Instructions for bnord.unittests.dpn4ifc.BSP0" + i + ".p2()V")
        println(n2.getIR().getInstructions().zipWithIndex.mkString("\n"))
    }

    @Test
    def testBSP01MF {
        val (cg, pa, readNode, writeNode) = stuff(1)
        val dia = new DPN4IFCAnalysis(cg, pa)
        dia.init()
        val readIdx = 10
        val writeIdx = 5
        val res = dia.mayFlowFromTo(writeNode, writeIdx, readNode, readIdx)
        assertFalse("there should be no flow", res)
    }

    def mrr(methodSig: String, cha: IClassHierarchy): IMethod = {
        val mr = StringStuff.makeMethodReference(methodSig)

        val m = cha.resolveMethod(mr);
        assertNotNull("IMethod Null", m)
        return m

    }

}