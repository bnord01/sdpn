package de.wwu.sdpn.tests.analyses

import java.io.IOException

import scala.collection.JavaConversions.asScalaSet

import org.junit.AfterClass
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertTrue
import org.junit.BeforeClass
import org.junit.Test

import com.ibm.wala.classLoader.IMethod
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.cha.IClassHierarchy
import com.ibm.wala.util.CancelException
import com.ibm.wala.util.strings.StringStuff

import de.wwu.sdpn.core.ta.xsb.XSBInterRunner
import de.wwu.sdpn.wala.analyses.DPN4IFCAnalysis
import de.wwu.sdpn.wala.analyses.SDPNTestProps
import de.wwu.sdpn.wala.analyses.SimpleAnalyses
import de.wwu.sdpn.wala.dpngen.symbols.StackSymbol
import junit.framework.JUnit4TestAdapter

object DPN4IFCTest {

    var stuff: Map[Int, (CallGraph, PointerAnalysis[InstanceKey], IClassHierarchy)] = Map()

    @BeforeClass
    def setUp() {
        for (i <- 1 to 3) {
            val (cg, pa) = SimpleAnalyses.getCGandPAfromCP(SDPNTestProps.get.classPath, "Lbnord/unittests/dpn4ifc/BSP0" + i)
            stuff += i -> (cg, pa, cg.getClassHierarchy)
        }
        /*
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
        * 
        */
    }

    @AfterClass
    def tearDown() {
        stuff = null
        XSBInterRunner.shutdown();
    }

    /**
     * test suite for JUnit3 and SBT compatibility
     */
    def suite(): junit.framework.Test = new JUnit4TestAdapter(classOf[DPN4IFCTest])
}

class DPN4IFCTest {
    import DPN4IFCTest.stuff

    @Test
    def testIndexMappingWithDPN4IFC {
        val (cg, pa, cha) = stuff(1)
        val dia = new DPN4IFCAnalysis(cg, pa)
        val im = mrr("bnord.unittests.dpn4ifc.BSP01.p2()V", cha)
        val nodes = cg.getNodes(im.getReference())
        assert(nodes.size == 1)
        val node = nodes.head
        val expResult = StackSymbol(node, 2, 0)
        val realResult = dia.getSS4NodeAndIndex(node, 5)
//        println(realResult)
        assertTrue("Got wrong StackSymbol got " + realResult + "\t expected" + expResult, expResult == realResult)

    }

    @Test
    def testBSP01MHS {
        val (cg, pa, cha) = stuff(1)
        val dia = new DPN4IFCAnalysis(cg, pa)
        var im = mrr("bnord.unittests.dpn4ifc.BSP01.p2()V", cha)
        var nodes = cg.getNodes(im.getReference())
        assert(nodes.size == 1)
        var node = nodes.head
        val writePos = StackSymbol(node, 2, 0)

        im = mrr("bnord.unittests.dpn4ifc.BSP01.p1()V", cha)
        nodes = cg.getNodes(im.getReference())
        assert(nodes.size == 1)
        node = nodes.head
        val readPos = StackSymbol(node, 5, 0)
        dia.init(null/*new PrintProgressMonitor*/)
        val res = dia.mayHappenSuccessively(writePos, readPos, null/*new PrintProgressMonitor*/)
        assertFalse("there should be no flow", res)

    }

    @Test
    def testBSP02MHS {
        val (cg, pa, cha) = stuff(2)
        val dia = new DPN4IFCAnalysis(cg, pa)
        var im = mrr("bnord.unittests.dpn4ifc.BSP02.p2()V", cha)
        var nodes = cg.getNodes(im.getReference())
        assert(nodes.size == 1)
        var node = nodes.head
        val writePos = StackSymbol(node, 2, 0)

        im = mrr("bnord.unittests.dpn4ifc.BSP02.p1()V", cha)
        nodes = cg.getNodes(im.getReference())
        assert(nodes.size == 1)
        node = nodes.head
        val readPos = StackSymbol(node, 9, 0)
        dia.init(null/*new PrintProgressMonitor*/)
        val res = dia.mayHappenSuccessively(writePos, readPos, null/*new PrintProgressMonitor*/)
        assertTrue("there should be flow", res)

    }

    @Test
    def testBSP03MHS {
        val (cg, pa, cha) = stuff(3)
        val dia = new DPN4IFCAnalysis(cg, pa)
        var im = mrr("bnord.unittests.dpn4ifc.BSP03.p2()V", cha)
        var nodes = cg.getNodes(im.getReference())
        assert(nodes.size == 1, "Expected 1 node but got: " + nodes.size)
        var node = nodes.head
//        println(node.getIR)
        val writePos = StackSymbol(node, 1, 0)

        im = mrr("bnord.unittests.dpn4ifc.BSP03.p1()V", cha)
        nodes = cg.getNodes(im.getReference())
        assert(nodes.size == 1)
        node = nodes.head
//        println(node.getIR)
        val readPos = StackSymbol(node, 2, 0)
        dia.init(null/*new PrintProgressMonitor*/)
        val res = dia.mayHappenSuccessively(writePos, readPos, null/*new PrintProgressMonitor*/)
        assertFalse("there should be no flow", res)

    }

    @Test
    def testBSP01MHP {
        val (cg, pa, cha) = stuff(1)
        val dia = new DPN4IFCAnalysis(cg, pa)
        var im = mrr("bnord.unittests.dpn4ifc.BSP01.p2()V", cha)
        var nodes = cg.getNodes(im.getReference())
        assert(nodes.size == 1)
        var node = nodes.head
        val writePos = StackSymbol(node, 2, 0)

        im = mrr("bnord.unittests.dpn4ifc.BSP01.p1()V", cha)
        nodes = cg.getNodes(im.getReference())
        assert(nodes.size == 1)
        node = nodes.head
        val readPos = StackSymbol(node, 5, 0)
        dia.init(null/*new PrintProgressMonitor*/)
        val res = dia.mayHappenInParallel(writePos, readPos, null/*new PrintProgressMonitor*/)
        assertFalse("there should be no flow", res)

    }

    @Test
    def testBSP01MHP2 {
        val (cg, pa, cha) = stuff(1)
        val dia = new DPN4IFCAnalysis(cg, pa)
        var im = mrr("bnord.unittests.dpn4ifc.BSP01.p2()V", cha)
        var nodes = cg.getNodes(im.getReference())
        assert(nodes.size == 1)
        var node = nodes.head
        val beforeWritePos = StackSymbol(node, 0, 0)

        im = mrr("bnord.unittests.dpn4ifc.BSP01.p1()V", cha)
        nodes = cg.getNodes(im.getReference())
        assert(nodes.size == 1)
        node = nodes.head
        val readPos = StackSymbol(node, 5, 0)
        dia.init(null/*new PrintProgressMonitor*/)
        val res = dia.mayHappenInParallel(beforeWritePos, readPos, null/*new PrintProgressMonitor*/)
        assertTrue("there should be no flow", res)

    }

    def mrr(methodSig: String, cha: IClassHierarchy): IMethod = {
        val mr = StringStuff.makeMethodReference(methodSig)

        val m = cha.resolveMethod(mr);
        assertNotNull("IMethod Null", m)
        return m

    }

}