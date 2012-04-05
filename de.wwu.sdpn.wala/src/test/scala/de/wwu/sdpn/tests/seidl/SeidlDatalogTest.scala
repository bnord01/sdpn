package de.wwu.sdpn.tests.seidl
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
import de.wwu.sdpn.core.analyses.SDPNProps
import de.wwu.sdpn.wala.dpngen.symbols.StackSymbol
import de.wwu.sdpn.core.util.EPMWrapper
import junit.framework.JUnit4TestAdapter
import org.junit.runners.Parameterized.Parameters
import org.junit.runners.Parameterized
import org.junit.runner.RunWith
import de.wwu.sdpn.wala.dpngen.MonitorDPNFactory
import com.ibm.wala.ipa.callgraph.CGNode
import de.wwu.sdpn.core.ta.datalog.reachability.MDPN2TA
import de.wwu.sdpn.core.ta.datalog.reachability.SingleSetConflictTA
import de.wwu.sdpn.core.ta.datalog.IntersectionEmptinessCheck
import de.wwu.sdpn.core.ta.xsb.reachability.{ MDPN2TA => XMDPN2TA }
import de.wwu.sdpn.core.ta.xsb.reachability.{ SingleSetConflictTA => XSSCTA }
import de.wwu.sdpn.core.ta.xsb.{ IntersectionEmptinessCheck => XIEC }
import java.io.File
import java.io.BufferedWriter
import java.io.FileOutputStream
import java.io.FileWriter
import java.io.PrintWriter

object SeidlDatalogTest {

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
    }
    //    @Parameters
    //    def data(): java.util.List[Array[Object]] = {
    //        val list = new java.util.ArrayList[Array[Object]]()
    //        list.add(Array[Object](true: java.lang.Boolean))
    //        list.add(Array[Object](false: java.lang.Boolean))
    //        return list
    //    }

    //    /**
    //     * test suite for JUnit3 and SBT compatibility
    //     */
    //    def suite(): junit.framework.Test = new JUnit4TestAdapter(classOf[SimpleAnalysesTest])
}

class SeidlDatalogTest {
    import SeidlDatalogTest.stuff

    @Test
    def testBenchmarkPrograms() {
        val progs = List(
            "Plain",
            "CrossCallsTwo",
            "CrossCallsThree",
            "CrossCallsFour",
            "CrossCallsFive",
            "CrossCallsSix",
            "CrossCallsSeven",
            "CrossCallsEight",
            "CrossCallsNine",
            "Println"
        )

        progs foreach { name =>
            val (cg, pa) = SimpleAnalyses.getCGandPAfromCP(SDPNTestProps.get.classPath, "Lbnord/benchmark/ssr/" + name)
            val mr = StringStuff.makeMethodReference("bnord.benchmark.ssr." + name + ".excludeMe()V")

            printCheck("sliced_" + name, (cg, pa, mr), true)
            printCheck("unsliced_" + name, (cg, pa, mr), false)
        }

        val tempDir = SDPNProps.get.tempDir
        val f = new File(tempDir, "runAll.sh")
        val out = new PrintWriter(new BufferedWriter(new FileWriter(f)))
        try {
            for (
                pref <- List("sliced", "unsliced");
                name <- progs
            ) {
                out.println("echo '>>>>>>   Running XSB on:        " + pref + " " + name + "    <<<<<<'")
                out.println("echo ' lines:'")
                out.printf("wc -l %s_%s_xsb.P %n", pref, name)
                out.printf("time xsb --quietload --nobanner %s_%s_xsb %n", pref, name)
                out.println("echo '>>>>>>   Running BOUND XSB on:  " + pref + " " + name + "    <<<<<<'")
                out.println("echo ' lines:'")
                out.printf("wc -l %s_%s_bound_xsb.P %n", pref, name)
                out.printf("time xsb --quietload --nobanner %s_%s_bound_xsb %n", pref, name)
                out.println("echo '>>>>>>   Running FSOLVE on :    " + pref + " " + name + "    <<<<<<'")
                out.println("echo ' lines:'")
                out.printf("wc -l %s_%s.P %n", pref, name)
                out.printf("time fsolve %s_%s.P %n", pref, name)

            }
        } finally {
            out.close()
        }
    }

    //    @Test
    //    def testUnslicedLockInsensSSR1() {
    //        
    //        val (cg, pa, mr) = stuff(1)
    //        assertFalse("There should be an Conflict", SimpleAnalyses.runSSRCheck(cg, pa, getStackSymbols(cg, mr), null, false))
    //    }
    @Test
    def testSlicedLockInsensSSR1() {
        printCheck("ssr1_sliced_true", stuff(1), true)
    }
    @Test
    def testUnslicedLockInsensSSR1() {
        printCheck("ssr1_unsliced_true", stuff(1), false)

    }
    
    def printCheck(name: String, input: (CallGraph, PointerAnalysis, MethodReference), slice: Boolean) {
        val (cg, pa, mr) = input
        val stackSyms = getStackSymbols(cg, mr)
        val sliceSet: Set[CGNode] = if (slice) stackSyms map { case StackSymbol(n, _, _) => n } else Set()
        val dpn = SimpleAnalyses.getMDPN(cg, pa, sliceSet, false)
        val fta = new MDPN2TA(dpn)
        val cta = new SingleSetConflictTA("conflict", stackSyms)
        val check = new IntersectionEmptinessCheck(fta, cta)
        val tempDir = SDPNProps.get.tempDir
        val f = new File(tempDir, name + ".P")
        val out = new BufferedWriter(new FileWriter(f))
        try {
            out.write(check.emptiness)
        } finally {
            out.close()
        }

        val xfta = new XMDPN2TA(dpn)
        val xcta = new XSSCTA("conflict", stackSyms)
        val xcheck = new XIEC(xfta, xcta)
        val f2 = new File(tempDir, name + "_xsb.P")
        val out2 = new BufferedWriter(new FileWriter(f2))
        try {
            out2.write(xcheck.emptiness)
            out2.write("\n")
            out2.write(":- " + xcheck.name + "_runCheck.")
        } finally {
            out2.close()
        }

        val f3 = new File(tempDir, name + "_bound_xsb.P")
        val out3 = new BufferedWriter(new FileWriter(f3))
        try {
            out3.write(check.emptiness.replace("?-",
                ":- table (name_ne/2).\n:-".replace("name", check.name)))
        } finally {
            out3.close()
        }
    }

    def getStackSymbols(cg: CallGraph, mr: MethodReference): Set[StackSymbol] = {
        val nodes = cg.getNodes(mr)
        var res = Set[StackSymbol]()
        nodes.foreach({ res += StackSymbol(_, 0, 0) })
        return res
    }

}