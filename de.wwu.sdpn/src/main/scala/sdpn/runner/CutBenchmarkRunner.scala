package sdpn.runner

import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import java.io.IOException
import java.io.InputStreamReader
import java.io.PrintWriter
import scala.annotation.elidable
import scala.collection.JavaConversions.asScalaSet
import scala.collection.JavaConversions.collectionAsScalaIterable
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.types.FieldReference
import com.ibm.wala.types.TypeName
import com.ibm.wala.util.strings.StringStuff
import sdpn.analysis.MyPreAnalysis
import sdpn.dpn.explicit.example.DPNFactory
import sdpn.dpn.explicit.StackSymbol
import sdpn.dpn.explicit.monitor.MonitorDPNFactory
import sdpn.dpn.explicit.BaseRule
import sdpn.ta.prolog.cuts.CutAcqStructComplTA
import sdpn.ta.prolog.cuts.CutAcqStructPrecutTA
import sdpn.ta.prolog.cuts.CutReleaseStructTA
import sdpn.ta.prolog.cuts.CutWellFormed
import sdpn.ta.prolog.cuts.DPN2CutTA
import sdpn.ta.prolog.cuts.FieldAccessAnnotations
import sdpn.ta.prolog.cuts.FwdCutLockSet
import sdpn.ta.prolog.cuts.IFlowNoOverwrite
import sdpn.ta.prolog.cuts.IFlowReading
import sdpn.ta.prolog.cuts.IFlowWriting
import sdpn.ta.prolog.reachability.DPN2TA
import sdpn.ta.prolog.reachability.FwdLockSet
import sdpn.ta.prolog.reachability.IntLockTA
import sdpn.ta.IntersectionEmptinessCheck
import sdpn.ta.IntersectionTA
import sdpn.ta.prolog.reachability.MDPN2TA
import sdpn.ta.prolog.reachability.SingleSetConflictTA
import sdpn.ta.WitnessIntersectionEmptinessCheck
import sdpn.util.BackwardSliceFilter
import sdpn.util.LockLocator
import sdpn.util.MonitorMatcher
import sdpn.util.UniqueInstanceLocator
import sdpn.analysis.SSRProps
import sdpn.analysis.XSBRunner

/**
 * Mockup to benchmark the iterated reachability analysis
 *
 * @author Benedikt Nordhoff
 */
object CutBenchmarkRunner {

    import System.{ currentTimeMillis => now }
    def main(args: Array[String]): Unit = {
        println("#### Running Benchmarks without conflict ####")
        println("--- Running sliced analysis without witness ----")
        for (mainClass <- List("CutTestOne","CutTestTwo","CutTestThree","CutTestFour","CutTestFive","CutTestSix")) {
            runCutAnalysis(mainClass, true, false)
            println("--------")            
        }
        println("--- Running sliced analysis with witness ----")
        for (mainClass <- List("CutTestOne","CutTestTwo","CutTestThree","CutTestFour","CutTestFive","CutTestSix")) {
            runCutAnalysis(mainClass, true, true)
            println("--------")            
        }
        
        println("#### Running Benchmarks with conflict ####")
        println("--- Running sliced analysis without witness ----")
        for (mainClass <- List("ConfCutTestOne","ConfCutTestTwo","ConfCutTestThree","ConfCutTestFour","ConfCutTestFive","ConfCutTestSix")) {
            runCutAnalysis(mainClass, true, false)
            println("--------")            
        }
        println("--- Running sliced analysis with witness ----")
        for (mainClass <- List("ConfCutTestOne","ConfCutTestTwo","ConfCutTestThree","ConfCutTestFour")) {
            runCutAnalysis(mainClass, true, true)
            println("--------")            
        }
        
        
        println("--- Running unsliced analysis without witness ----")
        for (mainClass <- List("CutTestOne","CutTestTwo","CutTestThree","CutTestFour","CutTestFive","CutTestSix","ConfCutTestOne","ConfCutTestTwo","ConfCutTestThree","ConfCutTestFour","ConfCutTestFive","ConfCutTestSix")) {
            runCutAnalysis(mainClass, false, false)
            println("--------")            
        }        

    }

    def runCutAnalysis(mainClass: String, slicing: Boolean, witness: Boolean) {
        mc = "Lbnord/benchmark/cuts/" + mainClass
        val t0 = now
        analysis = MyPreAnalysis.getStd(props.classPath, mc)
        val twala = now - t0
        confMethodSig = "bnord.benchmark.cuts." + mainClass + ".excludeMe()V"
        mr = StringStuff.makeMethodReference(confMethodSig)
        confSet = asScalaSet(analysis.cg.getNodes(mr))
        this.witness = witness
        this.slicing = slicing

        println("MainClass:\t\t" + mainClass)
        var cmd = genCutLockPrologTA
        runProlog(cmd)
        println("Time Wala:\t\t" + twala + "ms")
    }

    val props = SSRProps.get

    val xsbExe = props.xsbExe

    var witness = props.witness

    var slicing = props.slicing

    val tempDir = props.tempDir
    var mc = "Lbnord/examples/CutTest"
    var analysis = MyPreAnalysis.getStd(props.classPath, mc)

    var confMethodSig = "bnord.examples.CutTest.excludeMe()V"

    var mr = StringStuff.makeMethodReference(confMethodSig)
    var confSet: scala.collection.Set[CGNode] = asScalaSet(analysis.cg.getNodes(mr))

    val lockType = TypeName.findOrCreate(props.lockType)

    /**
     * Write iterated lock-sensitive tree-automata analysis to lscutflow.P
     */
    def genCutLockPrologTA = {
        val time1 = System.currentTimeMillis

        val timewala = System.currentTimeMillis

        val dpnfac = if (slicing) new MyPreAnalysis(analysis) with DPNFactory with MonitorMatcher with FieldAccessAnnotations with DPN2CutTA with LockLocator with UniqueInstanceLocator with BackwardSliceFilter {
            override val fieldOfInterest: (InstanceKey, FieldReference) = {
                val mainType = TypeName.findOrCreate(mc)
                val mks = pa.getInstanceKeys.filter(_.getConcreteType.getName == mainType)
                assert(mks.size == 1, "More than one InstanceKey for class found")
                val mik = mks.first
                assert(uniqueInstances(mik), "Instance key for field class not unique")
                val mic = mik.getConcreteType
                val fsig = mc + ".myfield I"
                val aif = mic.getAllInstanceFields.filter(x => x.getReference.getSignature == fsig)
                assert(aif.size == 1)

                (mik, aif.first.getReference)
            }

            val initialSet = confSet

            val name = "cflow"

            val safeLocks = {
                var m = Map[InstanceKey, Int]()
                var i = 0
                for (lock: InstanceKey <- locks.intersect(uniqueInstances)) {
                    if (lock.getConcreteType().getName().equals(lockType)) {
                        m += lock -> i
                        i += 1
                    }

                }
                m
            }
        }
        else new MyPreAnalysis(analysis) with DPNFactory with MonitorMatcher with FieldAccessAnnotations with DPN2CutTA with LockLocator with UniqueInstanceLocator {
            override val fieldOfInterest: (InstanceKey, FieldReference) = {
                val mainType = TypeName.findOrCreate(mc)
                val mks = pa.getInstanceKeys.filter(_.getConcreteType.getName == mainType)
                assert(mks.size == 1, "More than one InstanceKey for class found")
                val mik = mks.first
                assert(uniqueInstances(mik), "Instance key for field class not unique")
                val mic = mik.getConcreteType
                val fsig = mc + ".myfield I"
                val aif = mic.getAllInstanceFields.filter(x => x.getReference.getSignature == fsig)
                assert(aif.size == 1)

                (mik, aif.first.getReference)
            }

            val initalSet = confSet

            val name = "cflow"

            val safeLocks = {
                var m = Map[InstanceKey, Int]()
                var i = 0
                for (lock: InstanceKey <- locks.intersect(uniqueInstances)) {
                    if (lock.getConcreteType().getName().equals(lockType)) {
                        m += lock -> i
                        i += 1
                    }

                }
                m
            }
        }

        val fwdLS = new FwdCutLockSet("fwdLS", dpnfac.safeLocks.size)

        val flowls = new IntersectionTA(dpnfac, fwdLS) {
            override val name = "flowls"
        }

        var readStack = Set[StackSymbol]()
        var writeStack = Set[StackSymbol]()
        for (rule <- dpnfac.getDPN.getTransitions) {
            rule match {
                case BaseRule(_, _, _, _, toStack) =>
                    dpnfac.annotateRule(rule) match {
                        case "read" =>
                            readStack += toStack
                        case "write" =>
                            writeStack += toStack
                        case _ =>
                    }
                case _ =>
            }
        }

        val ifno = new IFlowNoOverwrite("ifno")
        val ifread = new IFlowReading("ifread", readStack)
        val ifwrite = new IFlowWriting("ifwrite", writeStack)

        val conflict = new IntersectionTA(ifno, new IntersectionTA(ifread, ifwrite)) { override val name = "ifconf" }

        val cwf = new CutWellFormed("cwf")
        val cwfc = new IntersectionTA(conflict, cwf) {
            override val name = "cwfc"
        }

        val relstr = new CutReleaseStructTA("crs", dpnfac.safeLocks.size)

        val inter1 = new IntersectionTA(cwfc, relstr) {
            override val name = "crf"
        }

        val lockTA = new CutAcqStructComplTA("compacq", dpnfac.safeLocks.size)
        val inter2 = new IntersectionTA(inter1, lockTA) {
            override val name = "craf"
        }

        val lockPreTA = new CutAcqStructPrecutTA("precutacq", dpnfac.safeLocks.size)

        val inter3 = new IntersectionTA(inter2, lockPreTA)

        val intercheck: { def name: String; def emptiness: String } = if (witness) new WitnessIntersectionEmptinessCheck(flowls, inter3) {
            override val name = "ccfrs"
        }
        else new IntersectionEmptinessCheck(flowls, inter3) {
            override val name = "ccfrs"
        }

        val out = new PrintWriter(new BufferedWriter(new FileWriter(tempDir + File.separator + "lscutflow.P")));
        out.println(intercheck.emptiness)
        out.close();

        val timedpn = System.currentTimeMillis - time1
        println("Generation Time:\t" + timedpn + "ms")
        println("Generated Rules:\t" + dpnfac.getDPN.getTransitions.size)
        "[lscutflow]," + intercheck.name + "_runCheck,halt."
    }

    /**
     * Run the command using XSB and print the results to stdOut
     * @param command the command
     */
    def runProlog(command: String) {

        val proc = new ProcessBuilder(xsbExe,
            "-e", "\"" + command + "\"", "--nobanner", "--quietload", "--noprompt").directory(new File(tempDir)).redirectErrorStream(true).start()
        val in = new BufferedReader(new InputStreamReader(proc.getInputStream))

        try {

            var line: String = in.readLine
            while (line != null && !line.contains("Error")) {
                if(!line.equals(""))
                    println(line)
                line = in.readLine
            }
            if (line != null)
                println(line)

            proc.destroy()

        } catch {
            case e: IOException =>
        }

    }

}

