package de.wwu.sdpn.runner

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
import de.wwu.sdpn.core.analyses.SDPNProps
import de.wwu.sdpn.wala.analyses.SDPNTestProps
import de.wwu.sdpn.wala.analyses.MyPreAnalysis

/**
 * Mockup to test the iterated reachability analysis
 * 
 * @author Benedikt Nordhoff
 */
object CutTATest {
        
    val props = SDPNProps.get
    val testProps = SDPNTestProps.get

    val xsbExe = props.xsbExe

    val witness = testProps.witness
    
    val slicing = testProps.slicing
    
    
    val tempDir = props.tempDir
    val mc = "Lbnord/examples/CutTest"
    val analysis = MyPreAnalysis.getStd(testProps.classPath, mc)

    val confMethodSig = "bnord.examples.CutTest.excludeMe()V"

    val mr = StringStuff.makeMethodReference(confMethodSig)
    val confSet: scala.collection.Set[CGNode] = asScalaSet(analysis.cg.getNodes(mr))

    val lockType = TypeName.findOrCreate(testProps.lockType)

    def main(args: Array[String]): Unit = {
       
        println("--- Running CutTATest ---")

        println("Witness:\t\t" + witness)
        println("Slicing:\t\t" + slicing)    
        var cmd = genCutLockPrologTA

        runProlog(cmd)
        println("--- End CutTATest ---")



    }

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
                val aif = mic.getAllInstanceFields.filter(x => x.getReference.getSignature == "Lbnord/examples/CutTest.myfield I")
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
                        println("safe lock " + i + ":\t\t" + lock)
                    }

                }
                m
            }
        } else new MyPreAnalysis(analysis) with DPNFactory with MonitorMatcher with FieldAccessAnnotations with DPN2CutTA with LockLocator with UniqueInstanceLocator {
            override val fieldOfInterest: (InstanceKey, FieldReference) = {
                val mainType = TypeName.findOrCreate(mc)
                val mks = pa.getInstanceKeys.filter(_.getConcreteType.getName == mainType)
                assert(mks.size == 1, "More than one InstanceKey for class found")
                val mik = mks.first
                assert(uniqueInstances(mik), "Instance key for field class not unique")
                val mic = mik.getConcreteType
                val aif = mic.getAllInstanceFields.filter(x => x.getReference.getSignature == "Lbnord/examples/CutTest.myfield I")
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
                        println("safe lock " + i + ":\t\t" + lock)
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

        println("readStack:\t\t" + readStack)
        println("writeStack:\t\t" + writeStack)

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
        println("Generation Time:\t" + timedpn)
        println("Generated Rules:\t" + dpnfac.getDPN.getTransitions.size)
        "[lscutflow]," + intercheck.name + "_runCheck,halt."
    }

    /** Run the command using XSB and print the results to stdOut
     * @param command the command
     */
    def runProlog(command: String) {

        val runcommand = xsbExe + " -e \"" + command + "\" --nobanner --quietload --noprompt"
        println("running:\t\t" + runcommand)

        val proc = new ProcessBuilder(xsbExe,
            "-e", "\"" + command + "\"", "--nobanner", "--quietload", "--noprompt").directory(new File(tempDir)).redirectErrorStream(true).start()
        val in = new BufferedReader(new InputStreamReader(proc.getInputStream))

        try {

            var line: String = in.readLine
            while (line != null && !line.contains("Error")) {
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

