package de.wwu.sdpn.runner
import de.wwu.sdpn.analysis.SSRProps
import de.wwu.sdpn.analysis.SingleSetReachability._
import de.wwu.sdpn.analysis.XSBRunner._
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import de.wwu.sdpn.ta.WitnessIntersectionEmptinessCheck
import java.io.PrintWriter
import java.io.BufferedWriter
import java.io.FileWriter
import java.io.File
import de.wwu.sdpn.ta.IntersectionEmptinessCheck
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.IOException
import System.{ currentTimeMillis => now }
import de.wwu.sdpn.analysis.MyPreAnalysis

object SSRBenchmarkRunner {

    //run Wala once for clearer results.
    MyPreAnalysis.getStd
    
    import de.wwu.sdpn.analysis.SSRProps.get.{ xsbExe }
    val tempDir = new File(SSRProps.get.tempDir)
    val tempFile = new File(tempDir.getPath() + File.separator + "benchmark.P")

    def main(args: Array[String]): Unit = {
        SSRProps.get.debug = false
        for (lockSens <- List(false, true)) {
            println("##### Running lock " + (if (lockSens) "" else "in") + "sensetive Analysis #####")
            println("--- Running sliced analyses without witness ---")
            for (mcname <- List("Plain", "CrossCallsTwo", "CrossCallsThree", "CrossCallsFour", "CrossCallsFive", "CrossCallsSix", "CrossCallsSeven", "CrossCallsEight", "CrossCallsNine", "LockCrossCallsTwo", "LockCrossCallsThree", "LockCrossCallsFour", "Println")) {
                val prop = getSSRProp(mcname, slicing = true, witness = false) copy (lockSens = lockSens)
                println("-------------")
                runSSRAnalysis(prop)
            }
            println
            println("--- Running sliced analyses with witness ---")
            for (mcname <- List("Plain", "CrossCallsTwo", "CrossCallsThree", "CrossCallsFour", "CrossCallsFive", "CrossCallsSix", "CrossCallsSeven", "CrossCallsEight", "CrossCallsNine","LockCrossCallsTwo", "LockCrossCallsThree", "Println")) {
                val prop = getSSRProp(mcname, slicing = true, witness = true) copy (lockSens = lockSens)
                println("-------------")
                runSSRAnalysis(prop)
            }
            println
            println("--- Running unsliced analyses without witness ---")
            for (mcname <- List("Plain", "LockCrossCallsTwo", "LockCrossCallsThree", "LockCrossCallsFour", "Println")) {
                val prop = getSSRProp(mcname, slicing = false, witness = false) copy (lockSens = lockSens)
                println("-------------")
                runSSRAnalysis(prop)
            }
            println
            println("--- Running unsliced analyses with witness ---")
            for (mcname <- List("Plain", "LockCrossCallsTwo", "LockCrossCallsThree", "Println")) {
                val prop = getSSRProp(mcname, slicing = false, witness = true) copy (lockSens = lockSens)
                println("-------------")
                runSSRAnalysis(prop)
            }
            
        }

    }

    def runSSRAnalysis(props: SSRProps) {
        import props._
        val lockFilter = if (lockSens) lockTypeFilter("Lbnord/examples/Lock") else { _: InstanceKey => false }
        println(mainClass)

        val t0 = now
        val (dpn, cset) = genDPNandCset(classPath, mainClass, exclusiveMethod, lockFilter, lockSens, slicing)

        if (slicing) print("slicing, ") else print("no slicing, ")
        if (witness) println("witness") else println("no witness")
        val twala = now - t0
        println("Num Rules:\t" + dpn.getTransitions.size)
        val t1 = now
        val (td, bu) = genAutomata(dpn, cset)
        if (witness) {
            runWitnessCheck(genWitnessCheck(td, bu))
        } else {
            runCheck(genCheck(td, bu))
        }
        println("Wala Time:\t" + twala + "ms")

    }

    def getSSRProp(mcname: String, slicing: Boolean, witness: Boolean): SSRProps = {
        val p0 = SSRProps.get
        val mc = "Lbnord/benchmark/ssr/" + mcname
        val exm = "bnord.benchmark.ssr." + mcname + ".excludeMe()V"

        return p0 copy (slicing = slicing, witness = witness, mainClass = mc, exclusiveMethod = exm)

    }

    /**
     * Run the given witness check
     *
     * @param check the check to execute
     * @return None iff the check yield that the ta is empty, eg there is no conflict
     * 	Some[witness] iff the ta isn't empty and witness is the generated witness.
     */
    def runWitnessCheck(check: WitnessIntersectionEmptinessCheck){
        this synchronized {
            val out = new PrintWriter(new BufferedWriter(new FileWriter(tempFile)));
            out.println(check.emptiness)
            out.close();

            var fname = tempFile.getName
            fname = fname.substring(0, fname.length - 2)

            val xsbcommand = "[" + fname + "]," + check.name + "_runCheck,halt."
            runXSBWitnessCheck(xsbcommand, check.name)
        }
    }

    /**
     * run a check
     * @param check the Check to execute
     * @return true iff the check yield "check.name is empty!"
     */
    def runCheck(check: IntersectionEmptinessCheck): Boolean = {
        this synchronized {
            val out = new PrintWriter(new BufferedWriter(new FileWriter(tempFile)));
            out.println(check.emptiness)
            out.close();

            var fname = tempFile.getName
            assert(fname.endsWith(".P") || fname.endsWith(".p"))
            fname = fname.substring(0, fname.length - 2)

            val xsbcommand = "[" + fname + "]," + check.name + "_runCheck,halt."
            runXSBCheck(xsbcommand, check.name)
        }
    }

    /**
     * Runs runs the given check command using xsbExe and tempDir declared in [[de.wwu.sdpn.analysis.SSRProps]] and interprets the result
     *
     * @param command the xsb command to evaluate which is assumed do originate from an IntersectionEmptinessCheck
     * @param name the name of the check to look for "name is empty!"
     * @return true iff the execution command yields "name is empty!".
     */
    def runXSBCheck(command: String, name: String): Boolean = {
        this synchronized {
            var retVal = false

            val runcommand = xsbExe + " -e \"" + command + "\" --nobanner --quietload --noprompt"

            val proc = new ProcessBuilder(xsbExe,
                "-e", "\"" + command + "\"", "--nobanner", "--quietload", "--noprompt").directory(tempDir).redirectErrorStream(true).start()

            val in = new BufferedReader(new InputStreamReader(proc.getInputStream))

            try {

                var line: String = "START"
                while (line != null && !line.contains("Error")) {
                    line = in.readLine
                    if (line != null && !line.equals("")) println(line)
                    if ((name + " is empty!").equals(line)) {
                        retVal = true
                    } else if (line != null && line.contains("Error")) {
                        proc.destroy()
                        throw new IOException("Error running XSB: " + line)
                    }
                }

                proc.destroy()

            } catch {
                case e: Exception =>
                    proc.destroy()
                    throw e
            }
            return retVal
        }
    }

    /**
     * Runs runs the given check command using xsbExe and tempDir declared in [[de.wwu.sdpn.analysis.SSRProps]] and interprets the result
     *
     * @param command the xsb command to evaluate which is assumed do originate from an WitnessIntersectionEmptinessCheck
     * @param name the name of the check to look for "name is empty!"
     * @return None iff the execution of the command yields "name is empty!".
     * 	{{{Some[witness]}}} iff the  execution of the command yields "name is not empty!" and the witness.
     */
    def runXSBWitnessCheck(command: String, name: String) {
        this synchronized {
            var retVal = false
            
            val runcommand = xsbExe + " -e \"" + command + "\" --nobanner --quietload --noprompt"

            val proc = new ProcessBuilder(xsbExe,
                "-e", "\"" + command + "\"", "--nobanner", "--quietload", "--noprompt").directory(tempDir).redirectErrorStream(true).start()

            val in = new BufferedReader(new InputStreamReader(proc.getInputStream))

            try {

                var line: String = "START"
                while (line != null && !line.contains("Error")) {
                    line = in.readLine
                    if (line != null && !line.equals("")) println(line)
                    if ((name + " is empty!").equals(line)) {
                        retVal = true
                    } else if ((name + " is not empty!").equals(line)) {
                        retVal = false
                        line = in.readLine
                        println(line)
                        assert("witness:".equals(line))
                        //kill witness
                        line = in.readLine
                    } else if (line != null && line.contains("Error")) {
                        proc.destroy()
                        throw new IOException("Error running XSB: " + line)
                    }
                }

                proc.destroy()

            } catch {
                case e: Exception =>
                    proc.destroy()
                    throw e
            }
        }
    }

}