package sdpn.analysis

import sdpn.ta.WitnessIntersectionEmptinessCheck
import sdpn.ta.IntersectionEmptinessCheck
import java.io.File
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.IOException
import java.io.PrintWriter
import java.io.BufferedWriter
import java.io.FileWriter
import sdpn.ta.FullWitnessIntersectionEmptinessCheck

/**
 * This Object contains helper functions to run an XSB process and interpret the result. 
 * These methos rely on an well formed check and the xsbExe and tempDir specified 
 * in [[sdpn.analysis.SSRProps]].
 *  
 * @author Benedikt Nordhoff
 */
object XSBRunner {
    import SSRProps.get.debug
    private var xsbExe = SSRProps.get.xsbExe
    private var tempDir = new File(SSRProps.get.tempDir)
    assert(tempDir isDirectory)
    private var tempFile = {
        new File(tempDir.getAbsolutePath() + File.separator + "check.P")
    }


    
    /**
     * Run the given witness check 
     * 
     * @param check the check to execute
     * @return None iff the check yield that the ta is empty, eg there is no conflict
     * 	Some[witness] iff the ta isn't empty and witness is the generated witness.
     */
    def runWitnessCheck(check: WitnessIntersectionEmptinessCheck): Option[String] = {
        XSBRunner synchronized {
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
     * Run the default witness check with parameters declared by [[sdpn.analysis.SSRProps]]
     * 
     * @return None iff the check yield that the ta is empty, eg there is no conflict
     * 	Some[witness] iff the ta isn't empty and witness is the generated witness.
     */
    def runWitnessCheck: Option[String] =
        {
            runWitnessCheck(SingleSetReachability.genWitnessCheck)
        }
    
    /**
     * Run the given witness check 
     * 
     * @param check the check to execute
     * @return None iff the check yield that the ta is empty, eg there is no conflict
     * 	Some[witness] iff the ta isn't empty and witness is the generated witness.
     */
    def runFullWitnessCheck(check: FullWitnessIntersectionEmptinessCheck): Option[String] = {
        XSBRunner synchronized {
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
        XSBRunner synchronized {
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
     * Run the default check with parameters declared by [[sdpn.analysis.SSRProps]]
     * @return true iff the check yield that the ta is empty, eg there is no conflict
     */
    def runCheck: Boolean =
        {
            runCheck(SingleSetReachability.genCheck)
        }

    /**
     * Runs runs the given check command using xsbExe and tempDir declared in [[sdpn.analysis.SSRProps]] and interprets the result
     *
     * @param command the xsb command to evaluate which is assumed do originate from an IntersectionEmptinessCheck
     * @param name the name of the check to look for "name is empty!"
     * @return true iff the execution command yields "name is empty!".
     */
    def runXSBCheck(command: String, name: String): Boolean = {
        XSBRunner synchronized {
            var retVal = false

            val runcommand = xsbExe + " -e \"" + command + "\" --nobanner --quietload --noprompt"
            if (debug) println("Running:\t\t" + runcommand)

            val proc = new ProcessBuilder(xsbExe,
                "-e", "\"" + command + "\"", "--nobanner", "--quietload", "--noprompt").directory(tempDir).redirectErrorStream(true).start()

            val in = new BufferedReader(new InputStreamReader(proc.getInputStream))

            try {

                var line: String = "START"
                while (line != null && !line.contains("Error")) {
                    line = in.readLine
                    if (debug && line != null) println(line)
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
     * Runs runs the given check command using xsbExe and tempDir declared in [[sdpn.analysis.SSRProps]] and interprets the result
     *
     * @param command the xsb command to evaluate which is assumed do originate from an WitnessIntersectionEmptinessCheck
     * @param name the name of the check to look for "name is empty!"
     * @return None iff the execution of the command yields "name is empty!".
     * 	{{{Some[witness]}}} iff the  execution of the command yields "name is not empty!" and the witness.
     */    
    def runXSBWitnessCheck(command: String, name: String): Option[String] = {
        XSBRunner synchronized {
            var retVal = false
            var witness: Option[String] = None

            val runcommand = xsbExe + " -e \"" + command + "\" --nobanner --quietload --noprompt"
            if (debug) println("Running:\t\t" + runcommand)

            val proc = new ProcessBuilder(xsbExe,
                "-e", "\"" + command + "\"", "--nobanner", "--quietload", "--noprompt").directory(tempDir).redirectErrorStream(true).start()

            val in = new BufferedReader(new InputStreamReader(proc.getInputStream))

            try {

                var line: String = "START"
                while (line != null && !line.contains("Error")) {
                    line = in.readLine
                    if (debug && line != null) println(line)
                    if ((name + " is empty!").equals(line)) {
                        retVal = true
                    } else if ((name + " is not empty!").equals(line)) {
                        retVal = false
                        line = in.readLine
                        if (debug) println(line)
                        assert("witness:".equals(line))
                        line = in.readLine
                        if (debug) println(line)
                        witness = Some(line)
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

            assert(retVal || witness.isDefined)
            return witness

        }
    }
}