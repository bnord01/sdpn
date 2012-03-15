package de.wwu.sdpn.core.ta.xsb

import java.io.BufferedReader
import java.io.BufferedWriter
import java.io.File
import java.io.FileWriter
import java.io.IOException
import java.io.InputStream
import java.io.InputStreamReader
import java.io.OutputStream
import java.io.PrintWriter
import scala.sys.process.ProcessIO
import de.wwu.sdpn.core.util.IProgressMonitor
import de.wwu.sdpn.core.util.ProgressMonitorUtil
import de.wwu.sdpn.core.analyses.SDPNProps

/**
 * This Object contains helper functions to run an XSB process and interpret the result.
 * These methos rely on an well formed check and the xsbExe and tempDir specified
 * in [[de.wwu.sdpn.analysis.SDPNProps]].
 *
 * @author Benedikt Nordhoff
 */
object XSBCommandRunner extends XSBRunner {
    private def debug = SDPNProps.get.debug
    import ProgressMonitorUtil._
    private def xsbExe = SDPNProps.get.xsbExe
    private def tempDir = new File(SDPNProps.get.tempDir)
    private def tempFile = {
        assert(tempDir isDirectory)
        new File(tempDir.getAbsolutePath() + File.separator + "check.P")
    }

    /**
     * Run the given witness check
     *
     * @param check the check to execute
     * @return None iff the check yield that the ta is empty, eg there is no conflict
     * 	Some[witness] iff the ta isn't empty and witness is the generated witness.
     */
    def runFullWitnessCheck(check: FullWitnessIntersectionEmptinessCheck, pm: IProgressMonitor): Option[String] = {
        XSBCommandRunner synchronized {
            val out = new PrintWriter(new BufferedWriter(new FileWriter(tempFile)));
            out.println(check.emptiness)
            out.println()
            out.println(":- " + check.name + "_runCheck.")
            out.close();

            var fname = tempFile.getName
            fname = fname.substring(0, fname.length - 2)
            
            val xwamfile = new File(tempFile.getParent() + File.separator + fname + ".xwam")
            if (xwamfile.exists())
                assert(xwamfile.delete, "Old xwam file present expect wrong result!")
            
            runXSBWitnessCheck(check.name,pm)
        }
    }
    
  def runCheck(check: IntersectionEmptinessCheck): Boolean = runCheck(check,null)
  def runFullWitnessCheck(check: FullWitnessIntersectionEmptinessCheck): Option[String] = runFullWitnessCheck(check,null)

    def shutdown() {
        //nothing to do here
    }

    /**
     * run a check
     * @param check the Check to execute
     * @return true iff the check yield "check.name is empty!"
     */
    def runCheck(check: IntersectionEmptinessCheck, pm0: IProgressMonitor): Boolean = {
        XSBCommandRunner synchronized {
            val out = new PrintWriter(new BufferedWriter(new FileWriter(tempFile)));
            out.println(check.emptiness)
            out.println()
            out.println(":- " + check.name + "_runCheck.")
            out.close();

            var fname = tempFile.getName
            assert(fname.endsWith(".P") || fname.endsWith(".p"))
            fname = fname.substring(0, fname.length - 2)

            val xwamfile = new File(tempFile.getParent() + File.separator + fname + ".xwam")
            if (xwamfile.exists())
                assert(xwamfile.delete, "Old xwam file present expect wrong result!")

            runXSBCheck(check.name, pm0)
        }
    }

    /**
     * Runs runs the given check command using xsbExe and tempDir declared in [[de.wwu.sdpn.analysis.SDPNProps]] and interprets the result
     *
     * @param command the xsb command to evaluate which is assumed do originate from an IntersectionEmptinessCheck
     * @param name the name of the check to look for "name is empty!"
     * @return true iff the execution command yields "name is empty!".
     */
    @throws(classOf[RuntimeException])
    @throws(classOf[IOException])
    private def runXSBCheck(name: String, pm: IProgressMonitor): Boolean = {
        XSBCommandRunner synchronized {
            var proc: scala.sys.process.Process = null
            try {
                beginTask(pm, "Running emptiness check with XSB", 2)
                subTask(pm, "Spawning XSB")

                var retVal = false
                var hadErr = false

                @volatile
                var ready = false

                val mainThread = Thread.currentThread()

                var fname = tempFile.getName
                assert(fname.endsWith(".P") || fname.endsWith(".p"))
                fname = fname.substring(0, fname.length - 2)

                val runcommand = xsbExe + " " + fname + " --nobanner --quietload"
                if (debug) println("Running:\t\t" + runcommand)

                val pb = scala.sys.process.Process(runcommand, tempDir)

                def input(out: OutputStream) {}
                def output(in0: InputStream) {
                    val in = new BufferedReader(new InputStreamReader(in0))
                    var line = "Start"
                    while (line != null) {
                        line = in.readLine
                        if (debug && line != null) println(line)
                        if ((name + " is empty!").equals(line)) {
                            retVal = true
                            ready = true
                            mainThread.interrupt()
                        }
                        if ((name + " is not empty!").equals(line)) {
                            retVal = false
                            ready = true
                            mainThread.interrupt()
                        }
                    }
                }

                def erroutput(in0: InputStream) {
                    val in = new BufferedReader(new InputStreamReader(in0))
                    var line = in.readLine
                    while (line != null) {
                        if (debug) println("XSBERROR: " + line)
                        if (line != "") {
                            hadErr = true
                            ready = true
                            mainThread.interrupt()
                        }
                        line = in.readLine
                    }
                }

                val pio = new ProcessIO(input _, output _, erroutput _)

                proc = pb.run(pio)
                worked(pm, 1)
                subTask(pm, "Waiting for XSB.")

                while (!ready) {
                    if (isCanceled(pm)) {
                        proc.destroy()
                        throw new RuntimeException("operation canceled");
                    }
                    try {
                        Thread.sleep(100)
                    } catch {
                        case e: InterruptedException =>
                    }
                }
                worked(pm, 1)
                if (ready && !hadErr) {
                    return retVal
                } else if (hadErr) {
                    throw new IOException("XSB Error")
                } else {
                    throw new IOException("Error running XSB")
                }
            } finally {
                done(pm)
            }
        }
    }

    /**
     * Runs runs the given check command using xsbExe and tempDir declared in [[de.wwu.sdpn.analysis.SDPNProps]] and interprets the result
     *
     * @param command the xsb command to evaluate which is assumed do originate from an WitnessIntersectionEmptinessCheck
     * @param name the name of the check to look for "name is empty!"
     * @return None iff the execution of the command yields "name is empty!".
     * 	{{{Some[witness]}}} iff the  execution of the command yields "name is not empty!" and the witness.
     */
    @throws(classOf[RuntimeException])
    @throws(classOf[IOException])
    private def runXSBWitnessCheck(name: String, pm: IProgressMonitor): Option[String] = {

        XSBCommandRunner synchronized {
            var proc: scala.sys.process.Process = null
            try {
                beginTask(pm, "Running emptiness check with XSB", 2)
                subTask(pm, "Spawning XSB")

                var retVal: Option[String] = None
                var hadErr = false

                @volatile
                var ready = false

                val mainThread = Thread.currentThread()

                var fname = tempFile.getName
                assert(fname.endsWith(".P") || fname.endsWith(".p"))
                fname = fname.substring(0, fname.length - 2)

                val runcommand = xsbExe + " " + fname + " --nobanner --quietload"
                if (debug) println("Running:\t\t" + runcommand)

                val pb = scala.sys.process.Process(runcommand, tempDir)

                def input(out: OutputStream) {}
                def output(in0: InputStream) {
                    val in = new BufferedReader(new InputStreamReader(in0))
                    var line = "Start"
                    while (line != null) {
                        line = in.readLine
                        if (debug && line != null) 
                            println(line)
                        if ((name + " is empty!") equals line) {
                            //Set result and wakeup main thread.
                            retVal = None
                            ready = true
                            mainThread.interrupt()
                        } else if ((name + " is not empty!") equals line) {
                            line = in.readLine
                            
                            //Skip a possible blank line on windows machines.
                            if("" equals line)
                                line = in.readLine
                                
                            if (debug) println(line)                            
                            assert("witness:" equals line, "Expected 'witness:' but got: " + line)
                            
                            line = in.readLine
                            
                            //Skip another possible blank line on windows machines.
                            if("" equals line)
                                line = in.readLine
                            if (debug) println(line)     
                            
                            assert(!("" equals line) && line != null, "Expected witness but got: " + line)
                            
                            //Set result and wakeup main thread.
                            retVal = Some(line)
                            ready = true
                            mainThread.interrupt()
                        }
                    }
                }

                def erroutput(in0: InputStream) {
                    val in = new BufferedReader(new InputStreamReader(in0))
                    var line = in.readLine
                    while (line != null) {
                        if (debug) println("XSBERROR: " + line)
                        if (line != "") {
                            hadErr = true
                            ready = true
                            mainThread.interrupt()
                        }
                        line = in.readLine
                    }
                }

                val pio = new ProcessIO(input _, output _, erroutput _)

                proc = pb.run(pio)
                worked(pm, 1)
                subTask(pm, "Waiting for XSB.")

                while (!ready) {
                    if (isCanceled(pm)) {
                        proc.destroy()
                        throw new RuntimeException("operation canceled");
                    }
                    try {
                        Thread.sleep(100)
                    } catch {
                        case e: InterruptedException =>
                    }
                }
                worked(pm, 1)
                if (ready && !hadErr) {
                    return retVal
                } else if (hadErr) {
                    throw new IOException("XSB Error")
                } else {
                    throw new IOException("Error running XSB")
                }
            } finally {
                done(pm)
            }
        }
        //    XSBCommandRunner synchronized {
        //      var retVal = false
        //      var witness: Option[String] = None
        //
        //      val runcommand = xsbExe + " -e \"" + command + "\" --nobanner --quietload --noprompt"
        //      if (debug) println("Running:\t\t" + runcommand)
        //
        //      val proc = new ProcessBuilder(xsbExe,
        //        "-e", "\"" + command + "\"", "--nobanner", "--quietload", "--noprompt").directory(tempDir).redirectErrorStream(true).start()
        //
        //      val in = new BufferedReader(new InputStreamReader(proc.getInputStream))
        //
        //      try {
        //
        //        var line: String = "START"
        //        while (line != null && !line.contains("Error")) {
        //          line = in.readLine
        //          if (debug && line != null) println(line)
        //          if ((name + " is empty!").equals(line)) {
        //            retVal = true
        //          } else if ((name + " is not empty!").equals(line)) {
        //            retVal = false
        //            line = in.readLine
        //            if (debug) println(line)
        //            assert("witness:".equals(line))
        //            line = in.readLine
        //            if (debug) println(line)
        //            witness = Some(line)
        //          } else if (line != null && line.contains("Error")) {
        //            proc.destroy()
        //            throw new IOException("Error running XSB: " + line)
        //          }
        //        }
        //
        //        proc.destroy()
        //
        //      } catch {
        //        case e: Exception =>
        //          proc.destroy()
        //          throw e
        //      }
        //
        //      assert(retVal || witness.isDefined)
        //      return witness
        //
        //    }
    }
}