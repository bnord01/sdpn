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
import com.declarativa.interprolog.XSBSubprocessEngine
import com.declarativa.interprolog.PrologEngine
import scala.sys.ShutdownHookThread
import com.declarativa.interprolog.util.IPInterruptedException
import de.wwu.sdpn.core.util.Logging

/**
 * This Object contains helper functions to run an XSB process and interpret the result.
 * These methos rely on an well formed check and the xsbExe and tempDir specified
 * in [[de.wwu.sdpn.analysis.SDPNProps]].
 *
 * @author Benedikt Nordhoff
 */
object XSBInterRunner extends XSBRunner with Logging {
    private def debug = SDPNProps.get.debug
    import ProgressMonitorUtil._
    private var xsbProcess: PrologEngine = null
    private var shutdownHook: ShutdownHookThread = null
    def XSB = {
        if (xsbProcess == null) {
            val proc = getXSBEngine(SDPNProps.get.xsbExe)
            xsbProcess = proc
            shutdownHook = ShutdownHookThread({ proc.shutdown() })
        }
        xsbProcess
    }
    //    private def tempDir = new File(SDPNProps.get.tempDir)
    private lazy val tempFile = File.createTempFile("check", ".P")
    //    private def tempFile = {
    //        assert(tempDir isDirectory)
    //        new File(tempDir.getAbsolutePath() + File.separator + "check.P")
    //    }

    override def runCheck(check: IntersectionEmptinessCheck, pm: IProgressMonitor): Boolean = {
        runCheck(check, pm, 0)
    }

    def runCheck(check: IntersectionEmptinessCheck, pm: IProgressMonitor, timeout: Long): Boolean = {
        logger.trace("Entered runCheck")
        try {
            beginTask(pm, "Running XSB-based emptiness check", 5)
            // just touch XSB so that an instance get's created if this is the first time.
            XSB
            // try to abolish tables within 2 seconds to reuse xsb instance 
            val abolishListener = listenOnCancel(2000, pm, () => XSB.interrupt(), 500)
            try {
                logger.trace("Abolishing all XSB tables")
                val abolished = XSB.deterministicGoal("abolish_all_tables")
                assert(abolished, "Could not abolish all XSB tables. Stopping here. Expect wrong results!")
                logger.trace("XSB abolished all tables")
            } catch {
                case ipe: IPInterruptedException =>
                    logger.warn( /*ipe, */ "Abolishing all XSB tables was interrupted, shutting down instance.")
                    logger.trace("Shutting down XSB instance.")
                    shutdown()
            } finally {
                delisten(abolishListener)
            }
            val out = new PrintWriter(new BufferedWriter(new FileWriter(tempFile)));
            logger.trace("Opened output stream for emptiness check")
            try {
                out.println(check.emptiness)
            } finally {
                out.close()
            }
            logger.trace("Written emptiness check to tempfile: %s", tempFile.getAbsolutePath())
            //            if (debug) {
            //                var i = 1
            //                var f = new File("/tmp/check" + i + ".P")
            //                while (f.exists) {
            //                    i += 1
            //                    f = new File("/tmp/check" + i + ".P")
            //                }
            //                val outDB = new PrintWriter(new BufferedWriter(new FileWriter(f)));
            //                try {
            //                    outDB.println(check.emptiness)
            //                } finally {
            //                    outDB.close()
            //                }
            //            }
            worked(pm, 1)
            logger.debug("Loading tempfile into XSB")
            val loaded = XSB.consultAbsolute(tempFile)
            assert(loaded, "Could not load the tempfile into XSB")
            logger.debug("Loaded tempfile into XSB")
            worked(pm, 1)
            if (isCanceled(pm))
                throw new RuntimeException("Emptiness check was canceled!")

            val empinessListener = listenOnCancel(timeout, pm, () => XSB.interrupt(), 500)
            try {
                logger.debug("Calling XSB on non-emptiness predicate")
                val rev = !XSB.deterministicGoal(check.name + "_notEmpty")
                logger.debug(s"XSB finished run on non-emptiness predicate, result: ${!rev} (${if (rev) "" else "not "} empty)")
                worked(pm, 3)
                logger.trace("Exiting runCheck")
                return rev
            } catch {
                case ipe: IPInterruptedException =>
                    logger.warn("Check was interrupted, probably by timeout.",ipe)
                    throw new RuntimeException("Emptiness check was canceled!", ipe)
            } finally {
                delisten(empinessListener)
            }
        } finally {
            done(pm)
        }
    }

    override def runCheck(check: IntersectionEmptinessCheck): Boolean = runCheck(check, null)

    override def runFullWitnessCheck(check: FullWitnessIntersectionEmptinessCheck): Option[String] = runFullWitnessCheck(check, null)

    override def runFullWitnessCheck(check: FullWitnessIntersectionEmptinessCheck, pm: IProgressMonitor): Option[String] = {
        logger.trace("Entered runFullWitnessCheck")
        try {
            beginTask(pm, "Running XSB-based emptiness check", 5)
            val abolishListener = listenOnCancel(2000, pm, () => XSB.interrupt(), 500)
            try {
                logger.trace("Abolishing all XSB tables")
                val abolished = XSB.deterministicGoal("abolish_all_tables")
                assert(abolished, "Could not abolish all XSB tables. Stopping here. Expect wrong results!")
                logger.trace("XSB abolished all tables")
            } catch {
                case ipe: IPInterruptedException =>
                    logger.warn( /*ipe, */ "Abolishing all XSB tables was interrupted, shutting down instance.")
                    logger.trace("Shutting down XSB instance.")
                    shutdown()
            } finally {
                delisten(abolishListener)
            }
            val out = new PrintWriter(new BufferedWriter(new FileWriter(tempFile)));
            logger.trace("Opened output stream for emptiness check")
            try {
                out.println(check.emptiness)
            } finally {
                out.close()
            }

            worked(pm, 1)
            logger.trace("Written emptiness check to tempfile: %s", tempFile.getAbsolutePath())
            logger.debug("Loading tempfile into XSB")
            val loaded = XSB.consultAbsolute(tempFile)
            assert(loaded, "Could not load the tempfile into XSB")
            logger.debug("Loaded tempfile into XSB")
            worked(pm, 1)
            if (isCanceled(pm))
                throw new RuntimeException("Emptiness check was canceled!")
            val empinessListener = listenOnCancel(pm, () => XSB.interrupt(), 1000)
            try {
                logger.debug("Calling XSB on non-emptiness predicate")
                val rev = XSB.deterministicGoal(check.name + "_notEmptyWitness(W)", null)
                logger.debug("XSB finished run on non-emptiness predicate, result: (%s empty)", if (rev == null) "" else "not ")
                worked(pm, 3)
                logger.trace("Exiting runCheck")
                if (rev == null)
                    return None
                else
                    return Some(rev(0).toString())
            } catch {
                case ipe: IPInterruptedException =>
                    logger.warn("Check was interrupted, probably by timeout.",ipe)
                    throw new RuntimeException("Emptiness check was canceled!", ipe)
            } finally {
                delisten(empinessListener)
            }
        } finally {
            done(pm)
        }
    }

    def shutdown() {
        if (xsbProcess != null) {
            xsbProcess.shutdown()
            shutdownHook.remove()
        }
        xsbProcess = null
        shutdownHook = null
    }

    private def findXSBConfigBin(xsbBin: String): List[File] = {
        //TODO This is still not very flexible.
        import File.separator
        val xsbExeMatcher = """^(.*)[/\\]bin[/\\]xsb(?:\.bat|\.exe)?$""".r
        val xsbConfigExeMatcher = """^(.*config[/\\][^/\\]*[/\\]bin[/\\]xsb(?:\.bat|\.exe)?)$""".r
        xsbBin match {
            case xsbConfigExeMatcher(_) =>
                val xf = new File(xsbBin)
                assert(xf.exists() && xf.canExecute(), "Path within xsb config dir given but file isn't executable: " + xsbBin)
                logger.trace("Possible XSB confic exe candidate: %s", xf.getAbsolutePath)
                return List(new File(xsbBin))
            case xsbExeMatcher(base) =>
                val croot = new File(base + separator + "config")

                assert(croot.isDirectory(), "No config dir found in dir(xsb_exe)/../config")

                val subDirs = croot.listFiles().filter(_.isDirectory())
                var candidates = List[File]()
                for (dir <- subDirs) {
                    val candidate = new File(dir.getAbsolutePath() + separator + "bin" + separator + "xsb")
                    if (candidate.exists() && candidate.canExecute()) {
                        candidates ::= candidate
                    }
                    val candidate2 = new File(dir.getAbsolutePath() + separator + "bin" + separator + "xsb.exe")
                    if (candidate2.exists() && candidate2.canExecute()) {
                        candidates ::= candidate2
                    }
                }
                return candidates
            case _ => sys.error("Invalid XSB executable: " + xsbBin)
        }

    }

    private def getXSBEngine(xsbBin: String): PrologEngine = {
        logger.debug("Obtaining XSB engine for bin dir: %s", xsbBin)
        val candidates = findXSBConfigBin(xsbBin)
        logger.trace("Candidates for xsb dir: %s", candidates)
        val it = candidates.iterator
        while (it.hasNext) {
            val xsbExe = it.next()
            try {
                val ret = new XSBSubprocessEngine(xsbExe.getAbsolutePath())
                logger.debug("Successfully created XSB engine from: %s", xsbExe.getAbsolutePath())
                return ret
            } catch {
                case e: Exception =>
                    logger.warn(s"Failed creating XSB engine on: %{xsbExe.getAbsolutePath()",e)
                    e.printStackTrace()
            }
        }
        val e = new IllegalArgumentException("Couldn't find XSB config dir from " + xsbBin)
        logger.error("Couldn't find XSB config dir",e)
        throw e
    }

}