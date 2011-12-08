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

/**
 * This Object contains helper functions to run an XSB process and interpret the result.
 * These methos rely on an well formed check and the xsbExe and tempDir specified
 * in [[de.wwu.sdpn.analysis.SDPNProps]].
 *
 * @author Benedikt Nordhoff
 */
object XSBInterRunner {
  import SDPNProps.get.debug
  import ProgressMonitorUtil._
  private var xsbProcess: PrologEngine = null
  def XSB = {
    if (xsbProcess == null) {
      xsbProcess = getXSBEngine(SDPNProps.get.xsbExe)
    }
    xsbProcess
  }
  private var tempDir = new File(SDPNProps.get.tempDir)
  assert(tempDir isDirectory)
  private var tempFile = {
    new File(tempDir.getAbsolutePath() + File.separator + "check.P")
  }

  def runCheck(check: IntersectionEmptinessCheck): Boolean = {
    val out = new PrintWriter(new BufferedWriter(new FileWriter(tempFile)));
    out.println(check.emptiness)
    out.close()
    XSB.consultAbsolute(tempFile)
    return !XSB.deterministicGoal(check.name + "_notEmpty")
  }

  def runFullWitnessCheck(check: FullWitnessIntersectionEmptinessCheck): Option[String] = {
    val out = new PrintWriter(new BufferedWriter(new FileWriter(tempFile)));
    out.println(check.emptiness)
    out.close()
    XSB.consultAbsolute(tempFile)
    val rev = XSB.deterministicGoal(check.name + "_notEmptyWitness(W)", null)
    if (rev == null)
      None
    else Some(rev(0).toString())
  }

  def shutdown {
    XSB.shutdown()
    xsbProcess = null
  }

  private def findXSBConfigBin(xsbBin: String): Set[File] = {
    //TODO add windows support!
    import File.separator
    assert(xsbBin.endsWith("/bin/xsb"), "Invalid XSB executable")
    val croot = new File(xsbBin.dropRight(8) + separator + "config")
    assert(croot.isDirectory(), "No config dir found in dir(xsb_exe)/../config")

    val subDirs = croot.listFiles().filter(_.isDirectory())
    var candidates = Set[File]()
    for (dir <- subDirs) {
      val candidate = new File(dir.getAbsolutePath() + separator + "bin" + separator + "xsb")
      if (candidate.exists() && candidate.canExecute()) {
        candidates += candidate
      }
    }
    return candidates
  }

  private def getXSBEngine(xsbBin: String): PrologEngine = {
    val candidates = findXSBConfigBin(xsbBin)
    val it = candidates.iterator
    while (it.hasNext) {
      val xsbExe = it.next()
      try {
        return new XSBSubprocessEngine(xsbExe.getAbsolutePath())
      } catch {
        case e: Throwable => e.printStackTrace()
      }
    }

    throw new IllegalArgumentException("Couldn't find XSB config dir from " + xsbBin)
  }

}