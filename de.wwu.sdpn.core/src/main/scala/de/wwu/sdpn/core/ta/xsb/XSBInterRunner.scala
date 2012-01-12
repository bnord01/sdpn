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

  def runCheck(check: IntersectionEmptinessCheck, pm: IProgressMonitor = null): Boolean = {
    try {
      beginTask(pm, "Running XSB-based emptiness check", 5)
      val out = new PrintWriter(new BufferedWriter(new FileWriter(tempFile)));
      out.println(check.emptiness)
      out.close()
      worked(pm, 1)
      XSB.consultAbsolute(tempFile)
      worked(pm, 1)
      if(isCanceled(pm))
        throw new RuntimeException("Canceled!")
      val future = listenOnCancel(pm, () => XSB.interrupt(), 1000)      
      try {
        val rev = !XSB.deterministicGoal(check.name + "_notEmpty")
        worked(pm, 3)
        return rev
      } finally {
        delisten(future)
      }
    } finally {
      done(pm)
    }
  }

  def runFullWitnessCheck(check: FullWitnessIntersectionEmptinessCheck, pm: IProgressMonitor = null): Option[String] = {
    try {
      beginTask(pm, "Running XSB-based emptiness check", 5)
      val out = new PrintWriter(new BufferedWriter(new FileWriter(tempFile)));
      out.println(check.emptiness)
      out.close()
      worked(pm, 1)
      XSB.consultAbsolute(tempFile)
      worked(pm, 1)
      val future = listenOnCancel(pm, () => XSB.interrupt(), 1000)
      try {
        val rev = XSB.deterministicGoal(check.name + "_notEmptyWitness(W)", null)        
        worked(pm, 3)
        if (rev == null)
          return None
        else
          return Some(rev(0).toString())
      } finally {
        delisten(future)
      }
    } finally {
      done(pm)
    }
  }

  def shutdown() {
    if(xsbProcess != null)
      XSB.shutdown()
    xsbProcess = null
  }

  private def findXSBConfigBin(xsbBin: String): Set[File] = {
    //TODO Maybe add windows support? But no one knows how XSB is called there.
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
        case e: Exception => e.printStackTrace()
      }
    }

    throw new IllegalArgumentException("Couldn't find XSB config dir from " + xsbBin)
  }
  
  Runtime.getRuntime().addShutdownHook(new Thread() {override def run(){shutdown()}})

}