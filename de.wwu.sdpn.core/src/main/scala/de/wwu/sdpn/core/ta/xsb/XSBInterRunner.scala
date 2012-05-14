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

/**
 * This Object contains helper functions to run an XSB process and interpret the result.
 * These methos rely on an well formed check and the xsbExe and tempDir specified
 * in [[de.wwu.sdpn.analysis.SDPNProps]].
 *
 * @author Benedikt Nordhoff
 */
object XSBInterRunner extends XSBRunner {
  private def debug = SDPNProps.get.debug
  import ProgressMonitorUtil._
  private var xsbProcess: PrologEngine = null
  def XSB = {
    if (xsbProcess == null) {
      val proc = getXSBEngine(SDPNProps.get.xsbExe)
      xsbProcess = proc
      ShutdownHookThread({ proc.shutdown() })
    }
    xsbProcess
  }
  private def tempDir = new File(SDPNProps.get.tempDir)
  private def tempFile = {
    assert(tempDir isDirectory)
    new File(tempDir.getAbsolutePath() + File.separator + "check.P")
  }

  override def runCheck(check: IntersectionEmptinessCheck, pm: IProgressMonitor): Boolean = {
    try {
      beginTask(pm, "Running XSB-based emptiness check", 5)
      val out = new PrintWriter(new BufferedWriter(new FileWriter(tempFile)));
      out.println(check.emptiness)
      out.close()
      worked(pm, 1)
      val loaded = XSB.consultAbsolute(tempFile)
      assert(loaded,"Could not load the ")
      worked(pm, 1)
      if (isCanceled(pm))
        throw new RuntimeException("Canceled!")
      val future = listenOnCancel(pm, () => XSB.interrupt(), 1000)
      try {
        val abolished = XSB.deterministicGoal("abolish_all_tables")
        assert(abolished, "Could not abolish all tables. Expect wrong results!")
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
  
  def runCheck(check: IntersectionEmptinessCheck, pm: IProgressMonitor, timeout:Long): Boolean = {
    try {
      beginTask(pm, "Running XSB-based emptiness check", 5)
      val out = new PrintWriter(new BufferedWriter(new FileWriter(tempFile)));
      out.println(check.emptiness)
      out.close()
      worked(pm, 1)
      val loaded = XSB.consultAbsolute(tempFile)
      assert(loaded,"Could not load the ")
      worked(pm, 1)
      if (isCanceled(pm))
        throw new RuntimeException("Canceled!")
      val future = listenOnCancel(timeout, pm, () => XSB.interrupt(), 500)
      try {
        val abolished = XSB.deterministicGoal("abolish_all_tables")
        assert(abolished, "Could not abolish all tables. Expect wrong results!")
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
  
  override def runCheck(check: IntersectionEmptinessCheck): Boolean = runCheck(check,null)
  
  override def runFullWitnessCheck(check: FullWitnessIntersectionEmptinessCheck): Option[String] = runFullWitnessCheck(check,null)

  override def runFullWitnessCheck(check: FullWitnessIntersectionEmptinessCheck, pm: IProgressMonitor): Option[String] = {
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
        val abolished = XSB.deterministicGoal("abolish_all_tables")
        assert(abolished, "Could not abolish all tables. Expect wrong results!")
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
    if (xsbProcess != null)
      xsbProcess.shutdown()
    xsbProcess = null
  }

  private def findXSBConfigBin(xsbBin: String): Set[File] = {
    //TODO This is still not very flexible.
    import File.separator
    val xsbExeMatcher = """^(.*)[/\\]bin[/\\]xsb(?:\.bat|\.exe)?$""".r
    val xsbConfigExeMatcher =  """^(.*config[/\\][^/\\]*[/\\]bin[/\\]xsb(?:\.bat|\.exe)?)$""".r
    xsbBin match {
      case xsbConfigExeMatcher(_) =>
        val xf = new File(xsbBin)
        assert(xf.exists() && xf.canExecute(),"Path within xsb config dir given but file isn't executable: " + xsbBin)
        if(debug)
          System.err.println("Possible XSB candidate: " + xf)
        return Set(new File(xsbBin))
      case xsbExeMatcher(base) =>
        val croot = new File(base + separator + "config")

        assert(croot.isDirectory(), "No config dir found in dir(xsb_exe)/../config")

        val subDirs = croot.listFiles().filter(_.isDirectory())
        var candidates = Set[File]()
        for (dir <- subDirs) {
          val candidate = new File(dir.getAbsolutePath() + separator + "bin" + separator + "xsb")
          if (candidate.exists() && candidate.canExecute()) {
            candidates += candidate
          }
          val candidate2 = new File(dir.getAbsolutePath() + separator + "bin" + separator + "xsb.exe")
          if (candidate2.exists() && candidate2.canExecute()) {
        	  candidates += candidate2
          }
        }
        if(debug)
          System.err.println("Possible XSB candidates: " + candidates)
        return candidates
      case _ => sys.error("Invalid XSB executable: " + xsbBin)
    }

  }

  private def getXSBEngine(xsbBin: String): PrologEngine = {
    val candidates = findXSBConfigBin(xsbBin)
    val it = candidates.iterator
    while (it.hasNext) {
      val xsbExe = it.next()
      try {
        if (debug)
          System.err.println("Trying XSBSubprocessEngine on: " + xsbExe.getAbsoluteFile())
        return new XSBSubprocessEngine(xsbExe.getAbsolutePath())
      } catch {
        case e: Exception => e.printStackTrace()
      }
    }

    throw new IllegalArgumentException("Couldn't find XSB config dir from " + xsbBin)
  }

}