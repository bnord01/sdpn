package de.wwu.sdpn.eclipse.launching
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jdt.core.IType
import de.wwu.sdpn.wala.util.WalaUtil
import de.wwu.sdpn.wala.analyses.DataraceAnalysis
import org.eclipse.ui.PlatformUI
import org.eclipse.jface.dialogs.MessageDialog
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.NullProgressMonitor

object DataraceLauncher {

  def runDataRaceAnalysisOnClass(proj: IJavaProject, mcname: String, pm0: IProgressMonitor = null) {
    val pm: IProgressMonitor = if (pm0 == null) new NullProgressMonitor else pm0

    try {
      pm.beginTask("Running DPN-based data race analysis on class " + mcname, 7)

      check(pm)

      val cp = proj.getResource().getLocation().removeTrailingSeparator().removeLastSegments(1).append(proj.getOutputLocation()).toFile().toString()
      val java_lib_names = Set("classes.jar", "rt.jar", "core.jar")
      val jrelib = proj.getPackageFragmentRoots().filter(x => java_lib_names(x.getElementName())).first.getPath().toFile().toString()

      println("Running data race analysis with parameters")
      println("Class Path: " + cp)
      println("JRELib: " + jrelib)
      println("Main class: " + mcname)

      pm subTask "Setting up WALA scope"
      val scope = WalaUtil.makeScopeFromCPnJRE(cp, jrelib)

      val mainClass = "L" + mcname.replace('.', '/')

      check(pm)
      pm worked 1

      pm subTask "Building WALA call graph and running pointer analysis."
      val (cg, pa) = WalaUtil.makeCGFromScopeAndMainClass(scope, mainClass)
      pm worked 2

      check(pm)
      pm subTask "Identifying field access points."
      val rda = new DataraceAnalysis(cg, pa)
      pm worked 1

      check(pm)
      pm subTask "Running DPN-based analyses for individual instances."

      val anyRace = rda.anyRacePossible

      pm worked 3

      println("Data race possible: " + anyRace)

      //      val shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell()
      val display = PlatformUI.getWorkbench().getDisplay()

      if (anyRace)
        display.syncExec(task(
          MessageDialog.openWarning(display.getActiveShell(), "Race found!", "A possible datarace was found in your program!")))
      else
        display.syncExec(task(
          MessageDialog.openInformation(display.getActiveShell(), "No race found", "There is no race in your program.")))

    } finally {
      pm done
    }
  }

  def task(body: => Unit): Runnable = new Runnable { override def run() { body } }

  def check(pm: IProgressMonitor) { if (pm.isCanceled()) throw new RuntimeException("Job canceled by user.") }

}