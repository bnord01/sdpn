package de.wwu.sdpn.eclipse.launching
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jdt.core.IType
import de.wwu.sdpn.wala.util.WalaUtil
import de.wwu.sdpn.wala.analyses.DataraceAnalysis
import org.eclipse.ui.PlatformUI
import org.eclipse.jface.dialogs.MessageDialog

object DataraceLauncher {
  
  def runDataRaceAnalysisOnClass(proj:IJavaProject, mcname: String) {
    
    val cp = proj.getResource().getLocation().removeTrailingSeparator().removeLastSegments(1).append(proj.getOutputLocation()).toFile().toString()
      val java_lib_names = Set("classes.jar", "rt.jar", "core.jar")
      val jrelib = proj.getPackageFragmentRoots().filter(x => java_lib_names(x.getElementName())).first.getPath().toFile().toString()

      println("Running data race analysis with parameters")
      println("Class Path: " + cp)
      println("JRELib: " + jrelib)
      println("Main class: " + mcname)
      val scope = WalaUtil.makeScopeFromCPnJRE(cp, jrelib)

      val mainClass = "L" + mcname.replace('.', '/')

      val (cg, pa) = WalaUtil.makeCGFromScopeAndMainClass(scope, mainClass)

      val rda = new DataraceAnalysis(cg, pa) 

      val anyRace = rda.anyRacePossible

      println("Es gibt einen Race: " + anyRace)

//      val shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell()
      val display = PlatformUI.getWorkbench().getDisplay()
      

      if (anyRace)
        display.syncExec(task(       
        MessageDialog.openWarning(display.getActiveShell(), "Race found!", "A possible datarace was found in your program!")))
      else
        display.syncExec(task(
        MessageDialog.openInformation(display.getActiveShell(), "No race found", "There is no race in your program.")))
    
  }
  
  
  def task(body : => Unit) : Runnable = new Runnable {override def run() {body}}

}