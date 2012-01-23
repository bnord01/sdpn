package de.wwu.sdpn.eclipse.launching

import org.eclipse.jdt.launching.AbstractJavaLaunchConfigurationDelegate
import de.wwu.sdpn.eclipse.Activator
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.CoreException
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.debug.core.ILaunch
import org.eclipse.debug.core.ILaunchConfiguration
import org.eclipse.debug.core.model.ILaunchConfigurationDelegate
import org.eclipse.jdt.launching.AbstractJavaLaunchConfigurationDelegate
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.resources.IProject
import org.eclipse.jdt.core.JavaCore
import de.wwu.sdpn.wala.util.WalaUtil
import de.wwu.sdpn.wala.analyses.DataraceAnalysis
import org.eclipse.swt.widgets.Display
import org.eclipse.ui.PlatformUI
import org.eclipse.jface.dialogs.MessageDialog
import org.eclipse.swt.SWT

class DataraceLaunchConfigurationDelegate extends AbstractJavaLaunchConfigurationDelegate {
  def launch(conf: ILaunchConfiguration, mode: String,
    launch: ILaunch, monitor: IProgressMonitor) {

    try {
      import DataraceLaunchConfig._
      val projName = conf.getAttribute(PROJECT_NAME, "")
      val mcName = conf.getAttribute(MAIN_CLASS_NAME, "")
      val iproj = ResourcesPlugin.getWorkspace().getRoot().findMember(projName).asInstanceOf[IProject]
      val jproj = JavaCore.create(iproj)
      val cp = jproj.getResource().getLocation().removeTrailingSeparator().removeLastSegments(1).append(jproj.getOutputLocation()).toFile().toString()
      val java_lib_names = Set("classes.jar", "rt.jar", "core.jar")
      val jrelib = jproj.getPackageFragmentRoots().filter(x => java_lib_names(x.getElementName())).first.getPath().toFile().toString()

      println("Running data race analysis with parameters")
      println("Class Path: " + cp)
      println("JRELib: " + jrelib)
      println("Main class: " + mcName)
      val scope = WalaUtil.makeScopeFromCPnJRE(cp, jrelib)

      val mainClass = "L" + mcName.replace('.', '/')

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

    } catch {
      case e: Exception => e.printStackTrace(); throw e //Activator.getDefault().showError(e, "A " + e.getClass().getSimpleName() + " has occured")
    }
  }
  
  def task(body : => Unit) : Runnable = new Runnable {override def run() {body}}
  
}