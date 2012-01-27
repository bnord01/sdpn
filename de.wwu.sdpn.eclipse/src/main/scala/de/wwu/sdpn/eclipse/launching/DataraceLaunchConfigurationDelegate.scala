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
import de.wwu.sdpn.wala.analyses.datarace.DataraceAnalysis
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
      
      DataraceLauncher.runDataRaceAnalysisOnClass(jproj,mcName,monitor)
    } catch {
      case e: Exception => e.printStackTrace(); throw e //Activator.getDefault().showError(e, "A " + e.getClass().getSimpleName() + " has occured")
    }
  }
  
  def task(body : => Unit) : Runnable = new Runnable {override def run() {body}}
  
}