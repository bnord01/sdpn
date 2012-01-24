package de.wwu.sdpn.eclipse.launching.ui

import org.eclipse.debug.ui.ILaunchShortcut
import org.eclipse.ui.IEditorPart
import org.eclipse.core.resources.IResource
import org.eclipse.jface.viewers.ISelection
import org.eclipse.debug.core.ILaunchConfiguration
import org.eclipse.jface.viewers.ITreeSelection
import org.eclipse.jdt.core.ICompilationUnit
import org.eclipse.jdt.core.IType
import org.eclipse.jdt.core.IMethod
import org.eclipse.jdt.core.IField
import de.wwu.sdpn.eclipse.launching.DataraceLauncher
import org.eclipse.jface.dialogs.ProgressMonitorDialog
import org.eclipse.jface.operation.IRunnableWithProgress
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.ui.PlatformUI

class DataraceLaunchShortcut extends ILaunchShortcut {

  //  def getLaunchConfigurations(selection: ISelection): Array[ILaunchConfiguration] = { null }
  //
  //  def getLaunchConfigurations(editorpart: IEditorPart): Array[ILaunchConfiguration] = { null }
  //
  //  def getLaunchableResource(selection: ISelection): IResource = { null }
  //
  //  def getLaunchableResource(editorpart: IEditorPart): IResource = { null }

  def launch(selection: ISelection, mode: String): Unit = {
    var mainClass: IType = null
    selection match {
      case is: ITreeSelection =>
        is.getFirstElement match {
          case cu: ICompilationUnit =>
            mainClass = cu.getTypes().filter(_.isClass()).first
          case it: IType =>
            mainClass = it
          case im: IMethod =>
            mainClass = im.getDeclaringType()
          case field: IField =>
            mainClass = field.getDeclaringType()
          case _ => throw new IllegalArgumentException("Did not expect to get " + is)
        }
      case _ => throw new IllegalArgumentException("Expecte TreeSelection but got " + selection)
    }

    val project = mainClass.getJavaProject()
    val className = mainClass.getFullyQualifiedName()

    val iop = new IRunnableWithProgress {
      override def run(pm: IProgressMonitor) {
        DataraceLauncher.runDataRaceAnalysisOnClass(project, className, pm)
      }
    }
    val display = PlatformUI.getWorkbench().getDisplay()

    display.syncExec(task(new ProgressMonitorDialog(display.getActiveShell()).run(true,true,iop)))

  }

  def task(body: => Unit): Runnable = new Runnable { override def run() { body } }
  def launch(editor: IEditorPart, mode: String): Unit = {
    throw new UnsupportedOperationException("Did not expect to get opend on editor!")

  }

}