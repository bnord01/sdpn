package de.wwu.sdpn.eclipse.launching.ui

import org.eclipse.core.resources.IProject;
import org.eclipse.jface.viewers.LabelProvider;

class ProjectSelectionDialogLabelProvider extends LabelProvider {

  override def getText(element: Object): String = {
    element match {
      case ip: IProject => ip.getName()
      case _ => element.toString()
    }
  }
}