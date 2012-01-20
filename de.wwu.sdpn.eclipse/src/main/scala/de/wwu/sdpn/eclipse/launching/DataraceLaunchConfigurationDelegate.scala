package de.wwu.sdpn.eclipse.launching

import org.eclipse.jdt.launching.AbstractJavaLaunchConfigurationDelegate
import de.wwu.sdpn.eclipse.Activator
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.model.ILaunchConfigurationDelegate;
import org.eclipse.jdt.launching.AbstractJavaLaunchConfigurationDelegate;

class DataraceLaunchConfigurationDelegate extends AbstractJavaLaunchConfigurationDelegate {
	def launch(configuration : ILaunchConfiguration , mode:String ,
			 launch:ILaunch, monitor:IProgressMonitor )  {
		
		try {
			
			
		} catch {
		  case e:Exception => //Activator.getDefault().showError(e, "A " + e.getClass().getSimpleName() + " has occured")
		} 
	}
}