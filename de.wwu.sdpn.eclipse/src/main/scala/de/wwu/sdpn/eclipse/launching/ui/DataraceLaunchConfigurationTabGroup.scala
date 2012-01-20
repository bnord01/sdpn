package de.wwu.sdpn.eclipse.launching.ui
import org.eclipse.debug.ui.ILaunchConfigurationTabGroup
import org.eclipse.debug.ui.ILaunchConfigurationTab
import org.eclipse.debug.ui.ILaunchConfigurationDialog
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy
import org.eclipse.debug.core.ILaunchConfiguration
import org.eclipse.debug.core.ILaunch
import org.eclipse.debug.ui.CommonTab
import de.wwu.sdpn.eclipse.launching.ui.DataraceLaunchConfigurationMainTab

class DataraceLaunchConfigurationTabGroup extends ILaunchConfigurationTabGroup {
  
private var tabs : List[ILaunchConfigurationTab] = List()
	
	def createTabs( dialog: ILaunchConfigurationDialog, mode : String) {  
	    tabs = List(
	        new DataraceLaunchConfigurationMainTab(),
	        new CommonTab())
	}

	def  getTabs() : Array[ILaunchConfigurationTab] = {
		return tabs.toArray
	}

	def dispose() { }

	def setDefaults( configuration:ILaunchConfigurationWorkingCopy) {
	    for (tab <- tabs) {
	        tab.setDefaults(configuration);	
	    }
	}

	def initializeFrom( configuration:ILaunchConfiguration) {
        for (tab <- tabs) {
            tab.initializeFrom(configuration);
        }
		
	}

	def performApply( configuration:ILaunchConfigurationWorkingCopy) {
        for (tab <- tabs) {
            tab.performApply(configuration);
        }		
	}

	def launched(launch : ILaunch) { }
}