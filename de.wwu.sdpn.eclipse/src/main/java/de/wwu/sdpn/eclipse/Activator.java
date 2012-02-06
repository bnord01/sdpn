package de.wwu.sdpn.eclipse;

import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import de.wwu.sdpn.eclipse.launching.ui.DataraceResultTreeModel;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {

	// The plug-in ID
	public static final String PLUGIN_ID = "de.wwu.sdpn.eclipse"; //$NON-NLS-1$

	// The shared instance
	private static Activator plugin;
	
	private DataraceResultTreeModel lastDataraceResult = null;
	private IJavaProject lastJavaProject = null;
	
	public DataraceResultTreeModel getLastDataraceResult() {
		return lastDataraceResult;
	}

	public void setLastDataraceResult(DataraceResultTreeModel lastDataraceResult) {
		this.lastDataraceResult = lastDataraceResult;
	}

	/**
	 * The constructor
	 */
	public Activator() {
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 *
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}

	public IJavaProject getLastJavaProject() {
		return lastJavaProject;
	}

	public void setLastJavaProject(IJavaProject lastJavaProject) {
		this.lastJavaProject = lastJavaProject;
	}

}
