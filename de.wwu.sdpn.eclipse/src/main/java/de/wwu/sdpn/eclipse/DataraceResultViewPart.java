package de.wwu.sdpn.eclipse;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;

import de.wwu.sdpn.eclipse.launching.ui.ResultTreeModel;
import swing2swt.layout.BorderLayout;

public class DataraceResultViewPart extends ViewPart {

	public static final String ID = "de.wwu.sdpn.eclipse.DataraceResultViewPart"; //$NON-NLS-1$
	private DataraceResultComposite dataraceResultComposite;

	public DataraceResultViewPart() {
	}

	/**
	 * Create contents of the view part.
	 * @param parent
	 */
	@Override
	public void createPartControl(Composite parent) {
		Composite container = new Composite(parent, SWT.NONE);
		container.setLayout(new BorderLayout(0, 0));
		{
			dataraceResultComposite = new DataraceResultComposite(container, SWT.NONE);
			dataraceResultComposite.setLayoutData(BorderLayout.CENTER);
		}

		createActions();
		initializeToolBar();
		initializeMenu();
	}
	
	private ResultTreeModel getModel() {
		return Activator.getDefault().getLastDataraceResult();
	}

	/**
	 * Create the actions.
	 */
	private void createActions() {
		// Create the actions
	}

	/**
	 * Initialize the toolbar.
	 */
	private void initializeToolBar() {
		IToolBarManager toolbarManager = getViewSite().getActionBars()
				.getToolBarManager();
	}

	/**
	 * Initialize the menu.
	 */
	private void initializeMenu() {
		IMenuManager menuManager = getViewSite().getActionBars()
				.getMenuManager();
	}

	@Override
	public void setFocus() {
		resetModel();
	}
	
	private void resetModel() {
		ResultTreeModel model = getModel();
		if(model != null)
			dataraceResultComposite.setModel(model);
	}

}
