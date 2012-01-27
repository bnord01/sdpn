package de.wwu.sdpn.eclipse;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.SWT;

import de.wwu.sdpn.eclipse.launching.ui.ResultTreeModel;

public class DataraceResultDialog extends Dialog {

	protected ResultTreeModel model;
	private DataraceResultComposite resultComposite;
	/**
	 * Create the dialog.
	 * @param parentShell
	 */
	public DataraceResultDialog(Shell parentShell,ResultTreeModel model) {
		super(parentShell);
		this.model = model;		
	}

	/**
	 * Create contents of the dialog.
	 * @param parent
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		Composite container = (Composite) super.createDialogArea(parent);
		container.setLayout(new FillLayout(SWT.HORIZONTAL));
		
		resultComposite = new DataraceResultComposite(container,SWT.NONE);	
		resultComposite.setModel(model);
		
		return container;
	}

	/**
	 * Create contents of the button bar.
	 * @param parent
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
	}

	/**
	 * Return the initial size of the dialog.
	 */
	@Override
	protected Point getInitialSize() {
		return new Point(800, 300);
	}

}
