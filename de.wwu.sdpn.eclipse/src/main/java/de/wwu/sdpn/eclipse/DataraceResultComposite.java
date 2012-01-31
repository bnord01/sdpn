package de.wwu.sdpn.eclipse;

import org.eclipse.swt.widgets.Composite;
import swing2swt.layout.BorderLayout;
import org.eclipse.swt.SWT;
import swing2swt.layout.FlowLayout;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.jface.viewers.TreeViewer;

import de.wwu.sdpn.eclipse.launching.ui.ResultTreeModel;
import de.wwu.sdpn.core.result.ResultValue;

import org.eclipse.swt.widgets.Text;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.ITreeSelection;

public class DataraceResultComposite extends Composite {

	private ResultTreeModel result;
	private TreeViewer treeViewer;
	private Label lblNumPossibleRace;
	private Label lblNumRaceFree;
	private Label lblTotalResults;
	
	public ResultValue getResult() {
		return result.getTotalResult();
	}
	
	/**
	 * Create the composite.
	 * @param parent
	 * @param style
	 */
	
	
	
	public DataraceResultComposite(Composite parent, int style) {		
		super(parent, style);
		setLayout(new BorderLayout(0, 0));
		
		Composite composite = new Composite(this, SWT.NONE);
		composite.setLayoutData(BorderLayout.NORTH);
		composite.setLayout(new FlowLayout(FlowLayout.LEFT, 5, 5));
		
		Label lblOverallResult = new Label(composite, SWT.NONE);
		lblOverallResult.setText("Overall Result:");
		
		lblTotalResults = new Label(composite, SWT.NONE);
		lblTotalResults.setText("Pos/Neg");
		
		lblNumRaceFree = new Label(composite, SWT.NONE);
		lblNumRaceFree.setText("Race free: y/z");
		
		lblNumPossibleRace = new Label(composite, SWT.NONE);
		lblNumPossibleRace.setText("Possible race: y/z");
		
		Composite composite_1 = new Composite(this, SWT.NONE);
		composite_1.setLayoutData(BorderLayout.CENTER);
		composite_1.setLayout(new BorderLayout(0, 0));
		
		SashForm sashForm = new SashForm(composite_1, SWT.NONE);
		sashForm.setLayoutData(BorderLayout.CENTER);
		
		treeViewer = new TreeViewer(sashForm, SWT.BORDER);
		Tree tree = treeViewer.getTree(); 
		sashForm.setWeights(new int[] {1});

	}

	@Override
	protected void checkSubclass() {
		// Disable the check that prevents subclassing of SWT components
	}
	
	public void setModel(ResultTreeModel result) {
		this.result = result;
		treeViewer.setContentProvider(result);
		treeViewer.setLabelProvider(result);
		treeViewer.setInput(result.getRoot());
		treeViewer.addDoubleClickListener(result);			
		lblNumRaceFree.setText("Race free: " + result.getNegativeCount() + "/" + result.getTotalCount());
		lblTotalResults.setImage(result.getImage(result.getRoot()));
		lblNumPossibleRace.setText("Possible race: " + result.getPositiveCount() + "/" + result.getTotalCount());

	}
	

}
