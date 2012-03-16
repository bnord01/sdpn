package de.wwu.sdpn.eclipse;

import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;

import swing2swt.layout.BorderLayout;
import swing2swt.layout.FlowLayout;
import de.wwu.sdpn.core.result.ResultValue;
import de.wwu.sdpn.eclipse.launching.ui.DataraceResultTreeModel;

public class DataraceResultComposite extends Composite {

	private DataraceResultTreeModel result;
	private TreeViewer treeViewer;
	private Label lblNumPossibleRace;
	private Label lblNumRaceFree;
	private Label lblTotalResults;
	private Menu menu;
	private Tree tree;
	private MenuItem mntmShowWitness;
	private MenuItem mntmSimmulateDpn;	
	
	private Listener menuListener;
	private SelectionListener menuShowWitnessListener;
	private SelectionListener menuShowDPNListener;
	

	public ResultValue getResult() {
		return result.getTotalResult();
	}

	/**
	 * Create the composite.
	 * 
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
		tree = treeViewer.getTree();

		menu = new Menu(tree);
		tree.setMenu(menu);

		mntmShowWitness = new MenuItem(menu, SWT.NONE);
		mntmShowWitness.setText("Show witness");
		mntmShowWitness.setEnabled(false);

		mntmSimmulateDpn = new MenuItem(menu, SWT.NONE);
		mntmSimmulateDpn.setText("Simmulate DPN");
		mntmSimmulateDpn.setEnabled(false);
		sashForm.setWeights(new int[] { 1 });

	}

	@Override
	protected void checkSubclass() {
		// Disable the check that prevents subclassing of SWT components
	}

	public void setModel(DataraceResultTreeModel newResult) {
		// Remove old listeners first if result already set!
		if(this.result != null) {
			menu.removeListener(SWT.Show, menuListener);
			mntmShowWitness.removeSelectionListener(menuShowWitnessListener);
			mntmSimmulateDpn.removeSelectionListener(menuShowDPNListener);
			treeViewer.removeDoubleClickListener(result);		
		}
		
		this.result = newResult;
		this.menuShowWitnessListener = result.getShowWitnessListener(treeViewer);
		this.menuShowDPNListener = result.getShowDPNListener(treeViewer);
		this.menuListener = result.getMenuListener(treeViewer, mntmShowWitness, mntmSimmulateDpn);
		
		treeViewer.setContentProvider(result);
		treeViewer.setLabelProvider(result);
		treeViewer.setInput(result.getRoot());
		treeViewer.addDoubleClickListener(result);		
		menu.addListener(SWT.Show, menuListener);
		mntmShowWitness.addSelectionListener(this.menuShowWitnessListener);
		mntmSimmulateDpn.addSelectionListener(this.menuShowDPNListener);
		
		lblNumRaceFree.setText("Race free: " + result.getNegativeCount() + "/"
				+ result.getTotalCount());
		lblTotalResults.setImage(result.getImage(result.getRoot()));
		lblNumPossibleRace.setText("Possible race: "
				+ result.getPositiveCount() + "/" + result.getTotalCount());

	}


}
