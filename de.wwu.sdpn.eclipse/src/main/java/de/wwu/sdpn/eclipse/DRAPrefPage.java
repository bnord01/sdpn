package de.wwu.sdpn.eclipse;

import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;

public class DRAPrefPage extends FieldEditorPreferencePage implements
		IWorkbenchPreferencePage {

	/**
	 * Create the preference page.
	 */
	public DRAPrefPage() {
		super(FLAT);
		setTitle("Data Race Analysis");
	}

	/**
	 * Create contents of the preference page.
	 */
	@Override
	protected void createFieldEditors() {
		// Create the field editors
		addField(new FileFieldEditor(DRAPreferences.S_XSB_EXE, "XSB executable", getFieldEditorParent()));
		addField(new DirectoryFieldEditor(DRAPreferences.S_TEMP_DIR, "Temp dir", getFieldEditorParent()));
		addField(new BooleanFieldEditor(DRAPreferences.B_MANUAL_JRE, "Use JRE Stubs", BooleanFieldEditor.DEFAULT, getFieldEditorParent()));
		addField(new FileFieldEditor(DRAPreferences.S_JRE_PATH, "Stubs path", getFieldEditorParent()));
		addField(new BooleanFieldEditor(DRAPreferences.B_PRUNE_WITNESS, "Prune generated witnesses", BooleanFieldEditor.DEFAULT, getFieldEditorParent()));
		addField(new BooleanFieldEditor(DRAPreferences.B_DONT_USE_INTERPROLOG, "Don't use Interprolog for communication with XSB", BooleanFieldEditor.DEFAULT, getFieldEditorParent()));
	}

	/**
	 * Initialize the preference page.
	 */
	public void init(IWorkbench workbench) {
		this.setPreferenceStore(Activator.getDefault().getPreferenceStore());
		// Initialize the preference page
	}

}
