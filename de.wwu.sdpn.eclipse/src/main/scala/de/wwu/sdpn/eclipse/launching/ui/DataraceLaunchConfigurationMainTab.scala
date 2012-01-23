package de.wwu.sdpn.eclipse.launching.ui
import org.eclipse.debug.ui.AbstractLaunchConfigurationTab
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy
import org.eclipse.debug.core.ILaunchConfiguration
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.SWT
import org.eclipse.swt.layout.GridLayout
import org.eclipse.swt.layout.GridData
import org.eclipse.swt.widgets.Group
import org.eclipse.swt.widgets.Text
import org.eclipse.swt.events.ModifyListener
import org.eclipse.swt.events.ModifyEvent
import org.eclipse.swt.events.SelectionAdapter
import org.eclipse.swt.events.SelectionEvent
import org.eclipse.swt.widgets.Button
import org.eclipse.ui.dialogs.ElementListSelectionDialog
import org.eclipse.core.resources.ResourcesPlugin
import org.eclipse.core.resources.IProject
import org.eclipse.swt.widgets.FileDialog
import org.eclipse.core.runtime.Path
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jdt.ui.JavaUI
import org.eclipse.jdt.core.JavaCore
import org.eclipse.jdt.core.search.SearchEngine
import org.eclipse.jface.window.Window
import org.eclipse.jdt.core.IType
import org.eclipse.jface.window.ApplicationWindow
import de.wwu.sdpn.eclipse.launching.DataraceLaunchConfig

class DataraceLaunchConfigurationMainTab extends AbstractLaunchConfigurationTab {

  private var projectText: Text = null

  private var standardConf: Button = null
  private var classText: Text = null
  /**
   * Modify listener that simply updates the owning launch configuration dialog.
   */
  val fBasicModifyListener = new ModifyListener() {
    def modifyText(evt: ModifyEvent) {
      updateLaunchConfigurationDialog();
    }
  };
  def getName() = "Data race check"

  def createControl(parent: Composite) {
    val comp = new Composite(parent, SWT.NONE);
    setControl(comp);
    val topLayout = new GridLayout(1, true);
    topLayout.horizontalSpacing = 10;
    comp.setLayout(topLayout);
    comp.setFont(parent.getFont());

    var gd: GridData = null;

    /* Group Project */
    val projectGroup = new Group(comp, SWT.NONE);
    val projectLayout = new GridLayout();
    projectLayout.numColumns = 3;
    projectGroup.setLayout(projectLayout);
    gd = new GridData(GridData.FILL_HORIZONTAL);
    gd.horizontalSpan = 1;
    projectGroup.setLayoutData(gd);
    projectGroup.setText("Project:");
    projectGroup.setFont(comp.getFont());

    projectText = new Text(projectGroup, SWT.SINGLE | SWT.BORDER);
    gd = new GridData(GridData.FILL_HORIZONTAL);
    gd.horizontalSpan = 2;
    projectText.setLayoutData(gd);
    projectText.addModifyListener(fBasicModifyListener);

    val projectButton = createPushButton(projectGroup, "Browse...", null);
    projectButton.addSelectionListener(new SelectionAdapter() {
      override def widgetSelected(evt: SelectionEvent) {
        handleProjectButtonSelected();
      }
    });

    standardConf = new Button(projectGroup, SWT.CHECK);
    standardConf.setText("Standard Configuration for this Project (this configuration is used for Assignement)");
    gd = new GridData();
    gd.horizontalSpan = 3;
    standardConf.setLayoutData(gd);
    standardConf.addSelectionListener(new SelectionAdapter() {
      override def widgetSelected(evt: SelectionEvent) {
        updateLaunchConfigurationDialog();
      }
    });

    /* Group Class */
    val classGroup = new Group(comp, SWT.NONE);
    val classLayout = new GridLayout();
    classLayout.numColumns = 3;
    classGroup.setLayout(classLayout);
    gd = new GridData(GridData.FILL_HORIZONTAL);
    gd.horizontalSpan = 1;
    classGroup.setLayoutData(gd);
    classGroup.setText("Hauptklasse:");
    classGroup.setFont(comp.getFont());

    classText = new Text(classGroup, SWT.SINGLE | SWT.BORDER);
    gd = new GridData(GridData.FILL_HORIZONTAL);
    gd.horizontalSpan = 2;
    classText.setLayoutData(gd);
    classText.addModifyListener(fBasicModifyListener);

    val classButton = createPushButton(classGroup, "Browse...", null);
    classButton.addSelectionListener(new SelectionAdapter() {
      override def widgetSelected(evt: SelectionEvent) {
        val exts = Array("*.java")
        handleBrowseFileButtonSelected(classText, exts);
      }
    });

  }

  private def handleProjectButtonSelected() {
    val lp = new ProjectSelectionDialogLabelProvider();

    val elsd = new ElementListSelectionDialog(getShell(), lp);
    elsd.setElements(ResourcesPlugin.getWorkspace().getRoot().getProjects().asInstanceOf[Array[Object]]);

    elsd.setMultipleSelection(false);
    elsd.open();

    if (elsd.getFirstResult() == null) return ;

    val result = elsd.getFirstResult().asInstanceOf[IProject];

    this.projectText.setText(result.getName());
  }

  private def handleBrowseFileButtonSelected(target: Text, extensions: Array[String]) {
    val ir = ResourcesPlugin.getWorkspace().getRoot().findMember(this.projectText.getText())

    val mtd = JavaUI.createMainTypeDialog(getShell(), new ApplicationWindow(getShell()), SearchEngine.createJavaSearchScope(Array(ir)), 0, false)
    mtd.open
    val res = mtd.getResult()
    if (res.length > 0)
      res(0) match {
        case it: IType => target.setText(it.getFullyQualifiedName())
        case _ => throw new IllegalArgumentException("Expected result of type IType but got: " + res(0))
      }

  }

  def setDefaults(conf: ILaunchConfigurationWorkingCopy) {
    import DataraceLaunchConfig._
    conf.setContainer(null);
    conf.setAttribute(PROJECT_NAME, PROJECT_NAME_DEFAULT)
    conf.setAttribute(MAIN_CLASS_NAME, MAIN_CLASS_NAME_DEFAULT)

  }

  def initializeFrom(conf: ILaunchConfiguration) {
    import DataraceLaunchConfig._
    projectText.setText(conf.getAttribute(PROJECT_NAME, PROJECT_NAME_DEFAULT))
    classText.setText(conf.getAttribute(MAIN_CLASS_NAME, MAIN_CLASS_NAME_DEFAULT))
  }

  def performApply(conf: ILaunchConfigurationWorkingCopy) {
    import DataraceLaunchConfig._
    conf.setAttribute(PROJECT_NAME, projectText.getText())
    conf.setAttribute(MAIN_CLASS_NAME, classText.getText())
  }
}