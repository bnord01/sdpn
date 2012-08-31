package de.wwu.sdpn.eclipse.launching
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.NullProgressMonitor
import org.eclipse.core.runtime.SubProgressMonitor
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jface.dialogs.MessageDialog
import org.eclipse.swt.widgets.Shell
import org.eclipse.ui.PlatformUI
import de.wwu.sdpn.core.analyses.SDPNProps
import de.wwu.sdpn.core.result.Negative
import de.wwu.sdpn.core.result.Positive
import de.wwu.sdpn.eclipse.launching.ui.DataraceResultTreeModel
import de.wwu.sdpn.eclipse.Activator
import de.wwu.sdpn.eclipse.DRAPreferences
import de.wwu.sdpn.eclipse.DataraceResultViewPart
import de.wwu.sdpn.wala.analyses.datarace.DataraceAnalysis
import de.wwu.sdpn.wala.analyses.SimpleAnalyses
import de.wwu.sdpn.eclipse.util.WalaEclipseUtil
import de.wwu.sdpn.eclipse.util.EPMWrapper
import de.wwu.sdpn.wala.analyses.datarace.DRAOptions

object DataraceLauncher {

    def runDataRaceAnalysisOnClass(proj: IJavaProject, mcname: String, pm0: IProgressMonitor = null) {
        val pm: IProgressMonitor = if (pm0 == null) new NullProgressMonitor else pm0

        try {
            pm.beginTask("Running DPN-based data race analysis on class " + mcname, 9)

            check(pm)

            val cp = proj.getResource().getLocation().removeTrailingSeparator().removeLastSegments(1).append(proj.getOutputLocation()).toFile().toString()

            val java_lib_names = Set("classes.jar", "rt.jar", "core.jar")

            loadAndSetSDPNPrefs();

            val useStubs = Activator.getDefault().getPreferenceStore().getBoolean(DRAPreferences.B_MANUAL_JRE);
            val jrelib = if (useStubs) {
                com.ibm.wala.ipa.callgraph.impl.Util.setNativeSpec("natives_empty.xml");
                Activator.getDefault().getPreferenceStore().getString(DRAPreferences.S_JRE_PATH);
            } else {
                com.ibm.wala.ipa.callgraph.impl.Util.setNativeSpec("natives.xml");
                proj.getPackageFragmentRoots().filter(x => java_lib_names(x.getElementName())).head.getPath().toFile().toString()
            }

            val useIPL = !Activator.getDefault().getPreferenceStore().getBoolean(DRAPreferences.B_DONT_USE_INTERPROLOG);
            
            SimpleAnalyses.useInterprolog(useIPL)
            
            println("Running data race analysis with parameters")
            println("Class Path: " + cp)
            println("JRELib: " + jrelib)
            println("Main class: " + mcname)

            pm subTask "Setting up WALA scope"
            val scope = WalaEclipseUtil.makeScopeFromCPnJRE(cp, jrelib)

            val mainClass = "L" + mcname.replace('.', '/')

            check(pm)
            pm worked 1

            pm subTask "Building WALA call graph and running pointer analysis."
            val (cg, pa) = WalaEclipseUtil.makeCGFromScopeAndMainClass(scope, mainClass)
            pm worked 2

            check(pm)
            pm subTask "Identifying field access points."
            val ops = new DRAOptions 
            ops.randomIsolation = Activator.getDefault().getPreferenceStore().getBoolean(DRAPreferences.B_RANDOMISOLATION)
            ops.ignoreWait = Activator.getDefault().getPreferenceStore().getBoolean(DRAPreferences.B_IGNORE_WAIT)
            val rda = new DataraceAnalysis(cg, pa,ops)
            pm worked 1

            check(pm)
            pm subTask "Running DPN-based analyses for individual instances."

            val result = rda.fullDetailedAnalysis(pm = new EPMWrapper(new SubProgressMonitor(pm, 5)))

            println("Data race possible: " + result.value)

            val display = PlatformUI.getWorkbench().getDisplay()

            Activator.getDefault().setLastDataraceResult(new DataraceResultTreeModel(proj, result))
            var page = PlatformUI.getWorkbench().getWorkbenchWindows()(0).getPages()(0)
            guiDo({
                var view = page.findView(DataraceResultViewPart.ID)
                if (view == null) {
                    println("View was null!")
                    view = page.showView(DataraceResultViewPart.ID)
                }

                view.setFocus()
            })

            result.value match {
                case Positive =>
                    guiDo(
                        MessageDialog.openWarning(new Shell(display), "Race found!", "There might be a race in your program\nSee result view."))
                case Negative =>
                    guiDo(
                        MessageDialog.openInformation(new Shell(display), "No race found", "There is no race in your program"))
                case _ =>
                    guiDo(
                        MessageDialog.openError(new Shell(display), "Something stupid", "I don't understand this result:\n" + result.toString()))
            }

        } finally {
            pm done
        }
    }

    def guiDo(body: => Unit) {
        val display = PlatformUI.getWorkbench().getDisplay()
        display.asyncExec(new Runnable { override def run() { body } })
    }
    def task(body: => Unit): Runnable = new Runnable { override def run() { body } }

    def check(pm: IProgressMonitor) { if (pm.isCanceled()) throw new RuntimeException("Job canceled by user.") }

    def loadAndSetSDPNPrefs() {
        val xsb = Activator.getDefault().getPreferenceStore().getString(DRAPreferences.S_XSB_EXE);
        val tmpDir = Activator.getDefault().getPreferenceStore().getString(DRAPreferences.S_TEMP_DIR);
        val debug = Activator.getDefault().getPreferenceStore().getBoolean(DRAPreferences.B_DEBUG);
        println(xsb);
        println(tmpDir);
        SDPNProps.set(SDPNProps(xsb, tmpDir, debug))
    }

}