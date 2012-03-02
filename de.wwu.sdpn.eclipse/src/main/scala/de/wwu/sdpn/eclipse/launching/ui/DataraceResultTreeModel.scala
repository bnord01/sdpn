package de.wwu.sdpn.eclipse.launching.ui

import org.eclipse.jface.viewers.ITreeContentProvider
import org.eclipse.jface.viewers.Viewer
import de.wwu.sdpn.wala.analyses.datarace._
import org.eclipse.jface.viewers.ILabelProvider
import org.eclipse.swt.graphics.Image
import DataraceAnalysis._
import org.eclipse.jface.viewers.ILabelProviderListener
import org.eclipse.jdt.ui.JavaUI
import org.eclipse.jdt.ui.ISharedImages
import com.ibm.wala.analysis.reflection.InstanceKeyWithNode
import com.ibm.wala.ipa.callgraph.propagation.ConstantKey
import com.ibm.wala.types.TypeReference
import de.wwu.sdpn.core.result._
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ssa.SSAFieldAccessInstruction
import com.ibm.wala.ssa.SSAGetInstruction
import com.ibm.wala.ssa.SSAPutInstruction
import org.eclipse.jdt.core.IJavaProject
import org.eclipse.jface.viewers.IDoubleClickListener
import org.eclipse.jface.viewers.DoubleClickEvent
import org.eclipse.jface.viewers.ITreeSelection
import de.wwu.sdpn.eclipse.util.WalaEclipseUtil
import org.eclipse.jdt.ui.JavaUI
import org.eclipse.jdt.core.IJavaElement
import org.eclipse.swt.widgets.Listener
import org.eclipse.swt.widgets.Event
import org.eclipse.swt.widgets.Menu
import org.eclipse.swt.widgets.Tree
import org.eclipse.jface.viewers.TreeViewer
import org.eclipse.swt.widgets.MenuItem
import org.eclipse.swt.events.SelectionListener
import org.eclipse.swt.events.SelectionEvent
import de.wwu.sdpn.core.ta.xsb.witness.WitnessTree
import de.wwu.sdpn.gui.ta.witness.zest.WTGraph
import org.eclipse.ui.PlatformUI
import org.eclipse.swt.widgets.Shell
import org.eclipse.swt.layout.FillLayout
import de.wwu.sdpn.eclipse.Activator
import de.wwu.sdpn.eclipse.DRAPreferences

class DataraceResultTreeModel(jproj: IJavaProject, result: DRResult) extends ITreeContentProvider with ILabelProvider with IDoubleClickListener {

    def getRoot: Object = result
    def getTotalResult = result.value

    def getTotalCount: Int = {
        var count = 0;
        for ((k, v) <- result.subResults; res <- v.subResults)
            count += 1
        count
    }

    def getPositiveCount: Int = getCount(Positive)
    def getNegativeCount: Int = getCount(Negative)

    def getCount(value: ResultValue): Int = {
        var count = 0;
        for ((k, v) <- result.subResults; (k1, res) <- v.subResults; if res.value == value)
            count += 1
        count
    }

    def getElements(inputElement: Object): Array[Object] = {
        inputElement match {
            case null => Array()
            case dbr: DRBaseResult =>
                val rv: Array[Object] = new Array(dbr.detail._2.size)
                for ((kv, nr) <- dbr.detail._2.zipWithIndex)
                    rv(nr) = (dbr, kv)
                rv
            case res: Result[AnyRef, AnyRef, AnyRef] => {
                val rv: Array[Object] = new Array(res.subResults.size)
                for (((k, v), nr) <- res.subResults.zipWithIndex)
                    rv(nr) = v
                rv
            }
        }
    }

    def getChildren(parentElement: Object): Array[Object] = {
        getElements(parentElement)
    }

    def getParent(element: Object): Object = {
        null
    }

    def hasChildren(element: Object): Boolean = {
        element match {
            case dbr: DRBaseResult => !dbr.detail._2.isEmpty
            case res: Result[_, _, _] => res.hasSubResults
            case (baseResult: DRBaseResult, (node: CGNode, instr: SSAFieldAccessInstruction)) => false
        }
    }

    def getImage(obj: Object): Image = {
        val si = JavaUI.getSharedImages

        obj match {
            case res: Result[_, _, _] => {
                res.value match {
                    case Positive => si.getImage(ISharedImages.IMG_OBJS_PRIVATE)
                    case Negative => si.getImage(ISharedImages.IMG_OBJS_PUBLIC)
                    case _        => si.getImage(ISharedImages.IMG_OBJS_PROTECTED)
                }
            }
            case (baseResult: DRBaseResult, (node: CGNode, instr: SSAFieldAccessInstruction)) =>
                instr match {
                    case _: SSAGetInstruction => si.getImage(ISharedImages.IMG_FIELD_PUBLIC)
                    case _: SSAPutInstruction => si.getImage(ISharedImages.IMG_FIELD_PRIVATE)
                }

        }
    }

    def getText(obj: Object): String = {
        def nn(tr: TypeReference): String = {
            if (tr.getName().toString().startsWith("L"))
                tr.getName().toString.drop(1).replace("/", ".")
            else
                tr.getName().toString()
        }
        obj match {
            case drr: DRResult     => "Overview"
            case fr: DRFieldResult => "Field: " + nn(fr.detail.getDeclaringClass()) + "." + fr.detail.getName() + " of type: " + nn(fr.detail.getFieldType())
            case br: DRBaseResult => br.detail._1 match {
                case nk: InstanceKeyWithNode => "Object created in: " + nk.getNode().getMethod().getSignature()
                case ck: ConstantKey[_]      => "Field on static object: " + ck.getValue()
                case k                       => k.toString()
            }
            case (baseResult: DRBaseResult, (node: CGNode, instr: SSAFieldAccessInstruction)) => instr match {
                case _: SSAGetInstruction => "Field read in " + node.getMethod().getSignature()
                case _: SSAPutInstruction => "Field write in " + node.getMethod().getSignature()
            }
        }
    }

    def dispose(): Unit = {}

    def inputChanged(viewer: Viewer, oldInput: Object, newInput: Object): Unit = {}

    def addListener(listener: ILabelProviderListener) {

    }

    def isLabelProperty(element: Object, property: String): Boolean = false

    /**
     * Removes a listener to this label provider.
     * Has no effect if an identical listener is not registered.
     *
     * @param listener a label provider listener
     */
    def removeListener(listener: ILabelProviderListener) {

    }

    def doubleClick(event: DoubleClickEvent) {
        event.getSelection() match {
            case ts: ITreeSelection =>
                ts.getFirstElement() match {
                    case drr: DRResult     =>
                    case fr: DRFieldResult =>
                    case br: DRBaseResult  =>
                    case (baseResult: DRBaseResult, (node: CGNode, instr: SSAFieldAccessInstruction)) =>
                        val imeth = WalaEclipseUtil.cgnode2IMethod(jproj, node)
                        if (imeth != null)
                            JavaUI.revealInEditor(JavaUI.openInEditor(imeth), imeth: IJavaElement);
                }

        }
    }

    def getMenuListener(treeViewer: TreeViewer, mntmShowWitness: MenuItem, mntmSimmulateDpn: MenuItem): Listener = {
        return new Listener {
            def handleEvent(event: Event) {
                treeViewer.getSelection() match {
                    case ts: ITreeSelection =>
                        ts.getFirstElement() match {
                            case br: DRBaseResult =>
                                mntmShowWitness.setEnabled(true)
                                mntmSimmulateDpn.setEnabled(true)

                            case (baseResult: DRBaseResult, (node: CGNode, instr: SSAFieldAccessInstruction)) =>
                                mntmShowWitness.setEnabled(true)
                                mntmSimmulateDpn.setEnabled(true)
                            case fr: DRFieldResult if fr.subResults.size == 1 =>
                                mntmShowWitness.setEnabled(true)
                                mntmSimmulateDpn.setEnabled(true)
                            case _ =>
                                mntmShowWitness.setEnabled(false)
                                mntmSimmulateDpn.setEnabled(false)
                        }
                    case _ =>
                        mntmShowWitness.setEnabled(false)
                        mntmSimmulateDpn.setEnabled(false)

                }

            }
        };
    }

    def getShowDPNListener(treeViewer: TreeViewer): SelectionListener = {
        return new SelectionListener {
            def widgetSelected(e: SelectionEvent) {
                treeViewer.getSelection() match {
                    case ts: ITreeSelection =>
                        ts.getFirstElement() match {
                            case br: DRBaseResult =>
                                println("Starting DPN View")
                                val dpn = br.detail._3.dpn
                                println("DPN size: " + dpn.getTransitions.size)
                                de.wwu.sdpn.core.gui.MonitorDPNView.show(dpn)
                            case (br: DRBaseResult, (node: CGNode, instr: SSAFieldAccessInstruction)) =>
                                println("Starting DPN View")
                                val dpn = br.detail._3.dpn
                                println("DPN size: " + dpn.getTransitions.size)
                                de.wwu.sdpn.core.gui.MonitorDPNView.show(dpn)
                            case fr: DRFieldResult if fr.subResults.size == 1 =>
                                val br = fr.subResults.values.first
                                println("Starting DPN View")
                                val dpn = br.detail._3.dpn
                                println("DPN size: " + dpn.getTransitions.size)
                                de.wwu.sdpn.core.gui.MonitorDPNView.show(dpn)
                            case _ =>
                        }
                    case _ =>

                }
            }

            def widgetDefaultSelected(e: SelectionEvent) {}
        }
    }

    def getShowWitnessListener(treeViewer: TreeViewer): SelectionListener = {
        return new SelectionListener {
            def widgetSelected(e: SelectionEvent) {
                val witness = treeViewer.getSelection() match {
                    case ts: ITreeSelection =>
                        ts.getFirstElement() match {
                            case br: DRBaseResult =>
                                Some(br.detail._3.cg, br.detail._3.getWitness)
                            case (br: DRBaseResult, (node: CGNode, instr: SSAFieldAccessInstruction)) =>
                                Some(br.detail._3.cg, br.detail._3.getWitness)
                            case fr: DRFieldResult if fr.subResults.size == 1 =>
                                val br = fr.subResults.values.first
                                Some(br.detail._3.cg, br.detail._3.getWitness)
                            case _ => None
                        }
                    case _ => None
                }
                witness match {
                    case Some((cg, Some(wt))) =>
                        val doPrune = Activator.getDefault().getPreferenceStore().getBoolean(DRAPreferences.B_PRUNE_WITNESS);

                        val decorator = (t: WitnessTree) => {
                            val ss = t.state.ss
                            val nr = ss.cg
                            val node = cg.getNode(nr)
                            val name = node.getMethod().getDeclaringClass().getName().toString().drop(1).split("/").last + "." + node.getMethod().getName()
                            var cn = t.getClass().getCanonicalName()
                            cn = cn.dropRight(4)
                            cn = cn split ('.') last;
                            if (doPrune)
                                "%s in  %s".format(cn, name, ss.bb, ss.instr)
                            else
                                "%s in  %s  at (%d,%d)".format(cn, name, ss.bb, ss.instr)
                        }

                        val selectionListener = (t: WitnessTree) => {
                            val num = t.state.ss.cg
                            val cgnode = cg.getNode(num)
                            val imeth = WalaEclipseUtil.cgnode2IMethod(jproj, cgnode)
                            if (imeth != null)
                                JavaUI.revealInEditor(JavaUI.openInEditor(imeth), imeth: IJavaElement);
                        }

                        val witnessTree = if (doPrune) WitnessTree.pruneBase(wt) else wt

                        val display = PlatformUI.getWorkbench().getDisplay()
                        display.asyncExec(new Runnable {
                            def run {
                                val shell = new Shell(display);
                                shell.setText("Witness View");
                                shell.setLayout(new FillLayout());
                                shell.setSize(400, 800);
                                new WTGraph(witnessTree, shell, decorator = decorator, selectionListener = selectionListener)
                                shell.open();
                            }
                        })

                    case _ =>
                }
            }

            def widgetDefaultSelected(e: SelectionEvent) {}
        }
    }

}