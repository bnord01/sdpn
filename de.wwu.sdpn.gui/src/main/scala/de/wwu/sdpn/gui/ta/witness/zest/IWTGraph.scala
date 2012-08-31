package de.wwu.sdpn.gui.ta.witness.zest

import scala.annotation.elidable
import scala.annotation.implicitNotFound
import org.eclipse.draw2d.geometry.Dimension
import org.eclipse.swt.graphics.Color
import org.eclipse.swt.layout.FillLayout
import org.eclipse.swt.widgets.Composite
import org.eclipse.swt.widgets.Display
import org.eclipse.swt.widgets.Shell
import org.eclipse.swt.SWT
import org.eclipse.zest.core.widgets.Graph
import org.eclipse.zest.core.widgets.GraphConnection
import org.eclipse.zest.core.widgets.GraphNode
import org.eclipse.zest.core.widgets.ZestStyles
import org.eclipse.zest.layouts.algorithms.TreeLayoutAlgorithm
import com.ibm.wala.ipa.callgraph.CallGraph
import annotation.elidable.ASSERTION
import de.wwu.sdpn.core.ta.xsb.witness.iterable.WitnessTree
import de.wwu.sdpn.core.ta.xsb.witness.iterable.BaseTree
import de.wwu.sdpn.core.ta.xsb.witness.iterable.Call1Tree
import de.wwu.sdpn.core.ta.xsb.witness.iterable.AcqTree
import de.wwu.sdpn.core.ta.xsb.witness.iterable.Call2Tree
import de.wwu.sdpn.core.ta.xsb.witness.iterable.UseTree
import de.wwu.sdpn.core.ta.xsb.witness.iterable.SpawnTree
import de.wwu.sdpn.core.ta.xsb.witness.iterable.NilTree
import de.wwu.sdpn.core.ta.xsb.witness.iterable.RetTree
import org.eclipse.draw2d.SWTEventDispatcher
import org.eclipse.swt.events.SelectionListener
import org.eclipse.swt.events.SelectionEvent
import de.wwu.sdpn.core.ta.xsb.witness.iterable.AbstractFullWitnessParser
import de.wwu.sdpn.wala.analyses.datarace.DRStateParser

class IWTGraph(tree: WitnessTree, parent: Composite, decorator: WitnessTree => String = _.toString, selectionListener: WitnessTree => Unit = _ => ()) extends Graph(parent, 0) {
    // Disable moving of nodes
    this.getLightweightSystem().setEventDispatcher(
        new SWTEventDispatcher() {
            override def dispatchMouseMoved(me: org.eclipse.swt.events.MouseEvent) {
                // Doing nothing
            }
        });

    this.addSelectionListener(new SelectionListener {
        def widgetSelected(se: SelectionEvent) {
            if (se != null && se.item != null)
                se.item.getData match {
                    case wt: WitnessTree => selectionListener(wt)
                    case _               =>
                }
        }
        def widgetDefaultSelected(se: SelectionEvent) {
        }
    })
    setConnectionStyle(ZestStyles.CONNECTIONS_DIRECTED)
    addTree(tree)

    private val tlayout = new TreeLayoutAlgorithm()
    tlayout.setNodeSpace(new Dimension(150, 40))
    setLayoutAlgorithm(tlayout, true);

    def addTree(t: WitnessTree): GraphNode = {
        t match {
            case BaseTree(annot,state, child) =>
                val v2 = addTree(child)
                val v1 = new GraphNode(this, SWT.None, t)
                v1.setText(decorator(t))
                val con = new GraphConnection(this, SWT.None, v1, v2)
                return v1

            case Call1Tree(annot,state, child) =>
                val v2 = addTree(child)
                val v1 = new GraphNode(this, SWT.None, t)
                v1.setText(decorator(t))
                val con = new GraphConnection(this, SWT.None, v1, v2)
                return v1

            case AcqTree(annot,state,  child) =>
                val v2 = addTree(child)
                val v1 = new GraphNode(this, SWT.None, t)
                v1.setText(decorator(t))
//                if (!reentrant) {
                    val color = this.getDisplay().getSystemColor(SWT.COLOR_CYAN)
                    v1.setBackgroundColor(color)
//                }
                val con = new GraphConnection(this, SWT.None, v1, v2)
                return v1

            case Call2Tree(annot,state, called, next) =>
                val v2 = addTree(called)
                val v3 = addTree(next)
                val v1 = new GraphNode(this, SWT.None, t)
                v1.setText(decorator(t))
                val conCall = new GraphConnection(this, SWT.None, v1, v2)
                conCall.setText("call")
                val conRet = new GraphConnection(this, SWT.None, v1, v3)
                conRet.setText("return")
                return v1

            case UseTree(annot,state, called, next) =>
                val v2 = addTree(called)
                val v3 = addTree(next)
                val v1 = new GraphNode(this, SWT.None, t)
                v1.setText(decorator(t))
//                if (!reentrant) {
                    val color = this.getDisplay().getSystemColor(SWT.COLOR_CYAN)
                    v1.setBackgroundColor(color)
//                }
                val conCall = new GraphConnection(this, SWT.None, v1, v2)
                conCall.setText("use")
                val conRet = new GraphConnection(this, SWT.None, v1, v3)
                conRet.setText("return")
                return v1

            case SpawnTree(annot,state, spawned, next) =>
                val v2 = addTree(spawned)
                val v3 = addTree(next)
                val v1 = new GraphNode(this, SWT.None, t)
                v1.setText(decorator(t))
                val color = this.getDisplay().getSystemColor(SWT.COLOR_GREEN)
                v1.setBackgroundColor(color)
                val conSpawn = new GraphConnection(this, SWT.None, v1, v2)
                conSpawn.setText("spawn")
                val conRet = new GraphConnection(this, SWT.None, v1, v3)
                conRet.setText("return")
                return v1

            case NilTree(annot,state) =>
                val v1 = new GraphNode(this, SWT.None, t)
                v1.setText(decorator(t))
                val color = new Color(this.getDisplay(), 255, 100, 100)

                v1.setBackgroundColor(color)
                return v1

            case RetTree(annot,state) =>
                val v1 = new GraphNode(this, SWT.None, t)
                v1.setText(decorator(t))
                return v1

        }
    }

}

object IWTGraph {
    def showTree(tree: WitnessTree, decorator: WitnessTree => String = _.toString) {
        val d = new Display();
        val shell = new Shell(d);
        shell.setText("Witness View");
        shell.setLayout(new FillLayout());
        shell.setSize(400, 800);
        val graph = new IWTGraph(tree, shell, decorator = decorator)
        shell.open();
        while (!shell.isDisposed()) {
            while (!d.readAndDispatch()) {
                d.sleep();
            }
        }
    }

    def showTree(tree: WitnessTree, cg: CallGraph) {
        val decorator = (t: WitnessTree) => {
            val ss = DRStateParser.parseState(t.state.toString).ss
            val nr = ss.cg
            val node = cg.getNode(nr)
            val name = node.getMethod().getName()
            var cn = t.getClass().getCanonicalName()
            cn = cn.dropRight(4)
            cn = cn split ('.') last;

            "%s in  %s  at (%d,%d)".format(cn, name, ss.bb, ss.instr)
        }
        showTree(tree, decorator)

    }

    def showTree(stree: String, cg: CallGraph) {
        val pr = AbstractFullWitnessParser.parseTree(stree)
        assert(pr.successful, "Parsing was not successfull!")
        val tree = pr.get
        showTree(tree, cg)
    }

    def showTree(stree: String) {
        val pr = AbstractFullWitnessParser.parseTree(stree)
        assert(pr.successful, "Parsing was not successfull!")
        val tree = pr.get
        showTree(tree)
    }

}