package de.wwu.sdpn.gui.jgraphx

import com.ibm.wala.ipa.callgraph.CallGraph
import de.wwu.sdpn.core.ta.xsb.witness.iterable.WitnessTree
import de.wwu.sdpn.core.ta.xsb.witness.iterable.BaseTree
import de.wwu.sdpn.core.ta.xsb.witness.iterable.Call1Tree
import de.wwu.sdpn.core.ta.xsb.witness.iterable.AcqTree
import de.wwu.sdpn.core.ta.xsb.witness.iterable.Call2Tree
import de.wwu.sdpn.core.ta.xsb.witness.iterable.UseTree
import de.wwu.sdpn.core.ta.xsb.witness.iterable.SpawnTree
import de.wwu.sdpn.core.ta.xsb.witness.iterable.NilTree
import de.wwu.sdpn.core.ta.xsb.witness.iterable.CutTree
import de.wwu.sdpn.core.ta.xsb.witness.iterable.RetTree
import com.mxgraph.view.mxGraph
import com.mxgraph.layout.hierarchical.mxHierarchicalLayout
import com.mxgraph.swing.mxGraphComponent
import javax.swing.JFrame
import de.wwu.sdpn.core.ta.xsb.witness.iterable.AbstractFullWitnessParser
import de.wwu.sdpn.wala.analyses.datarace.DRStateParser
import com.mxgraph.util.mxEvent
import com.mxgraph.util.mxEventSource.mxIEventListener
import com.mxgraph.util.mxEventObject
import com.mxgraph.view.mxGraphSelectionModel
import com.mxgraph.model.mxCell

case class IWTGraphNode(tree: WitnessTree, label: String) {
    override def toString = label
}

class IWTGraph(tree: WitnessTree, decorator: WitnessTree => String = _.toString, selectionListener: WitnessTree => Unit = _ => ()) {
    var graph = new mxGraph()
    var gparent = graph.getDefaultParent
    graph setAutoSizeCells true

    graph.getModel.beginUpdate
    var layout = new mxHierarchicalLayout(graph)

    try {
        addTree(tree)
        layout.execute(gparent)

    } finally {
        graph.getModel().endUpdate();
    }

    def addTree(tree: WitnessTree): Any = {
        val node = IWTGraphNode(tree, decorator(tree))
        tree match {
            case BaseTree(annot, state, child) =>
                val v2 = addTree(child)
                var v1 = graph.insertVertex(gparent, null, node, 0, 0, 80, 30)
                graph.updateCellSize(v1)
                graph.insertEdge(gparent, null, null, v1, v2)
                return v1

            case CutTree(annot, state, child) =>
                val v2 = addTree(child)
                var v1 = graph.insertVertex(gparent, null, node, 0, 0, 80, 30)
                graph.updateCellSize(v1)
                graph.insertEdge(gparent, null, null, v1, v2)
                return v1

            case Call1Tree(annot, state, child) =>
                val v2 = addTree(child)
                var v1 = graph.insertVertex(gparent, null, node, 0, 0, 80, 30)
                graph.updateCellSize(v1)
                graph.insertEdge(gparent, null, null, v1, v2)
                return v1

            case AcqTree(annot, state, child) =>
                val v2 = addTree(child)
                var v1 = graph.insertVertex(gparent, null, node, 0, 0, 80, 30, "fillColor=cyan")
                graph.updateCellSize(v1)
                graph.insertEdge(gparent, null, null, v1, v2)
                return v1

            case Call2Tree(annot, state, called, next) =>
                val v2 = addTree(called)
                val v3 = addTree(next)
                var v1 = graph.insertVertex(gparent, null, node, 0, 0, 80, 30)
                graph.updateCellSize(v1)
                graph.insertEdge(gparent, null, "call", v1, v2)
                graph.insertEdge(gparent, null, "return", v1, v3)
                return v1

            case UseTree(annot, state, called, next) =>
                val v2 = addTree(called)
                val v3 = addTree(next)
                var v1 = graph.insertVertex(gparent, null, node, 0, 0, 80, 30, "fillColor=cyan")
                graph.updateCellSize(v1)
                graph.insertEdge(gparent, null, "use", v1, v2)
                graph.insertEdge(gparent, null, "return", v1, v3)
                return v1

            case SpawnTree(annot, state, spawned, next) =>
                val v2 = addTree(spawned)
                val v3 = addTree(next)
                var v1 = graph.insertVertex(gparent, null, node, 0, 0, 80, 30, "fillColor=green")
                graph.updateCellSize(v1)
                graph.insertEdge(gparent, null, "spawn", v1, v2)
                graph.insertEdge(gparent, null, "return", v1, v3)
                return v1

            case NilTree(annot, state) =>
                var v1 = graph.insertVertex(gparent, null, node, 0, 0, 80, 30, "fillColor=red")
                graph.updateCellSize(v1)
                return v1

            case RetTree(annot, state) =>
                var v1 = graph.insertVertex(gparent, null, node, 0, 0, 80, 30)
                graph.updateCellSize(v1)
                return v1
        }
    }

    graph.getSelectionModel().addListener(mxEvent.CHANGE, new mxIEventListener() {
        override def invoke(sender: Object, evt: mxEventObject) {
            sender match {
                case sm: mxGraphSelectionModel =>
                    sm getCell match {
                        case cell: mxCell =>
                            cell getValue match {
                                case IWTGraphNode(tree, _) => selectionListener(tree)
                                case _                     =>
                            }
                        case _ =>
                    }
                case _ =>
            }
        }
    })
    graph setCellsLocked true

    val graphComponent = new mxGraphComponent(graph)
    graphComponent setConnectable false

}

object IWTGraph {
    def showTree(tree: WitnessTree, decorator: WitnessTree => String = _.toString) {
        val frame = new JFrame("Witness View")
        frame.setSize(400, 800);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
        val graph = new IWTGraph(tree, decorator = decorator)
        frame.getContentPane().add(graph.graphComponent);
        frame.setVisible(true)
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