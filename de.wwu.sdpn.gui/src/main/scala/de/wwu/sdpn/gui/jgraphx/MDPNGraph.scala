package de.wwu.sdpn.gui.jgraphx

import com.mxgraph.layout.mxCircleLayout
import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.view.mxGraph

import de.wwu.sdpn.core.dpn.monitor.BaseRule
import de.wwu.sdpn.core.dpn.monitor.DPNRule
import de.wwu.sdpn.core.dpn.monitor.MonitorDPN
import de.wwu.sdpn.core.dpn.monitor.PopRule
import de.wwu.sdpn.core.dpn.monitor.PushRule
import de.wwu.sdpn.core.dpn.monitor.SpawnRule
import javax.swing.JFrame

class MDPNGraph[G, S, A, L](
        mdpn: MonitorDPN[G, S, A, L],
        decorator: DPNDecorator[G, S, A, L] = ToStringDecorator.get) {
    import decorator._
    val nodeMap = scala.collection.mutable.Map[S, Object]()
    var graph = new mxGraph()
    var gparent = graph.getDefaultParent
    graph setAutoSizeCells true

    graph.getModel.beginUpdate
    var layout = new mxCircleLayout(graph)

    try {
        for (s <- mdpn.stackSymbols)
            nodeMap += s -> graph.insertVertex(gparent, null, decorateStack(s), 0, 0, 80, 30)

        for (rule <- mdpn.transitions) {
            rule match {
                case PushRule(_, s, a, _, sc, sr) =>
                    graph.insertEdge(gparent, null, decorateRule(rule), nodeMap(s), nodeMap(sr))
                case BaseRule(_, s, a, _, sn) =>
                    graph.insertEdge(gparent, null, decorateRule(rule), nodeMap(s), nodeMap(sn))
                case PopRule(_, s, a, _) =>
                    graph.insertEdge(gparent, null, decorateRule(rule), nodeMap(s), nodeMap(s))
                case SpawnRule(_, s, a, _, ss, _, sn) =>
                    graph.insertEdge(gparent, null, "Spawn: " + decorateRule(rule), nodeMap(s), nodeMap(ss))
                    graph.insertEdge(gparent, null, "Next: " + decorateRule(rule), nodeMap(s), nodeMap(sn))

            }
        }
        layout.execute(gparent)

    } finally {
        graph.getModel().endUpdate();
    }
    
    val graphComponent = new mxGraphComponent(graph)
}


trait DPNDecorator[G, S, A, L] {
    def decorateGlobal(g: G): String
    def decorateStack(s: S): String
    def decorateRule(a: DPNRule[G, S, A]): String
    def decorateState(g: G, s: S): String
    def decorateLock(l: L): String
}

object ToStringDecorator {
    def get[G, S, A, L]: DPNDecorator[G, S, A, L] = new DPNDecorator[G, S, A, L] {
        def decorateGlobal(g: G): String = g.toString
        def decorateStack(s: S): String = s.toString
        def decorateRule(a: DPNRule[G, S, A]): String = a.toString
        def decorateState(g: G, s: S): String = "(%s,%s)".format(g, s)
        def decorateLock(l: L): String = l.toString
    }
}

object MDPNGraph {
    def showMDPN[G, S, A, L](mdpn: MonitorDPN[G, S, A, L], decorator: DPNDecorator[G, S, A, L] = ToStringDecorator.get) {
        val frame = new JFrame("MDPN View")
        frame.setSize(1920, 1000);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
        val graph = new MDPNGraph(mdpn, decorator = decorator)
        frame.getContentPane().add(graph.graphComponent);
        frame.setVisible(true)
    }
}
