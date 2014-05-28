package de.wwu.sdpn.tests.gui

import javax.swing.JFrame
import com.mxgraph.swing.mxGraphComponent
import com.mxgraph.view.mxGraph
import com.mxgraph.layout.hierarchical.mxHierarchicalLayout
import com.mxgraph.util.mxEvent
import com.mxgraph.util.mxEventSource.mxIEventListener
import com.mxgraph.util.mxEventObject
import com.mxgraph.view.mxGraphSelectionModel
import com.mxgraph.model.mxCell

case class Blub(s: String)
object JGraphTest {

    def main(args: Array[String]) {
        println("Hello World!")
        var frame = new JGraphTest()
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(400, 320);
        frame.setVisible(true);

        println("Done")
    }
}

class JGraphTest extends JFrame("Hello World!") {

    private final val serialVersionUID = -2707712944901661771L;

    var graph = new mxGraph()
    var gparent = graph.getDefaultParent();

    graph.getModel().beginUpdate();
    try {
        var v1 = graph.insertVertex(gparent, null, Blub("Hello"), 0, 0, 80, 30, "fillColor=cyan");
        var v2 = graph.insertVertex(gparent, null, Blub("World!"), 0, 0, 80, 30);
        var v3 = graph.insertVertex(gparent, null, Blub("Beware"), 0, 0, 80, 30);
        var e1 = graph.insertEdge(gparent, null, "Edge", v1, v2);
        //        graph.setCellsMovable(false)
        //        graph.setCellsResizable(false)
        //        graph.setDropEnabled(false)
        //        graph.setEdgeLabelsMovable(false)
        //        graph.setCellsEditable(false)
        graph.setCellsLocked(true)
        //        graph.setConnectableEdges(false)

        graph.insertEdge(gparent, null, "Edge", v1, v3);

        var layout = new mxHierarchicalLayout(graph);
        layout.execute(graph.getDefaultParent());
    } finally {
        graph.getModel().endUpdate();
    }

    graph.getSelectionModel().addListener(mxEvent.CHANGE, new mxIEventListener() {
        override def invoke(sender: Object, evt: mxEventObject) {
            sender match {
                case sm: mxGraphSelectionModel =>
                    sm.getCell() match {
                        case cell: mxCell =>
                            cell.getValue() match {
                                case Blub(s) => println(s)
                                case _       => println("other " + cell.getValue())
                            }
                        case _ => println("No match")
                    }
                case _ =>
            }
            println(sender)
            println(evt)
        }
    })
    var graphComponent = new mxGraphComponent(graph);
    graphComponent.setConnectable(false)
    getContentPane().add(graphComponent);

}