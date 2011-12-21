package de.wwu.sdpn.wala.dpngen.symbols

import com.ibm.wala.ipa.callgraph.CGNode
import de.wwu.sdpn.core.ta.xsb.HasTermRepresentation
case class StackSymbol(node: CGNode, basicBlock: Int, instrNr: Int) extends HasTermRepresentation {
    override def toString = {
        "(" +
            (if (node != null && node.getMethod != null)
                "(" + node.getGraphNodeId + "," + node.getMethod().getName() + ")"
            else
                "null") +
            "," + basicBlock + "," + instrNr + ")"
    }
    def toTerm = "s(" + node.getGraphNodeId() + "," + basicBlock + "," + instrNr + ")"

}
object StackSymbol {
    implicit def toCGNode(ss:StackSymbol):CGNode =  ss.node
}