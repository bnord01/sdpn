package sdpn.dpn.explicit

import com.ibm.wala.ipa.callgraph.CGNode
case class StackSymbol(node: CGNode, basicBlock: Int, instrNr: Int) {
    override def toString = {
        "(" +
            (if (node != null && node.getMethod != null)
                "(" + node.getGraphNodeId + "," + node.getMethod().getName() + ")"
            else
                "null") +
            "," + basicBlock + "," + instrNr + ")"
    }

}