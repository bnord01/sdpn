package de.wwu.sdpn.pfg.wala
import de.wwu.sdpn.pfg.{BaseEdge => BE, SpawnEdge => SE, CallEdge => CE,Edge=>UE}
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ssa.SSAInstruction
import de.wwu.sdpn.pfg.ParFlowGraph

trait WalaPFG extends ParFlowGraph[CGNode, Node, Action, State, Edge]

sealed trait Edge extends UE[CGNode,Node,Action,State] 

case class BaseEdge(src:Node, ba: BaseAction, snk:Node) extends BE[CGNode,Node,Action,State] with Edge

case class CallEdge(src:Node, proc: CGNode, returns:Map[State,Node]) extends CE[CGNode,Node,Action,State] with Edge{
    def ba = CallAction(proc)
}

case class SpawnEdge(src:Node, proc: CGNode, snk:Node) extends SE[CGNode,Node,Action,State] with Edge {
    def ba = SpawnAction(proc)
}

case class Node(state: State, point: CFGPoint){
    def proc = point.proc
    override def toString = "(" + state + ", " + proc.getGraphNodeId() + ":" + proc.getMethod().getDeclaringClass().getName() + "." + proc.getMethod().getName() + ", " + point.bb + ", " + point.instr + ")"
}


case class CFGPoint(proc: CGNode,bb: Int, instr: Int)

sealed trait State
case object N extends State
case object E extends State

sealed trait Action 
case class CallAction(proc: CGNode) extends Action
case class SpawnAction(proc: CGNode) extends Action
sealed trait BaseAction extends Action
case class SSAAction(instr: SSAInstruction) extends BaseAction
case object Skip extends BaseAction with Action 

