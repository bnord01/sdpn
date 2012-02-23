package de.wwu.sdpn.wala.dpngen.symbols

import com.ibm.wala.ssa.SSAAbstractInvokeInstruction
import com.ibm.wala.ssa.SSAMonitorInstruction
import com.ibm.wala.ssa.SSAInvokeInstruction
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ssa.SSAInstruction

sealed trait DPNAction {
    def getSSAInstruction: Option[SSAInstruction]
}

case object NoAction extends DPNAction { val getSSAInstruction = None }

case object Return extends DPNAction { val getSSAInstruction = None }

case object Spawn extends DPNAction { val getSSAInstruction = None }

sealed case class MonitorEnter(instruction: SSAMonitorInstruction, instanceKey: InstanceKey) extends DPNAction {
    val getSSAInstruction = Some(instruction)
}
sealed case class MonitorExit(instruction: SSAMonitorInstruction,enter:StackSymbol) extends DPNAction{
    val getSSAInstruction = Some(instruction)
}

sealed case class SyncMethodEnter(instruction: SSAAbstractInvokeInstruction, monitor: InstanceKey) extends DPNAction{
    val getSSAInstruction = Some(instruction)
}
sealed case class SyncMethodExit(monitor: InstanceKey) extends DPNAction { val getSSAInstruction = None }

sealed case class ExceptionCatch(priority: Int) extends DPNAction { val getSSAInstruction = None }
sealed case class ExceptionAction(action: DPNAction) extends DPNAction{
    def getSSAInstruction = action.getSSAInstruction
}

sealed case class SSAAction(instruction: SSAInstruction) extends DPNAction {
    override def toString() = {
        instruction match {
            case a: SSAInvokeInstruction =>
                "SSAAction(" + a.getDeclaredTarget().getSignature + ")"
            case _ => "SSAAction(" + instruction + ")"
        }
    }
    val getSSAInstruction = Some(instruction)
}
sealed case class ExceptionInMonitor(instruction: SSAMonitorInstruction) extends DPNAction{
    val getSSAInstruction = Some(instruction)
}
sealed case class SkipAction(action: DPNAction) extends DPNAction{
    def getSSAInstruction = action.getSSAInstruction
}