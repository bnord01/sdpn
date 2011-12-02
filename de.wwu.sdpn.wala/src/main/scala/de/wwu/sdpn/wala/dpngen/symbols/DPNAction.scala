package de.wwu.sdpn.wala.dpngen.symbols

import com.ibm.wala.ssa.SSAAbstractInvokeInstruction
import com.ibm.wala.ssa.SSAMonitorInstruction
import com.ibm.wala.ssa.SSAInvokeInstruction
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ssa.SSAInstruction

sealed trait DPNAction 

case object NoAction extends DPNAction
case object Return extends DPNAction

case object Spawn extends DPNAction

sealed case class MonitorEnter (instruction: SSAMonitorInstruction, instanceKey : InstanceKey) extends DPNAction 
sealed case class MonitorExit (instruction: SSAMonitorInstruction, instanceKey : InstanceKey) extends DPNAction

sealed case class SyncMethodEnter (instruction: SSAAbstractInvokeInstruction,monitor: InstanceKey) extends DPNAction
sealed case class SyncMethodExit (monitor: InstanceKey) extends DPNAction

sealed case class ExceptionCatch(priority : Int) extends DPNAction
sealed case class ExceptionAction(action: DPNAction) extends DPNAction

sealed case class SSAAction (instruction: SSAInstruction) extends DPNAction {
	override def toString() = {
		instruction match {
			case a: SSAInvokeInstruction =>
				"SSAAction(" + a.getDeclaredTarget().getSignature + ")"
			case _ => "SSAAction("	+ instruction + ")"
		}
	}
}
sealed case class ExceptionInMonitor(monitor:InstanceKey) extends DPNAction
sealed case class SkipAction(action: DPNAction) extends DPNAction