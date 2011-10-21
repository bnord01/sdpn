package sdpn.util

import com.ibm.wala.ssa.SSAAbstractInvokeInstruction
import com.ibm.wala.ssa.SSANewInstruction
import com.ibm.wala.ssa.SSAThrowInstruction
import com.ibm.wala.ssa.SSAInvokeInstruction
import com.ibm.wala.types.TypeReference
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ssa.SSAMonitorInstruction
import com.ibm.wala.ipa.callgraph.CallGraph
import scala.collection.JavaConversions._

class ExceptionTypeLocator (val cg: CallGraph,val pa: PointerAnalysis){
	lazy val exceptionTypes = locateETypes
	
	//TODO there may be some exceptions missing!
	// What about Subclasses?
	// What about classes out of scope?
	private def locateETypes = {
		var et = Set[TypeReference]()
		for (node <- cg if node.getIR != null){
//			if(node.getMethod != null && node.getMethod.getDeclaredExceptions != null)
//				et ++= node.getMethod.getDeclaredExceptions			
			for(instr <- node.getIR.iterateNormalInstructions if instr != null){
				et ++= instr.getExceptionTypes
				instr match {
					case st : SSAThrowInstruction =>
					val exval = st.getException
					val pk = pa.getHeapModel().getPointerKeyForLocal(node, exval)
					for (ik <- pa.getPointsToSet(pk))
						et += ik.getConcreteType.getReference
					case st : SSAAbstractInvokeInstruction =>					  
					  for (t <- cg.getPossibleTargets(node, st.getCallSite) 
					 		  if t.getMethod.getDeclaredExceptions != null)
					 	  et ++= t.getMethod.getDeclaredExceptions
					case _ =>
				}				
			}
		}
		et	
	}
	
}