package de.wwu.sdpn.util.proxies.dataflow;

import de.wwu.sdpn.util.SetVariable;

import com.ibm.wala.dataflow.graph.AbstractMeetOperator;
import com.ibm.wala.fixpoint.IVariable;

public abstract class SetMeetOperatorProxy extends AbstractMeetOperator<SetVariable>{
	
	public abstract byte evaluate( SetVariable lhs,SetVariable[] rhs);
	
	public byte evaluate(SetVariable lhs, IVariable[] rhs) {
		SetVariable [] nrhs = new SetVariable[rhs.length];
		for(int i = 0; i < rhs.length;i++) {
			if(rhs[i] instanceof SetVariable) {
				nrhs[i] = (SetVariable) rhs[i];
			} else 
				throw new IllegalArgumentException("Excpected SetVariable array but got " + rhs[i].getClass().getName());
		}
		return evaluate(lhs,nrhs);
	}

}
