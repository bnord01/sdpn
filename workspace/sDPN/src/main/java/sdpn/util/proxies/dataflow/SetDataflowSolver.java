package sdpn.util.proxies.dataflow;

import sdpn.util.SetVariable;

import com.ibm.wala.dataflow.graph.DataflowSolver;
import com.ibm.wala.ipa.callgraph.CGNode;

public class SetDataflowSolver extends DataflowSolver<CGNode, SetVariable> {

	public SetDataflowSolver(SetKilldallFramework problem) {
		super(problem);
	}
	
	@Override
	protected SetVariable makeEdgeVariable(CGNode src, CGNode dst) {
		// TODO Auto-generated method stub
		throw new UnsupportedOperationException("No");
		
	}

	@Override
	protected SetVariable makeNodeVariable(CGNode n, boolean IN) {
		// TODO Auto-generated method stub
		return new SetVariable();
	}

	@Override
	protected SetVariable[] makeStmtRHS(int size) {
		SetVariable [] v = new SetVariable [size];
		for(int i = 0; i < size;i++)
			v[i] = new SetVariable();
		return v;
	}

}
