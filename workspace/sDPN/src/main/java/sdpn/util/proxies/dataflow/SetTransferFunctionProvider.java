package sdpn.util.proxies.dataflow;

import sdpn.util.SetVariable;

import com.ibm.wala.dataflow.graph.ITransferFunctionProvider;
import com.ibm.wala.ipa.callgraph.CGNode;

public interface SetTransferFunctionProvider extends ITransferFunctionProvider<CGNode,SetVariable>{

}
