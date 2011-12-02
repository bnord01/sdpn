package de.wwu.sdpn.wala.util.proxies.dataflow;

import de.wwu.sdpn.wala.util.SetVariable;

import com.ibm.wala.dataflow.graph.ITransferFunctionProvider;
import com.ibm.wala.ipa.callgraph.CGNode;

public interface SetTransferFunctionProvider extends ITransferFunctionProvider<CGNode,SetVariable>{

}
