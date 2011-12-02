package de.wwu.sdpn.util.proxies.dataflow;

import de.wwu.sdpn.util.SetVariable;

import com.ibm.wala.dataflow.graph.IKilldallFramework;
import com.ibm.wala.ipa.callgraph.CGNode;

public interface SetKilldallFramework extends IKilldallFramework<CGNode,SetVariable>{

}
