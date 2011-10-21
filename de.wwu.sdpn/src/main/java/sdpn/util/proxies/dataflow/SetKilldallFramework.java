package sdpn.util.proxies.dataflow;

import sdpn.util.SetVariable;

import com.ibm.wala.dataflow.graph.IKilldallFramework;
import com.ibm.wala.ipa.callgraph.CGNode;

public interface SetKilldallFramework extends IKilldallFramework<CGNode,SetVariable>{

}
