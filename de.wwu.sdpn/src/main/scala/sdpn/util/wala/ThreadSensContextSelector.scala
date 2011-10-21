package sdpn.util.wala

import com.ibm.wala.ipa.callgraph.propagation.ReceiverInstanceContext
import com.ibm.wala.ipa.callgraph.Context
import com.ibm.wala.classLoader.IMethod
import com.ibm.wala.classLoader.CallSiteReference
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ipa.callgraph.ContextSelector
import com.ibm.wala.util.intset.IntSet
import com.ibm.wala.util.intset.SparseIntSet


/**
 * A context selector selecting a ReciverInstanceContext for java.util.Thread.start invocations.
 * 
 * @author Benedikt Nordhoff
 */
class ThreadSensContextSelector extends ContextSelector {
   
    /*override def getCalleeTarget(caller: CGNode, site: CallSiteReference, callee: IMethod,  reciever: InstanceKey): Context = {
    if (isThreadStart(callee))
      new ReceiverInstanceContext(reciever)
    else
      null
  }*/
  
    override def getCalleeTarget(caller: CGNode, site: CallSiteReference, callee: IMethod,  actualParameters: Array[InstanceKey]): Context = {
    if (isThreadStart(callee) && actualParameters != null && actualParameters.length == 1)
      new ReceiverInstanceContext(actualParameters(0))
    else
      null
  }

  private def isThreadStart(method: IMethod) =
    "java.lang.Thread.start()V".equals(method.getSignature)
    
  override def getRelevantParameters(caller:CGNode, site:CallSiteReference): IntSet = {
    if (isThreadStart(caller.getMethod))
      return SparseIntSet.singleton(0)
    else 
      return new SparseIntSet()
  }
}
