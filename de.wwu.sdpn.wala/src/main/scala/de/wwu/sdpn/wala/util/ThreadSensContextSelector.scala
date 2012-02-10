package de.wwu.sdpn.wala.util

import com.ibm.wala.ipa.callgraph.propagation.ReceiverInstanceContext
import com.ibm.wala.ipa.callgraph.Context
import com.ibm.wala.classLoader.IMethod
import com.ibm.wala.classLoader.CallSiteReference
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ipa.callgraph.ContextSelector
import com.ibm.wala.util.intset.IntSet
import com.ibm.wala.util.intset.SparseIntSet
import com.ibm.wala.types.TypeReference


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
    if (isThreadMethod(callee) && actualParameters != null && actualParameters.length > 0 && actualParameters(0) != null)
      new ReceiverInstanceContext(actualParameters(0))
    else
      null
  }

   private def isThreadMethod( method: IMethod) : Boolean =  {
    val cha = method.getClassHierarchy();
    return !method.isStatic() && cha.isAssignableFrom(cha.lookupClass(TypeReference.JavaLangThread), method.getDeclaringClass());
  }
    
  override def getRelevantParameters(caller:CGNode, site:CallSiteReference): IntSet = {
      if (site.getDeclaredTarget().getNumberOfParameters() > 0)
      return SparseIntSet.singleton(0);
    else 
      return new SparseIntSet();
  
  }
}
