package de.wwu.sdpn.wala.util
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ssa.SSAFieldAccessInstruction
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import scala.collection.JavaConversions._

object FieldUtil {

    def getIKS4FieldInstr(pa: PointerAnalysis, cgnode: CGNode, instr: SSAFieldAccessInstruction): Set[InstanceKey] = {
        val hm = pa.getHeapModel()
        val field = instr getDeclaredField ()
        if (instr isStatic ()) {
            val declClass = instr getDeclaredField () getDeclaringClass
            val ik = hm getInstanceKeyForClassObject (declClass)
            Set(ik)
        } else {
            val refNr = instr getRef ()
            val pk = hm getPointerKeyForLocal (cgnode, refNr)
            Set() ++ pa.getPointsToSet(pk)
        }
    }

}