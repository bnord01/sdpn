package de.wwu.sdpn.wala.util
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ssa.SSAFieldAccessInstruction
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import scala.collection.JavaConversions._
import com.ibm.wala.ssa.SSAPutInstruction
import com.ibm.wala.ssa.SSAGetInstruction

class DefUseUtil(cg: CallGraph, pa: PointerAnalysis) {

    lazy val hm = pa getHeapModel ()

    lazy val (writeMap, readMap): (Map[(InstanceKey, String), Set[(CGNode, Int)]], Map[(InstanceKey, String), Set[(CGNode, Int)]]) = {
        var wm: Map[(InstanceKey, String), Set[(CGNode, Int)]] = Map().withDefaultValue(Set())
        var rm: Map[(InstanceKey, String), Set[(CGNode, Int)]] = Map().withDefaultValue(Set())

        for (
            node <- cg if node.getIR() != null;
            (instr: SSAFieldAccessInstruction, idx) <- node.getIR().getInstructions().zipWithIndex;
            ik <- getIKS4FieldInstr(node, instr)
        ) {
            val name = instr getDeclaredField () getName () toString ()

            instr match {
                case put: SSAPutInstruction =>
                    wm += (ik, name) -> (wm((ik, name)) + ((node, idx)))
                case get: SSAGetInstruction =>
                    rm += (ik, name) -> (rm((ik, name)) + ((node, idx)))
            }
        }
        (wm, rm)
    }
    
    lazy val possibleDeps: Set[((CGNode, Int), (CGNode, Int))] = {
        for (
            key <- writeMap.keySet intersect readMap.keySet;
            put <- writeMap(key);
            get <- readMap(key)
        ) yield (put, get)
    }

    private def getIKS4FieldInstr(cgnode: CGNode, instr: SSAFieldAccessInstruction): Set[InstanceKey] = {
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