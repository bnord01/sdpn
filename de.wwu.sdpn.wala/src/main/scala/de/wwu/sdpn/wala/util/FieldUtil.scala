package de.wwu.sdpn.wala.util
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ssa.SSAFieldAccessInstruction
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import scala.collection.JavaConversions._
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.util.strings.Atom
import com.codahale.logula.Logging
import com.ibm.wala.ssa.SSAPutInstruction
import com.ibm.wala.classLoader.IField

object FieldUtil extends Logging {

    def getIKS4FieldInstr(pa: PointerAnalysis, cgnode: CGNode, instr: SSAFieldAccessInstruction): Set[InstanceKey] = {
        log.trace("Obtaining possible instance keys for field instruction %s on node %s", instr, cgnode)
        val hm = pa.getHeapModel()
        val field = instr getDeclaredField ()
        if (instr isStatic ()) {
            val declClass = instr getDeclaredField () getDeclaringClass
            val ik = hm getInstanceKeyForClassObject (declClass)
            log.trace("Instruction is static, declaring class key is %s", ik)
            Set(ik)
        } else {
            val refNr = instr getRef ()
            val pk = hm getPointerKeyForLocal (cgnode, refNr)
            val ps = pa.getPointsToSet(pk).toSet
            log.trace("Instruction is non static, ref is %d, pointer key is %s, points to set is %s", refNr, pk, ps)
            ps
        }
    }

    def getFieldWrites(cg: CallGraph, pa: PointerAnalysis, fieldObj: InstanceKey, field: IField): Set[(CGNode, SSAPutInstruction)] = {
        val cha = cg.getClassHierarchy()
        log.trace("Locating writes to field %s on object %s", field, fieldObj)
        val s = for (
            node <- cg;
            x = node.getIR();
            if x != null;
            i0 <- x.getInstructions();
            if i0.isInstanceOf[SSAPutInstruction];
            instr = i0.asInstanceOf[SSAPutInstruction];
            if field == cha.resolveField(instr.getDeclaredField())  &&
                getIKS4FieldInstr(pa, node, instr)(fieldObj)
        ) yield (node, instr)
        val r = s.toSet
        if (log.isTraceEnabled)
            log.trace("Located %d writes for field %s on object %s:  %s", r.size, fieldObj, field, r)
        return r
    }
        
    def getFieldWrites(cg: CallGraph, pa: PointerAnalysis, field: IField): Set[(CGNode, SSAPutInstruction)] = {
        log.trace("Locating all writes to field %s", field)
        val cha = cg.getClassHierarchy()
        val s = for (
            node <- cg;
            x = node.getIR();
            if x != null;
            i0 <- x.getInstructions();
            if i0.isInstanceOf[SSAPutInstruction];
            instr = i0.asInstanceOf[SSAPutInstruction];
            if cha.resolveField(instr.getDeclaredField()) == field
        ) yield (node, instr)
        val r = s.toSet
        if (log.isTraceEnabled)
            log.trace("Located %d writes for field %s:  %s", r.size, field, r)
        return r
    }        
}