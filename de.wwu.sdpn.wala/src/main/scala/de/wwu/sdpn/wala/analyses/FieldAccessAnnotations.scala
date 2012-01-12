package de.wwu.sdpn.wala.analyses

import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ssa.SSAFieldAccessInstruction
import com.ibm.wala.ssa.SSAGetInstruction
import com.ibm.wala.ssa.SSAInstruction
import com.ibm.wala.ssa.SSAPutInstruction
import com.ibm.wala.types.FieldReference

import de.wwu.sdpn.core.dpn.monitor.BaseRule
import de.wwu.sdpn.core.dpn.monitor.DPNRule
import de.wwu.sdpn.core.ta.xsb.cuts.DPNAnnotater
import de.wwu.sdpn.wala.dpngen.symbols.DPNAction
import de.wwu.sdpn.wala.dpngen.symbols.ExceptionAction
import de.wwu.sdpn.wala.dpngen.symbols.GlobalState
import de.wwu.sdpn.wala.dpngen.symbols.SSAAction
import de.wwu.sdpn.wala.dpngen.symbols.StackSymbol

/**
 * A DPNAnnotater which annotates BaseRules with "read", "write" or "none" 
 * when they read, write or don't do neither to a field specified by fieldOfInterest.
 *    
 * @author Benedikt Nordhoff
 */
trait FieldAccessAnnotations extends DPNAnnotater[GlobalState, StackSymbol, DPNAction] {
    def pa: PointerAnalysis

    def fieldOfInterest: (InstanceKey, FieldReference)
    override type RuleAnnotation = String

    //TODO check this!
    override def annotateRule(rule: DPNRule[GlobalState, StackSymbol, DPNAction]): String = {
        rule match {
            case BaseRule(_, StackSymbol(node, _, _), SSAAction(action), _, _) =>
                genRWAnnotation(node,action,false)
            case BaseRule(_, StackSymbol(node, _, _), ExceptionAction(SSAAction(action)), _, _) =>
                genRWAnnotation(node,action,true)                
            case BaseRule(_,_,_,_,_) => return "none"
            case _ =>
                throw new IllegalArgumentException(rule.toString)
        }
    }
    
    def genRWAnnotation(node:CGNode,action:SSAInstruction,isEx:Boolean) : String = {
        action match {
                    case acc: SSAFieldAccessInstruction =>
                        if (acc.getDeclaredField != fieldOfInterest._2)
                            return "none"
                        if (acc.isStatic)
                            acc match {
                                case get: SSAGetInstruction =>
                                    return if(isEx) "none" else "read"
                                case put: SSAPutInstruction =>
                                    return "write"
                            }
                        val pk = pa.getHeapModel.getPointerKeyForLocal(node, acc.getRef)
                        val iks = pa.getPointsToSet(pk)
                        if (iks.size == 1 && iks.contains(fieldOfInterest._1))
                            acc match {
                                case get: SSAGetInstruction =>
                                    return if(isEx) "none" else "read"
                                case put: SSAPutInstruction =>
                                    return "write"
                            }
                        return "none"

                    case _ => return "none"
                }
    }
}