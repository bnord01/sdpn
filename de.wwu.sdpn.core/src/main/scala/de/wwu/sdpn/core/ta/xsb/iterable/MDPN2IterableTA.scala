package de.wwu.sdpn.core.ta.xsb.iterable

import scala.collection.SortedSet
import scala.collection.JavaConversions._
import scala.collection.Map
import de.wwu.sdpn.core.dpn.monitor.MonitorDPN
import de.wwu.sdpn.core.ta.xsb.{ HasTermRepresentation => HTR }
import de.wwu.sdpn.core.dpn.monitor.BaseRule
import de.wwu.sdpn.core.dpn.monitor.PushRule
import de.wwu.sdpn.core.dpn.monitor.PopRule
import de.wwu.sdpn.core.dpn.monitor.SpawnRule
import de.wwu.sdpn.core.dpn.monitor.DPNRule

/**
 * A tree automaton that models the control flow of a dpn which
 * is given by the DPNFactory in which this trait is mixed in and
 * allows arbitrary cut nodes.
 * It annotates base-rules using the DPNAnnotator id is mixed in.
 * It matches monitors using the given MonitorMatcher
 *
 * @author b_nord01
 */
class MDPN2IterableTA[GlobalState <% HTR, StackSymbol <% HTR, DPNAction, Lock](
        dpn: MonitorDPN[GlobalState, StackSymbol, DPNAction, Lock], 
        val name: String = "dpn", 
        annotater: DPNRule[GlobalState, StackSymbol, DPNAction] => String = null) extends IterableTreeAutomata {

    val lockMap: Map[Lock, Int] = {
        var m = Map[Lock, Int]()
        var i = 0
        for (l <- dpn.locks) {
            m += (l -> i)
            i += 1
        }
        m
    }

    require(lockMap.size < 9, "Can handle at most 8 locks!")

    /**
     * The state consists of one global state plus the stack symbol consisting of the cgnode,
     * the basic block and the instruction number which represents the state at which this execution visited the branch.
     * Another global state represents the state at which the execution ended.
     * Plus a bit indicating if a branch has terminated.
     */
    def isForwardRule = alphabet.keySet + "final"
    def genScript = {
        val buffer = new StringBuilder()
        def out(stuff: Any*) {
            for (x <- stuff)
                buffer.append(x)
        }

        out(name,"_cut(cpt(_,S),c(G,S,G2,T),c(G,S,G2,T)).\n")
        out(name,"_nil(S,c(G,S,G,0)).\n")

        out("""
%% Defining all predicates just to be sure.
name_acq(_,_,_) :- fail.
name_use(_,_,_,_) :- fail.
name_ret(_,_) :- fail.
name_base(_,_,_) :- fail.
name_spawn(_,_,_,_) :- fail.
name_call1(_,_,_) :- fail.
name_call2(_,_,_,_) :- fail.
""".replace("name", name))

        for (rule <- dpn.getTransitions) {
            val annot = annotateRule(rule)
            rule match {
                case BaseRule(fromState, fromStack, action, toState, toStack) =>
                    base(annot, fromState, fromStack, toState, toStack)

                case PushRule(fromState, fromStack, action, toState, callStack, returnStack) =>
                    dpn.usedLock(rule) match {
                        case Some(ikey) =>
                            use(annot,ikey, fromState, fromStack, toState, callStack, returnStack)
                            acq(annot,ikey, fromState, fromStack, toState, callStack)
                        case None =>
                            call2(annot,fromState, fromStack, toState, callStack, returnStack)
                            call1(annot,fromState, fromStack, toState, callStack)
                    }
                case PopRule(fromState, fromStack, action, toState) =>
                    ret(annot,fromState, fromStack, toState)
                case SpawnRule(fromState, fromStack, action, toState, toStack, spawnState, spawnStack) =>
                    spawn(annot,fromState, fromStack, toState, toStack, spawnState, spawnStack)
            }
        }

        def ret(annot:String,fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState) {
            out(name,"_ret(",annot,",c(",fromState.toTerm,",",stack(fromStack),",",toState.toTerm,",1)).\n")
        }

        def call2(annot:String,fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, callStack: StackSymbol, returnStack: StackSymbol) {
            out(name,"_call2(",annot,",c(",toState.toTerm,",",stack(callStack),", Gc,1) , ") //the call has terminated at toState,toStack and reached state Gc
            out("c(Gc, ",stack(returnStack),",Gf,T), ") //and then we reached Gf from there
            out("c(",fromState.toTerm,",",stack(fromStack),", Gf,T)).\n") //so we can annotate it with fromState,fromStack Gf and T
        }

        def call1(annot:String,fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, callStack: StackSymbol) {
            out(name,"_call1(",annot,",")
            out("c(" + toState.toTerm + "," + stack(callStack) + ", Gc,0) , ") //the call has not terminated at toState,toStack and reached state Gc			
            out("c(" + fromState.toTerm + "," + stack(fromStack) + ", Gc,0)).\n") //so we can annotate it with fromState,fromStack Gc and 0
        }

        def spawn(annot:String,fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, toStack: StackSymbol, spawnState: GlobalState, spawnStack: StackSymbol) {
            out(name,"_spawn(",annot,",")
            out("c(" + spawnState.toTerm + "," + stack(spawnStack) + ",_,_ ), ") //some thread has been spawned
            out("c(" + toState.toTerm + ", " + stack(toStack) + ",Gf,T), ") //and then we reached Gf , T
            out("c(" + fromState.toTerm + "," + stack(fromStack) + ", Gf,T)).\n") //so we can annotate it with fromState,fromStack Gf and T
        }

        def acq(annot:String,lock: Lock, fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, toStack: StackSymbol) {
            out(name,"_acq(ra(",annot,",")
            out("la(",lockMap(lock),",_)),")
            out("c(" + toState.toTerm + "," + stack(toStack) + ",StateFinal,0), " +
                "c( " + fromState.toTerm + "," + stack(fromStack) + ",StateFinal,0)).\n")
        }

        def use(annot:String,lock: Lock, fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, callStack: StackSymbol, returnStack: StackSymbol) {
            out(name,"_use(ra(",annot,",la(" + lockMap(lock) + ",_)),")
            out("c(" + toState.toTerm + "," + stack(callStack) + " ,Gc,1) , ") //the we have terminated at toState,toStack and reached Gc
            out("c(Gc, " + stack(returnStack) + ",Gf,T), ") //and then we reached Gf from there
            out("c(" + fromState.toTerm + "," + stack(fromStack) + ", Gf,T)).\n") //so we can annotate it with fromState,fromStack Gf and T			
        }

        /**
         * name_base(G1,P1,Gf,T) -> (G,P,Gf,T)  if dpn_base(G,P,_,G1,P1)
         */
        def base(annot: String, fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, toStack: StackSymbol) {
            out(name + "_base(")
            out(annot + ",")
            out("c(" + toState.toTerm + "," + stack(toStack) + ",StateFinal,T), " +
                "c( " + fromState.toTerm + "," + stack(fromStack) + ",StateFinal,T)).\n")
        }

        out("%All states beginning at the initial state + stack are final\n")
        out(name + "_final(")
        out("c(" + dpn.initialState.toTerm + "," + stack(dpn.initialStack) + ",_,_)).\n")

        buffer.toString
    }

    def annotateRule(rule: DPNRule[GlobalState, StackSymbol, DPNAction]): String = {
        if (annotater == null)
            "0"
        else
            annotater(rule)
    }

    def stack(s: StackSymbol) = s.toTerm
}
