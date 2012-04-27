package de.wwu.sdpn.core.ta.xsb.iterable

import scala.collection.SortedSet
import scala.collection.JavaConversions._
import scala.collection.Map
import de.wwu.sdpn.core.dpn.monitor.MonitorDPN
import de.wwu.sdpn.core.ta.xsb.{HasTermRepresentation=>HTR}
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
class MDPN2IterableTA[GlobalState<%HTR, StackSymbol<%HTR, DPNAction, Lock](dpn: MonitorDPN[GlobalState, StackSymbol, DPNAction, Lock], val name: String = "cflow", annotater: DPNAnnotater[GlobalState, StackSymbol, DPNAction] = null) extends IterableTreeAutomata {

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
    import buffer.{ append => out }
    out(name + "_cut(cpt(_,S),c(G,S,G2,T),c(G,S,G2,T)).\n")
    out(name + "_nil(S,c(G,S,G,0)).\n")

    if (lockMap.isEmpty) { //TODO this assumes that at least one safe lock is used somewhere otherwise 
      out("""
%% No safe locks used in program  
name_acq(_,c(_,_,_,_),c(_,_,_,_)) :- fail.
name_use(_,c(_,_,_,_),c(_,_,_,_),c(_,_,_,_)) :- fail.
""".replace("name", name))
    }

    for (rule <- dpn.getTransitions) {
      rule match {
        case BaseRule(fromState, fromStack, action, toState, toStack) =>
          base(annotateRule(rule), fromState, fromStack, toState, toStack)

        case PushRule(fromState, fromStack, action, toState, callStack, returnStack) =>
          dpn.usedLock(rule) match {
            case Some(ikey) =>
              use(ikey, fromState, fromStack, toState, callStack, returnStack)
              acq(ikey, fromState, fromStack, toState, callStack)
            case None =>
              call2(fromState, fromStack, toState, callStack, returnStack)
              call1(fromState, fromStack, toState, callStack)
          }
        case PopRule(fromState, fromStack, action, toState) =>
          ret(fromState, fromStack, toState)
        case SpawnRule(fromState, fromStack, action, toState, toStack, spawnState, spawnStack) =>
          spawn(fromState, fromStack, toState, toStack, spawnState, spawnStack)
      }
    }

    def ret(fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState) {
      out(name)
      out("_ret(c(")
      out(fromState.toTerm)
      out(",")
      out(stack(fromStack))
      out(",")
      out(toState.toTerm)
      out(",1)).\n")
    }

    def call2(fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, callStack: StackSymbol, returnStack: StackSymbol) {
      out(name + "_call2(")
      out("c(" + toState.toTerm + "," + stack(callStack) + ", Gc,1) , ") //the call has terminated at toState,toStack and reached state Gc
      out("c(Gc, " + stack(returnStack) + ",Gf,T), ") //and then we reached Gf from there
      out("c(" + fromState.toTerm + "," + stack(fromStack) + ", Gf,T)).\n") //so we can annotate it with fromState,fromStack Gf and T
    }

    def call1(fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, callStack: StackSymbol) {
      out(name + "_call1(")
      out("c(" + toState.toTerm + "," + stack(callStack) + ", Gc,0) , ") //the call has not terminated at toState,toStack and reached state Gc			
      out("c(" + fromState.toTerm + "," + stack(fromStack) + ", Gc,0)).\n") //so we can annotate it with fromState,fromStack Gc and 0
    }

    def spawn(fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, toStack: StackSymbol, spawnState: GlobalState, spawnStack: StackSymbol) {
      out(name + "_spawn(")
      out("c(" + spawnState.toTerm + "," + stack(spawnStack) + ",_,_ ), ") //some thread has been spawned
      out("c(" + toState.toTerm + ", " + stack(toStack) + ",Gf,T), ") //and then we reached Gf , T
      out("c(" + fromState.toTerm + "," + stack(fromStack) + ", Gf,T)).\n") //so we can annotate it with fromState,fromStack Gf and T
    }

    def acq(lock: Lock, fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, toStack: StackSymbol) {
      out(name + "_acq(")
      out("la(" + lockMap(lock) + ",_),")
      out("c(" + toState.toTerm + "," + stack(toStack) + ",StateFinal,0), " +
        "c( " + fromState.toTerm + "," + stack(fromStack) + ",StateFinal,0)).\n")
    }

    def use(lock: Lock, fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, callStack: StackSymbol, returnStack: StackSymbol) {
      out(name + "_use(la(" + lockMap(lock) + ",_),")
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
      annotater.annotateRule(rule).toString
  }

  def stack(s: StackSymbol) = s.toTerm
}
