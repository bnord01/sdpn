package de.wwu.sdpn.core.ta.xsb.reachability

import scala.collection.immutable.Map
import de.wwu.sdpn.core.dpn.monitor.MonitorDPN
import de.wwu.sdpn.core.ta.xsb.{HasTermRepresentation => HTR}
import de.wwu.sdpn.core.dpn.monitor.BaseRule
import de.wwu.sdpn.core.dpn.monitor.PushRule
import de.wwu.sdpn.core.dpn.monitor.PopRule
import de.wwu.sdpn.core.dpn.monitor.SpawnRule

/**
 * Tree automaton representing the control flow of the given MonitorDPN.
 * It uses the format  t(G,s(X,Y,Z)) to annotated a nil-node with
 * the control state G and stack symbol (X,Y,Z).
 *
 * @author b_nord01
 *
 */
class MDPN2TA[GS <% HTR, StackSymbol <% HTR, DPNAction, Lock](dpn: MonitorDPN[GS, StackSymbol, DPNAction, Lock], val name: String = "cflow") extends LockTreeAutomataAlphabet {

  val lockMap: Map[Lock, Int] = {
    var m = Map[Lock, Int]()
    var i = 0
    for (l <- dpn.locks) {
      m += (l -> i)
      i += 1
    }
    m
  }

  /**
   * The state consists of one global state plus the stack symbol consisting of the cgnode,
   * the basic block and the instruction number which represents the state at which this execution visited the branch.
   * Another global state represents the state at which the execution ended.
   * Plus a bit indicating if a branch has terminated.
   */
  val stateSize = 1
  def isForwardRule = alphabet.keySet + "final"
  def genScript = {
    val buffer = new StringBuilder()
    import buffer.{ append => out }
    out(name + "_nil(t(G,S),c(G,S,G,0)).\n")

    // Define all predicates just in case the DPN doesn't contains some of them. 
    out("""
%% No safe locks used in program  
name_acq(_,c(_,_,_,_),c(_,_,_,_)) :- fail.
name_use(_,c(_,_,_,_),c(_,_,_,_),c(_,_,_,_)) :- fail.
name_ret(_) :- fail.
name_base(_,_) :- fail.
name_spawn(_,_,_) :- fail.
name_call1(_,_) :- fail.
name_call2(_,_,_) :- fail.
""".replace("name", name))
    

    for (rule <- dpn.getTransitions) {
      rule match {
        case BaseRule(fromState, fromStack, action, toState, toStack) =>
          base(fromState, fromStack, toState, toStack)
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

    def ret(fromState: GS, fromStack: StackSymbol, toState: GS) {
      out(name)
      out("_ret(c(")
      out(fromState.toTerm)
      out(",")
      out(fromStack.toTerm)
      out(",")
      out(toState.toTerm)
      out(",1)).\n")
    }

    def call2(fromState: GS, fromStack: StackSymbol, toState: GS, callStack: StackSymbol, returnStack: StackSymbol) {
      out(name + "_call2(")
      out("c(" + toState.toTerm + "," + callStack.toTerm + ", Gc,1) , ") //the call has terminated at toState,toStack and reached state Gc
      out("c(Gc, " + returnStack.toTerm + ",Gf,T), ") //and then we reached Gf from there
      out("c(" + fromState.toTerm + "," + fromStack.toTerm + ", Gf,T)).\n") //so we can annotate it with fromState,fromStack Gf and T
    }

    def call1(fromState: GS, fromStack: StackSymbol, toState: GS, callStack: StackSymbol) {
      out(name + "_call1(")
      out("c(" + toState.toTerm + "," + callStack.toTerm + ", Gc,0) , ") //the call has not terminated at toState,toStack and reached state Gc			
      out("c(" + fromState.toTerm + "," + fromStack.toTerm + ", Gc,0)).\n") //so we can annotate it with fromState,fromStack Gc and 0
    }

    def spawn(fromState: GS, fromStack: StackSymbol, toState: GS, toStack: StackSymbol, spawnState: GS, spawnStack: StackSymbol) {
      out(name + "_spawn(")
      out("c(" + spawnState.toTerm + "," + spawnStack.toTerm + ",_,_ ), ") //some thread has been spawned
      out("c(" + toState.toTerm + ", " + toStack.toTerm + ",Gf,T), ") //and then we reached Gf , T
      out("c(" + fromState.toTerm + "," + fromStack.toTerm + ", Gf,T)).\n") //so we can annotate it with fromState,fromStack Gf and T
    }

    def acq(lock: Lock, fromState: GS, fromStack: StackSymbol, toState: GS, toStack: StackSymbol) {
      out(name + "_acq(")
      out("la(" + lockMap(lock) + ",_),")
      out("c(" + toState.toTerm + "," + toStack.toTerm + ",StateFinal,0), " +
        "c( " + fromState.toTerm + "," + fromStack.toTerm + ",StateFinal,0)).\n")
    }

    def use(lock: Lock, fromState: GS, fromStack: StackSymbol, toState: GS, callStack: StackSymbol, returnStack: StackSymbol) {
      out(name + "_use(la(" + lockMap(lock) + ",_),")
      out("c(" + toState.toTerm + "," + callStack.toTerm + " ,Gc,1) , ") //the we have terminated at toState,toStack and reached Gc
      out("c(Gc, " + returnStack.toTerm + ",Gf,T), ") //and then we reached Gf from there
      out("c(" + fromState.toTerm + "," + fromStack.toTerm + ", Gf,T)).\n") //so we can annotate it with fromState,fromStack Gf and T			
    }

    /**
     * name_base(G1,P1,Gf,T) -> (G,P,Gf,T)  if dpn_base(G,P,_,G1,P1)
     */
    def base(fromState: GS, fromStack: StackSymbol, toState: GS, toStack: StackSymbol) {
      out(name + "_base(")
      out("c(" + toState.toTerm + "," + toStack.toTerm + ",StateFinal,T), " +
        "c( " + fromState.toTerm + "," + fromStack.toTerm + ",StateFinal,T)).\n")
    }

    out("%All states beginning at the initial state + stack are final\n")
    out(name + "_final(")
    out("c(" + dpn.initialState.toTerm + "," + dpn.initialStack.toTerm + ",_,_)).\n")

    buffer.toString
  }

}
