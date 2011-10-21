package de.wwu.sdpn.ta.prolog.cuts

import scala.collection.SortedSet
import de.wwu.sdpn.dpn.explicit._
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import scala.collection.JavaConversions._
import scala.collection.Map
import de.wwu.sdpn.dpn.explicit.monitor.MonitorDPN

/**
 * A tree automaton that models the control flow of a dpn which
 * is given by the DPNFactory in which this trait is mixed in and
 * allows arbitrary cut nodes.
 * It annotates base-rules using the DPNAnnotator id is mixed in.
 * It matches monitors using the given MonitorMatcher
 *
 * @author b_nord01
 */
class MDPN2CutTA(dpn: MonitorDPN[GlobalState, StackSymbol, DPNAction, InstanceKey], val name: String = "cflow", annotator: DPNAnnotater = null) extends CutLockTreeAutomataAlphabet {

  val lockMap: Map[InstanceKey, Int] = {
    var m = Map[InstanceKey, Int]()
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
    out(name + "_cut(t(G,s(X,Y,Z)),c(G,s(X,Y,Z),G2,T),c(G,s(X,Y,Z),G2,T)).\n")
    out(name + "_nil(t(G,s(X,Y,Z)),c(G,s(X,Y,Z),G,0)).\n")

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
      out(fromState.num)
      out(",")
      out(stack(fromStack))
      out(",")
      out(toState.num)
      out(",1)).\n")
    }

    def call2(fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, callStack: StackSymbol, returnStack: StackSymbol) {
      out(name + "_call2(")
      out("c(" + toState.num + "," + stack(callStack) + ", Gc,1) , ") //the call has terminated at toState,toStack and reached state Gc
      out("c(Gc, " + stack(returnStack) + ",Gf,T), ") //and then we reached Gf from there
      out("c(" + fromState.num + "," + stack(fromStack) + ", Gf,T)).\n") //so we can annotate it with fromState,fromStack Gf and T
    }

    def call1(fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, callStack: StackSymbol) {
      out(name + "_call1(")
      out("c(" + toState.num + "," + stack(callStack) + ", Gc,0) , ") //the call has not terminated at toState,toStack and reached state Gc			
      out("c(" + fromState.num + "," + stack(fromStack) + ", Gc,0)).\n") //so we can annotate it with fromState,fromStack Gc and 0
    }

    def spawn(fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, toStack: StackSymbol, spawnState: GlobalState, spawnStack: StackSymbol) {
      out(name + "_spawn(")
      out("c(" + spawnState.num + "," + stack(spawnStack) + ",_,_ ), ") //some thread has been spawned
      out("c(" + toState.num + ", " + stack(toStack) + ",Gf,T), ") //and then we reached Gf , T
      out("c(" + fromState.num + "," + stack(fromStack) + ", Gf,T)).\n") //so we can annotate it with fromState,fromStack Gf and T
    }

    def acq(lock: InstanceKey, fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, toStack: StackSymbol) {
      out(name + "_acq(")
      out("la(" + lockMap(lock) + ",_),")
      out("c(" + toState.num + "," + stack(toStack) + ",StateFinal,0), " +
        "c( " + fromState.num + "," + stack(fromStack) + ",StateFinal,0)).\n")
    }

    def use(lock: InstanceKey, fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, callStack: StackSymbol, returnStack: StackSymbol) {
      out(name + "_use(la(" + lockMap(lock) + ",_),")
      out("c(" + toState.num + "," + stack(callStack) + " ,Gc,1) , ") //the we have terminated at toState,toStack and reached Gc
      out("c(Gc, " + stack(returnStack) + ",Gf,T), ") //and then we reached Gf from there
      out("c(" + fromState.num + "," + stack(fromStack) + ", Gf,T)).\n") //so we can annotate it with fromState,fromStack Gf and T			
    }

    /**
     * name_base(G1,P1,Gf,T) -> (G,P,Gf,T)  if dpn_base(G,P,_,G1,P1)
     */
    def base(annot: String, fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, toStack: StackSymbol) {
      out(name + "_base(")
      out(annot + ",")
      out("c(" + toState.num + "," + stack(toStack) + ",StateFinal,T), " +
        "c( " + fromState.num + "," + stack(fromStack) + ",StateFinal,T)).\n")
    }

    out("%All states beginning at the initial state + stack are final\n")
    out(name + "_final(")
    out("c(" + dpn.initialState.num + "," + stack(dpn.initialStack) + ",_,_)).\n")

    buffer.toString
  }

  def annotateRule(rule: de.wwu.sdpn.dpn.explicit.DPNRule[de.wwu.sdpn.dpn.explicit.GlobalState, de.wwu.sdpn.dpn.explicit.StackSymbol, de.wwu.sdpn.dpn.explicit.DPNAction]): String = {
    if (annotator == null)
      "0"
    else
      annotator.annotateRule(rule).toString
  }

  def stack(s: StackSymbol) = "s(" + s.node.getGraphNodeId + "," + s.basicBlock + "," + s.instrNr + ")"
  //    def stack(s:StackSymbol) = stackMap.get(s).get

  //    lazy val stackMap: Map[StackSymbol, Int] = genStackMap
  //
  //    protected def genStackMap: Map[StackSymbol, Int] = {
  //        val map = scala.collection.mutable.Map[StackSymbol, Int]()
  //
  //        implicit object SymOrd extends Ordering[StackSymbol] {
  //            override def compare(x: StackSymbol, y: StackSymbol): Int = {
  //                require(x != null, "tried to compare null StackSymbol")
  //                require(y != null, "tried to compare null StackSymbol")
  //                require(x.node != null)
  //                require(y.node != null)
  //                if (x.node.getGraphNodeId == y.node.getGraphNodeId && x.basicBlock == y.basicBlock && x.instrNr == y.instrNr)
  //                    return 0
  //                if (x.node.getGraphNodeId < y.node.getGraphNodeId)
  //                    return -1
  //                if (x.node.getGraphNodeId == y.node.getGraphNodeId && x.basicBlock < y.basicBlock)
  //                    return -1
  //                if (x.node.getGraphNodeId == y.node.getGraphNodeId && x.basicBlock == y.basicBlock && x.instrNr < y.instrNr)
  //                    return -1
  //
  //                return 1
  //            }
  //        }
  //
  //        val syms = getDPN.getStackSymbols
  //        var sortedSyms = SortedSet[StackSymbol]()
  //        sortedSyms ++= syms
  //
  //        var index = 0
  //        for (sym <- sortedSyms) {
  //            map += sym -> index
  //            index += 1
  //        }
  //
  //        return map
  //    }
}
