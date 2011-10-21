package sdpn.zzz.ta

import scala.collection.JavaConversions.collectionAsScalaIterable
import scala.collection.Map
import scala.collection.SortedSet
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import sdpn.dpn.explicit.example.DPNFactory
import sdpn.dpn.explicit.GlobalState
import sdpn.dpn.explicit.MonitorEnter
import sdpn.dpn.explicit.MonitorExit
import sdpn.dpn.explicit.StackSymbol
import sdpn.dpn.explicit.SyncMethodEnter
import sdpn.dpn.explicit.BaseRule
import sdpn.dpn.explicit.PopRule
import sdpn.dpn.explicit.PushRule
import sdpn.dpn.explicit.SpawnRule
import sdpn.util.MonitorMatcher

/**
 * Generate a tree automata for conflict analysis from a DPN.
 * Trait to be mixed in into a DPNFactory. 
 * Does monitor matching using the given MonitorMatcher itself. 
 * Ignores wait calls. 
 * Uses locks specified by safeLocks.
 * Intended for datalog interpretation, doesn't use nested terms.
 * @deprecated
 * @author Benedikt Nordhoff
 */
trait PrologDPN2TA extends LockTreeAutomataAlphabet {
    this: DPNFactory with MonitorMatcher =>

    def safeLocks: Map[InstanceKey, Int]

    /**
     * The state consists of one global state plus the stack symbol consisting of the cgnode,
     * the basic block and the instruction number which represents the state at which this execution visited the branch.
     * Another global state represents the state at which the execution ended.
     * Plus a bit indicating if a branch has terminated.
     */
    val stateSize = 1 + 1 + 1 + 1
    val isForwardRule = alphabet.keySet + "final"
    def genScript = {
        val buffer = new StringBuilder()
        import buffer.{ append => out }
        val dpn = getDPN
        out(name + "_nil(G,Stack,G,Stack,G,0).\n")

        if (safeLocks.isEmpty) { //TODO this assumes that at least one safe lock is used somewhere otherwise 
            out("""
name_acq(_,_,_,_,_,_,_,_,_) :- fail.
name_use(_,_,_,_,_,_,_,_,_,_,_,_,_) :- fail.
""".replace("name", name))
        }

        for (rule <- dpn.getTransitions) {
            rule match {
                case BaseRule(fromState, fromStack, action, toState, toStack) =>
                    action match {
                        case MonitorEnter(instr, ikey) if (safeLocks.contains(ikey)) =>
                            acq(ikey, fromState, fromStack, toState, toStack)
                            //TODO has it to be from or to here?
                            for (bb <- getExits(fromStack.node, fromStack.basicBlock)) {
                                //TODO IR and CFG shouldn't be null but maybe check anyway?
                                //TODO maybe we should 
                                for (suc <- fromStack.node.getIR().getControlFlowGraph().getNormalSuccessors(bb)) {
                                    use(ikey, fromState, fromStack, toState, toStack, StackSymbol(fromStack.node, suc.getGraphNodeId, 0))
                                }
                                //use(ikey,fromState,fromStack,toState,toStack,StackSymbol(fromStack.node,bb.getGraphNodeId,0))								
                            }
                        case MonitorExit(instr, ikey) if (safeLocks.contains(ikey)) =>
                            //TODO this may not be correct where is this consistent with the return address. 
                            ret(fromState, fromStack, toState)
                        case _ =>
                            base(fromState, fromStack, toState, toStack)
                    }
                case PushRule(fromState, fromStack, action, toState, callStack, returnStack) =>
                    action match {
                        case SyncMethodEnter(instr, ikey) if (safeLocks.contains(ikey)) =>
                            use(ikey, fromState, fromStack, toState, callStack, returnStack)
                            acq(ikey, fromState, fromStack, toState, callStack)
                        case _ =>
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
            out("_ret(")
            out(fromState.num)
            out(",")
            out(stack(fromStack))
            out(",")
            out(toState.num)
            out(",1).\n")
        }

        def call2(fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, callStack: StackSymbol, returnStack: StackSymbol) {
            out(name + "_call2(")
            out(toState.num + "," + stack(callStack) + ", Gc,1 , ") //the call has terminated at toState,toStack and reached state Gc
            out("Gc, " + stack(returnStack) + ",Gf,T, ") //and then we reached Gf from there
            out(fromState.num + "," + stack(fromStack) + ", Gf,T).\n") //so we can annotate it with fromState,fromStack Gf and T
        }

        def call1(fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, callStack: StackSymbol) {
            out(name + "_call1(")
            out(toState.num + "," + stack(callStack) + ", Gc,0 , ") //the call has not terminated at toState,toStack and reached state Gc			
            out(fromState.num + "," + stack(fromStack) + ", Gc,0).\n") //so we can annotate it with fromState,fromStack Gc and 0
        }

        def spawn(fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, toStack: StackSymbol, spawnState: GlobalState, spawnStack: StackSymbol) {
            out(name + "_spawn(")
            out(spawnState.num + "," + stack(spawnStack) + ",_,_ , ") //some thread has been spawned
            out(toState.num + ", " + stack(toStack) + ",Gf,T, ") //and then we reached Gf , T
            out(fromState.num + "," + stack(fromStack) + ", Gf,T).\n") //so we can annotate it with fromState,fromStack Gf and T
        }

        def acq(lock: InstanceKey, fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, toStack: StackSymbol) {
            out(name + "_acq(")
            out(safeLocks(lock) + ",")
            out(toState.num + "," + stack(toStack) + ",StateFinal,0, " + fromState.num + "," + stack(fromStack) + ",StateFinal,0).\n")
        }

        def use(lock: InstanceKey, fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, callStack: StackSymbol, returnStack: StackSymbol) {
            out(name + "_use(" + safeLocks(lock) + ",")
            out(toState.num + "," + stack(callStack) + " ,Gc,1 , ") //the we have terminated at toState,toStack and reached state Gc
            out("Gc, " + stack(returnStack) + ",Gf,T, ") //and then we reached Gf from there
            out(fromState.num + "," + stack(fromStack) + ", Gf,T).\n") //so we can annotate it with fromState,fromStack Gf and T			
        }

        /**
         * name_base(G1,P1,Gf,T) -> (G,P,Gf,T)  if dpn_base(G,P,_,G1,P1)
         */
        def base(fromState: GlobalState, fromStack: StackSymbol, toState: GlobalState, toStack: StackSymbol) {
            out(name + "_base(")
            out(toState.num + "," + stack(toStack) + ",StateFinal,T, " + fromState.num + "," + stack(fromStack) + ",StateFinal,T).\n")
        }

        out("%All states beginning at the initial state + stack are final\n")
        out(name + "_final(")
        out(dpn.initialState.num + "," + stack(dpn.initialStack) + ",_,_).\n")

        buffer.toString
    }

    //def stack(s: StackSymbol) = s.node.getGraphNodeId + "," + s.basicBlock + "," + s.instrNr
    def stack(s: StackSymbol) = stackMap.get(s).get

    lazy val stackMap: Map[StackSymbol, Int] = genStackMap

    protected def genStackMap: Map[StackSymbol, Int] = {
        val map = scala.collection.mutable.Map[StackSymbol, Int]()

        implicit object SymOrd extends Ordering[StackSymbol] {
            override def compare(x: StackSymbol, y: StackSymbol): Int = {
                require(x != null, "tried to compare null StackSymbol")
                require(y != null, "tried to compare null StackSymbol")
                require(x.node != null)
                require(y.node != null)
                if (x.node.getGraphNodeId == y.node.getGraphNodeId && x.basicBlock == y.basicBlock && x.instrNr == y.instrNr)
                    return 0
                if (x.node.getGraphNodeId < y.node.getGraphNodeId)
                    return -1
                if (x.node.getGraphNodeId == y.node.getGraphNodeId && x.basicBlock < y.basicBlock)
                    return -1
                if (x.node.getGraphNodeId == y.node.getGraphNodeId && x.basicBlock == y.basicBlock && x.instrNr < y.instrNr)
                    return -1

                return 1
            }
        }

        val syms = getDPN.getStackSymbols
        var sortedSyms = SortedSet[StackSymbol]()
        sortedSyms ++= syms

        var index = 0
        for (sym <- sortedSyms) {
            map += sym -> index
            index += 1
        }

        return map
    }
}