package sdpn.ta.prolog.reachability

import sdpn.dpn.explicit.StackSymbol
import com.ibm.wala.ipa.callgraph.CGNode
/**
 * Tree automaton checking that at least two processes have reached some nil node 
 * annotated with a StackSymbol from cset.
 * 
 * @param name the name of the tree automaton
 * @param cset the set of StackSymbols to be checked for mutual exclusion.
 * @author Benedikt Nordhoff
 */
class SingleSetConflictTA(val name:String,cset:scala.collection.Set[StackSymbol]) extends LockTreeAutomataAlphabet {
	val stateSize = 1
	val isForwardRule = Set("final","ret","base","call1","acq")
	def genScript :String = {
		val buffer = new StringBuilder();
		import buffer.{append => out}
		
		out("%Defining bad stack symbols\n")
		for (StackSymbol(n,b,i) <- cset) {
			out (name)
			out("_badStack(s(")
			out(n.getGraphNodeId + "," + b + "," + i)
			out(")).\n")
		}
		
		out(
"""%defining conflict automata.
name_nil(t(_,X),1) :- name_badStack(X).
name_nil(t(_,X),0) :- not(name_badStack(X)).

name_ret(0).
name_base(X,X).
name_call1(X,X).

name_call2(1,0,1).
name_call2(0,1,1).
name_call2(0,0,0).
name_call2(1,1,2).
name_call2(_,2,2).
name_call2(2,_,2).

name_use(_,1,0,1).
name_use(_,0,1,1).
name_use(_,0,0,0).
name_use(_,1,1,2).
name_use(_,_,2,2).
name_use(_,2,_,2).

name_acq(_,X,X).
name_spawn(1,0,1).
name_spawn(0,1,1).
name_spawn(0,0,0).
name_spawn(1,1,2).
name_spawn(_,2,2).
name_spawn(2,_,2).
name_final(2).
""".replace("name",name)
		)	
		
		
		
		buffer.toString
		
		
	}

}
