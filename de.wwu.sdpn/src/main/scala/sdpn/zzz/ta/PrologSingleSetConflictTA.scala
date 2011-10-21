package sdpn.zzz.ta

import com.ibm.wala.ipa.callgraph.CGNode

/**
 * Tree automata which checks if two processes reach a nil node annotated with a symbol from cset.
 * Intended for datalog interpretation, doesn't use nested terms.
 * @deprecated
 * @author Benedikt Nordhoff
 */
class PrologSingleSetConflictTA(val name: String, cset: scala.collection.Set[Int]) extends LockTreeAutomataAlphabet {
    val stateSize = 1
    val isForwardRule = alphabet.keySet + "final"
    def genScript: String = {
        val buffer = new StringBuilder();
        import buffer.{ append => out }

        out("%defining bad nodes\n")
        for (node <- cset) {
            out(name)
            out("_badNode(")
            out(node)
            out(").\n")
        }

        out(
            """%defining conflict automata.
name_nil(_,X,1) :- name_badNode(X).
name_nil(_,X,0) :- not(name_badNode(X)).

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
""".replace("name", name)
        )

        buffer.toString

    }

}