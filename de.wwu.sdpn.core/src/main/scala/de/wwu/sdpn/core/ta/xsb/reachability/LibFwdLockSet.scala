package de.wwu.sdpn.core.ta.xsb.reachability
import de.wwu.sdpn.core.ta.xsb.LockOperations


/**
 * A tree automaton annotating use and acq operations whether they are reentrant or not by calculating
 * the set of locks held by each process at every point of the execution.
 * This is top down deterministic.
 * 
 * @author Benedikt Nordhoff
 */
class LibFwdLockSet(val name: String, lo:LockOperations) extends LockTreeAutomataAlphabet {
    val stateSize = 1

    val isForwardRule = alphabet.keySet + "final"

    def genScript: String = {
        val buf = new StringBuilder()        
        import buf.{ append => out }
        

        def outln(s:String) {out(s);out("\n")}
        outln(lo.genScript)
        //outln (name + "_allLocks(X) :- X is 2'" + ("1" * numLocks) + ".")
        
        //name_disjoint(X,Y) :- 0 is X '/\' Y.
        //name_isUnion(X,Y,XY) :- XY is X '\/' Y.

        out("""
%Helper functions for bitvector operations

%TA rules
name_nil(_,_).
name_ret(_).
name_base(X,X).
name_call1(X,X).
name_call2(X,X,X).
name_use(la(Lock,0),LX,X,X) :- LON_isNoElem(Lock,X), LON_isElemUnion(Lock,X,LX).
name_use(la(Lock,1),X,X,X) :- LON_isElem(Lock,X).
name_acq(la(Lock,0),LX,X) :- LON_isNoElem(Lock,X), LON_isElemUnion(Lock,X,LX).
name_acq(la(Lock,1),X,X) :- LON_isElem(Lock,X).
name_spawn(L0,X,X) :- LON_emptySet(L0).

%Final state
name_final(L0) :- LON_emptySet(L0). 

""".replace("LON",lo.name).replace("name",name))
     
        buf.toString
    }
}
