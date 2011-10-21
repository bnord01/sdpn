package de.wwu.sdpn.ta.prolog.reachability


/**
 * A tree automaton annotating use and acq operations whether they are reentrant or not by calculating
 * the set of locks held by each process at every point of the execution.
 * This is top down deterministic.
 * 
 * @author Benedikt Nordhoff
 */
class FwdLockSet(val name: String, numLocks: Int) extends LockTreeAutomataAlphabet {
    require(numLocks <= 8 && numLocks > 0,"Habe" + numLocks + "locks")
    val stateSize = 1

    val isForwardRule = alphabet.keySet + "final"

    def genScript: String = {
        val buf = new StringBuilder()
        import buf.{ append => out }

        def outln(s:String) {out(s);out("\n")}
        for (i <- 0 until numLocks){
        	outln(name + "_lock(" + i + ").")
        }
        out("\n")
        //outln (name + "_allLocks(X) :- X is 2'" + ("1" * numLocks) + ".")
        
        //name_disjoint(X,Y) :- 0 is X '/\' Y.
        //name_isUnion(X,Y,XY) :- XY is X '\/' Y.

        out("""
%Helper functions for bitvector operations
name_isElemUnion(Elem,X,EX) :- EX is X '\/' (1 '<<' Elem).
name_notin(Elem,X) :- 0 is X '/\' (1 '<<' Elem).

%TA rules
name_nil(_,_).
name_ret(_).
name_base(X,X).
name_call1(X,X).
name_call2(X,X,X).
name_use(la(Lock,0),LX,X,X) :- name_notin(Lock,X), name_isElemUnion(Lock,X,LX).
name_use(la(Lock,1),LX,X,X) :- not(name_notin(Lock,X)), name_isElemUnion(Lock,X,LX).
name_acq(la(Lock,0),LX,X) :- name_notin(Lock,X), name_isElemUnion(Lock,X,LX).
name_acq(la(Lock,1),LX,X) :- not(name_notin(Lock,X)), name_isElemUnion(Lock,X,LX).
name_spawn(0,X,X).

%Final state
name_final(0). 

""".replace("name",name))
     
        buf.toString
    }
}
