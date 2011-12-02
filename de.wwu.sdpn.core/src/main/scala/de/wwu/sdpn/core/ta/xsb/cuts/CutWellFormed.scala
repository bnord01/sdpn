package de.wwu.sdpn.core.ta.xsb.cuts

/**
 * Automaton that checks that a tree is cut well formed.
 * 
 * @param name the name of the tree automaton
 * @author Benedikt Nordhoff
 */
class CutWellFormed(val name: String) extends CutLockTreeAutomataAlphabet {
    def isForwardRule = Set() //Set("nil","ret","base","call1","acq","final")
    def genScript: String = {
        val buf = new StringBuilder()
        import buf.{ append => out }

        def outln(s: String) { out(s); out("\n") }
        //outln (name + "_allLocks(X) :- X is 2'" + ("1" * numLocks) + ".")

        //name_disjoint(X,Y) :- 0 is X '/\' Y.
        //name_isUnion(X,Y,XY) :- XY is X '\/' Y.

        out("""
%Helper functions for bitvector operations

name_bs(bot).
name_bs(bots).
name_bt(bot).
name_bt(tops).

%TA rules
name_nil(_,bot).
name_ret(bot).
name_base(_,X,X).
name_call1(X,X).
name_acq(_,X,X).

name_call2(BT,top,top) :- name_bt(BT).
name_call2(top,BS,top) :- name_bs(BS).
name_call2(tops,BT,tops) :- name_bt(BT).
name_call2(bots,BS,bots) :- name_bs(BS).
name_call2(bot,tops,tops).
name_call2(bot,bots,bots).
name_call2(bot,bot,bot).
name_use(_,X,Y,Z) :- name_call2(X,Y,Z).

name_spawn(top,top,top).
name_spawn(top,BT,tops) :- name_bt(BT).
name_spawn(BS1,BS2,bots) :- name_bs(BS1), name_bs(BS2).

name_cut(_,bot,top).
name_cut(_,bots,top).

%Final state
name_final(top). 

""".replace("name", name))

        buf.toString
    }

}