package de.wwu.sdpn.core.ta.xsb.iterable

/**
 * Automaton that checks that a tree is cut well formed at the given level.
 * 
 * @param name the name of the tree automaton
 * @author Benedikt Nordhoff
 */
class CutWellFormed(val name: String,cutNumber:Int) extends IterableTreeAutomata {
    require(cutNumber >= 0, "Cuts should be enumerated by positive numbers.")
    def isForwardRule = Set() //Set("nil","ret","base","call1","acq","final")
    def genScript: String = {
        val buf = new StringBuilder()
        import buf.{ append => out }

        def outln(s: String) { out(s); out("\n") }
        out("""
%Helper functions for bitvector operations

name_bs(bot).
name_bs(bots).
name_bt(bot).
name_bt(tops).

%TA rules
name_nil(_,bot).
name_ret(_,bot).
name_base(_,X,X).
name_call1(_,X,X).
name_acq(_,X,X).

name_call2(_,BT,top,top) :- name_bt(BT).
name_call2(_,top,BS,top) :- name_bs(BS).
name_call2(_,tops,BT,tops) :- name_bt(BT).
name_call2(_,bots,BS,bots) :- name_bs(BS).
name_call2(_,bot,tops,tops).
name_call2(_,bot,bots,bots).
name_call2(_,bot,bot,bot).
name_use(_,X,Y,Z) :- name_call2(_,X,Y,Z).

name_spawn(_,top,top,top).
name_spawn(_,top,BT,tops) :- name_bt(BT).
name_spawn(_,BS1,BS2,bots) :- name_bs(BS1), name_bs(BS2).

name_cut(cpt(cutNumber,_),bot,top).
name_cut(cpt(cutNumber,_),bots,top).                   

%Final state
name_final(top). 

""".replace("cutNumber",cutNumber.toString).replace("name", name))

	for(i <- 0 until cutNumber){
		out(
				"""
% cut number i            
name_cut(cpt(i,_),bot,tops).
name_cut(cpt(i,_),tops,tops).      
name_cut(cpt(i,_),top,top).      
""".replace("i",i.toString).replace("name",name)
				)    
	}

        buf.toString
    }

}