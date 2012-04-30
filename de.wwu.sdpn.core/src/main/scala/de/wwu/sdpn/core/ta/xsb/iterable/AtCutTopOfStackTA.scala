package de.wwu.sdpn.core.ta.xsb.iterable
import de.wwu.sdpn.core.ta.xsb.HasTermRepresentation

/**
 * A tree automaton checking that there exists a cut node annotated with a stack symbol from tos.
 * @author Benedikt Nordhoff
 */

class AtCutTopOfStackTA[StackSymbol<%HasTermRepresentation](cutNumber:Int,override val name:String,tos:Set[StackSymbol]) extends IterableTreeAutomata {
	val isForwardRule: Set[String] = Set[String]()
    
    def genScript(): String = { 
        val buf = new StringBuilder()
        import buf.{ append => out }
        
        
        def outln(s:String) {out(s);out("\n")}
        
        for(ss <- tos)
            outln(name + "_tos(" + ss.toTerm + ").")
        

        out("""
%Helper functions 
name_or(bot,bot,bot).
name_or(bot,top,top).
name_or(top,bot,top).
name_or(top,top,top).

%TA rules
name_nil(_,bot).
name_ret(bot).
name_cut(cpt(cutNumber,S),_,top) :- name_tos(S).
name_cut(_,C,C).
                
                


name_base(_,X,X).
name_call1(X,X).
name_acq(_,X,X).

name_use(_,X,Y,XoY) :- name_or(X,Y,XoY).
name_call2(X,Y,XoY) :- name_or(X,Y,XoY).
name_spawn(X,Y,XoY) :- name_or(X,Y,XoY).

name_final(top). 

""".replace("cutNumber",cutNumber.toString).replace("name",name))
     
        buf.toString }

}