package de.wwu.sdpn.core.ta.xsb.iterable
import de.wwu.sdpn.core.ta.xsb.HasTermRepresentation

/**
 * For context = a1,...an of use/call annotations,
 * this automata checks if there is a process with stack S in the execution tree
 * for which there exist (possibly empty stacks) b1,..,bn such that S = an,bn,...,b2,a1,b1 
 * 
 * 
 * @author Benedikt Nordhoff
 */
class ContextTOSTA[A<%HasTermRepresentation](override val name:String,context:List[A]) extends IterableTreeAutomata {
    require(!(name.contains("name")),"Name for automata contains 'name' this causes problems!")
    require(!(context.map(_.toTerm).contains("bot")),"Context terms contain term 'bot' this is a reserved term!")
    
	val isForwardRule: Set[String] = Set[String]()
    
    def genScript(): String = { 
        val buf = new StringBuilder()
        import buf.{ append => out }
        
        require(!context.isEmpty,"Can")
        
        def outln(s:String) {out(s);out("\n")}        
        
        for((sym,nr) <- context.reverse.zipWithIndex) {
            outln(name+"_nextSym("+nr+", "+sym.toTerm+", "+(nr + 1)+").")
        }
        
        outln(name+"_tos(" + context.reverse.head.toTerm + ").")

        out("""
name_join(bosname,_,bosname).
name_join(Y,X,X) :- not(Y = bosname).

%TA rules
name_ret(_,bot).
name_nil(S,1) :- name_nextSym(0,S,1).
name_nil(S,bot) :- not(name_nextSym(0,S,1)).

name_base(_,X,X).
name_call1(S,C,C1) :- name_nextSym(C,S,C1).
name_call1(S,C,C) :- not(name_nextSym(C,S,_)).  
name_acq(ra(S,_),C,C1) :- name_nextSym(C,S,C1).
name_acq(ra(S,_),C,C) :- not(name_nextSym(C,S,_)).
name_cut(_,X,X).

name_use(_,X,Y,XoY) :- name_join(X,Y,XoY).
name_call2(_,X,Y,XoY) :- name_join(X,Y,XoY).
name_spawn(_,X,Y,XoY) :- name_join(X,Y,XoY).

name_final(bosname). 

""".replace("bosname",context.size.toString).replace("name",name))
     
        buf.toString }

}