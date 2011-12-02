package de.wwu.sdpn.core.ta.xsb.cuts
import de.wwu.sdpn.core.ta.xsb.HasTermRepresentation

/**
 * A tree automaton checking that there exists a cut node annotated with a stack symbol from writingStack.
 * @author Benedikt Nordhoff
 */
class IFlowWriting[StackSymbol<%HasTermRepresentation](override val name:String,writingStack:Set[StackSymbol]) extends CutLockTreeAutomataAlphabet {
	val isForwardRule: Set[String] = Set[String]()
    
    def genScript(): String = { 
        val buf = new StringBuilder()
        import buf.{ append => out }
        
        
        def outln(s:String) {out(s);out("\n")}
        
        for(ss <- writingStack)
            outln(name + "_ws(" + ss.toTerm + ").")
        

        out("""
%Helper functions 
name_or(bot,bot,bot).
name_or(bot,top,top).
name_or(top,bot,top).
name_or(top,top,top).

%TA rules
name_nil(_,bot).
name_ret(bot).
name_cut(t(_,S),_,top) :- name_ws(S).
name_cut(t(_,S),C,C) :- not(name_ws(S)).

name_base(_,X,X).
name_call1(X,X).
name_acq(_,X,X).

name_use(_,X,Y,XoY) :- name_or(X,Y,XoY).
name_call2(X,Y,XoY) :- name_or(X,Y,XoY).
name_spawn(X,Y,XoY) :- name_or(X,Y,XoY).

name_final(top). 

""".replace("name",name))
     
        buf.toString }

}