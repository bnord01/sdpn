package de.wwu.sdpn.core.ta.xsb.iterable
//TODO This has only been copied here!
/**
 * An automaton checking that there exists no base_write(...) node  
 * under the cut. 
 * @author Benedikt Nordhoff
 */
class NoBadActionTA(override val name:String) extends IterableTreeAutomata {
	val isForwardRule: Set[String] = Set[String]()
    
    def genScript(): String = { 
        val buf = new StringBuilder()
        import buf.{ append => out }
        
        

        out("""
%Helper functions 
name_or(bot,bot,bot).
name_or(bot,top,top).
name_or(top,bot,top).
name_or(top,top,top).

%TA rules
name_ret(bot).
name_nil(_,bot).

name_base(none,X,X).
name_base(read,X,X).
name_base(write,_,top).
name_call1(X,X).
name_acq(_,X,X).
name_cut(_,bot,bot).

name_use(_,X,Y,XoY) :- name_or(X,Y,XoY).
name_call2(X,Y,XoY) :- name_or(X,Y,XoY).
name_spawn(X,Y,XoY) :- name_or(X,Y,XoY).

name_final(top).
name_final(bot). 

""".replace("name",name))
     
        buf.toString }

}