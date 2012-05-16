package de.wwu.sdpn.core.ta.xsb.cuts

/**
 * An automaton checking that there exists no base_write(...) node  
 * under the cut. 
 * @author Benedikt Nordhoff
 */
class IFlowNoOverwrite(override val name:String) extends CutLockTreeAutomataAlphabet {
	val isForwardRule: Set[String] = Set[String]()
    
    def genScript(): String = { 
        val buf = new StringBuilder()
        import buf.{ append => out }
        
        // the new version
                out("""
%TA rules
name_ret(n).
name_nil(_,n).

name_base(write,n,a).
name_base(read,n,n).
name_base(none,n,n).
name_base(_,a,a).
name_base(_,c,c).                
name_call1(X,X).
name_acq(_,X,X).

                %%% Returning calls %%%
% With cut                
name_call2(c,n,c).	
name_call2(n,c,c).
name_call2(a,c,c). % Bad action before the cut
        		   % no rule for c,a as a lies after
                   % the cut.

% Without cut just propagate existing a               
name_call2(n,n,n).
name_call2(n,a,a).
name_call2(a,n,a).                
name_call2(a,a,a).                
                %%% Use just as calls as usual %%%
name_use(_,X,Y,XoY) :- name_call2(X,Y,XoY).


                %%% Rules for spawns %%%
% With cut                
name_spawn(c,n,n). % Don't propagate cuts from the spawned process!
name_spawn(c,c,c). % If we've seen a cut so must the spawned process.
name_spawn(c,a,a). % We haven't seen our cut but we must be above it.
                   % This a will be discarded.
                
% Without cut
name_spawn(n,n,n).                
name_spawn(a,n,a).
name_spawn(n,a,a).                
name_spawn(a,a,a).                

                %%% The cut we are looking for %%%
name_cut(_,n,c).                

                %%%  c is the only accepting state   %%%
                %%% the tree must be cut well formed %%%
name_final(c).                 
        """.replace("name",name))

        // the old unprecise version
//        out("""
//%Helper functions 
//name_or(bot,bot,bot).
//name_or(bot,top,top).
//name_or(top,bot,top).
//name_or(top,top,top).
//
//%TA rules
//name_ret(bot).
//name_nil(_,bot).
//
//name_base(none,X,X).
//name_base(read,X,X).
//name_base(write,_,top).
//name_call1(X,X).
//name_acq(_,X,X).
//name_cut(_,bot,bot).
//
//name_use(_,X,Y,XoY) :- name_or(X,Y,XoY).
//name_call2(X,Y,XoY) :- name_or(X,Y,XoY).
//name_spawn(X,Y,XoY) :- name_or(X,Y,XoY).
//
//name_final(top).
//name_final(bot). 
//
//""".replace("name",name))
     
        buf.toString }

}