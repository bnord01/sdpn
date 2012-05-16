package de.wwu.sdpn.core.ta.xsb.iterable

import de.wwu.sdpn.core.ta.xsb.HasTermRepresentation

/**
 * An automaton checking that no base action annotated with an 
 * action from the set actions is executed after the cut with the 
 * given number.
 * 
 * @author Benedikt Nordhoff
 */
class ActionNotAfterCutTA[T<%HasTermRepresentation](cutNumber:Int,actions: Set[T],override val name:String) extends IterableTreeAutomata {
	val isForwardRule: Set[String] = Set[String]()
	require(!actions.isEmpty,"Empty set of actions for " + name)
    
    def genScript(): String = { 
        val buf = new StringBuilder()
        import buf.{ append => out }
        
        out("""
        		%%% Tree automaton checking that the following bad actions don't happen
                %%% after the cut number cutNumber
                
                %%% Bad actions that shouldn't happen after the cut! %%%
                """.replace("cutNumber",cutNumber.toString))
        for(a <- actions.map(_.toTerm)) {
            out(name)
            out("_badAction(")
            out(a)
            out(").\n")
        }

        out("""
%Helper functions 
name_or(bot,bot,bot).
name_or(bot,top,top).
name_or(top,bot,top).
name_or(top,top,top).

%TA rules
name_ret(_,n).
name_nil(_,n).

name_base(A,n,a) :- name_badAction(A).
name_base(A,n,n) :- not(name_badAction(A)).                
name_base(_,a,a).
name_base(_,c,c).                
name_call1(_,X,X).
name_acq(_,X,X).

                %%% Returning calls %%%
% With cut                
name_call2(_,c,n,c).	
name_call2(_,n,c,c).
name_call2(_,a,c,c). % Bad action before the cut
        		   % no rule for c,a as a lies after
                   % the cut.

% Without cut just propagate existing a               
name_call2(_,n,n,n).
name_call2(_,n,a,a).
name_call2(_,a,n,a).                
name_call2(_,a,a,a).                
                %%% Use just as calls as usual %%%
name_use(_,X,Y,XoY) :- name_call2(_,X,Y,XoY).


                %%% Rules for spawns %%%
% With cut                
name_spawn(_,c,n,n). % Don't propagate cuts from the spawned process!
name_spawn(_,c,c,c). % If we've seen a cut so must the spawned process.
name_spawn(_,c,a,a). % We haven't seen our cut but we must be above it.
                   % This a will be discarded.
                
% Without cut
name_spawn(_,n,n,n).                
name_spawn(_,a,n,a).
name_spawn(_,n,a,a).                
name_spawn(_,a,a,a).                

                
                %%% Ignore previous cuts %%%
""".replace("name",name))

	
	for (i <- firstCutNumber until cutNumber){
		out("name_cut(cpt(i,_),X,X).\n".replace("i",i.toString).replace("name",name))
	}
        
        out(""" 
                %%% The cut we are looking for %%%
name_cut(cpt(cutNumber,_),n,c).                

                %%%  c is the only accepting state   %%%
                %%% the tree must be cut well formed %%%
name_final(c). 
                
        """.replace("cutNumber",cutNumber.toString).replace("name",name))
     
        buf.toString }

}