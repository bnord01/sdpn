package de.wwu.sdpn.core.ta.xsb.iterable


/**
 * 
 */
class BaseTrimmedTA[GS, SS, A, L](origTA: MDPN2IterableTA[GS,SS,A,L],interesting:Set[SS],val name:String) extends IterableTreeAutomata{
    
    def this(origTA: MDPN2IterableTA[GS,SS,A,L],interesting:Set[SS]) {
        this(origTA,interesting,origTA.name + "_trimmed")
    }
    def isForwardRule = origTA.isForwardRule
    
    def genScript : String = {
        val buf = new StringBuilder
        val othername = origTA.name
        import buf.{append => out}
        def outln(x:Any){out(x);out("\n")}
        outln("%%% Original tree automata")
        outln(origTA.genScript)
        outln("""%%% interesting states
:- table name_interesting/1.
%%% Stacks defined as interesting """)

		for(s <- interesting) {
		    outln(name+"_interesting(S) :- "+othername+"_nil("+origTA.stack(s)+",S).")
		}
	

        outln(
"""
%%% Stuff interesting because of control flow

name_interesting(S) :- othername_ret(_,S).
name_interesting(S) :- othername_call1(_,S,_).
name_interesting(S) :- othername_call1(_,_,S).
name_interesting(S) :- othername_call2(_,S,_,_).
name_interesting(S) :- othername_call2(_,_,S,_).
name_interesting(S) :- othername_call2(_,_,_,S).
name_interesting(S) :- othername_acq(_,S,_).
name_interesting(S) :- othername_acq(_,_,S).
name_interesting(S) :- othername_use(_,S,_,_).
name_interesting(S) :- othername_use(_,_,S,_).
name_interesting(S) :- othername_use(_,_,_,S).
name_interesting(S) :- othername_spawn(_,S,_,_).
name_interesting(S) :- othername_spawn(_,_,S,_).
name_interesting(S) :- othername_spawn(_,_,_,S).
name_interesting(S) :- othername_base(A1,_,S), othername_base(A2,S,_), not(A1 == A2).

%%% transitive closure of base statements with equal actions
:- table name_basetrans/3.
name_basetrans(A,S1,S2) :- othername_base(A,S1,S2).
name_basetrans(A,S1,S3) :- name_basetrans(A,S1,S2), name_basetrans(A,S2,S3).



%%% final base transitions
:- table name_base/3.

name_base(A,S1,S2) :- name_basetrans(A,S1,S2), name_interesting(S1), name_interesting(S2).


%%% For non base rules do whatever the other automata want's to do.
name_cut(A,S1,S2) :- othername_cut(A,S1,S2).
name_nil(A,S) :- othername_nil(A,S).
name_ret(A,S) :- othername_ret(A,S).
name_call1(A,S1,S2) :- othername_call1(A,S1,S2).
name_call2(A,S1,S2,S3) :- othername_call2(A,S1,S2,S3).
name_acq(A,S1,S2) :- othername_acq(A,S1,S2).
name_use(A,S1,S2,S3) :- othername_use(A,S1,S2,S3).
name_spawn(A,S1,S2,S3) :- othername_spawn(A,S1,S2,S3).
name_final(S) :- othername_final(S).
""".replace("othername",othername).replace("name",name))
                
        
        
        
        buf.toString
    }

}