package sdpn.ta.prolog.cuts


/**
 * A XSB tree automata checking the acquisition structure of the 
 * tree until the cut.
 * 
 * @todo remove V and Vc as they are unnecessary
 * @param name the name of the automaton
 * @param numLocks 1 to 8 the number of locks used.
 * @author Benedikt Nordhoff
 */
class CutAcqStructPrecutTA(override val name: String,numLocks:Int) extends CutLockTreeAutomataAlphabet {
	require(0 < numLocks && numLocks <= 8)
	
    val isForwardRule: Set[String] = Set[String]()
    
    def genScript(): String = { 
        val buf = new StringBuilder()
        import buf.{ append => out }

        def outln(s:String) {out(s);out("\n")}
        for (i <- 0 until numLocks){
        	outln(name + "_lock(" + i + ").")
        }
        out("\n")

        out("""
%Helper functions for bitvector operations
name_isElemUnion(Elem,X,EX) :- EX is X '\/' (1 '<<' Elem).
name_disjoint(X,Y) :- 0 is X '/\' Y.
name_notin(Elem,X) :- 0 is X '/\' (1 '<<' Elem).
name_isElem(Elem,X) :- not(0 is X '/\' (1 '<<' Elem)).
name_isUnion(X,Y,XY) :- XY is X '\/' Y.
name_isGraphUnion(X,U,G,GUX) :- GUX is G '\/' (U '<<' (numLocks * X)).
name_isElemUnion2(Elem,X,Y,EXY) :- EXY is X '\/' (Y '\/' (1 '<<' Elem)).
name_isUnion3(X,Y,Z,XYZ) :- XYZ is X '\/' (Y '\/' Z).

%TA rules
name_nil(_,a(bot,0,0,0)).
name_ret(a(bot,0,0,0)).
name_cut(_,a(bot,_,_,_),a(top,0,0,0)).
name_base(_,X,X).
name_call1(X,X).
name_acq(la(_,1),X,X).

%A call completly under/over the cut
name_call2(
        a(bot,A1,U1,G1),
        a(BT,A2,U2,G2),        
        a(BT,A,U,G)) 
        :- 		name_disjoint(A1,A2),
        		name_isUnion(A1,A2,A), 
        		name_isUnion(U1,U2,U),
        		name_isUnion(G1,G2,G).

%A call with a cut within we drop locks acqired afterwards 
name_call2(
        a(top,A,U,G),
        a(bot,_,_,_),        
        a(top,A,U,G)).

name_use(la(_,1),X,Y,Z) :- name_call2(X,Y,Z).


%A use completly under/over the cut or with a cut afterwards
name_use(la(Lock,0),
        a(bot,A1,U1,G1),
        a(BT,A2,U2,G2),        
        a(BT,A,U,G)) 
        :- 		name_disjoint(A1,A2),
        		name_isUnion(A1,A2,A), 
        		name_isElemUnion2(Lock,U1,U2,U),
        		name_isUnion(G1,G2,G).

%A use with a cut within is handled like an acq over the cut
name_use(la(Lock,0),
        a(top,A1,U1,G1),
        a(bot,_,_,_),        
        a(top,A,U,G)) :-
        		name_notin(Lock,U1),
        		name_isElemUnion(Lock,A1,A),
        		name_isElemUnion(Lock,U1,U),
        		name_isGraphUnion(Lock,U1,G1,G).

%A acq under a cut is ignored we could also reset everything but this also happens at the cut
name_acq(la(_,0),
        a(bot,A,U,G),                
        a(bot,A,U,G)).

%A acq over a cut
name_acq(la(Lock,0),
        a(top,A1,U1,G1),
        a(top,A,U,G)) :-
        		name_notin(Lock,U1),
        		name_isElemUnion(Lock,A1,A),
        		name_isElemUnion(Lock,U1,U),
        		name_isGraphUnion(Lock,U1,G1,G).

%A spawn
name_spawn(
        a(_,A1,U1,G1),
        a(BT,A2,U2,G2),        
        a(BT,A,U,G)) 
        :- 		name_disjoint(A1,A2),
        		name_isUnion(A1,A2,A), 
        		name_isUnion(U1,U2,U),
        		name_isUnion(G1,G2,G).

:-table(name_edge/3).
name_edge(G,X,Y) :- name_lock(X), name_lock(Y), not(0 is (G '/\' (1 '<<' (Y + (numLocks * X))))).

:-table(name_path/3).
name_path(G,X,Y) :- name_edge(G,X,Y).
name_path(G,X,Z) :- name_path(G,X,Y),name_edge(G,Y,Z).

name_hasCycle(G) :- name_lock(X),name_path(G,X,X).

name_final(a(_,_,_,G)) :- not(name_hasCycle(G)). 

""".replace("name",name).replace("numLocks",numLocks.toString))
     
        buf.toString }

}