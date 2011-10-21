package sdpn.ta.prolog.cuts
//TODO check and REMOVE V and Vc!!!!
/**
 * A XSB tree automata checking the acquisition structure of the whole
 * tree plus that no lock finaly acquired before the cut is used afterwards.
 * 
 * @todo remove V and Vc as they are unnecessary
 * @param name the name of the automaton
 * @param numLocks 1 to 8 the number of locks used.
 * @author Benedikt Nordhoff
 */
class CutAcqStructComplTA(override val name: String,numLocks:Int) extends CutLockTreeAutomataAlphabet {
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
        //outln (name + "_allLocks(X) :- X is 2'" + ("1" * numLocks) + ".")
        
        //name_disjoint(X,Y) :- 0 is X '/\' Y.
        //name_isUnion(X,Y,XY) :- XY is X '\/' Y.

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
name_nil(_,a(bot,0,0,0,0,0,0,0)).
name_ret(a(bot,0,0,0,0,0,0,0)).
name_cut(_,a(bot,A,U,V,_,Uc,Vc,G),a(top,A,U,V,0,Uc,Vc,G)).
name_base(_,X,X).
name_call1(X,X).
name_acq(la(_,1),X,X).

%A call completly under/over the cut
name_call2(
        a(bot,A1,U1,V1,Ac1,Uc1,Vc1,G1),
        a(bot,A2,U2,V2,Ac2,Uc2,Vc2,G2),        
        a(bot,A,U,V,Ac,Uc,Vc,G)) 
        :- 		name_disjoint(A1,A2),
        		name_isUnion(A1,A2,A), 
        		name_isUnion(U1,U2,U),
        		name_isUnion(V1,V2,V),
        		name_isUnion(Ac1,Ac2,Ac),
        		name_isUnion(Uc1,Uc2,Uc),
        		name_isUnion(Vc1,Vc2,Vc),
        		name_isUnion(G1,G2,G).

%A call with a cut within we drop locks acqired afterwards 
name_call2(
        a(top,A1,U1,V1,Ac1,Uc1,Vc1,G1),
        a(bot,A2,U2,V2,_,Uc2,Vc2,G2),        
        a(top,A,U,V,Ac1,Uc,Vc,G)) 
        :- 		name_disjoint(A1,A2),
        		name_isUnion(A1,A2,A), 
        		name_isUnion(U1,U2,U),
        		name_isUnion(V1,V2,V),
        		name_isUnion(Uc1,Uc2,Uc),
        		name_isUnion(Vc1,Vc2,Vc),
        		name_isUnion(G1,G2,G).

%A call with a cut afterwards we drop Uc1 
name_call2(
        a(bot,A1,U1,V1,Ac1,_,Vc1,G1),
        a(top,A2,U2,V2,Ac2,Uc2,Vc2,G2),        
        a(top,A,U,V,Ac,Uc,Vc,G)) 
        :- 		name_disjoint(A1,A2),
        		name_isUnion(A1,A2,A), 
        		name_isUnion(U1,U2,U),
        		name_isUnion(V1,V2,V),
        		name_isUnion(Ac1,Ac2,Ac),
        		name_isUnion(Vc1,Uc2,Uc),
        		name_isUnion(Vc1,Vc2,Vc),
        		name_isUnion(G1,G2,G).

name_use(la(_,1),X,Y,Z) :- name_call2(X,Y,Z).


%A use completly under/over the cut
name_use(la(Lock,0),
        a(bot,A1,U1,V1,Ac1,Uc1,Vc1,G1),
        a(bot,A2,U2,V2,Ac2,Uc2,Vc2,G2),        
        a(bot,A,U,V,Ac,Uc,Vc,G)) 
        :- 		name_disjoint(A1,A2),
        		name_isUnion(A1,A2,A), 
        		name_isElemUnion2(Lock,U1,U2,U),
        		name_isUnion(V1,V2,V),
        		name_isUnion(Ac1,Ac2,Ac),
        		name_isElemUnion2(Lock,Uc1,Uc2,Uc),
        		name_isUnion(Vc1,Vc2,Vc),
        		name_isUnion(G1,G2,G).

%A use with a cut within we drop locks acqired afterwards and don't collect Lock in Uc
name_use(la(Lock,0),
        a(top,A1,U1,V1,Ac1,Uc1,Vc1,G1),
        a(bot,A2,U2,V2,_,Uc2,Vc2,G2),        
        a(top,A,U,V,Ac1,Uc,Vc,G)) 
        :- 		name_disjoint(A1,A2),
        		name_isUnion(A1,A2,A), 
        		name_isElemUnion2(Lock,U1,U2,U),
        		name_isUnion(V1,V2,V),
        		name_isUnion(Uc1,Uc2,Uc),
        		name_isUnion(Vc1,Vc2,Vc),
        		name_isUnion(G1,G2,G).

%A use with a cut afterwards we drop Uc1 and don't collect Lock in Uc
name_use(la(Lock,0),
        a(bot,A1,U1,V1,Ac1,_,Vc1,G1),
        a(top,A2,U2,V2,Ac2,Uc2,Vc2,G2),        
        a(top,A,U,V,Ac,Uc,Vc,G)) 
        :- 		name_disjoint(A1,A2),
        		name_isUnion(A1,A2,A), 
        		name_isElemUnion2(Lock,U1,U2,U),
        		name_isUnion(V1,V2,V),
        		name_isUnion(Ac1,Ac2,Ac),
        		name_isUnion(Vc1,Uc2,Uc),
        		name_isUnion(Vc1,Vc2,Vc),
        		name_isUnion(G1,G2,G).

%A acq under a cut
name_acq(la(Lock,0),
        a(bot,A1,U1,V1,Ac1,Uc1,Vc1,G1),                
        a(bot,A,U,V1,Ac,Uc,Vc1,G)) :-
        		name_notin(Lock,V1),
        		name_isElemUnion(Lock,A1,A),
        		name_isElemUnion(Lock,U1,U),
        		name_isElemUnion(Lock,Ac1,Ac),
        		name_isElemUnion(Lock,Uc1,Uc),
        		name_isGraphUnion(Lock,U1,G1,G).

%A acq over a cut
name_acq(la(Lock,0),
        a(top,A1,U1,V1,Ac1,Uc1,Vc1,G1),                
        a(top,A,U,V1,Ac,Uc1,Vc1,G)) :-
        		name_notin(Lock,V1),
        		name_isElemUnion(Lock,A1,A),
        		name_isElemUnion(Lock,U1,U),
        		name_isElemUnion(Lock,Ac1,Ac),
        		name_isGraphUnion(Lock,U1,G1,G).

name_spawn(
        a(_,A1,U1,V1,Ac1,Uc1,Vc1,G1),
        a(C,A2,U2,V2,Ac2,Uc2,Vc2,G2),        
        a(C,A,U,V,Ac,Uc,Vc,G)) 
        :- 		name_disjoint(A1,A2),
        		name_isUnion(A1,A2,A), 
        		name_isUnion(U1,U2,U),
        		name_isUnion3(V1,V2,U,V),
        		name_isUnion(Ac1,Ac2,Ac),
        		name_isUnion(Uc1,Uc2,Uc),
        		name_isUnion3(Vc1,Vc2,Uc,Vc),
        		name_isUnion(G1,G2,G).

:-table(name_edge/3).
name_edge(a(_,_,G),X,Y) :- name_lock(X), name_lock(Y), not(0 is (G '/\' (1 '<<' (Y + (numLocks * X))))).
name_edge(a(Ac,Uc,_),X,Y) :- name_lock(X), name_lock(Y), name_isElem(X,Ac), name_isElem(Y,Uc).

:-table(name_path/3).
name_path(G,X,Y) :- name_edge(G,X,Y).
name_path(G,X,Z) :- name_path(G,X,Y),name_edge(G,Y,Z).

name_hasCycle(G) :- name_lock(X),name_path(G,X,X).
name_final(a(_,_,_,_,Ac,Uc,_,G)) :- name_disjoint(Ac,Uc), not(name_hasCycle(a(Ac,Uc,G))). 

""".replace("name",name).replace("numLocks",numLocks.toString))
     
        buf.toString }

}