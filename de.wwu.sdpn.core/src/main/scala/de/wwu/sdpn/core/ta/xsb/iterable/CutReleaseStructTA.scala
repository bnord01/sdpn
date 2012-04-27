package de.wwu.sdpn.core.ta.xsb.iterable

/**
 * An tree automaton checking the release structure after the cut.
 * @author Benedikt Nordhoff
 */
class CutReleaseStructTA(override val name: String,numLocks:Int) extends IterableTreeAutomata {
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
name_notin(Elem,X) :- 0 is X '/\' (1 '<<' Elem).
name_isUnion(X,Y,XY) :- XY is X '\/' Y.
name_isGraphUnion(X,U,G1,G2,GUX) :- GUX is G1 '\/' (G2 '\/' (U '<<' (numLocks * X))).
name_isElemUnion2(Elem,X,Y,EXY) :- EXY is X '\/' (Y '\/' (1 '<<' Elem)).

%TA rules
name_nil(_,r(bot,0,0)).
name_ret(r(bot,0,0)).
name_cut(_,r(bot,U,G),r(top,U,G)).
name_base(_,X,X).
name_call1(X,X).
name_acq(_,X,X).

name_call2(r(BT,Uc,Gc),r(bot,Ur,Gr),r(BT,U,G)) :- name_isUnion(Uc,Ur,U), name_isUnion(Gc,Gr,G).
name_call2(r(bot,_,Gc),r(top,Ur,Gr),r(top,Ur,G)) :- name_isUnion(Gc,Gr,G).

name_use(la(_,1),X,Y,Z) :- name_call2(X,Y,Z).
name_use(la(_,0),r(bot,_,Gc),r(top,Ur,Gr),r(top,Ur,G)) :- name_isUnion(Gc,Gr,G).
name_use(la(Lock,0),r(bot,Uc,Gc),r(bot,Ur,Gr),r(bot,U,G)) :- name_isElemUnion2(Lock,Uc,Ur,U), name_isUnion(Gc,Gr,G).
name_use(la(Lock,0),r(top,Uc,Gc),r(bot,Ur,Gr),r(top,U,G)) :- name_isUnion(Uc,Ur,U), name_isGraphUnion(Lock,Uc,Gc,Gr,G).

name_spawn(r(bot,_,0),r(bot,Ur,0),r(bot,Ur,0)).
name_spawn(r(top,_,Gs),r(BT,Ur,Gr),r(BT,Ur,G)) :- name_isUnion(Gs,Gr,G).

name_edge(G,X,Y) :- name_lock(X), name_lock(Y), not(0 is (G '/\' (1 '<<' (Y + (numLocks * X))))).
:-table(name_path/3).
name_path(G,X,Y) :- name_edge(G,X,Y).
name_path(G,X,Z) :- name_path(G,X,Y),name_edge(G,Y,Z).

name_hasCycle(G) :- name_lock(X),name_path(G,X,X).
name_final(r(_,_,G)) :- not(name_hasCycle(G)).
 

""".replace("name",name).replace("numLocks",numLocks.toString))
     
        buf.toString }

}