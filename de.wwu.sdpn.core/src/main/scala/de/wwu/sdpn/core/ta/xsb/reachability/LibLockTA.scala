package de.wwu.sdpn.core.ta.xsb.reachability
import de.wwu.sdpn.core.ta.xsb.LockOperations
import de.wwu.sdpn.core.ta.xsb.AbsLibLockTA

/**
 * An tree automaton checking the acquisition history of an execution tree.
 * The automaton uses xsb integers to store lock sets and the acquition graph so it can handle
 * at most eight locks on a 64bit architecture.
 *
 * @param name the name of the tree automaton
 * @param numLocks 1 to 8 the number of locks used by the execution tree which are labeled from 0 until numLocks.
 * @author Benedikt Nordhoff
 */
class LibLockTA(val name: String, val lo: LockOperations) extends LockTreeAutomataAlphabet with AbsLibLockTA{

    val stateSize = 1

    val isForwardRule = Set[String]()

    def genScript: String = {
        val lon = lo.name
        implicit val buf = new StringBuilder()
        out(lo.genScript)
        out("\n")
        
        def l(a:LSVar,u:LSVar,g:GVar) = "l("+a+","+u+","+g+")"
        def la(l:LVar,i:TBVar) = "la("+l+","+i+")"
        


        out("%%% Definition of tree-automata for lock sensitive schedules: " + name + "\n")

        out("\n%%% Defining transitions\n")
        
        NIL("_", l(A, U, G)) :- (emptyLockSet(A), emptyLockSet(U), emptyGraph(G))!

        RET(l(A, U, G)) :- (emptyLockSet(A), emptyLockSet(U), emptyGraph(G))!

        CALL1(l(A1, U1, G1), l(A1, U1, G1))!

        BASE(l(A1, U1, G1), l(A1, U1, G1))!

        CALL2(l(A1, U1, G1), l(A2, U2, G2), l(A3, U3, G3)) :-
            (disjoint(A1, A2),
                isUnion(A1, A2, A3),
                isUnion(U1, U2, U3),
                isGraphUnion(G1, G2, G3))!

        USE(la(X, bot), l(A1, U1, G1), l(A2, U2, G2), l(A3, U3, G3)) :-
            (
                disjoint(A1, A2),
                isUnion(A1, A2, A3),
                isElemUnion2(X, U1, U2, U3),
                isGraphUnion(G1, G2, G3))!

        USE(la(__, top), l(A1, U1, G1), l(A2, U2, G2), l(A3, U3, G3)) :-
            (disjoint(A1, A2),
                isUnion(A1, A2, A3),
                isUnion(U1, U2, U3),
                isGraphUnion(G1, G2, G3))!

        ACQ(la(X, bot), l(A1, U1, G1), l(A2, U2, G2)) :-
            (isNoElem(X, U1),
                isElemUnion(X, A1, A2),
                isElemUnion(X, U1, U2),
                isGraphXUUnion(X, U1, G1, G2))!

        ACQ(la(__, top), l(A1, U1, G1), l(A1, U1, G1))!
        
        SPAWN(l(A1, U1, G1), l(A2, U2, G2), l(A3, U3, G3)) :-
            (disjoint(A1, A2),
                isUnion(A1, A2, A3),
                isUnion(U1, U2, U3),
                isGraphUnion(G1, G2, G3))!

		FINAL(l(__,__,G)) :- notCyclic(G)!		
        

        return buf.toString
    }

}

