package de.wwu.sdpn.core.ta.xsb.iterable

import de.wwu.sdpn.core.ta.xsb.LockOperations
import de.wwu.sdpn.core.ta.xsb.AbsLibLockTA

class LibRelTA(cutNumber: Int, val name: String, val lo: LockOperations) extends IterableTreeAutomata with AbsLibLockTA {

    def isForwardRule = Set[String]()

    def genScript: String = {
        implicit val buf = new StringBuilder

        def r(c: TBVar, u: LSVar, g: GVar) = "r(" + c + ", " + u + ", " + g + ")"
        def la(l: LVar, i: TBVar) = "ra(_,la(" + l + "," + i + "))"
        def P = "P"
        def cutN(i: Int) = "cpt(" + i + ", _)"

        NIL("_", r(bot, A, G)) :- (emptyLockSet(A), emptyGraph(G))!

        RET("_",r(bot, A, G)) :- (emptyLockSet(A), emptyGraph(G))!

        BASE("_", P, P)!

        ACQ("_", P, P)!

        CALL1("_",P, P)!

        %%%("A returning call")

        CALL2("_",r(C, Uc, Gc), r(bot, Ur, Gr), r(C, U, G)) :- (
            isUnion(Uc, Ur, U),
            isGraphUnion(Gc, Gr, G))!

        CALL2("_",r(bot, __, Gc), r(top, Ur, Gr), r(top, Ur, G)) :- (
            isGraphUnion(Gc, Gr, G))!

        %%%("Reentrant uses and uses over the cut ar handled like calls")

        USE(la(__, top), r(C, Uc, Gc), r(bot, Ur, Gr), r(C, U, G)) :- (
            isUnion(Uc, Ur, U),
            isGraphUnion(Gc, Gr, G))!

        USE(la(__, __), r(bot, __, Gc), r(top, Ur, Gr), r(top, Ur, G)) :- (
            isGraphUnion(Gc, Gr, G))!

        %%%("Not reentrant uses possibly under the cut")

        USE(la(X, bot), r(bot, Uc, Gc), r(bot, Ur, Gr), r(bot, U, G)) :- (
            isElemUnion2(X,Uc, Ur, U),
            isGraphUnion(Gc, Gr, G))!
            
        %%%("Not reentrant use with the cut in the returning branch")

        USE(la(X, bot), r(top, Uc, Gc), r(bot, Ur, Gr), r(bot, U, G)) :- (
            isUnion(Uc, Ur, U),
            isGraphXUUnion2(X,Uc,Gc, Gr, G))!
            
        %%%("Spawns")
    
        SPAWN("_",r(bot,__,G),r(bot,Ur,G),r(bot,Ur,G)) :- emptyGraph(G)!
        
        SPAWN("_",r(top,__,Gs),r(C,Ur,Gr),r(C,Ur,G)) :- isGraphUnion(Gs,Gr,G)!

        %%%("The cut we are looking for")

        CUT(cutN(cutNumber), r(bot, A, G), r(top, A, G))!

        %%%("Previous cuts")

        for (i <- firstCutNumber until cutNumber)
            CUT(cutN(i), P, P)!

        FINAL(r(__, __, G)) :- acyclic(G)!

        return buf.toString
    }

}