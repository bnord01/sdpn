package de.wwu.sdpn.core.ta.xsb.iterable

import de.wwu.sdpn.core.ta.xsb.AbsLibLockTA
import de.wwu.sdpn.core.ta.xsb.LockOperations

class LocksHeldTroughoutTA(cutNumber: Int, val name: String, val lo: LockOperations) extends IterableTreeAutomata with AbsLibLockTA {

    def isForwardRule = Set[String]()

    def genScript: String = {
        implicit val buf = new StringBuilder

        def P = "P"

        def h(c: TBVar, a: LSVar, u: LSVar) = "h(" + c + ", " + a + ", " + u + ")"

        def la(x: LVar, c: TBVar) = "ra(_,la(" + x + ", " + c + "))"

        def cutN(i: Int) = "cpt(" + i + ", _)"

        %%%("Base Cases")

        NIL("_", h(bot, A, A)) :- emptyLockSet(A)!

        RET("_",h(bot, A, A)) :- emptyLockSet(A)!

        BASE("_", P, P)!

        CALL1("_",P, P)!

        %("Reentrant acq")

        ACQ(la(__, top), P, P)!

        %%%("A returning call without cuts, just collect A and U")

        CALL2("_",h(bot, A1, U1), h(bot, A2, U2), h(bot, A3, U3)) :- (
            isUnion(A1, A2, A3),
            isUnion(U1, U2, U3))!

        %%%("A cut in the returning branch, drop locks acquired afterwards")

        CALL2("_",h(top, A1, U1), h(bot, __, U2), h(top, A1, U3)) :-
            isUnion(U1, U2, U3)!

        %%%("A cut after the the returning branch, drop locks used befores")

        CALL2("_",h(bot, A1, __), h(top, A2, U2), h(top, A3, U2)) :-
            isUnion(A1, A2, A3)!

        %%%("Uses which lie over the cut or are reentrant are handled just as calls")

        %("A cut in the returning branch, drop locks acquired afterwards, the use ist over the cut!")

        USE(la(__, __), h(top, A1, U1), h(bot, __, U2), h(top, A1, U3)) :-
            isUnion(U1, U2, U3)!

        %("A cut after the the returning branch, drop locks used befores, the use is over the cut!")

        USE(la(__, __), h(bot, A1, __), h(top, A2, U2), h(top, A3, U2)) :-
            isUnion(A1, A2, A3)!

        %("A returning reentrant use without cuts, just collect A and U")

        USE(la(__, top), h(bot, A1, U1), h(bot, A2, U2), h(bot, A3, U3)) :- (
            isUnion(A1, A2, A3),
            isUnion(U1, U2, U3))!

        %%%("A returning not reentrant use without cuts, just collect A and U and the lock")

        USE(la(X, bot), h(bot, A1, U1), h(bot, A2, U2), h(bot, A3, U3)) :- (
            isUnion(A1, A2, A3),
            isElemUnion2(X, U1, U2, U3))!

        %%%("A not reentrant acq under the cut")

        ACQ(la(X, bot), h(bot, A, U), h(bot, A, U1)) :- isElemUnion(X, U, U1)!

        %%%("A not reentrant acq over the cut.")

        ACQ(la(X, bot), h(top, A, U), h(top, A1, U)) :- isElemUnion(X, A, A1)!

        %%%("A spawn, collect A and U")

        SPAWN("_",h(__, A1, U1), h(C, A2, U2), h(C, A3, U3)) :- (
            isUnion(A1, A2, A3),
            isUnion(U1, U2, U3))!

        %%%("The cut we are looking for")

        CUT(cutN(cutNumber), h(bot, A, U), h(top, A, U))!

        %%%("Previous cuts")

        for (i <- firstCutNumber until cutNumber)
            CUT(cutN(i), P, P)!

        %%%("The final states")

        FINAL(h(__, A, U)) :- disjoint(A, U)!

        return buf.toString

    }

}