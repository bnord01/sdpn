package de.wwu.sdpn.core.ta.xsb.reachability
import de.wwu.sdpn.core.ta.xsb.LockOperations

/**
 * An tree automaton checking the acquisition history of an execution tree.
 * The automaton uses xsb integers to store lock sets and the acquition graph so it can handle
 * at most eight locks on a 64bit architecture.
 *
 * @param name the name of the tree automaton
 * @param numLocks 1 to 8 the number of locks used by the execution tree which are labeled from 0 until numLocks.
 * @author Benedikt Nordhoff
 */
class LibLockTA(val name: String, lo: LockOperations) extends LockTreeAutomataAlphabet {

    val stateSize = 1

    val isForwardRule = Set("base", "call1", "nil", "ret")

    def genScript: String = {
        val lon = lo.name
        val buf = new StringBuilder()
        import buf.{ append => out }
        out(lo.genScript)
        out("\n")
        case class FSym(str: String) {
            def apply(args: String*): String = args.mkString(name + "_" + str + "(", " , ", ")")
        }

        val nil = FSym("nil")
        val ret = FSym("ret")
        val base = FSym("base")
        val call1 = FSym("call1")
        val call2 = FSym("call2")
        val use = FSym("use")
        val acq = FSym("acq")
        val spawn = FSym("spawn")

        implicit def str2ding(str: String) = new Object() {
            def :-(constr: String*) = { str + constr.mkString(" :- \n\t", " ,\n\t", "") }
            def ! : Unit = { out(str); out(".\n") }
        }

        def l(args: String*) = args.mkString("l(", ",", ")")
        def la(args: String*) = args.mkString("la(", ",", ")")

        val A = "A"
        val U = "U"
        val G = "G"
        val A1 = "A1"
        val U1 = "U1"
        val G1 = "G1"
        val A2 = "A2"
        val U2 = "U2"
        val G2 = "G2"
        val A3 = "A3"
        val U3 = "U3"
        val G3 = "G3"
        val A1c = "A1c"
        val U1c = "U1c"
        val G1c = "G1c"
        val A2c = "A2c"
        val U2c = "U2c"
        val G2c = "G2c"
        val A3c = "A3c"
        val U3c = "U3c"
        val G3c = "G3c"
        val X = "X"

        //def isDifference(X:String,Y:String,XmY:String) = XmY + (" is X '/\\' (Y '><' " + allLocks +")").replace("X",X).replace("Y",Y)

        def disjoint(x: String, y: String) = lon + "_disjoint(" + x + "," + y + ")"
        def isUnion(x: String, y: String, xy: String) = lon + "_isUnion(" + x + "," + y + "," + xy + ")"
        def isUnion3(x: String, y: String, z: String, xyz: String) = lon + "_isUnion3(" + x + ", " + y + ", " + z + ", " + xyz + ")"

        def isElem(elem: String, set: String) = lon + "_isElem(" + elem + "," + set + ")"
        def isNoElem(elem: String, set: String) = lon + "_isNoElem(" + elem + ", " + set + ")"
        def isElemUnion(elem: String, set: String, eset: String) = lon + "_isElemUnion(" + elem + "," + set + "," + eset + ")"
        def isElemUnion2(elem: String, set1: String, set2: String, eset: String) = lon + "_isElemUnion(" + elem + "," + set1 + ", " + set2 + ", " + eset + ")"
        def isGraphUnion(g1: String, g2: String, g3: String) =
            lon + "_isGraphUnion(" + g1 + ", " + g2 + ", " + g3 + ")"
        def isGraphXUUnion(x: String, u: String, g1: String, g2: String) =
            lon + "_isGraphXUUnion(" + x + ", " + u + ", " + g1 + ", " + g2 + ")"
        //def lock(s: String) = name + "_lock(" + s + ")"
        def setCopy(g1: String, g2: String) = lon + "_setCopy(" + g1 + ", " + g2 + ")"
        def graphCopy(g1: String, g2: String) = lon + "_graphCopy(" + g1 + ", " + g2 + ")"
        def emptyGraph(g: String) = lon + "_emptyGraph(" + g + ")"
        def emptySet(g: String) = lon + "_emptySet(" + g + ")"
        def notCyclic(g: String) = lon + "_notCyclic(" + g + ")"

        out("%%% Definition of tree-automata for lock sensitive schedules: " + name + "\n")

        out("\n%%% Defining transitions\n")
        nil("_", l(A, U, G)) :- (emptySet(A), emptySet(U), emptyGraph(G))!

        ret(l(A, U, G)) :- (emptySet(A), emptySet(U), emptyGraph(G))!

        call1(l(A1, U1, G1), l(A1c, U1c, G1c)) :- (setCopy(A1, A1c), setCopy(U1, U1c), graphCopy(G1, G1c))!

        base(l(A1, U1, G1), l(A1c, U1c, G1c)) :- (setCopy(A1, A1c), setCopy(U1, U1c), graphCopy(G1, G1c))!

        call2(l(A1, U1, G1), l(A2, U2, G2), l(A3, U3, G3)) :-
            (disjoint(A1, A2),
                isUnion(A1, A2, A3),
                isUnion(U1, U2, U3),
                isGraphUnion(G1, G2, G3))!

        use(la(X, "0"), l(A1, U1, G1), l(A2, U2, G2), l(A3, U3, G3)) :-
            (
                disjoint(A1, A2),
                isUnion(A1, A2, A3),
                isElemUnion2(X, U1, U2, U3),
                isGraphUnion(G1, G2, G3))!

        use(la("_", "1"), l(A1, U1, G1), l(A2, U2, G2), l(A3, U3, G3)) :-
            (disjoint(A1, A2),
                isUnion(A1, A2, A3),
                isUnion(U1, U2, U3),
                isGraphUnion(G1, G2, G3))!

        acq(la(X, "0"), l(A1, U1, G1), l(A2, U2, G2)) :-
            (isNoElem(X, U1),
                isElemUnion(X, A1, A2),
                isElemUnion(X, U1, U2),
                isGraphXUUnion(X, U1, G1, G2))!

        acq(la("_", "1"), l(A1, U1, G1), l(A1, U1, G1))!

        spawn(l(A1, U1, G1), l(A2, U2, G2), l(A3, U3, G3)) :-
            (disjoint(A1, A2),
                isUnion(A1, A2, A3),
                isUnion(U1, U2, U3),
                isGraphUnion(G1, G2, G3))!

        out("""
%%% Defining final states of name
""")
        (name + "_final(l(_,_,G))") :- (notCyclic("G")) !

        return buf.toString
    }

}

