package sdpn.zzz.ta

/**
 * Tree automata for locksensetive execution trees. 
 * The use of V is unnecessary.
 * This doesn't handle reentrant monitors correctly.
 * 
 * Intended for datalog interpretation, doesn't use nested terms.
 * @deprecated
 * @author Benedikt Nordhoff
 */
class PrologIntLockTA(val name: String, numLocks: Int) extends LockTreeAutomataAlphabet {
    require(numLocks <= 8 && numLocks > 0)
    val stateSize = 4
    
    val isForwardRule = Set("base","call1","nil","ret")

    def genScript: String = {
        val buf = new StringBuilder()
        import buf.{ append => out }
        implicit object thisStuff extends ImplStuff {
            val prefix = name
            val buffer = buf
        }

        val A1 = "A1"
        val U1 = "U1"
        val V1 = "V1"
        val G1 = "G1"
        val A2 = "A2"
        val U2 = "U2"
        val V2 = "V2"
        val G2 = "G2"
        val A3 = "A3"
        val U3 = "U3"
        val V3 = "V3"
        val G3 = "G3"
        val X = "X"

        def disjoint(x: String, y: String) = "is(0,'/\\'(" + x + "," + y + "))"
        def isUnion(x: String, y: String, xy: String) = "is(" + xy + ",'\\/'(" + x + "," + y + "))"
        def isUnion2(x: String, y: String, z: String, xyz: String) = "is(" + xyz + ",'\\/'(" + z + ",'\\/'(" + x + "," + y + ")))"
        def contains(elem: String, set: String) = "not(is(0,'/\\'('<<'(1," + elem + ")," + set + ")))"
        def doesntcontain(elem: String, set: String) = "is(0,'/\\'('<<'(1," + elem + ")," + set + "))"
        def isElemUnion(elem: String, set: String, eset: String) = "is(" + eset + ",'\\/'('<<'(1," + elem + ")," + set + "))"
        def isElemUnion2(elem: String, set1: String, set2: String, eset: String) = "is(" + eset + ",'\\/'('\\/'('<<'(1," + elem + ")," + set1 + ")," + set2 + "))"
        def graphUnion(x: String, u: String, g1: String, g2: String) =
            "is(" + g2 + ",'\\/'(" + g1 + ",'<<'(" + u + ",*("+numLocks+"," + x + "))))"
        def lock(s: String) = name + "_lock(" + s + ")"

        for (n <- 0 until numLocks)
            out(name + "_lock(" + n + ").\n")

        nil("_","_","0", "0", "0", "0")

        ret("0", "0", "0", "0")

        call1(A1, U1, V1, G1, A1, U1, V1, G1)()
        base(A1, U1, V1, G1, A1, U1, V1, G1)()

        call2(A1, U1, V1, G1, A2, U2, V2, G2, A3, U3, V3, G3)(disjoint(A1, A2), isUnion(A1, A2, A3), isUnion(U1, U2, U3), isUnion(V1, V2, V3), isUnion(G1, G2, G3))

        use(X, A1, U1, V1, G1, A2, U2, V2, G2, A3, U3, V3, G3)(lock(X), disjoint(A1, A2), isUnion(A1, A2, A3), isElemUnion2(X, U1, U2, U3), isUnion(V1, V2, V3), isUnion(G1, G2, G3))

        acq(X, A1, U1, V1, G1, A2, U2, V1, G2)(lock(X), isElemUnion(X, A1, A2), isElemUnion(X, U1, U2), graphUnion(X, U1, G1, G2), doesntcontain(X, V1))

        spawn(A1, U1, V1, G1, A2, U2, V2, G2, A3, U3, V3, G3)(disjoint(A1, A2), isUnion(A1, A2, A3), isUnion(U1, U2, U3), isUnion2(V1, U1, V2, V3), isUnion(G1, G2, G3))
        //V1 subset U1 so isUnion2 could be replaced by isUnion

        out("%%% Defining final states \n")

        out(("%1$s_edge(G,X,Y) :- %1$s_lock(X), %1$s_lock(Y), not(is(X,Y)), not(is(0,'/\\'(G,'<<'(1,+(Y,*("+numLocks+",X)))))).\n").format(name))

        out(":-table(%1$s_path/3).\n".format(name))
        out("%1$s_path(G,X,Y) :- %1$s_edge(G,X,Y).\n".format(name))
        out("%1$s_path(G,X,Z) :- %1$s_path(G,X,Y),%1$s_edge(G,Y,Z).\n".format(name))

        out("%1$s_hasCycle(G) :- %1$s_lock(X),%1$s_path(G,X,X).\n".format(name))
        out(name + "_final(_,_,_,G) :- not(" + name + "_hasCycle(G)).\n")
        buf.toString

    }

    def call1(args: String*)(constr: String*)(implicit stuff: ImplStuff) {
        import stuff.buffer.{ append => out }
        out(stuff.prefix)
        out("_call1(")
        out(args.mkString(","))
        out(")")
        if (!constr.isEmpty) {
            out(" :- ")
            out(constr.mkString(" , "))
        }
        out(".\n")
    }
    def base(args: String*)(constr: String*)(implicit stuff: ImplStuff) {
        import stuff.buffer.{ append => out }
        out(stuff.prefix)
        out("_base(")
        out(args.mkString(","))
        out(")")
        if (!constr.isEmpty) {
            out(" :- ")
            out(constr.mkString(" , "))
        }
        out(".\n")
    }
    def call2(args: String*)(constr: String*)(implicit stuff: ImplStuff) {
        import stuff.buffer.{ append => out }
        out(stuff.prefix)
        out("_call2(")
        out(args.mkString(","))
        out(")")
        if (!constr.isEmpty) {
            out(" :- ")
            out(constr.mkString(" , "))
        }
        out(".\n")
    }
    def nil(args: String*)(implicit stuff: ImplStuff) {
        import stuff.buffer.{ append => out }
        out(stuff.prefix)
        out("_nil(")
        out(args.mkString(","))
        out(").\n")
    }
    def ret(args: String*)(implicit stuff: ImplStuff) {
        import stuff.buffer.{ append => out }
        out(stuff.prefix)
        out("_ret(")
        out(args.mkString(","))
        out(").\n")
    }
    def spawn(args: String*)(constr: String*)(implicit stuff: ImplStuff) {
        import stuff.buffer.{ append => out }
        out(stuff.prefix)
        out("_spawn(")
        out(args.mkString(","))
        out(")")
        if (!constr.isEmpty) {
            out(" :- ")
            out(constr.mkString(" , "))
        }
        out(".\n")
    }
    def use(args: String*)(constr: String*)(implicit stuff: ImplStuff) {
        import stuff.buffer.{ append => out }
        out(stuff.prefix)
        out("_use(")
        out(args.mkString(","))
        out(")")
        if (!constr.isEmpty) {
            out(" :- ")
            out(constr.mkString(" , "))
        }
        out(".\n")
    }
    def acq(args: String*)(constr: String*)(implicit stuff: ImplStuff) {
        import stuff.buffer.{ append => out }
        out(stuff.prefix)
        out("_acq(")
        out(args.mkString(","))
        out(")")
        if (!constr.isEmpty) {
            out(" :- ")
            out(constr.mkString(" , "))
        }
        out(".\n")
    }

    trait ImplStuff {
        val prefix: String
        val buffer: StringBuilder
    }

}

object testlockgen {
    def main(args: Array[String]) {
        object L1 extends PrologIntLockTA("lock1",2)
        object L2 extends PrologIntLockTA("lock2",1) 
        val inter = new PrologIntersectionTreeAutomata(L1,L2) with PrologBUEmptinessCheck
        println(inter.genScript)
        println(inter.emptiness)
    }
}