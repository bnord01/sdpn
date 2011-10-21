package de.wwu.sdpn.ta.prolog.reachability

/**
 * An tree automaton checking the acquisition history of an execution tree.
 * The automaton uses xsb integers to store lock sets and the acquition graph so it can handle
 * at most eight locks on a 64bit architecture.
 * 
 * @param name the name of the tree automaton
 * @param numLocks 1 to 8 the number of locks used by the execution tree which are labeled from 0 until numLocks. 
 * @author Benedikt Nordhoff
 */
class IntLockTA(val name: String, numLocks: Int) extends LockTreeAutomataAlphabet {
    require(numLocks <= 8 && numLocks > 0)
    val stateSize = 1

    val isForwardRule = Set("base", "call1", "nil", "ret")

    def genScript: String = {
        val buf = new StringBuilder()
        import buf.{ append => out }

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

        val A1 = "A1"
        val U1 = "U1"
        val G1 = "G1"
        val A2 = "A2"
        val U2 = "U2"
        val G2 = "G2"
        val A3 = "A3"
        val U3 = "U3"
        val G3 = "G3"
        val X = "X"
        	
        val allLocks = "2'" + ("1" * numLocks)
        
        
        def isDifference(X:String,Y:String,XmY:String) = XmY + (" is X '/\\' (Y '><' " + allLocks +")").replace("X",X).replace("Y",Y)
       
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

        out("%%% Definition of tree-automata for lock sensitive schedules: " + name + "\n")
        out("%%% Defining locks\n")
        for (n <- 0 until numLocks)
            out(lock(n.toString) + ".\n")
            
       out("""
%Check for 64 bit if more than 5 Locks!
:- (((numLocks > 5, not(xsb_configuration(word_size, '64')));numLocks>8)
	-> throw('ERROR TO MANY LOCKS!'); true).
       """.replace("numLocks",numLocks.toString))
            
        out("\n%%% Defining transitions\n")
        nil("_", "l(0,0,0)")!

        ret("l(0,0,0)")!

        call1(l(A1, U1, G1), l(A1, U1, G1))!

        base(l(A1, U1, G1), l(A1, U1, G1))!

        call2(l(A1, U1, G1), l(A2, U2, G2), l(A3, U3, G3)) :-
            (disjoint(A1, A2),
                isUnion(A1, A2, A3),
                isUnion(U1, U2, U3),
                isUnion(G1, G2, G3))!

        use(la(X,"0"), l(A1, U1, G1), l(A2, U2, G2), l(A3, U3, G3)) :-
            (lock(X), 
            		disjoint(A1, A2), 
            		isUnion(A1, A2, A3), 
            		isElemUnion2(X, U1, U2, U3), 
            		isUnion(G1, G2, G3))!
            		
        use(la("_","1"), l(A1, U1, G1), l(A2, U2, G2), l(A3, U3, G3)) :-
            (disjoint(A1, A2),
                isUnion(A1, A2, A3),
                isUnion(U1, U2, U3),
                isUnion(G1, G2, G3))!

        //TODO correct graphUnion
        acq(la(X,"0"), l(A1, U1, G1), l(A2, U2, G2)) :-
            (lock(X), isElemUnion(X, A1, A2), 
            		isElemUnion(X, U1, U2), 
            		graphUnion(X, U1, G1, G2), 
            		doesntcontain(X, U1))!
            		
        acq(la("_","1"), l(A1, U1, G1), l(A1, U1, G1))!

        spawn(l(A1, U1, G1), l(A2, U2, G2), l(A3, U3, G3)) :-
            (disjoint(A1, A2), 
            		isUnion(A1, A2, A3), 
            		isUnion(U1, U2, U3), 
            		isUnion(G1, G2, G3))!
            		
        //V1 subset U1 so isUnion2 could be replaced by isUnion

        out("""
%%% Defining final states of name
name_edge(G,X,Y) :- name_lock(X), name_lock(Y), not(0 is (G '/\' (1 '<<' (Y + (numLocks * X))))).
:-table(name_path/3).
name_path(G,X,Y) :- name_edge(G,X,Y).
name_path(G,X,Z) :- name_path(G,X,Y),name_edge(G,Y,Z).

name_hasCycle(G) :- name_lock(X),name_path(G,X,X).
name_final(l(_,_,G)) :- not(name_hasCycle(G)).
""".replace("name",name).replace("numLocks",numLocks.toString))


        buf.toString
    }

}

