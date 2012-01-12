package de.wwu.sdpn.wala.mnfs

import com.ibm.wala.util.strings.StringStuff
import com.ibm.wala.ipa.callgraph.CallGraph
import scala.collection.Set
import scala.collection.JavaConversions._
import de.wwu.sdpn.wala.dpngen.symbols.GlobalState
import de.wwu.sdpn.wala.dpngen.symbols.StackSymbol
import de.wwu.sdpn.wala.dpngen.symbols.NState

/**
 * Generate M nondeterministic finite state machine with P = {N,P}
 * which can be translated to Prolog
 * @deprecated
 * @author Benedikt Nordhoff
 */
object ConflictMNFSGenerator {

    def generateConflictSet4Method(cg: CallGraph, methodSig: String) = {
        val mr = StringStuff.makeMethodReference(methodSig)
        val cgnodes = cg.getNodes(mr)
        var cset = Set[(GlobalState, StackSymbol)]()
        for (node <- cgnodes) {
            cset += ((NState, StackSymbol(node, 0, 0)))
        }
        cset
    }

    def generateMNFS[P, G](pset: Set[P], gset: Set[G], cset1: Set[(P, G)], cset2: Set[(P, G)]): MNFS[P, G, Int, Int] = {
        val mnfs = new MNFS[P, G, Int, Int](pset, gset)

        mnfs.setStart(0)
        for (p <- pset) {
            //empty transition from S0_p back to S0
            mnfs.addSPSEdge(0, p, 0)
            for (g <- gset) {
                if (cset1(p, g)) {
                    //g1 transition from S0_p1 to T2
                    mnfs.addSPTEdge(0, p, g, 2)
                }
                if (cset2(p, g)) {
                    //g2 transition from S0_p2 to T3
                    mnfs.addSPTEdge(0, p, g, 3)
                } else if (!cset1(p, g)) {
                    //g transition from SP_p to T1
                    mnfs.addSPTEdge(0, p, g, 1)
                }
            }
        }

        //G loops for T1,T2,T3
        for (g <- gset) {
            mnfs.addTTEdge(1, g, 1)
            mnfs.addTTEdge(2, g, 2)
            mnfs.addTTEdge(3, g, 3)
        }

        //empty transition from T1 back to S0
        mnfs.addTSEdge(1, 0)

        //now waiting for (p2,g2) in S1
        mnfs.addTSEdge(2, 1)
        for (p <- pset) {
            //empty transition from S1_p back to S1
            mnfs.addSPSEdge(1, p, 1)
            for (g <- gset) {
                if (cset2(p, g)) {
                    //g2 transition from S1_p2 to T6
                    mnfs.addSPTEdge(1, p, g, 6)
                } else {
                    //g transition from S1_p to T4
                    mnfs.addSPTEdge(1, p, g, 4)
                }
            }
        }
        for (g <- gset) {
            mnfs.addTTEdge(4, g, 4)
        }
        //empty transition from T4 back to S1
        mnfs.addTSEdge(4, 1)

        //waiting for (p1,g1) in S2
        //empty transition from T3 to S2
        mnfs.addTSEdge(3, 2)
        for (p <- pset) {
            //empty transition from S2_p back to S2
            mnfs.addSPSEdge(2, p, 2)
            for (g <- gset) {
                if (cset1(p, g)) {
                    //g1 transition from S2_p1 to T6
                    mnfs.addSPTEdge(2, p, g, 6)
                } else {
                    //g transition from S2_p to T5
                    mnfs.addSPTEdge(2, p, g, 5)
                }
            }
        }
        for (g <- gset) {
            mnfs.addTTEdge(5, g, 5)
        }
        //empty transition from T5 back to S2
        mnfs.addTSEdge(5, 2)

        for (g <- gset) {
            mnfs.addTTEdge(6, g, 6)
        }

        //empty transition from T6 to S3
        mnfs.addTSEdge(6, 3)

        for (p <- pset) {
            //empty transition from S3_p back to S3
            mnfs.addSPSEdge(3, p, 3)
            //set S3_p final
            mnfs.setFinalSP(3, p)
            for (g <- gset) {
                //g transition from S3_p to T6
                mnfs.addSPTEdge(3, p, g, 6)

            }
        }
        mnfs.setFinalT(6);

        return mnfs
    }

    def generateConflictSet4Method2(cg: CallGraph, methodSig: String) = {
        val mr = StringStuff.makeMethodReference(methodSig)
        val cgnodes = cg.getNodes(mr)
        var cset = Set[StackSymbol]()
        for (node <- cgnodes) {
            cset += StackSymbol(node, 0, 0)
        }
        cset
    }
    
    def generateTA(gmap: Map[GlobalState, Int], smap: Map[StackSymbol, Int], cset: Set[StackSymbol], startConf: (GlobalState, StackSymbol)): StringBuffer = {
        val rev = new StringBuffer()
        import rev.{ append => out }
        out(
"""
%%% Defining automata
:- table(ta/5).
%% ta (startstate,stack,endstate,badcount,terminated)

ta(X,_,X,0,0). %% all leafs are not empty

""")
        
        for (s <- cset) {
        	out("ta(X," + smap(s) + ",X,1,0). %a bad state!!\n")        	
        }
        
        out("""

%%%BaseSaturation%%%
ta(P,G,Pf,C,T) :- base_rule(P,G,_,P1,G1), ta(P1,G1,Pf,C,T).
%%%PopSaturation%%%
ta(P,G,Pf,0,1) :- pop_rule(P,G,_,Pf).
%%%PushSaturation%%%
ta(P,G,Pf,C,T) :- push_rule(P,G,_,P1,G1,G2), ta(P1,G1,Pc,0,1), ta(Pc,G2,Pf,C,T).
ta(P,G,Pf,2,T) :- push_rule(P,G,_,P1,G1,G2), ta(P1,G1,Pc,1,1), ta(Pc,G2,Pf,1,T).
ta(P,G,Pf,2,T) :- push_rule(P,G,_,P1,G1,G2), ta(P1,G1,Pc,2,1), ta(Pc,G2,Pf,_,T).
ta(P,G,Pf,2,T) :- push_rule(P,G,_,P1,G1,G2), ta(P1,G1,Pc,_,1), ta(Pc,G2,Pf,2,T).
ta(P,G,Pf,C,T) :- push_rule(P,G,_,P1,G1,G2), ta(P1,G1,Pc,C,1), ta(Pc,G2,Pf,0,T).
ta(P,G,Pf,C,0) :- push_rule(P,G,_,P1,G1,G2), ta(P1,G1,Pc,C,0).
%%%SpawnSaturation%%%
ta(P,G,Pf,C,T) :- spawn_rule(P,G,_,P1,G1,P2,G2), ta(P2,G2,_,0,_), ta(P1,G1,Pf,C,T).
ta(P,G,Pf,2,T) :- spawn_rule(P,G,_,P1,G1,P2,G2), ta(P2,G2,_,1,_), ta(P1,G1,Pf,1,T).
ta(P,G,Pf,C,T) :- spawn_rule(P,G,_,P1,G1,P2,G2), ta(P2,G2,_,C,_), ta(P1,G1,Pf,0,T).
ta(P,G,Pf,2,T) :- spawn_rule(P,G,_,P1,G1,P2,G2), ta(P2,G2,_,2,_), ta(P1,G1,Pf,_,T).
ta(P,G,Pf,2,T) :- spawn_rule(P,G,_,P1,G1,P2,G2), ta(P2,G2,_,_,_), ta(P1,G1,Pf,2,T).


%%% Check if Startconf is Accepted
conflict :- cputime(X0),
""" )
	val (g, s) = startConf
     out(   "ta(" + gmap(g) + ", " + smap(s) + ", _,2,_),")
out( """
 cputime(X1), X is X1 - X0, write('cputime: '), write(X), nl.

""")
        return rev
    }
    
    def generateSymbolicMNFS(gmap: Map[GlobalState, Int], smap: Map[StackSymbol, Int], cset: Set[StackSymbol], startConf: (GlobalState, StackSymbol)): StringBuffer = {
        val rev = new StringBuffer()
        import rev.{ append => out }
        val gset = gmap.keySet
        val rset = smap.keySet -- cset
        out(
"""
%%% Defining automata

""")

        for (c <- cset)
            out("confset(" + smap(c) + ").\n")

        for (r <- rset)
            out("restset(" + smap(r) + ").\n")

        val smax = 2
        var tmax = 4

        for (s <- 0 to smax; p <- gset) {
            out("sp(" + s + ", " + gmap(p) + ", " + tmax + ").\n")
            out("ts(" + tmax + ", " + s + ").\n")
            tmax += 1
        }

        out(
            """

allStack(C) :- confset(C).
allStack(R) :- restset(R).

:- table(tt/3).

tt(SP,R,0) :- sp(0,_,SP),restset(R).
ts(0,0).
tt(0,G,0) :- allStack(G).


tt(SP,C,1) :- sp(0,_,SP),confset(C).
tt(1,G,1) :- allStack(G).
ts(1,1).

tt(SP,R,2) :- sp(1,_,SP),restset(R).
tt(2,G,2) :- allStack(G).
ts(2,1).

tt(SP,C,3) :- sp(1,_,SP), confset(C).
tt(3,G,3) :- allStack(G).
ts(3,2).

tt(SP,G,3) :- sp(2,_,SP), allStack(G).

final(3).
final(SP) :- sp(2,_,SP).

%%%BaseSaturation%%%
tt(SP,G,T) :- base_rule(P,G,_,P1,G1), sp(S,P1,SP1), tt(SP1,G1,T), sp(S,P,SP).
%%%PopSaturation%%%
tt(SP,G,SP1) :- pop_rule(P,G,_,P1), sp(S,P1,SP1), sp(S,P,SP).
%%%PushSaturation%%%
tt(SP,G,T) :- push_rule(P,G,_,P1,G1,G2), sp(S,P1,SP1), tt(SP1,G1,T1), tt(T1,G2,T), sp(S,P,SP).
%%%SpawnSaturation%%%
tt(SP,G,T) :- spawn_rule(P,G,_,P1,G1,P2,G2), sp(S,P2,SP2), tt(SP2,G2,T1), ts(T1,S1), sp(S1,P1,S1P1), tt(S1P1,G1,T), sp(S,P,SP).
""")

        val (g, s) = startConf
        out("\n%%% Check if Startconf is Accepted\n")
        out("conflict :- cputime(X0), tt( SP, " + smap(s) + ", F),sp(0," + gmap(g) + ",SP), final(F),\n" +
            " cputime(X1), X is X1 - X0, write('cputime: '), write(X), nl.\n")

        return rev
    }

    def generateSymbolicDatalogMNFS(gmap: Map[GlobalState, Int], smap: Map[StackSymbol, Int], cset: Set[StackSymbol], startConf: (GlobalState, StackSymbol)): StringBuffer = {
        val rev = new StringBuffer()
        import rev.{ append => out }
        val gset = gmap.keySet
        val rset = smap.keySet -- cset
        out(
"""
.include "dpn.datalog"

### Defining automata
S 3
T """ + (4 + 3*gset.size) + """

confset(c:STACK)
restset(r:STACK)
allStack(g:STACK)
sp(s:S,p:GLOBAL,sp:T)
tt(t1:T,g:STACK,t2:T)
ts(t:T,s:S)
final(t:T)

""")

        for (c <- cset)
            out("confset(" + smap(c) + ").\n")

        for (r <- rset)
            out("restset(" + smap(r) + ").\n")

        val smax = 2
        var tmax = 4

        for (s <- 0 to smax; p <- gset) {
            out("sp(" + s + ", " + gmap(p) + ", " + tmax + ").\n")
            out("ts(" + tmax + ", " + s + ").\n")
            tmax += 1
        }

        out(
            """


allStack(C) :- confset(C).
allStack(R) :- restset(R).


tt(SP,R,0) :- sp(0,_,SP),restset(R).
ts(0,0).
tt(0,G,0) :- allStack(G).


tt(SP,C,1) :- sp(0,_,SP),confset(C).
tt(1,G,1) :- allStack(G).
ts(1,1).

tt(SP,R,2) :- sp(1,_,SP),restset(R).
tt(2,G,2) :- allStack(G).
ts(2,1).

tt(SP,C,3) :- sp(1,_,SP), confset(C).
tt(3,G,3) :- allStack(G).
ts(3,2).

tt(SP,G,3) :- sp(2,_,SP), allStack(G).

final(3).
final(SP) :- sp(2,_,SP).

##%%%BaseSaturation%%%
tt(SP,G,T) :- base_rule(P,G,_,P1,G1), sp(S,P1,SP1), tt(SP1,G1,T), sp(S,P,SP).
###%%%PopSaturation%%%
tt(SP,G,SP1) :- pop_rule(P,G,_,P1), sp(S,P1,SP1), sp(S,P,SP).
###%%%PushSaturation%%%
tt(SP,G,T) :- push_rule(P,G,_,P1,G1,G2), sp(S,P1,SP1), tt(SP1,G1,T1), tt(T1,G2,T), sp(S,P,SP).
###%%%SpawnSaturation%%%
tt(SP,G,T) :- spawn_rule(P,G,_,P1,G1,P2,G2), sp(S,P2,SP2), tt(SP2,G2,T1), ts(T1,S1), sp(S1,P1,S1P1), tt(S1P1,G1,T), sp(S,P,SP).
""")

        val (g, s) = startConf
        out("\n### Check if Startconf is Accepted\n")
        out("tt( SP, " + smap(s) + ", F),sp(0," + gmap(g) + ",SP), final(F)?\n")

        return rev
    }
}