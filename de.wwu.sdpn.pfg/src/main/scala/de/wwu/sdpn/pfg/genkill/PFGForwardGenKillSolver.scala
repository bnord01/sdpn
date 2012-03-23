package de.wwu.sdpn.pfg.genkill

import de.wwu.sdpn.pfg.fixedpoint.BasicFixedpointSolver
import de.wwu.sdpn.pfg.ParFlowGraph
import de.wwu.sdpn.pfg.lattices.genkill._
import de.wwu.sdpn.pfg.lattices._
import de.wwu.sdpn.pfg.Edge
import de.wwu.sdpn.pfg.BaseEdge
import de.wwu.sdpn.pfg.CallEdge
import de.wwu.sdpn.pfg.SpawnEdge
import de.wwu.sdpn.pfg.fixedpoint.FixedpointSolver

class PFGForwardGenKillSolver[L: Lattice, P, N, BA, R, E <: Edge[P, N, BA, R]](
        pfg: ParFlowGraph[P, N, BA, R, E],
        transfer: E => GenKill[L],
        flowSolver: Option[FixedpointSolver[PFGVar[L]]] = None) {

    import scala.collection.mutable.{ Map => MMap, Set => MSet }

    private val solver = flowSolver.getOrElse(new BasicFixedpointSolver[PFGVar[L]])

    import solver.addStmts

    private val R: MMap[N, LVar[L]] = MMap()
    private val S: MMap[N, GKVar[L]] = MMap()
    private val LS: MMap[N, LVar[L]] = MMap()
    private val SB: MMap[N, LVar[L]] = MMap()
    private val L: MMap[N, LVar[L]] = MMap()
    private val SP: MMap[N, LVar[L]] = MMap()
    private val PI: MMap[N, LVar[L]] = MMap()

    //    val R: MMap[N, LVar[L]] = MMap()
    //    val S: MMap[N, GKVar[L]] = MMap()
    //    val LS: MMap[N, LVar[L]] = MMap()
    //    val SB: MMap[N, LVar[L]] = MMap()
    //    val L: MMap[N, LVar[L]] = MMap()
    //    val SP: MMap[N, LVar[L]] = MMap()
    //    val PI: MMap[N, LVar[L]] = MMap()

    implicit val gklat = implicitly[GenKillLattice[L]]
    val lat = implicitly[Lattice[L]]

    protected def initialize(): Unit = {
        def entry(node: N) = pfg.entryNode(pfg.procOf(node))
        val ID = gklat.id

        for (node <- pfg.nodes) {
            R += node -> new LVar
            S += node -> new GKVar
            LS += node -> new LVar
            SB += node -> new LVar
            L += node -> new LVar
            SP += node -> new LVar
            PI += node -> new LVar
        }
        import GKExpr._
        import pfg._

        //Set up statements for main procedure
        val emain = pfg.entryNode(pfg.mainProc)
        addStmts(
            R(emain) ⊒ lat.bottom // R.init
        )

        //Set up statements for individual nodes 
        for (node <- pfg.nodes) {
            val ep = pfg.entryNode(pfg.procOf(node))
            addStmts(
                R(node) ⊒ (R(ep) andThen S(node)), // R.reach
                LS(node) ⊒ lat.bottom // LS.init(2)    
                //L(node) ⊒ (lat.bottom andThen LS(node))
                ,
                PI(node) ⊒ (PI(ep) join SP(node)) // PI.reach
            )
            for (rnode <- retNodes(procOf(node)).values) {
                addStmts( //L(node) ⊒ (SB(node) before L(rnode))
                )

            }

        }

        // Set up statements for initial nodes
        for (eNode <- pfg.entryNode.values) {
            addStmts(
                S(eNode) ⊒ ID, //S.init
                SB(eNode) ⊒ lat.bottom //SB'.init
            )
        }

        // Set up statements for return nodes 
        for (rnodes <- pfg.retNodes.values; rNode <- rnodes.values) {
            addStmts()

        }

        // Set up statements for edges
        for (e <- pfg.edges) {
            e match {
                case be: BaseEdge[P, N, BA, R] =>
                    addStmts(
                        S(be.snk) ⊒ (S(be.src) andThen transfer(e)), //S.base
                        LS(be.src) ⊒ (LConst(transfer(e).gen) join LS(be.snk)), //LS.base
                        SB(be.snk) ⊒ (LConst(transfer(e).gen) join SB(be.src)) //SB'.base 
                        ,
                        L(be.src) ⊒ (LConst(transfer(e).gen) join L(be.snk)) // L.base
                        ,
                        SP(be.snk) ⊒ SP(be.src) //sp.edge (base)
                    )
                case ce: CallEdge[P, N, BA, R] =>
                    for ((rval, snk) <- ce.returns) {
                        val rnode = retNodes(ce.proc)(rval)
                        addStmts(
                            S(snk) ⊒ (S(ce.src) andThen transfer(e) andThen S(rnode)), //S.call
                            LS(ce.src) ⊒ (LConst(transfer(e).gen) join SB(rnode) join LS(snk)), // LS.call2 // modified
                            //SB(ce.src) ⊒ ( transfer(e) before SB(entryNode(ce.proc)) before SB(snk) ) // SB.call // fix this! only the appropriate sink!
                            SB(snk) ⊒ (SB(ce.src) join transfer(e).gen join SB(rnode)) //SB'.call
                            ,
                            L(ce.src) ⊒ (LConst(transfer(e).gen) join SB(rnode) join L(snk)) //L.call2
                            ,
                            L(rnode) ⊒ L(snk) //L.ret     
                            ,
                            SP(snk) ⊒ (SP(ce.src) join SP(rnode)) //SP.call

                        )

                    }
                    val ep = pfg.entryNode(ce.proc)
                    addStmts(
                        R(ep) ⊒ (R(ce.src) andThen transfer(e)),
                        L(ce.src) ⊒ (LConst(transfer(e).gen) join LS(ep)) //L.call1
                        ,
                        LS(ce.src) ⊒ (LConst(transfer(e).gen) join LS(ep)) // LS.call1    
                        ,
                        PI(ep) ⊒ PI(ce.src) // PI.trans1
                    )

                case se: SpawnEdge[P, N, BA, R] =>
                    val eq = pfg.entryNode(se.proc)
                    addStmts(
                        S(se.snk) ⊒ (S(se.src) andThen transfer(e)), //S.spawn
                        R(pfg.entryNode(se.proc)) ⊒ (R(se.src) andThen transfer(e)), //R.spawn
                        LS(se.src) ⊒ (LConst(transfer(e).gen) join LS(entryNode(se.proc)) join LS(se.snk)), //LS.spawn
                        SB(se.snk) ⊒ (SB(se.src) join transfer(e).gen join LS(entryNode(se.proc))) //SB'.spawn
                        ,
                        L(se.src) ⊒ (LConst(transfer(e).gen) join LS(entryNode(se.proc)) join L(se.snk)) //L.spawn
                        ,
                        SP(se.snk) ⊒ LS(eq) //SP.spawnt
                        ,
                        SP(se.snk) ⊒ SP(se.src) //SP.edge (spawn)
                        ,
                        PI(eq) ⊒ PI(se.src) //PI.trans2
                        ,
                        PI(eq) ⊒ L(se.snk) //PI.trans3
                    )
            }
        }

    }

    def solve(canceled: => Boolean) {
        initialize()
        solver.solve(canceled)

    }

    def result(n: N): L = R(n).elem ⊔ PI(n).elem
    def resultPI(n: N): L = PI(n).elem

    def results: Map[N, L] = Map() ++ (for (n <- pfg.nodes) yield n -> (R(n).elem ⊔ PI(n).elem))

    def resultsPI: Map[N, L] = Map() ++ (for (n <- pfg.nodes) yield n -> (PI(n).elem))

    def printSResults: String = {
        val buf = new StringBuffer
        def outln(s: Any) = { buf.append(s.toString()); buf.append("\n") }
        def out(s: Any) = buf.append(s)
        for ((n, s) <- S) {
            out("λl. ( l ⊓ " + s.kill + " ) ⊔ " + s.gen)
            out(" for : ")
            outln(n)
        }

        buf.toString()
    }
    def printResults: String = {
        val buf = new StringBuffer
        def outln(s: Any) = { buf.append(s.toString()); buf.append("\n") }
        def out(s: Any) = buf.append(s)
        for (n <- pfg.nodes) {
            out(result(n))
            out(" for : ")
            outln(n)
        }
        buf.toString()
    }

    def getNumberOfStatements = solver.getNumberOfStatements
}