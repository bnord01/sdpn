package de.wwu.sdpn.pfg.genkill

import de.wwu.sdpn.pfg.fixedpoint.BasicFixedpointSolver
import de.wwu.sdpn.pfg.ParFlowGraph
import de.wwu.sdpn.pfg.lattices.genkill._
import de.wwu.sdpn.pfg.lattices._
import de.wwu.sdpn.pfg.Edge
import de.wwu.sdpn.pfg.BaseEdge
import de.wwu.sdpn.pfg.CallEdge
import de.wwu.sdpn.pfg.SpawnEdge

/**
 * This isn't finished!
 */
class PFGBackwardGenKillSolver[L: Lattice, P, N, BA, R, E <: Edge[P, N, BA, R]](pfg: ParFlowGraph[P, N, BA, R, E], transfer: E => GenKill[L]) extends BasicFixedpointSolver[PFGVar[L]] {

    import scala.collection.mutable.{ Map => MMap, Set => MSet }

    val R: MMap[N, LVar[L]] = MMap()
    val S: MMap[N, GKVar[L]] = MMap()
    val LS: MMap[N, GKVar[L]] = MMap()
    val SB: MMap[N, GKVar[L]] = MMap()
    val SBb: MMap[N, GKVar[L]] = MMap()
    val L: MMap[N, LVar[L]] = MMap()

    implicit val gklat = implicitly[GenKillLattice[L]]
    val lat = implicitly[Lattice[L]]

    protected def initialize(): Unit = {
        def entry(node:N) = pfg.entryNode(pfg.procOf(node))
        val ID = gklat.id
        
        for (node <- pfg.nodes) {
            R += node -> new LVar
            S += node -> new GKVar
            LS += node -> new GKVar
            SB += node -> new GKVar
            SBb += node -> new GKVar
            L += node -> new LVar
        }
        import GKExpr._
        import pfg._

        //Set up statements for main procedure
        val emain = pfg.entryNode(pfg.mainProc)
        addStmt(
            R(emain) ⊒ lat.bottom // R.init
        )

        //Set up statements for individual nodes 
        for (node <- pfg.nodes) {
            val ep = pfg.entryNode(pfg.procOf(node))
            addStmts(
                R(node) ⊒ (R(ep) andThen S(node)), // R.reach
                LS(node) ⊒ ID  // LS.init(2)    
                //L(node) ⊒ (lat.bottom andThen LS(node))
                )
            for(rnode <- retNodes(procOf(node)).values) {
                addStmts(
                	//L(node) ⊒ (SB(node) before L(rnode))
                )
                
            }

        }

        // Set up statements for initial nodes
        for (eNode <- pfg.entryNode.values) {
            addStmts(
                S(eNode) ⊒ ID, //S.init
                SB(eNode) ⊒ ID //SB'.init
            )
        }

        // Set up statements for return nodes 
        for(rnodes <- pfg.retNodes.values; rNode <- rnodes.values) {
            addStmts(
            	
            )
            
        }
        
        // Set up statements for edges
        for (e <- pfg.edges) {
            e match {
                case be: BaseEdge[P, N, BA, R] =>
                    addStmts(
                        S(be.snk) ⊒ (S(be.src) andThen transfer(e)), //S.base
                        LS(be.src) ⊒ (transfer(e) before LS(be.snk)), //LS.base
                        SB(be.snk) ⊒ ( SB(be.src) before transfer(e) ) //SB'.base // swichted order because of ambiguity of andThen here 
                        ,
                        L(be.src) ⊒ ( transfer(e) before L(be.snk) ) // L.base
                        
                        
                        
                    )
                case ce: CallEdge[P, N, BA, R] =>
                    for ((rval, snk) <- ce.returns) {
                        val rnode = retNodes(ce.proc)(rval)
                            addStmts(
                                S(snk) ⊒ (S(ce.src) andThen transfer(e) andThen S(rnode)),//S.call
                                LS(ce.src) ⊒ ( transfer(e) before LS(entryNode(ce.proc)) ), // LS.call1
                                LS(ce.src) ⊒ ( transfer(e) before SB(rnode) before LS(snk) ), // LS.call2 // modified
                                //SB(ce.src) ⊒ ( transfer(e) before SB(entryNode(ce.proc)) before SB(snk) ) // SB.call // fix this! only the appropriate sink!
                                SB(snk) ⊒ (SB(ce.src) andThen transfer(e) andThen SB(rnode)) //SB'.call
                                ,
                                L(ce.src) ⊒ ( transfer(e) before SB(rnode) before L(snk) ) //L.call2
                                ,
                                L(rnode) ⊒ L(snk) //L.ret                                
                            )
                        
                    }
                    val ep = pfg.entryNode(ce.proc)
                    addStmts(
                        R(ep) ⊒ (R(ce.src) andThen transfer(e)),
                        L(ce.src) ⊒ (transfer(e) before LS(ep) before lat.bottom)
                    )

                case se: SpawnEdge[P, N, BA, R] =>
                    addStmts(
                            S(se.snk) ⊒ (S(se.src) andThen transfer(e)), //S.spawn
                            R(pfg.entryNode(se.proc)) ⊒ (R(se.src) andThen transfer(e)), //R.spawn
                            LS(se.src) ⊒ ( transfer(e) before LS(entryNode(se.proc)) before LS(se.snk)), //LS.spawn
                            SB(se.snk) ⊒ ( SB(se.src) andThen transfer(e) andThen LS(entryNode(se.proc))) //SB'.spawn
                            ,
                            L(se.src) ⊒ (transfer(e) before LS(entryNode(se.proc)) before L(se.snk)) //L.spawn
                            )
            }
        }

    }

    def printResults: String = {
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

}