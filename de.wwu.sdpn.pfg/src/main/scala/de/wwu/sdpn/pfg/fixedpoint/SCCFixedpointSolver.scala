package de.wwu.sdpn.pfg.fixedpoint

import de.wwu.sdpn.pfg.util.Util

/**
 * Fixedpoint solver based on saturation of strongly connected components. 
 * There is something bad here!
 */
class SCCFixedpointSolver[T] extends FixedpointSolver[T, Statement[T]] {

    import scala.collection.mutable.{ Map => MMap, Set => MSet }

    private val dependendStatements: MMap[T, MSet[Statement[T]]] = MMap().withDefaultValue(MSet())
    private var wl: List[Statement[T]] = Nil
    private val ws: MSet[Statement[T]] = MSet()
    private var nbrOfStmts = 0

    override def solve(canceled: => Boolean = false) {
        val t0 = System.currentTimeMillis
        // Build the dependency graph, t -> s iff t defines a variable read by s
        val graph: MMap[Statement[T], MSet[Statement[T]]] = MMap() ++ ws.map(x => (x -> dependendStatements(x.lhs)))
        val t1 = System.currentTimeMillis
        // Compute the strongly connected components, in topologically sorted list. 
        val sccs = Util.scc(graph)
        println("Number of sccs:      " + sccs.size)
        println("Biggest scc:         " + sccs.map(_.size).max)
        println("Time computing sccs: " + (System.currentTimeMillis - t1))
        println("Time graph            " + (t1-t0))
        for (scc <- sccs) {
            ws.clear
            ws ++= scc
            wl = Nil
            wl ++= scc
            while (!ws.isEmpty) {
                if (canceled)
                    throw new RuntimeException("Canceled!")
                val st = wl.head
                wl = wl.tail
                ws -= st
                st.evaluate() match {
                    case Changed =>
                        for (s <- dependendStatements(st.lhs))
                            if (!ws(s) && scc(s)) {
                                ws += s
                                wl = s :: wl
                            }
                    case ChangedAndFixed =>
                        for (s <- dependendStatements(st.lhs))
                            if (!ws(s) && scc(s)) {
                                ws += s
                                wl = s :: wl
                            }
                    case NotChanged         =>
                    // Do nothing
                    case NotChangedAndFixed =>
                    //Do nothing
                }
            }
        }
    }

    override def addStmts(stmts: Statement[T]*) {
        for (stmt <- stmts) {
            nbrOfStmts += 1
            for (v <- stmt.rhs) {
                dependendStatements.getOrElseUpdate(v, MSet()) += stmt
            }
            ws += stmt
            wl = stmt :: wl
        }
    }

    def addStmt(stmt: Statement[T]) {
        nbrOfStmts += 1
        for (v <- stmt.rhs) {
            dependendStatements.getOrElseUpdate(v, MSet()) += stmt
        }
        ws += stmt
        wl = stmt :: wl
    }

    def getNumberOfStatements = nbrOfStmts
}