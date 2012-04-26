package de.wwu.sdpn.pfg.fixedpoint

class ParFixedpointSolver[T] extends FixedpointSolver[T, Statement[T]] {

    import scala.collection.mutable.{ Map => MMap, Set => MSet }
    import scala.collection.parallel.mutable.{ ParSet }

    private val dependendStatements: MMap[T, MSet[Statement[T]]] = MMap().withDefaultValue(MSet())
    private var ws: ParSet[Statement[T]] = ParSet()
    private var nbrOfStmts = 0

    private def schedule(s: Statement[T]): Unit = synchronized {
        ws += s
    }

    override def solve(canceled: => Boolean = false) {
        while (!ws.isEmpty) {
            val oldWs = ws
            ws = ParSet()
            if (canceled)
                throw new RuntimeException("Canceled!")
            for (st <- oldWs) {
                st.evaluate() match {
                    case Changed =>
                        for (s <- dependendStatements(st.lhs))
                            schedule(s)
                    case ChangedAndFixed =>
                        for (s <- dependendStatements(st.lhs))
                            schedule(s)
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
        }
    }

    def addStmt(stmt: Statement[T]) {
        nbrOfStmts += 1
        for (v <- stmt.rhs) {
            dependendStatements.getOrElseUpdate(v, MSet()) += stmt
        }
        ws += stmt
    }

    def getNumberOfStatements = nbrOfStmts
}

//object BasicFixedpointSolverFactory extends FixedpointSolverFactory {
//    def createSolver[T] = new BasicFixedpointSolver[T]
//}