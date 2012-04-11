package de.wwu.sdpn.pfg.fixedpoint

class BasicFixedpointSolver[T] extends FixedpointSolver[T] {

    import scala.collection.mutable.{ Map => MMap, Set => MSet }

    private val dependendStatements: MMap[T, MSet[Statement[T]]] = MMap().withDefaultValue(MSet())
    private var wl: List[Statement[T]] = Nil
    private val ws: MSet[Statement[T]] = MSet()
    private var nbrOfStmts = 0

    override def solve(canceled: => Boolean = false) {
        while (!ws.isEmpty) {
            if (canceled)
                throw new RuntimeException("Canceled!")
            val st = wl.head
            wl = wl.tail
            ws -= st
            st.evaluate() match {
                case Changed 			=>
                    for (s <- dependendStatements(st.lhs))
                        if (!ws(s)) {
                            ws += s
                            wl = s :: wl
                        }
                case ChangedAndFixed 	=>
                    for (s <- dependendStatements(st.lhs))
                        if (!ws(s)) {
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

object BasicFixedpointSolverFactory extends FixedpointSolverFactory {
    def createSolver[T] = new BasicFixedpointSolver[T]
}