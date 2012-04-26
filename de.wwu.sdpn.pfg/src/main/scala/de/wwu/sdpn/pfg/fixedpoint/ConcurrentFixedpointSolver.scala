package de.wwu.sdpn.pfg.fixedpoint
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.Executors

class ConcurrentFixedpointSolver[T] extends FixedpointSolver[T,Statement[T]] {	
    import scala.collection.mutable.{ Map => MMap, Set => MSet }

    private val dependendStatements: MMap[T, MSet[Statement[T]]] = MMap().withDefaultValue(MSet())
    private var nbrOfStmts = 0

    private val numActive = new AtomicLong()
    private var toSchedule: List[Runnable] = Nil
    
    val pool = Executors.newFixedThreadPool(8)

    override def solve(canceled: => Boolean = false) {
        toSchedule foreach {x => numActive incrementAndGet; pool execute x}
        while(numActive.longValue() > 0) {
            Thread.sleep(10)
        }
    }

    override def addStmts(stmts: Statement[T]*) {
        for (stmt <- stmts) {
            nbrOfStmts += 1
            for (v <- stmt.rhs) {
                dependendStatements.getOrElseUpdate(v, MSet()) += stmt
            }
            toSchedule ::= mkTask(stmt)
        }
    }

    def addStmt(stmt: Statement[T]) {
        nbrOfStmts += 1
        for (v <- stmt.rhs) {
            dependendStatements.getOrElseUpdate(v, MSet()) += stmt
        }
        toSchedule ::= mkTask(stmt)
    }

    def mkTask(stmt: Statement[T]): Runnable = new Runnable {
        override def run() {
            stmt.evaluate() match {
                case Changed =>
                    for (s <- dependendStatements(stmt.lhs)) {
                        numActive incrementAndGet;                        
                        pool execute mkTask(s)
                    }
                case ChangedAndFixed =>
                    for (s <- dependendStatements(stmt.lhs)) {
                        numActive incrementAndGet;                        
                        pool execute mkTask(s)
                    }
                case NotChanged         =>
                // Do nothing
                case NotChangedAndFixed =>
                //Do nothing
            }
            numActive decrementAndGet
        }
    }

    def getNumberOfStatements = nbrOfStmts
}
//object ConcurrentFixedpointSolverFactory extends FixedpointSolverFactory {
//    def createSolver[T] = new ConcurrentFixedpointSolver[T]
//}