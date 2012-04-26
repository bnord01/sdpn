package de.wwu.sdpn.pfg.fixedpoint

trait FixedpointSolver[T,-S <: Statement[T]] {
    def addStmts(stmt: S*):Unit    
    
    def solve(canceled : =>Boolean):Unit
    
    def getNumberOfStatements:Int
}

//trait FixedpointSolverFactory[S] {
//    def createSolver[T]:FixedpointSolver[T,Statement[T] with S]
//}