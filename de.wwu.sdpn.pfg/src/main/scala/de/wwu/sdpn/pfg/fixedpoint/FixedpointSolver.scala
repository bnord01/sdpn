package de.wwu.sdpn.pfg.fixedpoint

trait FixedpointSolver[T] {        
    def addStmts(stmt: Statement[T]*):Unit    
    
    def solve(canceled : =>Boolean):Unit
    
    def getNumberOfStatements:Int
}

trait FixedpointSolverFactory {
    def createSolver[T]:FixedpointSolver[T]
}