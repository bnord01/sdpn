package de.wwu.sdpn.pfg.fixedpoint

trait FixedpointSolver[T] {    
    protected def initialize():Unit
    
    def addStmts(stmt: Statement[T]*):Unit    
    
    def solve(canceled : =>Boolean):Unit
}