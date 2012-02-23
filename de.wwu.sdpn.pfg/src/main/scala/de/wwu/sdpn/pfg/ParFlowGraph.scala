package de.wwu.sdpn.pfg

trait ParFlowGraph[P,N,BA,R,E<:Edge[P,N,BA,R]] {
	def edges : Set[E]
	def retNodes : Map[P,Map[R,Set[N]]]
	def nodes: Set[N]
	def procs: Set[P]
	def procOf : N => P
	def entryNode : Map[P,N]
	def mainProc: P
}

trait Edge[P,N,BA,R] {
    def src: N
    def ba: BA
}

trait BaseEdge[P,N,BA,R] extends Edge[P,N,BA,R] {
    def snk:N
}

trait CallEdge[P,N,BA,R] extends Edge[P,N,BA,R] {
    def proc:P    
    def returns:Map[R,N]
}

trait SpawnEdge[P,N,BA,R] extends Edge[P,N,BA,R] {
    def proc: P
    def snk:N
} 

