package de.wwu.sdpn.pfg.util

object Util {
    def scc[T](graph: Map[T, Set[T]]):Set[Set[T]] = {
        var unvisited = graph.keySet
        var maxdfs = 0
        var stack: List[T] = Nil
        var sccs: Set[Set[T]] = Set()
        import scala.collection.mutable.{ Map => MMap, Set => MSet }
        
        val lowlink = MMap[T,Int]()
        val dfs = MMap[T,Int]()
        val onStack = MSet[T]()
        def tarjan(v: T) {
            dfs += v -> maxdfs
            lowlink += v -> maxdfs
            maxdfs += 1
            stack ::= v
            onStack += v
            unvisited -= v
            for(u <- graph(v)) {
                if(unvisited(u)) {
                    tarjan(u)
                } 
                else if (onStack(u)) {
                    val udfs = dfs(u)
                    if(udfs < lowlink(v))
                        lowlink += v -> udfs
                }
            }
            if(lowlink(v) == dfs(v)) {
                var vscc = Set[T]()
                var cur = stack.head
                do {
                    cur = stack.head
                    vscc += cur
                    onStack -= cur
                    stack = stack.tail                    
                } while (cur != v)
                sccs += vscc
            }
        }

        while (!unvisited.isEmpty) {
            val v = unvisited.head
            tarjan(v)
        }

        return sccs
    }

}