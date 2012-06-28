package de.wwu.sdpn.pfg.util

object Util {

    /**
     * Calculates the strongly connected components of the given graph.
     * The returned list contains a topological sorting of the DAG consisting
     * of the SCCs.
     * e.g.
     *
     * scc(Map(1->Set(2),2->Set(1,3),3->Set(4,5),4->Set[Int](),5->Set(3)))
     * =  List(Set(2, 1), Set(3, 5), Set(4))
     */
    def scc[T](graph: scala.collection.Map[T, Iterable[T]]): List[Set[T]] = {
        var unvisited = graph.keySet
        var maxdfs = 0
        var stack: List[T] = Nil
        var sccs: List[Set[T]] = Nil
        import scala.collection.mutable.{ Map => MMap, Set => MSet }

        val lowlink = MMap[T, Int]()
        val dfs = MMap[T, Int]()
        val onStack = MSet[T]()

        def tarjan(v: T) {
            dfs += v -> maxdfs
            lowlink += v -> maxdfs
            maxdfs += 1
            stack ::= v
            onStack += v
            unvisited -= v
            for (u <- graph(v)) {
                if (unvisited(u)) {
                    tarjan(u)
                    val ulowlink = lowlink(u)
                    if (ulowlink < lowlink(v))
                        lowlink += v -> ulowlink

                } else if (onStack(u)) {
                    val udfs = dfs(u)
                    if (udfs < lowlink(v))
                        lowlink += v -> udfs
                }
            }
            if (lowlink(v) == dfs(v)) {
                var vscc = Set[T]()
                var cur = stack.head
                do {
                    cur = stack.head
                    vscc += cur
                    onStack -= cur
                    stack = stack.tail
                } while (cur != v)
                sccs ::= vscc
            }
        }

        while (!unvisited.isEmpty) {
            val v = unvisited.head
            tarjan(v)
        }

        return sccs
    }

}