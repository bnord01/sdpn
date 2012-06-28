package de.wwu.sdpn.test.pfg
import org.junit._
import org.junit.Assert._
import de.wwu.sdpn.pfg.util.Util
import scala.util.Random

class SCCTest {
    val rnd = new Random(123)
    import rnd.nextInt

    //    @Test
    def testOneGraph {
        val g = Map(0 -> Set(1), 1 -> Set(2), 2 -> Set(0))
        val result = Util.scc(g)
        check(g, result)

    }

    @Test
    def testSomeGraphs {
        for (nodes <- 2 to 5; edges <- 1 to nodes; iteration <- 1 to nodes * nodes * 100) {
            val graph = ranGraph(nodes, edges)
            val result = Util.scc(graph)
            check(graph, result)
        }
    }

    def ranGraph(nodes: Int, maxEdges: Int): Map[Int, Set[Int]] = {
        var g = Map[Int, Set[Int]]()
        for (i <- 0 until nodes) {
            var x = Set[Int]()
            for (j <- 1 until nextInt(maxEdges)) {
                x += nextInt(nodes)
            }
            g += i -> x
        }
        g
    }

    def check(graph: Map[Int, Set[Int]], result: List[Set[Int]]) {
        val nodes = graph.keySet
        val tcl = trcl(graph)
        for (u <- nodes; v <- nodes) {
            if (tcl(u)(v) && tcl(v)(u)) {
                // they are in the same SCC check such one exists
                assert(result.exists(p => p(u) && p(v)), "There exists no SCC for %s and %s %nGraph: %s %nSCCs: %s".format(u, v, graph, result))
            }
            if (result.exists(p => p(u) && p(v))) {
                assert(tcl(u)(v) && tcl(v)(u), "Not really in the same SCC: %s and %s %nGraph: %s %nSCCs: %s".format(u, v, graph, result))
            }
        }

    }

    def trcl(graph: Map[Int, Set[Int]]): Map[Int, Set[Int]] = {
        var t = graph
        for (x <- t.keySet)
            t += x -> (t(x) + x)
        var change = true
        while (change) {
            change = false
            for ((u, vs) <- t; v <- vs; w <- t(v)) {
                if (!t(u)(w)) {
                    change = true
                    t += u -> (t(u) + w)
                }
            }
        }
        t
    }

}