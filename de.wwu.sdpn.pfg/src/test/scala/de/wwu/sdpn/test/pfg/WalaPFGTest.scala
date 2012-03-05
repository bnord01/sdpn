package de.wwu.sdpn.test.pfg
import de.wwu.sdpn.wala.util.PreAnalysis
import de.wwu.sdpn.wala.analyses.MyPreAnalysis
import org.junit.BeforeClass
import de.wwu.sdpn.pfg.wala.PFGFactory
import org.junit.Test
import de.wwu.sdpn.pfg.wala.WalaPFG
import de.wwu.sdpn.pfg.wala.BaseEdge
import de.wwu.sdpn.pfg.wala.SpawnEdge
import de.wwu.sdpn.pfg.wala.CallEdge
import com.ibm.wala.util.strings.StringStuff
import scala.collection.JavaConversions._
import de.wwu.sdpn.pfg.GenKillFunction
import de.wwu.sdpn.pfg.GenKill
import de.wwu.sdpn.pfg.PFGGenKillSolver
import de.wwu.sdpn.pfg.wala.Edge
import org.junit.Assert._
import de.wwu.sdpn.pfg.genkill.PFGForwardGenKillSolver
import de.wwu.sdpn.pfg.wala.Node
import de.wwu.sdpn.pfg.wala.N
import de.wwu.sdpn.pfg.wala.CFGPoint
import de.wwu.sdpn.pfg.lattices.LMap
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import de.wwu.sdpn.pfg.wala.SSAAction
import com.ibm.wala.ssa.SSAPutInstruction
import com.ibm.wala.types.FieldReference

object WalaPFGTest {
    var pa: PreAnalysis = null
    @BeforeClass
    def setUp {
        pa = MyPreAnalysis.getStd
    }

}

class WalaPFGTest {
    import WalaPFGTest._

    @Test
    def testFactory1 {
        val fac = new PFGFactory(pa)
        val pfg = fac.getPFG
        println("Number of Edges: " + pfg.edges.size)
        println("Number of Nodes: " + pfg.nodes.size)
        import pfg._
        for (edge <- edges) {
            println(edge)
            assert(nodes.contains(edge.src), "Edge src not contained in nodes set.")

        }
        for (node <- pfg.nodes) {
            println(node)
        }

        println("""
        ---------- HERE COMES DOT!------------
                
                
        """)
        println(pfg2Dot(pfg))

        println("""
                
                
                
        ---------- HERE ENDES DOT!------------
                
                
        """)

        val mr = StringStuff.makeMethodReference("bnord.testapps.Main.p2()V")
        val im = pa.cg.getNodes(mr)
        assert(im.size() == 1)
        println(im.first.getIR())
    }

    @Test
    def testSolver1 {
        val mr = StringStuff.makeMethodReference("bnord.testapps.Main.p2()V")
        val im = pa.cg.getNodes(mr)
        val node = im.first
        object GenKillMHP extends GenKillFunction[Edge, Boolean] {
            def apply(edge: Edge) = {
                new GenKill[Boolean] {
                    val gen = node equals (edge.src.proc)
                    val kill = false
                }
            }
        }
        val fac = new PFGFactory(pa)
        val pfg = fac.getPFG
        val solver = new PFGGenKillSolver(pfg, GenKillMHP)
        solver.solve(false)
        println(solver.printResults)

    }

    @Test
    def testSolver2 {
        val mr = StringStuff.makeMethodReference("bnord.testapps.Main.p2()V")
        val im = pa.cg.getNodes(mr)
        val node = im.first
        def genKill(edge: Edge) = {
            de.wwu.sdpn.pfg.lattices.genkill.GenKill(Node(N, CFGPoint(node, 0, 0)) equals (edge.src), false)
        }
        val fac = new PFGFactory(pa)
        val pfg = fac.getPFG
        val solver = new PFGForwardGenKillSolver(pfg, genKill _)
        solver.solve(false)

        println(solver.printResults)

        for (n <- pfg.nodes) {
            if (solver.result(n))
                println("MHP: " + n)
        }
        //        
        //        for (n <- pfg.nodes){
        //            if(solver.resultPI(n))
        //                println("PI: " + n)
        //        }
        //        
        //        for (n <- pfg.nodes){
        //            if(solver.LS(n).elem)
        //                println("LS: " + n)
        //        }
        //        for (n <- pfg.nodes){
        //            if(solver.L(n).elem)
        //                println("L: " + n)
        //        }
        //        for (n <- pfg.nodes){
        //            if(solver.SB(n).elem)
        //                println("SB: " + n)
        //        }
        //        for (n <- pfg.nodes){
        //            if(solver.SP(n).elem)
        //                println("SP: " + n)
        //        }

    }

    //    @Test
    def testBothSolvers {

        val mr = StringStuff.makeMethodReference("bnord.testapps.Main.p2()V")
        val im = pa.cg.getNodes(mr)
        val node = im.first
        object GenKillMHP extends GenKillFunction[Edge, Boolean] {
            def apply(edge: Edge) = {
                new GenKill[Boolean] {
                    val gen = node equals (edge.src.proc)
                    val kill = false
                }
            }
        }
        val fac = new PFGFactory(pa)
        val pfg = fac.getPFG
        val solver1 = new PFGGenKillSolver(pfg, GenKillMHP)
        solver1.solve(false)

        def genKill(edge: Edge) = {
            de.wwu.sdpn.pfg.lattices.genkill.GenKill(node equals (edge.src.proc), false)
        }

        val solver2 = new PFGForwardGenKillSolver(pfg, genKill _)
        solver2.solve(false)

        var diff = 0
        //        for((k,v) <- solver2.S){
        //            solver1.svar.get(k) match {
        //                case None =>
        //                    println("Original doesn't contain: ")
        //                    println("    " + k -> v)
        //                    diff += 1
        //                case Some(v2) =>
        //                    if(v.gen != v2.gen || v.kill != v2.kill) {
        //                        println("Original differs : ")
        //                        println("  Node:   " + k)
        //                        println("  OValue: gen: " + v2.gen + " kill: " + v2.kill)
        //                        println("  NValue: gen: " + v.gen + " kill: " + v.kill)
        //                        diff += 1
        //                    }
        //                
        //            }
        //        }
        //        assertEquals("Old and new constrait Systems differ",0,diff)

    }

    @Test
    def testSolver2Times {

        val mr = StringStuff.makeMethodReference("bnord.testapps.Main.p2()V")
        val im = pa.cg.getNodes(mr)
        val node = im.first
        val fac = new PFGFactory(pa)
        val pfg = fac.getPFG

        def genKill(edge: Edge) = {
            de.wwu.sdpn.pfg.lattices.genkill.GenKill(node equals (edge.src.proc), false)
        }

        val solver1 = new PFGForwardGenKillSolver(pfg, genKill _)
        solver1.solve(false)

        val solver2 = new PFGForwardGenKillSolver(pfg, genKill _)
        solver2.solve(false)

        var diff = 0
        for (node <- pfg.nodes) {
            val r1 = solver1.result(node)
            val r2 = solver2.result(node)
            if (r1 != r2) {
                diff += 1
                println("Different results in solvers:")
                println("  Node:   " + node)
                println("  Run1: " + r1)
                println("  Run2: " + r2)
            }

        }

        assertEquals("Old and new constrait Systems differ", 0, diff)

    }

    @Test
    def testDefUseSolver {
        val mr = StringStuff.makeMethodReference("bnord.testapps.Main.p2()V")
        val im = pa.cg.getNodes(mr)
        val pointerAnalysis = pa.pa
        val node = im.first
        import de.wwu.sdpn.pfg.lattices._
        import de.wwu.sdpn.pfg.lattices.genkill.GenKill

        val lat = implicitly[Lattice[LMap[FieldReference, LMap[CFGPoint, Boolean]]]]
        def genKill(edge: Edge): GenKill[LMap[FieldReference, LMap[CFGPoint, Boolean]]] = {
            edge match {
                case BaseEdge(Node(_, src @ CFGPoint(cgnode, _, _)), SSAAction(act: SSAPutInstruction), Node(N, _)) =>
                    val field = act.getDeclaredField()
                    GenKill(
                        BottomMap[FieldReference, LMap[CFGPoint, Boolean]](Map(field -> BottomMap(Map(src -> true)))),
                        TopMap[FieldReference, LMap[CFGPoint, Boolean]](Map(field -> BottomMap(Map(src -> true))))
                    )

                case _ => GenKill(lat.bottom, lat.top)
            }

        }
        val fac = new PFGFactory(pa)
        val pfg = fac.getPFG
        val solver = new PFGForwardGenKillSolver(pfg, genKill _)
        solver.solve(false)

        println(solver.printResults)

//        for (n <- pfg.nodes) {
//            if (solver.result(n))
//                println("MHP: " + n)
//        }
    }

    def pfg2Dot(pfg: WalaPFG): String = {
        val bld = new StringBuilder()
        def pln(n: String) { bld.append(n + "\n") }
        pln("digraph pfg { ")

        def quote(n: Any) = "\"" + n.toString() + "\""

        for (edge <- pfg.edges) {
            edge match {
                case BaseEdge(src, a, snk) =>
                    pln(quote(src) + " -> " + quote(snk) + "[label=\"" + a + "\"]")
                case SpawnEdge(src, proc, snk) =>
                    pln(quote(src) + " -> " + quote(snk) + "[label=\"Spawn: " + proc.getMethod().getDeclaringClass().getName() + "." + proc.getMethod().getName() + "\"]")
                case CallEdge(src, proc, returns) =>
                    for ((r, snk) <- returns) {
                        pln(quote(src) + " -> " + quote(snk) + "[label=\"" + r + " : " + proc.getMethod().getDeclaringClass().getName() + "." + proc.getMethod().getName() + "\"]")
                    }
            }
        }

        pln("}")

        bld.toString()
    }

}