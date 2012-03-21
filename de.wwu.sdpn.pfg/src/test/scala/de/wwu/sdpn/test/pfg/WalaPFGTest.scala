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
import DefUseUtil._
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.callgraph.CallGraph
import de.wwu.sdpn.pfg.lattices.genkill.GenKill
import de.wwu.sdpn.pfg.lattices.TopMap
import de.wwu.sdpn.pfg.lattices.BottomMap
import de.wwu.sdpn.pfg.lattices.Lattice
import de.wwu.sdpn.pfg.wala.E

object WalaPFGTest {
    var pa: PreAnalysis = null
    @BeforeClass
    def setUp {
        pa = MyPreAnalysis.getStd
    }

}

class WalaPFGTest {

    def pfgTest01: (WalaPFG, PointerAnalysis, CallGraph) = {
        import de.wwu.sdpn.wala.analyses.SDPNTestProps
        val cp = SDPNTestProps.get.classPath
        val mc = "Lbnord/unittests/defuse/Test01"
        val preAnalysis = MyPreAnalysis.getStd(cp, mc)
        val pfg = PFGFactory.getPFG(preAnalysis)
        (pfg, preAnalysis.pa, preAnalysis.cg)
    }
    def pfgTestPrintln01: (WalaPFG, PointerAnalysis, CallGraph) = {
        import de.wwu.sdpn.wala.analyses.SDPNTestProps
        val cp = SDPNTestProps.get.classPath
        val mc = "Lbnord/unittests/defuse/TestPrintln01"
        val preAnalysis = MyPreAnalysis.getStd(cp, mc)
        val pfg = PFGFactory.getPFG(preAnalysis)
        (pfg, preAnalysis.pa, preAnalysis.cg)
    }

    import WalaPFGTest._

    @Test
    def testFactory1 {
        val pfg = PFGFactory.getPFG(pa)
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
        def genKill(edge: Edge) = {
            GenKill(Node(N, CFGPoint(node, 0, 0)) equals (edge.src), false)
        }
        val pfg = PFGFactory.getPFG(pa)
        val solver = new PFGForwardGenKillSolver(pfg, genKill _)
        solver.solve(false)

        println(solver.printResults)

        for (n <- pfg.nodes) {
            if (solver.result(n))
                println("MHP: " + n)
        }

    }

    @Test
    def testSolver2Times {

        val mr = StringStuff.makeMethodReference("bnord.testapps.Main.p2()V")
        val im = pa.cg.getNodes(mr)
        val node = im.first
        val pfg = PFGFactory.getPFG(pa)

        def genKill(edge: Edge) = {
            GenKill(node equals (edge.src.proc), false)
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
        val (pfg, pa, cg) = pfgTest01

        val solver = new PFGForwardGenKillSolver(pfg, genKill _)
        solver.solve(false)

        //        println(solver.printResults)
        println("---------------------- Results Def/Use ----------------------")
        val result = solver.results
        println(printRes(result))
        checkResult(result)
    }

    //    @Test // This takes about 20 seconds.
    def testDefUseSolver2 {
        val (pfg, pa, cg) = pfgTestPrintln01

        val solver = new PFGForwardGenKillSolver(pfg, genKill _)
        solver.solve(false)

        val result = solver.results
        checkResult(result)
    }

    @Test
    def testDefUseSolver3 {
        val (pfg, pa, cg) = pfgTest01

        val solver = new PFGForwardGenKillSolver(pfg, getGenKill(cg, pa) _)
        solver.solve(false)

        val result = solver.results
        println(printRes(result))
        checkResult(result)
    }
    
//    @Test //This takes about 2 minutes
    def testDefUseSolver4 {
        val (pfg, pa, cg) = pfgTestPrintln01

        val solver = new PFGForwardGenKillSolver(pfg, getGenKill(cg, pa) _)
        solver.solve(false)

        val result = solver.results
        checkResult(result)
    }

    def genKill(edge: Edge): GenKill[LMap[FieldReference, LMap[CFGPoint, Boolean]]] = {
        val lat = implicitly[Lattice[LMap[FieldReference, LMap[CFGPoint, Boolean]]]]
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

    def getGenKill(cg: CallGraph, pa: PointerAnalysis)(edge: Edge): GenKill[LMap[(InstanceKey, FieldReference), LMap[CFGPoint, Boolean]]] = {
        val lat = implicitly[Lattice[LMap[(InstanceKey, FieldReference), LMap[CFGPoint, Boolean]]]]
        val hm = pa.getHeapModel()
        edge match {
            case BaseEdge(Node(_, src @ CFGPoint(cgnode, _, _)), SSAAction(act: SSAPutInstruction), Node(nstate, _)) =>
                val field = act.getDeclaredField()                
                val iks: Iterable[InstanceKey] =
                    if (act.isStatic()) {
                        val declClass = act.getDeclaredField().getDeclaringClass()
                        val ik = hm.getInstanceKeyForClassObject(declClass)
                        Set(ik)
                    } else {
                        val refNr = act.getRef()
                        val pk = hm.getPointerKeyForLocal(cgnode, refNr)
                        pa.getPointsToSet(pk)
                    }

                val defs = Map() ++ (for (ik <- iks) yield ((ik, field) -> BottomMap(Map(src -> true))))
                val kill = nstate match {
                    case N => TopMap[(InstanceKey, FieldReference), LMap[CFGPoint, Boolean]](defs)
                    case E => lat.top
                }
                GenKill(
                    BottomMap[(InstanceKey, FieldReference), LMap[CFGPoint, Boolean]](defs),
                    kill
                )

            case _ => GenKill(lat.bottom, lat.top)
        }

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