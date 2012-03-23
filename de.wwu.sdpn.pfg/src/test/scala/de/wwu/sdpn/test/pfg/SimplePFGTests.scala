package de.wwu.sdpn.test.pfg
import de.wwu.sdpn.pfg.genkill.PFGForwardGenKillSolver
import org.junit.Test
import org.junit.Assert._
import de.wwu.sdpn.pfg.lattices.genkill.GenKill
import de.wwu.sdpn.pfg.lattices.BottomMap
import de.wwu.sdpn.pfg.lattices.LMap
import de.wwu.sdpn.pfg.lattices.TopMap
import de.wwu.sdpn.pfg.lattices.Lattice
import DefUseTestUtil._

class BasicPFGTests {

    def pfg1 = new SPFG {
        val main_start = SN("Main.start")
        val main_ret = SN("Main.ret")
        val main = SProc("Main", main_start, main_ret)
        val main_before_spawn = SN("Main.before_spawn")
        val main_after_spawn = SN("Main.after_spawn")
        val mainProcOf = Map(main_start -> main, main_ret -> main, main_before_spawn -> main, main_after_spawn -> main)

        val q_start = SN("Q.start")
        val q_ret = SN("Q.ret")
        val q = SProc("Q", q_start, q_ret)
        val qProcOf = Map(q_start -> q, q_ret -> q)

        val edges = Set[SimpleEdge](
            //edges of main
            SBase(main_start, Skip, main_before_spawn),
            SSpawn(main_before_spawn, q, main_after_spawn),
            SBase(main_after_spawn, Skip, main_ret),

            //edges of q
            SBase(q_start, Write("x"), q_ret)
        )

        val retNodes = Map(
            main -> Map(() -> main_ret),
            q -> Map(() -> q_ret)
        )
        val nodes = Set(
            main_start,
            main_ret,
            main_before_spawn,
            main_after_spawn,
            q_start,
            q_ret
        )
        val procs = Set(main, q)
        def procOf = mainProcOf ++ qProcOf
        val entryNode = Map(main -> main_start, q -> q_start)
        val mainProc = main

    }

    def pfg2 = new SPFG {
        val main_start = SN("Main.start")
        val main_ret = SN("Main.ret")
        val main = SProc("Main", main_start, main_ret)
        val main_before_spawn = SN("Main.before_spawn")
        val main_after_spawn = SN("Main.after_spawn")
        val mainProcOf = Map(main_start -> main, main_ret -> main, main_before_spawn -> main, main_after_spawn -> main)
        val mainRetNodes = Map(main -> Map(() -> main_ret))
        val mainNodes = Set(main_start, main_before_spawn, main_after_spawn, main_ret)
        val mainEntry = Map(main -> main_start)

        val q_start = SN("Q.start")
        val q_ret = SN("Q.ret")
        val q = SProc("Q", q_start, q_ret)
        val qProcOf = Map(q_start -> q, q_ret -> q)
        val qRetNodes = Map(q -> Map(() -> q_ret))
        val qNodes = Set(q_start, q_ret)
        val qEntry = Map(q -> q_start)

        val p_start = SN("P.start")
        val p_ret = SN("P.ret")
        val p = SProc("P", p_start, p_ret)
        val pProcOf = Map(p_start -> p, p_ret -> p)
        val pRetNodes = Map(p -> Map(() -> p_ret))
        val pNodes = Set(p_start, p_ret)
        val pEntry = Map(p -> p_start)

        val edges = Set[SimpleEdge](
            //edges of main
            SBase(main_start, Skip, main_before_spawn),
            SSpawn(main_before_spawn, q, main_after_spawn),
            SCall(main_after_spawn, p, main_ret),

            //edges of q
            SBase(q_start, Write("x"), q_ret),

            //edges of p
            SBase(p_start, Write("y"), p_ret)
        )

        val retNodes = mainRetNodes ++ qRetNodes ++ pRetNodes
        val nodes = mainNodes ++ qNodes ++ pNodes
        val procs = Set(main, q, p)
        def procOf = mainProcOf ++ qProcOf ++ pProcOf
        val entryNode = mainEntry ++ qEntry ++ pEntry
        val mainProc = main

    }

    def genKill(edge: SimpleEdge): GenKill[LMap[String, LMap[SimpleEdge, Boolean]]] = {
        val lat = implicitly[Lattice[LMap[String, LMap[SimpleEdge, Boolean]]]]
        edge match {
            case e@SBase(src, Write(x), _) =>
                GenKill(
                    BottomMap[String, LMap[SimpleEdge, Boolean]](Map(x -> BottomMap(Map(e -> true)))),
                    TopMap[String, LMap[SimpleEdge, Boolean]](Map(x -> BottomMap(Map(e -> true))))
                )

            case _ => GenKill(lat.bottom, lat.top)
        }

    }

    @Test
    def testDefUseSolver1 {
        val solver = new PFGForwardGenKillSolver(pfg1, genKill _)
        solver.solve(false)

        println("---------------------- Results pfg1 ----------------------")        
        val result = solver.results
        println(printRes(result))
        checkResult(result)
    }

    @Test
    def testDefUseSolver2 {
        val solver = new PFGForwardGenKillSolver(pfg2, genKill _)
        solver.solve(false)

        println("---------------------- Results pfg2 ----------------------")
        val result = solver.results
        println(printRes(result))
        checkResult(result)
    }
    
   
    
    

}