package de.wwu.sdpn.test.pfg
import de.wwu.sdpn.pfg.genkill.PFGForwardGenKillSolver
import org.junit.Test

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
    
    
    @Test
    def testDefUseSolver {
       
        import de.wwu.sdpn.pfg.lattices._
        import de.wwu.sdpn.pfg.lattices.genkill.GenKill

        val lat = implicitly[Lattice[LMap[String, LMap[SN, Boolean]]]]
        def genKill(edge: SimpleEdge): GenKill[LMap[String, LMap[SN, Boolean]]] = {
            edge match {
                case SBase(src, Write(x), _) =>
                    GenKill(
                        BottomMap[String, LMap[SN, Boolean]](Map(x -> BottomMap(Map(src -> true)))),
                        TopMap[String, LMap[SN, Boolean]](Map(x -> BottomMap(Map(src -> true))))
                    )

                case _ => GenKill(lat.bottom, lat.top)
            }

        }
        val solver = new PFGForwardGenKillSolver(pfg1, genKill _)
        solver.solve(false)

        println(solver.printResults)

//        for (n <- pfg.nodes) {
//            if (solver.result(n))
//                println("MHP: " + n)
//        }
    }

}