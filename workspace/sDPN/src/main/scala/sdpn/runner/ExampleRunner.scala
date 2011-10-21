package sdpn.runner
import sdpn.analysis.SSRProps
import sdpn.analysis.SingleSetReachability._
import sdpn.analysis.XSBRunner._


/**
 * Runs examples Lbnord/examples/BSP01 to 04 the classPath should be specified by [[sdpn.analysis.SSRProps]]
 * debug should be set in order to obtain detailed runtime information
 * 
 * @author Benedikt Nordhoff
 */
object ExampleRunner {
    import System.{ currentTimeMillis => now }
    def main(args: Array[String]): Unit = {
        val mainClasses = 1 to 4 map (x => ("Lbnord/examples/BSP0" + x, "bnord.examples.BSP0" + x + ".excludeMe()V"))
        import SSRProps.get.{ classPath, witness, lockSens, slicing }
        println("%%%% Analyzing Examples %%%%%")
        println("Class Path:\t\t" + classPath)
        println("Witness:\t\t" + witness)
        println("Lock sensitive:\t\t" + lockSens)
        println("Slicing:\t\t" + slicing)
        println()

        for ((mc, methodSig) <- mainClasses) {
            val begin = now
            println("--- Begin Analysis of " + mc + " ---")

            val lockFilter = lockTypeFilter("Lbnord/examples/Lock")
            val (dpn, cset) = genDPNandCset(classPath, mc, methodSig, lockFilter, lockSens, slicing)
            val (td, bu) = genAutomata(dpn, cset)
            if (witness) {
                println("Result of witnessCheck:\t"+ runWitnessCheck(genWitnessCheck(td, bu)))
            } else {
                println("Result of check:\t" + runCheck(genCheck(td, bu)))
            }

            println("Overall Time:\t\t" + (now - begin)+"ms")
            println("---- End Analysis of " + mc + " ----\n\n")

        }
    }

}