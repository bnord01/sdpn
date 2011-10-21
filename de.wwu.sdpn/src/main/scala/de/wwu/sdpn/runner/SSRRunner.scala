package de.wwu.sdpn.runner
import de.wwu.sdpn.analysis.SSRProps
import de.wwu.sdpn.analysis.XSBRunner

/**
 * Runs a single set reachability analysis with the parameters specified by [[de.wwu.sdpn.analysis.SSRProps]].
 *  
 * @author Benedikt Nordhoff
 */
object SSRRunner {

    def main(args: Array[String]): Unit = {
        if (SSRProps.get.witness)
            println("Result of witnesscheck:\t" + XSBRunner.runWitnessCheck)
        else
            println("Result of check:\t" + XSBRunner.runCheck)
    }

}