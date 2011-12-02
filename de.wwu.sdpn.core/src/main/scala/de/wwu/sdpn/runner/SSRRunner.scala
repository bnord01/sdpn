package de.wwu.sdpn.runner
import de.wwu.sdpn.analysis.SDPNProps
import de.wwu.sdpn.analysis.XSBRunner

/**
 * Runs a single set reachability analysis with the parameters specified by [[de.wwu.sdpn.analysis.SDPNProps]].
 *  
 * @author Benedikt Nordhoff
 */
object SSRRunner {

    def main(args: Array[String]): Unit = {
        if (SDPNProps.get.witness)
            println("Result of witnesscheck:\t" + XSBRunner.runWitnessCheck)
        else
            println("Result of check:\t" + XSBRunner.runCheck)
    }

}