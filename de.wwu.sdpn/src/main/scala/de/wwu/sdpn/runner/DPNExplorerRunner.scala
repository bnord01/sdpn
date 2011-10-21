package de.wwu.sdpn.runner

import com.ibm.wala.util.strings.StringStuff
import de.wwu.sdpn.util.BackwardSliceFilter
import de.wwu.sdpn.mnfs.ConflictMNFSGenerator
import de.wwu.sdpn.util.CGCycleCounter
import scala.swing.MainFrame
import de.wwu.sdpn.dpn.explicit.monitor.MonitorDPNFactory
import scala.collection.JavaConversions._
import com.ibm.wala.ipa.callgraph.CGNode
import de.wwu.sdpn.gui.MonitorDPNView
import de.wwu.sdpn.analysis.MyPreAnalysis
import de.wwu.sdpn.analysis.SDPNProps

/**
 * Runs the DPNExplorer for the MonitorDPN obtained by the properties specified by [[de.wwu.sdpn.analysis.SDPNProps]].
 *
 * @author Benedikt Nordhoff
 */
object DPNExplorerRunner {
    def main(args: Array[String]) {
        println("Initializing WALA")
        val analysis = MyPreAnalysis.getStd
        import analysis._

        //        val entryNode = cg.getEntrypointNodes().iterator().next()
        val methodSig = SDPNProps.get.exclusiveMethod

        val mr = StringStuff.makeMethodReference(methodSig)
        val confSet: scala.collection.Set[CGNode] = asScalaSet(cg.getNodes(mr))

        val analysis2 = if (SDPNProps.get.slicing) new MyPreAnalysis(analysis) with BackwardSliceFilter {
            val initialSet = confSet
        }
        else analysis

        val issliced = if(SDPNProps.get.slicing) "sliced" else "full"
        
        println("Generating " + issliced + " DPN")        
        val dpnfac = new MonitorDPNFactory(analysis2)
        val dpn = dpnfac.getDPN

        println("Generated rules: " + dpn.getTransitions.size)

        println("Used locks: " + dpn.locks.size)
        for (l <- dpnfac.getDPN.locks)
            println("\t " + l)

        val gui = new MainFrame {
            title = "DPN Explorer"
            contents = new MonitorDPNView(dpn)
            maximize()
            visible = true
        }

    }

}

