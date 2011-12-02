package de.wwu.sdpn.wala.runner

import com.ibm.wala.util.strings.StringStuff
import scala.swing.MainFrame
import scala.collection.JavaConversions._
import com.ibm.wala.ipa.callgraph.CGNode
import de.wwu.sdpn.gui.MonitorDPNView
import de.wwu.sdpn.wala.analyses.MyPreAnalysis
import de.wwu.sdpn.wala.analyses.SDPNTestProps
import de.wwu.sdpn.wala.util.BackwardSliceFilter
import de.wwu.sdpn.wala.dpngen.MonitorDPNFactory

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
        val methodSig = SDPNTestProps.get.exclusiveMethod

        val mr = StringStuff.makeMethodReference(methodSig)
        val confSet: scala.collection.Set[CGNode] = asScalaSet(cg.getNodes(mr))

        val analysis2 = if (SDPNTestProps.get.slicing) new MyPreAnalysis(analysis) with BackwardSliceFilter {
            val initialSet = confSet
        }
        else analysis

        val issliced = if(SDPNTestProps.get.slicing) "sliced" else "full"
        
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

