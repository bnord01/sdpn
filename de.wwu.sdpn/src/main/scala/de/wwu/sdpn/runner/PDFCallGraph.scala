package de.wwu.sdpn.runner

import com.ibm.wala.types.ClassLoaderReference
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.util.collections.Filter
import com.ibm.wala.util.collections.CollectionFilter
import com.ibm.wala.util.graph.GraphSlicer
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ssa.SSACFG
import com.ibm.wala.viz.DotUtil
import com.ibm.wala.ssa.IR
import com.ibm.wala.ipa.cha.IClassHierarchy
import com.ibm.wala.viz.PDFViewUtil
import com.ibm.wala.ipa.callgraph.impl.Everywhere
import com.ibm.wala.util.strings.StringStuff
import scala.collection.JavaConversions._
import de.wwu.sdpn.analysis.MyPreAnalysis
import java.io.File
import de.wwu.sdpn.analysis.SDPNProps

/**
 * This generates a call graph for main class specified by SDPNProps.get 
 * and prints it using /usr/bin/dot to CallGraph.pdf which is opened by /usr/bin/xdg-open.
 * 
 * @author Benedikt Nordhoff
 */
object PDFCallGraph {

    def main(args: Array[String]): Unit = {
        val tDir = (new File(SDPNProps.get.tempDir)).getAbsolutePath
        val pdfFile = tDir + File.separator + "CallGraph.pdf"
        val dotFile = tDir + File.separator + "CallGraph.dot"
        val dotExe = "/usr/bin/dot"
        val pdfViewExe = "/usr/bin/xdg-open"
        val analysis = MyPreAnalysis.getStd

        val slice = GraphSlicer.slice(analysis.cg, new ApplicationLoaderFilter());
        val g = GraphSlicer.prune(analysis.cg, new CollectionFilter(slice));

        DotUtil.dotify(g, null, dotFile, pdfFile, dotExe);

        PDFViewUtil.launchPDFView(pdfFile, pdfViewExe);
    }

}

class ApplicationLoaderFilter extends Filter[CGNode] {

    def accepts(o: CGNode): Boolean = {
        val isApp = o.getMethod().getDeclaringClass().getClassLoader().getReference().equals(ClassLoaderReference.Application);
        val isThreadStart = "java.lang.Thread.start()V".equals(o.getMethod.getSignature)
        isApp || isThreadStart
        //      o.getMethod().getDeclaringClass().getName()
    }
}