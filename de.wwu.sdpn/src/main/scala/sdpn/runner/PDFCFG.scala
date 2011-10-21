package sdpn.runner

import com.ibm.wala.ssa.SSACFG
import com.ibm.wala.viz.DotUtil
import com.ibm.wala.ssa.IR
import com.ibm.wala.ipa.cha.IClassHierarchy
import com.ibm.wala.viz.PDFViewUtil
import com.ibm.wala.ipa.callgraph.impl.Everywhere
import com.ibm.wala.util.strings.StringStuff
import scala.collection.JavaConversions._
import sdpn.analysis.MyPreAnalysis
import sdpn.analysis.SSRProps
import java.io.File
import com.ibm.wala.ipa.callgraph.AnalysisCache

/**
 * This generates a control flow graph for the exclusiveMethod specified by SSRProps.get
 * and prints it using /usr/bin/dot to CFG.pdf which is opened by /usr/bin/xdg-open.
 *
 * @author Benedikt Nordhoff
 */
object PDFCFG {
    val dotExe = "/usr/bin/dot"
    val pdfViewExe = "/usr/bin/xdg-open"

    def main(args: Array[String]): Unit = {
        var methodSig = "";
        //     methodSig = "com.ibm.wala.model.java.lang.System.arraycopy(Ljava/lang/Object;Ljava/lang/Object;)V"
        //     methodSig = "bnord.testapps.Main.main([Ljava/lang/String;)V "
        //	   methodSig = "java.lang.SecurityManager.checkPropertyAccess([Ljava/lang/String;)V "
        //     methodSig = "bnord.testapps.MonitorTest.nestedMonitor()V"
        //	   methodSig = "bnord.testapps.Main.run()V"
        //	   methodSig = "bnord.testapps.App.main([Ljava/lang/String;)V"

        //MonitorExample:
        //A single Monitor:
        //	   methodSig = "bnord.examples.MonitorExample.singleMonitor()V"
        //Two nesdtedMonitors:  
        //     methodSig = "bnord.examples.MonitorExample.nestedMonitor()V"

        //Simple CFG example
        //     methodSig = "bnord.examples.Einstein.rechne()I"

        //Thread creation Example
        //    methodSig = "bnord.examples.MyThread.createAndRun()V"
        //methodSig = SSRProps.get.exclusiveMethod
        
        methodSig= "bnord.unittests.Main.p2()V"

        val sanitize = false;

        val tDir = (new File(SSRProps.get.tempDir)).getAbsolutePath
        val pdfFile = tDir + File.separator + "CFG.pdf"
        val dotFile = tDir + File.separator + "CFG.dot"

        val analysis = MyPreAnalysis.getStd

        val mr = StringStuff.makeMethodReference(methodSig)

        import analysis.cha
        val m = analysis.cha.resolveMethod(mr);
        val cache = new AnalysisCache()
        //        val ir = cache.getSSACache().findOrCreateIR(m, Everywhere.EVERYWHERE,
        //            options.getSSAOptions());
        val ir = cache.getIR(m);
        println(ir);

        if (sanitize)
            PDFViewUtil.ghostviewIR(cha, ir, pdfFile, dotFile, dotExe, pdfViewExe)
        else
            ghostviewFullIR(ir, dotFile, pdfFile)

    }

    /**
     * Generates a CFG for the given IR with all exceptional edges
     * @param ir the IR to print
     * @param dotFile the dot file to produce
     * @param pdfFile the pdf file to produce
     */
    def ghostviewFullIR(ir: IR, dotFile: String, pdfFile: String) {
        val g = ir.getControlFlowGraph();

        val labels = PDFViewUtil.makeIRDecorator(ir);

        DotUtil.dotify(g, labels, dotFile, pdfFile, dotExe);

        PDFViewUtil.launchPDFView(pdfFile, pdfViewExe);

    }

}