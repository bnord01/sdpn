package de.wwu.sdpn.wala.analyses

import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.classLoader.IMethod
import de.wwu.sdpn.wala.util.ThreadSensContextSelector
import com.ibm.wala.ipa.callgraph.propagation.SSAPropagationCallGraphBuilder
import com.ibm.wala.ipa.callgraph.impl.Util
import com.ibm.wala.core.tests.callGraph.CallGraphTestUtil
import com.ibm.wala.util.io.FileProvider
import com.ibm.wala.util.config.AnalysisScopeReader
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ipa.cha.IClassHierarchy
import com.ibm.wala.ipa.callgraph.AnalysisCache
import com.ibm.wala.ipa.callgraph.AnalysisOptions
import com.ibm.wala.ipa.cha.ClassHierarchy
import com.ibm.wala.util.io.FileUtil
import java.util.LinkedList
import scala.collection.JavaConversions._
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import de.wwu.sdpn.wala.util.PreAnalysis
import de.wwu.sdpn.wala.util.BackwardSliceFilter
import scala.collection.Set

/**
 * A helper which holds defaults for the Wala specific settings and is easily extendable
 */
case class MyPreAnalysis(
    cha: IClassHierarchy,
    cg: CallGraph,
    pa: PointerAnalysis,
    ii: CGNode => Boolean = _ => true,
    sl: (InstanceKey, CGNode) => Boolean = (_, _) => false) extends PreAnalysis {
    def isThreadStart(cgnode: CGNode): Boolean = {
        cgnode != null &&
            cgnode.getMethod != null &&
            "java.lang.Thread.start()V".equals(cgnode.getMethod.getSignature)
    }

    def this(a: PreAnalysis) = {
        this(a.cha, a.cg, a.pa, a.isInteresting, a.safeLock)
    }
    def this(cg:CallGraph,pa:PointerAnalysis) = this(cg.getClassHierarchy,cg,pa)

    def entryNode = cg.getFakeRootNode
    def isInteresting(node: CGNode) = ii(node)
    def safeLock(ik: InstanceKey, node: CGNode) = sl(ik, node)

    def +(isl: (InstanceKey, CGNode) => Boolean):MyPreAnalysis = {
        new MyPreAnalysis(this) {
            override def safeLock(ik: InstanceKey, node: CGNode) = isl(ik, node)
        }
    }
    def +(sliceSet: Set[CGNode]):MyPreAnalysis = {
        new MyPreAnalysis(this) with BackwardSliceFilter {
            override val initialSet = sliceSet
        }
    }

}

/**
 * Helper functions to set up a WALA
 * @author Benedikt Nordhoff
 */
object MyPreAnalysis {

    /**
     * Generates a PreAnalysis based on the class path and main class from [[de.wwu.sdpn.analysis.SDPNProps]]
     */
    def getStd: PreAnalysis = {
        val props = SDPNTestProps.get
        val cp = props.classPath
        val mc = props.mainClass
        getStd(cp, mc)
    }

    /**
     * Generates a PreAnalysis initializing wala by using the given parameters,
     * A scope based on {{primordial.txt}} with
     * exclusions found in {{Java60RegressionExclusions.txt}}.
     * And a VanillaZeroOneCFA extended by [[de.wwu.sdpn.util.ThreadSensContextSelector]]
     * Everything is interesting, no lock is safe.
     * @param cp the class path to analyze
     * @param mc the main class to analyze
     * @return the PreAnalysis
     */
    def getStd(cp: String, mc: String): PreAnalysis = {

        val scope = AnalysisScopeReader.makeJavaBinaryAnalysisScope(
            cp,
            FileProvider.getFile("Java60RegressionExclusions.txt"))

        val cha = ClassHierarchy.make(scope);

        val entrypoints = Util.makeMainEntrypoints(scope, cha, mc);

        val options = new AnalysisOptions(scope, entrypoints);

        val cache = new AnalysisCache();

        val cgbuilder: SSAPropagationCallGraphBuilder =
            Util.makeVanillaZeroOneCFABuilder(options, cache, cha, scope, new ThreadSensContextSelector(), null);

        val cg = cgbuilder.makeCallGraph(options)

        val pa = cgbuilder.getPointerAnalysis

        val isInteresting = { x: CGNode => true }
        val safeLock = { (_: InstanceKey, _: CGNode) => false }

        return MyPreAnalysis(cha, cg, pa, isInteresting, safeLock)

    }

}

