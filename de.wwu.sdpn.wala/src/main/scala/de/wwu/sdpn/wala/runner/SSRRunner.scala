package de.wwu.sdpn.wala.runner
import de.wwu.sdpn.wala.analyses.MyPreAnalysis
import de.wwu.sdpn.wala.analyses.SimpleAnalyses
import de.wwu.sdpn.wala.analyses.SDPNTestProps
import com.ibm.wala.util.strings.StringStuff
import scala.collection.JavaConversions._
import de.wwu.sdpn.wala.dpngen.symbols.StackSymbol
import com.ibm.wala.ipa.callgraph.CGNode

/**
 * Runs a single set reachability analysis with the parameters specified by [[de.wwu.sdpn.analysis.SDPNProps]].
 *
 * @author Benedikt Nordhoff
 */
object SSRRunner {

  def main(args: Array[String]): Unit = {

    val preA = MyPreAnalysis.getStd

    import preA._
    val ms = SDPNTestProps.get.exclusiveMethod
    val mr = StringStuff.makeMethodReference(ms)
    val cnset = cg.getNodes(mr)
    val confSet = scala.collection.Set() ++ (cnset map (StackSymbol(_, 0, 0)))
    val sliceSet: Set[CGNode] = if (SDPNTestProps.get.slicing) Set() ++ cnset else Set.empty
    val lockSens = SDPNTestProps.get.lockSens

    /*if (SDPNTestProps.get.witness){
          val res = SimpleAnalyses.runSSRCheck(cg, pa, confSet, sliceSet, lockSens)
            println("Result of witnesscheck:\t" + res)
        }
        else*/
    {
      val res = SimpleAnalyses.runSSRCheck(cg, pa, confSet, sliceSet, lockSens)
      println("Result of check:\t" + res)
    }
  }

}