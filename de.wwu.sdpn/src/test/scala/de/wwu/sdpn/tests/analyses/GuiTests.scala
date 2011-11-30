package de.wwu.sdpn.tests.analyses
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import org.junit.BeforeClass
import de.wwu.sdpn.analysis.SimpleAnalyses
import de.wwu.sdpn.analysis.SDPNProps
import org.junit.AfterClass
import org.junit.Test
import de.wwu.sdpn.dpn.explicit.StackSymbol
import com.ibm.wala.util.strings.StringStuff
import scala.collection.JavaConversions._
import scala.collection.Set
import org.junit.Assert._
import com.ibm.wala.types.MethodReference
import de.wwu.sdpn.ta.witness.FullWitnessParser
import de.wwu.sdpn.ta.witness.WTGraph

object GuiTests {
	
	var stuff: Map[Int,(CallGraph,PointerAnalysis,MethodReference)] = Map()
	
	@BeforeClass
	def setUp(){
	  	for (i <- 5 to 5) {
	  	  val (cg,pa) = SimpleAnalyses.getCGandPAfromCP(SDPNProps.get.classPath,"Lbnord/unittests/simpleAnalyses/BSP0"+i)
	  	  val mr = StringStuff.makeMethodReference("bnord.unittests.simpleAnalyses.BSP0" + i + ".excludeMe()V")
	  	  stuff += i -> (cg,pa,mr)
	  	}
	}
	
	@AfterClass
	def tearDown() {
		stuff = null
	}
}

class GuiTests {
  import GuiTests._
@Test
  def testWTGUI() {
    val (cg,pa,mr) = stuff(5)
    val nodes = cg.getNodes(mr)
    val res = SimpleAnalyses.runWitnessTSRCheck(cg,pa,getStackSymbols(cg,mr),getStackSymbols(cg,mr),nodes,true)
    
    assertFalse("There should be an Conflict",res == None)
    val switness = res.get
    val parser = new FullWitnessParser()
    val witness = parser.parseTree(switness)
    WTGraph.showTree(witness.get,cg)
    
  }
  
   def getStackSymbols(cg:CallGraph, mr:MethodReference) : Set[StackSymbol] = {
    val nodes = cg.getNodes(mr)
    var res = Set[StackSymbol]()
    nodes.foreach({res += StackSymbol(_,0,0)})
    return res    
  }
}