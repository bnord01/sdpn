package de.wwu.sdpn.tests.analyses
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import org.junit.BeforeClass
import org.junit.AfterClass
import org.junit.Test
import com.ibm.wala.util.strings.StringStuff
import scala.collection.JavaConversions._
import scala.collection.Set
import org.junit.Assert._
import com.ibm.wala.types.MethodReference
import de.wwu.sdpn.wala.analyses.SimpleAnalyses
import de.wwu.sdpn.wala.analyses.SDPNTestProps
import de.wwu.sdpn.wala.dpngen.symbols.StackSymbol

object SimpleAnalysesTest {
	
	var stuff: Map[Int,(CallGraph,PointerAnalysis,MethodReference)] = Map()
	
	@BeforeClass
	def setUp(){
	  	for (i <- 1 to 5) {
	  	  val (cg,pa) = SimpleAnalyses.getCGandPAfromCP(SDPNTestProps.get.classPath,"Lbnord/unittests/simpleAnalyses/BSP0"+i)
	  	  val mr = StringStuff.makeMethodReference("bnord.unittests.simpleAnalyses.BSP0" + i + ".excludeMe()V")
	  	  stuff += i -> (cg,pa,mr)
	  	}
	}
	
	@AfterClass
	def tearDown() {
		stuff = null
	}
}
class SimpleAnalysesTest {
  import SimpleAnalysesTest.stuff
  @Test
  def printCGandStackSymbols(){
    val (cg,pa,mr) = stuff(1)
    println("Number of CGNodes:\t"+cg.getNumberOfNodes())
    println("Identified StackSymbols:\t" + getStackSymbols(cg,mr))
  }
  
  
  @Test
  def testUnslicedLockInsensSSR1() {
    val (cg,pa,mr) = stuff(1)
    assertFalse("There should be an Conflict",SimpleAnalyses.runSSRCheck(cg,pa,getStackSymbols(cg,mr),null,false))
  }
  @Test
  def testSlicedLockInsensSSR1() {
    val (cg,pa,mr) = stuff(1)
    val nodes = cg.getNodes(mr)
    assertFalse("There should be an Conflict",SimpleAnalyses.runSSRCheck(cg,pa,getStackSymbols(cg,mr),nodes,false))
  }
  @Test
  def testUnslicedLockInsensTSR1() {
    val (cg,pa,mr) = stuff(1)
    assertFalse("There should be an Conflict",SimpleAnalyses.runTSRCheck(cg,pa,getStackSymbols(cg,mr),getStackSymbols(cg,mr),null,false))
  }  
  
  @Test
  def testSlicedLockInsensWitnessTSR1() {
    val (cg,pa,mr) = stuff(1)
    val nodes = cg.getNodes(mr)
    SimpleAnalyses.runWitnessTSRCheck(cg,pa,getStackSymbols(cg,mr),getStackSymbols(cg,mr),nodes,false) match {
      case None => fail
      case Some(_) => 
    }
  }
  
  
  @Test
  def testSlicedLockSensSSR1() {
    val (cg,pa,mr) = stuff(2)
    val nodes = cg.getNodes(mr)
    assertTrue("There shouldn't be an Conflict",SimpleAnalyses.runSSRCheck(cg,pa,getStackSymbols(cg,mr),nodes,true))
  }
  
  @Test
  def testSlicedLockSensSSR1LS() {
    val (cg,pa,mr) = stuff(2)
    val nodes = cg.getNodes(mr)
    var locks = SimpleAnalyses.getPossibleLocks(cg,pa)
    locks = SimpleAnalyses.filterByClassLoader(locks)
    println("PossibleLocks: " + locks)
    assertTrue("There shouldn't be an Conflict",SimpleAnalyses.runSSRCheck(cg,pa,getStackSymbols(cg,mr),nodes,locks))
  }
  
  @Test
  def testSlicedLockSensSSR2() {
    val (cg,pa,mr) = stuff(3)
    val nodes = cg.getNodes(mr)
    assertTrue("There shouldn't be an Conflict",SimpleAnalyses.runSSRCheck(cg,pa,getStackSymbols(cg,mr),nodes,true))
  }
  
  @Test
  def testSlicedLockSensSSR2LS() {
    val (cg,pa,mr) = stuff(3)
    val nodes = cg.getNodes(mr)
    var locks = SimpleAnalyses.getPossibleLocks(cg,pa)
    locks = SimpleAnalyses.filterByClassLoader(locks)
    println("PossibleLocks: " + locks)
    assertTrue("There shouldn't be an Conflict",SimpleAnalyses.runSSRCheck(cg,pa,getStackSymbols(cg,mr),nodes,locks))
  }
  
  @Test
  def testSlicedWitnessLockSensTSR2() {
    val (cg,pa,mr) = stuff(3)
    val nodes = cg.getNodes(mr)
    val res = SimpleAnalyses.runWitnessTSRCheck(cg,pa,getStackSymbols(cg,mr),getStackSymbols(cg,mr),nodes,true)
    
    assertTrue("There shouldn't be an Conflict",res == None)
  }
  
  @Test
  def testSlicedWitnessLockSensTSR3() {
    val (cg,pa,mr) = stuff(5)
    val nodes = cg.getNodes(mr)
    val res = SimpleAnalyses.runWitnessTSRCheck(cg,pa,getStackSymbols(cg,mr),getStackSymbols(cg,mr),nodes,true)
    
    assertFalse("There should be an Conflict",res == None)
  }
  
  
  
  
  
  def getStackSymbols(cg:CallGraph, mr:MethodReference) : Set[StackSymbol] = {
    val nodes = cg.getNodes(mr)
    var res = Set[StackSymbol]()
    nodes.foreach({res += StackSymbol(_,0,0)})
    return res    
  }

  
  
}