package de.wwu.sdpn.tests.util

import com.ibm.wala.ipa.callgraph.CGNode
import de.wwu.sdpn.util.WaitMap
import de.wwu.sdpn.util.MonitorMatcher
import com.ibm.wala.classLoader.IMethod
import com.ibm.wala.util.strings.StringStuff
import de.wwu.sdpn.util.ExceptionTypeLocator
import de.wwu.sdpn.util.UniqueInstanceLocator
import de.wwu.sdpn.util.LockLocator
import org.junit._
import com.ibm.wala.ipa.callgraph.CallGraph
import org.junit.Assert._
import de.wwu.sdpn.util.PreAnalysis
import de.wwu.sdpn.analysis.MyPreAnalysis
import de.wwu.sdpn.analysis.SDPNProps
import com.ibm.wala.ipa.callgraph.AnalysisCache
import scala.collection.JavaConversions._
import com.ibm.wala.ssa.SSACFG
import com.ibm.wala.classLoader.IBytecodeMethod
import com.ibm.wala.ssa.SSAInvokeInstruction
import de.wwu.sdpn.util.LockWithOriginLocator
import de.wwu.sdpn.analysis.DPN4IFCAnalysis
import de.wwu.sdpn.dpn.explicit.StackSymbol
import com.ibm.wala.util.graph.Graph
import com.ibm.wala.util.graph.impl.BasicOrderedMultiGraph
import com.ibm.wala.util.graph.GraphPrint
import de.wwu.sdpn.util.GraphCycleFinder

object UtilTest {
  var analysis: PreAnalysis = null

  @BeforeClass
  def setUp() {
    analysis = MyPreAnalysis.getStd(SDPNProps.get.classPath, "Lbnord/unittests/Main")
  }

  @AfterClass
  def tearDown() {
    analysis = null
  }
}

class UtilTest {
  val analysis = UtilTest.analysis
  import analysis._
  import StringStuff.makeMethodReference

  @Test
  def testEntryPoint {
    assertNotNull(cg)

    val it = cg.getEntrypointNodes.iterator
    assertTrue("Keine Entypoint Node", it.hasNext)
    val ep = it.next
    assertFalse("Mehrere Entrypoint Nodes", it.hasNext)
    println(ep)
  }

  @Test
  def testLockLocator {
    val ll = new MyPreAnalysis(analysis) with LockLocator
    val ui = new MyPreAnalysis(analysis) with UniqueInstanceLocator
    assertTrue("gibt locks instances", ll.locks.size > 0)
    println("Locks: " + ll.locks)
    val lock = ll.locks.iterator.next
    println(lock.getClass)
    println("Unique Instances: " + ui.uniqueInstances)

    val ul = ll.locks.intersect(ui.uniqueInstances)

    assertTrue("Locks Unique", ul.subsetOf(ui.uniqueInstances))

    for (l <- ul)
      println("Unique Lock:\t " + l)

  }
  
  @Test
  def testLockWithOriginLocator {
    val lol = new MyPreAnalysis(analysis) with LockWithOriginLocator
    val ll = new MyPreAnalysis(analysis) with LockLocator
    val loli = lol.locks
    val lli = ll.locks
    
    for (l <- loli)
      println("LockMapping:\t " + l)
    assert(lli.equals(loli.keySet))

  }

  //@Test 
  def testExceptionTypes {
    val etl = new ExceptionTypeLocator(cg, pa).exceptionTypes
    for (e <- etl) {
      println("Exception Type: \t" + e)
    }
  }

  @Test
  def testComponentMethod {
    //TODO: Check why this fails at command line but runs within eclipse ...
    //		val methodSig = "java.awt.Component.setComponentZOrder(Ljava/awt/Component;I;)V"
    val methodSig = "java.awt.Component.setComponentZOrder(Ljava/awt/Component;I)V"

    val mr = StringStuff.makeMethodReference(methodSig)

    val m = cha.resolveMethod(mr);
    assertNull("ComponentMethodNull", m)

  }

  @Test
  def testMonitorMatcher {
    val im = mrr("bnord.unittests.MonitorTest.nestedMonitors()V")
    val cache = new AnalysisCache()
    val ir = cache.getIR(im)
    val cfg = ir.getControlFlowGraph
    val enters = MonitorMatcher.getEnters(cfg)
    assertFalse("No Monitor Enter", enters.isEmpty)
    println("Enters: " + enters)
    val exits = MonitorMatcher.getExits(cfg, enters.iterator.next.getGraphNodeId)
    println("Exits: " + exits)
    assertEquals(2, exits.size)

  }

  @Test
  def testWaitSigs() {
    val sigs = List("java.lang.Object.wait()V",
      "java.lang.Object.wait(J)V",
      "java.lang.Object.wait(JI)V")
    for (sig <- sigs) {
      val m = mrr(sig)
      assertEquals(sig, m.getReference.getSignature)
    }
  }

  @Test
  def testWaitMap() {
    val wm = new WaitMap(analysis, _ => true)
    println("WaitMap: " + wm.waitMap);
    assertFalse("No Waits found", wm.waitMap.isEmpty)
  }

  def mrr(methodSig: String): IMethod = {
    val mr = StringStuff.makeMethodReference(methodSig)

    val m = cha.resolveMethod(mr);
    assertNotNull("IMethod Null", m)
    return m

  }
  @Test
  def testIndexMapping() {
    val im = mrr("bnord.unittests.MonitorTest.nestedMonitors()V")
    val cache = new AnalysisCache()
    val ir = cache.getIR(im)
    val cfg = ir.getControlFlowGraph
    val instrArr = ir.getInstructions()
    val bcM = im.asInstanceOf[IBytecodeMethod]
    var found = false
    //looking for line 18
    for (bb0 <- cfg) {
      val bb = bb0.asInstanceOf[SSACFG#BasicBlock]
      var index = 0
      for(instr <- bb.iteratePhis()){
        index += 1
      }
      if(bb.isCatchBlock())
        index += 1
      for(instr <- bb.iterateNormalInstructions()){
        val start = bb.getFirstInstructionIndex()
        val stop = bb.getLastInstructionIndex()
        var arrIndex = -1
        for(i <- start to stop) {
          if(instr.equals(instrArr(i))){
            arrIndex = i
          }         
        }
        val bIndex = bcM.getBytecodeIndex(arrIndex)
        val ln = im.getLineNumber(bIndex)
        
        if(ln == 18){
          found = true
          println ("Found Match for: " + instr)
          println ("bbnr: " + bb.getNumber + "\t index: " + index + "\t arrIndex: " + arrIndex)
          println("bIndex: " + bIndex + "\t LineNumber: " + ln)
          assert(instr.isInstanceOf[SSAInvokeInstruction])
        }
        
      }
    } 	
  }
  
  @Test 
  def testGraphCycleFinder() {
    val graph = new BasicOrderedMultiGraph[String]()
    graph addNode "Start"
    graph addNode "A"
    graph addNode "B"
    graph addNode "C"
    graph addNode "D"
    graph addNode "E"
    graph addNode "F"
    graph addNode "G"
    graph addNode "H"
    graph addNode "End" 
    
    graph addEdge ("Start", "A")
    graph addEdge ("A","B")
    graph addEdge ("B","C")
    graph addEdge ("C","A")
    graph addEdge ("C","H")
    graph addEdge ("H","End")
    graph addEdge ("Start","D")    
    graph addEdge ("D","G")
    graph addEdge ("Start","E")
    graph addEdge ("E","F")
    graph addEdge ("F","G")
    graph addEdge ("G","End")
    
    println("Printing graph")
    println(GraphPrint.genericToString(graph))
    
    val gcf = new GraphCycleFinder(graph)
    assert(gcf.solve(null),"Couldnt Solve problem")
    
    assert(gcf.inCycle("A"),"A should be in cycle but is not!")
    assert(gcf.inCycle("B"),"B should be in cycle but is not!")
    assert(gcf.inCycle("C"),"C should be in cycle but is not!")
    assert(!gcf.inCycle("H"),"H shouldn't be in cycle but is!")
    assert(!gcf.inCycle("Start"),"Start shouldn't be in cycle but is!")
    assert(!gcf.inCycle("D"),"D shouldn't be in cycle but is!")
    assert(!gcf.inCycle("E"),"E shouldn't be in cycle but is!")
    assert(!gcf.inCycle("F"),"F shouldn't be in cycle but is!")
    assert(!gcf.inCycle("G"),"G shouldn't be in cycle but is!")
    assert(!gcf.inCycle("End"),"End shouldn't be in cycle but is!")
    
    
  }
  
 

}