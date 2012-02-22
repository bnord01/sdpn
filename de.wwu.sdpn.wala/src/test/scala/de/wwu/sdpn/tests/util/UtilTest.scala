package de.wwu.sdpn.tests.util

import scala.collection.JavaConversions.asScalaIterator
import scala.collection.JavaConversions.iterableAsScalaIterable
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.AfterClass
import org.junit.BeforeClass
import org.junit.Test
import com.ibm.wala.classLoader.IBytecodeMethod
import com.ibm.wala.classLoader.IMethod
import com.ibm.wala.ipa.callgraph.AnalysisCache
import com.ibm.wala.ssa.SSACFG
import com.ibm.wala.ssa.SSAInvokeInstruction
import com.ibm.wala.util.graph.impl.BasicOrderedMultiGraph
import com.ibm.wala.util.graph.GraphPrint
import com.ibm.wala.util.strings.StringStuff
import de.wwu.sdpn.core.ta.xsb.witness.FullWitnessParser
import de.wwu.sdpn.wala.analyses.MyPreAnalysis
import de.wwu.sdpn.wala.analyses.SDPNTestProps
import de.wwu.sdpn.wala.util.ExceptionTypeLocator
import de.wwu.sdpn.wala.util.GraphCycleFinder
import de.wwu.sdpn.wala.util.LockLocator
import de.wwu.sdpn.wala.util.LockWithOriginLocator
import de.wwu.sdpn.wala.util.MonitorMatcher
import de.wwu.sdpn.wala.util.PreAnalysis
import de.wwu.sdpn.wala.util.UniqueInstanceLocator
import de.wwu.sdpn.wala.util.WaitMap
import junit.framework.JUnit4TestAdapter

object UtilTest {
  var analysis: PreAnalysis = null

  @BeforeClass
  def setUp() {
    analysis = MyPreAnalysis.getStd(SDPNTestProps.get.classPath, "Lbnord/unittests/Main")
  }

  @AfterClass
  def tearDown() {
    analysis = null
  }
//  
//  /**
//   * test suite for JUnit3 and SBT compatibility 
//   */
//  def suite() :junit.framework.Test =  new JUnit4TestAdapter(classOf[UtilTest]) 

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
      for (instr <- bb.iteratePhis()) {
        index += 1
      }
      if (bb.isCatchBlock())
        index += 1
      for (instr <- bb.iterateNormalInstructions()) {
        val start = bb.getFirstInstructionIndex()
        val stop = bb.getLastInstructionIndex()
        var arrIndex = -1
        for (i <- start to stop) {
          if (instr.equals(instrArr(i))) {
            arrIndex = i
          }
        }
        val bIndex = bcM.getBytecodeIndex(arrIndex)
        val ln = im.getLineNumber(bIndex)

        if (ln == 18) {
          found = true
          println("Found Match for: " + instr)
          println("bbnr: " + bb.getNumber + "\t index: " + index + "\t arrIndex: " + arrIndex)
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
    graph addEdge ("A", "B")
    graph addEdge ("B", "C")
    graph addEdge ("C", "A")
    graph addEdge ("C", "H")
    graph addEdge ("H", "End")
    graph addEdge ("Start", "D")
    graph addEdge ("D", "G")
    graph addEdge ("Start", "E")
    graph addEdge ("E", "F")
    graph addEdge ("F", "G")
    graph addEdge ("G", "End")

    println("Printing graph")
    println(GraphPrint.genericToString(graph))

    val gcf = new GraphCycleFinder(graph)
    assert(gcf.solve(null), "Couldnt Solve problem")

    assert(gcf.inCycle("A"), "A should be in cycle but is not!")
    assert(gcf.inCycle("B"), "B should be in cycle but is not!")
    assert(gcf.inCycle("C"), "C should be in cycle but is not!")
    assert(!gcf.inCycle("H"), "H shouldn't be in cycle but is!")
    assert(!gcf.inCycle("Start"), "Start shouldn't be in cycle but is!")
    assert(!gcf.inCycle("D"), "D shouldn't be in cycle but is!")
    assert(!gcf.inCycle("E"), "E shouldn't be in cycle but is!")
    assert(!gcf.inCycle("F"), "F shouldn't be in cycle but is!")
    assert(!gcf.inCycle("G"), "G shouldn't be in cycle but is!")
    assert(!gcf.inCycle("End"), "End shouldn't be in cycle but is!")

  }

  @Test
  def testFullWitnessParserLockInsens() {
	  val pr = FullWitnessParser.parseTree(lisTestTree)
	  println(pr)
	  assert(pr.successful,"Couldn't parse lock insensitive test tree with two set reachability.")
  }
  @Test
  def testFullWitnessParserLockSens() {
	  val pr = FullWitnessParser.parseTree(lsTestTree)
	  println(pr)
	  assert(pr.successful,"Couldn't parse lock sensitive test tree with two set reachability.")
  }

  val lisTestTree = "st(c(c(0,s(0,0,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,0,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,1,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,1,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,2,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,2,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,3,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,3,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,4,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,4,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,5,0),0,0),i(2,i(top,top))),call1(st(c(c(0,s(5,0,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(5,1,0),0,0),i(2,i(top,top))),call1(st(c(c(0,s(10,0,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(10,1,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(10,1,1),0,0),i(2,i(top,top))),call2(st(c(c(0,s(16,0,0),0,1),i(1,i(top,top))),spawn(st(c(c(0,s(20,0,0),0,0),i(1,i(top,top))),base(st(c(c(0,s(20,1,0),0,0),i(1,i(top,top))),call1(st(c(c(0,s(23,0,0),0,0),i(1,i(top,top))),base(st(c(c(0,s(23,1,0),0,0),i(1,i(top,top))),call1(st(c(c(0,s(17,0,0),0,0),i(1,i(top,top))),nil(t(0,s(17,0,0)))))))))))),st(c(c(0,s(16,1,0),0,1),i(0,i(bot,bot))),ret))),st(c(c(0,s(10,1,2),0,0),i(1,i(top,top))),base(st(c(c(0,s(10,2,0),0,0),i(1,i(top,top))),call1(st(c(c(0,s(17,0,0),0,0),i(1,i(top,top))),nil(t(0,s(17,0,0))))))))))))))))))))))))))))))))))))))))"
  val lsTestTree = "st(c(i(c(0,s(0,0,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,0,1),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,1,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,1,1),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,2,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,2,1),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,3,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,3,1),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,4,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,4,1),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,5,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),call1(st(c(i(c(0,s(5,0,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(5,1,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),call1(st(c(i(c(0,s(10,0,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(10,1,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(10,1,1),0,0),0),i(l(3,3,2),i(2,i(top,top)))),call2(st(c(i(c(0,s(16,0,0),0,1),0),i(l(2,2,0),i(1,i(top,top)))),spawn(st(c(i(c(0,s(21,0,0),0,0),0),i(l(2,2,0),i(1,i(top,top)))),base(st(c(i(c(0,s(21,1,0),0,0),0),i(l(2,2,0),i(1,i(top,top)))),call1(st(c(i(c(0,s(24,0,0),0,0),0),i(l(2,2,0),i(1,i(top,top)))),base(st(c(i(c(0,s(24,1,0),0,0),0),i(l(2,2,0),i(1,i(top,top)))),base(st(c(i(c(0,s(24,1,1),0,0),0),i(l(2,2,0),i(1,i(top,top)))),acq(la(1,0),st(c(i(c(0,s(24,1,2),0,0),2),i(l(0,0,0),i(1,i(top,top)))),base(st(c(i(c(0,s(24,2,0),0,0),2),i(l(0,0,0),i(1,i(top,top)))),call1(st(c(i(c(0,s(18,0,0),0,0),2),i(l(0,0,0),i(1,i(top,top)))),nil(t(0,s(18,0,0)))))))))))))))))),st(c(i(c(0,s(16,1,0),0,1),0),i(l(0,0,0),i(0,i(bot,bot)))),ret))),st(c(i(c(0,s(10,1,2),0,0),0),i(l(1,3,2),i(1,i(top,top)))),base(st(c(i(c(0,s(10,2,0),0,0),0),i(l(1,3,2),i(1,i(top,top)))),base(st(c(i(c(0,s(10,2,1),0,0),0),i(l(1,3,2),i(1,i(top,top)))),acq(la(0,0),st(c(i(c(0,s(10,2,2),0,0),1),i(l(0,2,0),i(1,i(top,top)))),base(st(c(i(c(0,s(10,3,0),0,0),1),i(l(0,2,0),i(1,i(top,top)))),base(st(c(i(c(0,s(10,3,1),0,0),1),i(l(0,2,0),i(1,i(top,top)))),use(la(1,0),st(c(i(c(0,s(10,3,2),0,1),3),i(l(0,0,0),i(0,i(bot,bot)))),base(st(c(i(c(0,s(10,4,0),0,1),3),i(l(0,0,0),i(0,i(bot,bot)))),base(st(c(i(c(0,s(10,4,1),0,1),3),i(l(0,0,0),i(0,i(bot,bot)))),base(st(c(i(c(0,s(10,5,0),0,1),3),i(l(0,0,0),i(0,i(bot,bot)))),ret))))))),st(c(i(c(0,s(10,6,0),0,0),1),i(l(0,0,0),i(1,i(top,top)))),base(st(c(i(c(0,s(10,6,1),0,0),1),i(l(0,0,0),i(1,i(top,top)))),base(st(c(i(c(0,s(10,9,0),0,0),1),i(l(0,0,0),i(1,i(top,top)))),call1(st(c(i(c(0,s(18,0,0),0,0),1),i(l(0,0,0),i(1,i(top,top)))),nil(t(0,s(18,0,0))))))))))))))))))))))))))))))))))))))))))))))))))))))"

  @Test
  def testMethodParametersOfMethodNotCalled() {
      import com.ibm.wala.util.strings.StringStuff
      
      val mr = StringStuff.makeMethodReference("bnord.testapps.Main.dummy2(I)V")
      val im = cha.resolveMethod(mr)
      
      val cache = new AnalysisCache()
      val ir = cache.getIR(im)
      println("Number of Parameters dummy2: " + ir.getNumberOfParameters())
      println("Value number of parameter 0: " + ir.getParameter(0))
      assertEquals(ir.getNumberOfParameters ,1)
      assertEquals(ir.getParameter(0),1)
      
  }
}