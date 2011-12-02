package de.wwu.sdpn.tests.analyses
import org.junit.Test
import org.junit.BeforeClass
import org.junit.AfterClass
import com.ibm.wala.classLoader.IMethod
import scala.collection.JavaConversions._
import com.ibm.wala.util.strings.StringStuff
import org.junit.Assert._
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.types.MethodReference
import com.ibm.wala.ipa.cha.IClassHierarchy
import com.ibm.wala.util.ProgressMonitorDelegate
import de.wwu.sdpn.wala.analyses.SimpleAnalyses
import de.wwu.sdpn.wala.analyses.SDPNTestProps
import de.wwu.sdpn.wala.analyses.DPN4IFCAnalysis
import de.wwu.sdpn.wala.dpngen.symbols.StackSymbol
import de.wwu.sdpn.wala.dpngen.symbols.StackSymbol

object DPN4IFCTest {

  var stuff: Map[Int, (CallGraph, PointerAnalysis,IClassHierarchy)] = Map()

  @BeforeClass
  def setUp() {
    for (i <- 1 to 3) {
      val (cg, pa) = SimpleAnalyses.getCGandPAfromCP(SDPNTestProps.get.classPath, "Lbnord/unittests/dpn4ifc/BSP0" + i)
      stuff += i -> (cg, pa,cg.getClassHierarchy)
    }
  }

  @AfterClass
  def tearDown() {
    stuff = null
  }
}

class DPN4IFCTest {
  import DPN4IFCTest.stuff

  @Test
  def testIndexMappingWithDPN4IFC {
    val (cg,pa,cha) = stuff(1)
    val dia = new DPN4IFCAnalysis(cg, pa)
    val im = mrr("bnord.unittests.dpn4ifc.BSP01.p2()V",cha)
    val nodes = cg.getNodes(im.getReference())
    assert(nodes.size == 1)
    val node = nodes.first
    val expResult = StackSymbol(node, 2, 0)
    val realResult = dia.getSS4NodeAndIndex(node, 5)
    println(realResult)
    assertTrue("Got wrong StackSymbol got " + realResult + "\t expected" + expResult, expResult == realResult)

  }
  
  @Test
  def testBSP01 {
    val (cg,pa,cha) = stuff(1)
    val dia = new DPN4IFCAnalysis(cg, pa)
    var im = mrr("bnord.unittests.dpn4ifc.BSP01.p2()V",cha)
    var nodes = cg.getNodes(im.getReference())
    assert(nodes.size == 1)
    var node = nodes.first
    val writePos = StackSymbol(node, 2, 0)
    
    im = mrr("bnord.unittests.dpn4ifc.BSP01.p1()V",cha)
    nodes = cg.getNodes(im.getReference())
    assert(nodes.size == 1)
    node = nodes.first
    val readPos = StackSymbol(node, 5, 0)
    dia.init(ProgressMonitorDelegate.createProgressMonitorDelegate((new PrintingPM())))
    val res = dia.runWeakCheck(writePos,readPos,ProgressMonitorDelegate.createProgressMonitorDelegate(new PrintingPM()))
    assertFalse("there should be no flow", res)
    
    
  }

  @Test
  def testBSP02 {
    val (cg,pa,cha) = stuff(2)
    val dia = new DPN4IFCAnalysis(cg, pa)
    var im = mrr("bnord.unittests.dpn4ifc.BSP02.p2()V",cha)
    var nodes = cg.getNodes(im.getReference())
    assert(nodes.size == 1)
    var node = nodes.first
    val writePos = StackSymbol(node, 2, 0)
    
    im = mrr("bnord.unittests.dpn4ifc.BSP02.p1()V",cha)
    nodes = cg.getNodes(im.getReference())
    assert(nodes.size == 1)
    node = nodes.first
    val readPos = StackSymbol(node, 9, 0)
    dia.init(ProgressMonitorDelegate.createProgressMonitorDelegate((new PrintingPM())))
    val res = dia.runWeakCheck(writePos,readPos,ProgressMonitorDelegate.createProgressMonitorDelegate(new PrintingPM()))
    assertTrue("there should be flow", res)
    
    
  }
  
  @Test
  def testBSP03 {
    val (cg,pa,cha) = stuff(3)
    val dia = new DPN4IFCAnalysis(cg, pa)
    var im = mrr("bnord.unittests.dpn4ifc.BSP03.p2()V",cha)
    var nodes = cg.getNodes(im.getReference())
    assert(nodes.size == 1,"Expected 1 node but got: " + nodes.size)
    var node = nodes.first
    println(node.getIR)
    val writePos = StackSymbol(node, 1, 0)
    
    im = mrr("bnord.unittests.dpn4ifc.BSP03.p1()V",cha)
    nodes = cg.getNodes(im.getReference())
    assert(nodes.size == 1)
    node = nodes.first
    println(node.getIR)
    val readPos = StackSymbol(node, 2, 0)
    dia.init(ProgressMonitorDelegate.createProgressMonitorDelegate((new PrintingPM())))
    println(dia.getPossibleLocks)
    val res = dia.runWeakCheck(writePos,readPos,ProgressMonitorDelegate.createProgressMonitorDelegate(new PrintingPM()))
    assertFalse("there should be no flow", res) 
    
    
  }
  
  def mrr(methodSig: String,cha: IClassHierarchy): IMethod = {
    val mr = StringStuff.makeMethodReference(methodSig)

    val m = cha.resolveMethod(mr);
    assertNotNull("IMethod Null", m)
    return m

  }

}