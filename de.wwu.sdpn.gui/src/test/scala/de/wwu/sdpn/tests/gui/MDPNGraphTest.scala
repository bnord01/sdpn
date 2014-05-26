package de.wwu.sdpn.tests.gui

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
import de.wwu.sdpn.wala.dpngen.symbols.GlobalState
import de.wwu.sdpn.wala.dpngen.symbols.DPNAction
import de.wwu.sdpn.gui.dpn.zest.MDPNGraph
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import de.wwu.sdpn.gui.dpn.zest.ToStringDecorator

object MDPNGraphTest {

  var stuff: Map[Int, (CallGraph, PointerAnalysis[InstanceKey], MethodReference)] = Map()

  @BeforeClass
  def setUp() {
    for (i <- 5 to 5) {
      val (cg, pa) = SimpleAnalyses.getCGandPAfromCP(SDPNTestProps.get.classPath, "Lbnord/unittests/simpleAnalyses/BSP0" + i)
      val mr = StringStuff.makeMethodReference("bnord.unittests.simpleAnalyses.BSP0" + i + ".excludeMe()V")
      stuff += i -> (cg, pa, mr)
    }
  }

  @AfterClass
  def tearDown() {
    stuff = null
  }
}

class MDPNGraphTest {
  import MDPNGraphTest._

  val lisTestTree = "st(c(c(0,s(0,0,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,0,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,1,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,1,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,2,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,2,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,3,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,3,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,4,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,4,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,5,0),0,0),i(2,i(top,top))),call1(st(c(c(0,s(5,0,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(5,1,0),0,0),i(2,i(top,top))),call1(st(c(c(0,s(10,0,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(10,1,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(10,1,1),0,0),i(2,i(top,top))),call2(st(c(c(0,s(16,0,0),0,1),i(1,i(top,top))),spawn(st(c(c(0,s(20,0,0),0,0),i(1,i(top,top))),base(st(c(c(0,s(20,1,0),0,0),i(1,i(top,top))),call1(st(c(c(0,s(23,0,0),0,0),i(1,i(top,top))),base(st(c(c(0,s(23,1,0),0,0),i(1,i(top,top))),call1(st(c(c(0,s(17,0,0),0,0),i(1,i(top,top))),nil(t(0,s(17,0,0)))))))))))),st(c(c(0,s(16,1,0),0,1),i(0,i(bot,bot))),ret))),st(c(c(0,s(10,1,2),0,0),i(1,i(top,top))),base(st(c(c(0,s(10,2,0),0,0),i(1,i(top,top))),call1(st(c(c(0,s(17,0,0),0,0),i(1,i(top,top))),nil(t(0,s(17,0,0))))))))))))))))))))))))))))))))))))))))"

    

  @Test
  def testMDPNGUI() {
    val (cg, pa, mr) = stuff(5)
    val nodes = cg.getNodes(mr)
    val mdpn = SimpleAnalyses.getMDPN(cg, pa, nodes, true)

    
    MDPNGraph.showMDPN(mdpn,ToStringDecorator.get[GlobalState,StackSymbol,DPNAction,InstanceKey])

  }

  def getStackSymbols(cg: CallGraph, mr: MethodReference): Set[StackSymbol] = {
    val nodes = cg.getNodes(mr)
    var res = Set[StackSymbol]()
    nodes.foreach({ res += StackSymbol(_, 0, 0) })
    return res
  }

}