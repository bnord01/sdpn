package de.wwu.sdpn.tests.gui

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
import de.wwu.sdpn.ta.witness.zest.WTGraph

object GuiTests {

  var stuff: Map[Int, (CallGraph, PointerAnalysis, MethodReference)] = Map()

  @BeforeClass
  def setUp() {
    for (i <- 5 to 5) {
      val (cg, pa) = SimpleAnalyses.getCGandPAfromCP(SDPNProps.get.classPath, "Lbnord/unittests/simpleAnalyses/BSP0" + i)
      val mr = StringStuff.makeMethodReference("bnord.unittests.simpleAnalyses.BSP0" + i + ".excludeMe()V")
      stuff += i -> (cg, pa, mr)
    }
  }

  @AfterClass
  def tearDown() {
    stuff = null
  }
}

class GuiTests {
  import GuiTests._

  val lisTestTree = "st(c(c(0,s(0,0,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,0,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,1,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,1,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,2,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,2,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,3,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,3,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,4,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,4,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,5,0),0,0),i(2,i(top,top))),call1(st(c(c(0,s(5,0,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(5,1,0),0,0),i(2,i(top,top))),call1(st(c(c(0,s(10,0,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(10,1,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(10,1,1),0,0),i(2,i(top,top))),call2(st(c(c(0,s(16,0,0),0,1),i(1,i(top,top))),spawn(st(c(c(0,s(20,0,0),0,0),i(1,i(top,top))),base(st(c(c(0,s(20,1,0),0,0),i(1,i(top,top))),call1(st(c(c(0,s(23,0,0),0,0),i(1,i(top,top))),base(st(c(c(0,s(23,1,0),0,0),i(1,i(top,top))),call1(st(c(c(0,s(17,0,0),0,0),i(1,i(top,top))),nil(t(0,s(17,0,0)))))))))))),st(c(c(0,s(16,1,0),0,1),i(0,i(bot,bot))),ret))),st(c(c(0,s(10,1,2),0,0),i(1,i(top,top))),base(st(c(c(0,s(10,2,0),0,0),i(1,i(top,top))),call1(st(c(c(0,s(17,0,0),0,0),i(1,i(top,top))),nil(t(0,s(17,0,0))))))))))))))))))))))))))))))))))))))))"

    // One test at most!
  //@Test 
  def testStaticGUI() {
    val lsTestTree = "st(c(i(c(0,s(0,0,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,0,1),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,1,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,1,1),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,2,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,2,1),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,3,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,3,1),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,4,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,4,1),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,5,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),call1(st(c(i(c(0,s(5,0,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(5,1,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),call1(st(c(i(c(0,s(10,0,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(10,1,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(10,1,1),0,0),0),i(l(3,3,2),i(2,i(top,top)))),call2(st(c(i(c(0,s(16,0,0),0,1),0),i(l(2,2,0),i(1,i(top,top)))),spawn(st(c(i(c(0,s(21,0,0),0,0),0),i(l(2,2,0),i(1,i(top,top)))),base(st(c(i(c(0,s(21,1,0),0,0),0),i(l(2,2,0),i(1,i(top,top)))),call1(st(c(i(c(0,s(24,0,0),0,0),0),i(l(2,2,0),i(1,i(top,top)))),base(st(c(i(c(0,s(24,1,0),0,0),0),i(l(2,2,0),i(1,i(top,top)))),base(st(c(i(c(0,s(24,1,1),0,0),0),i(l(2,2,0),i(1,i(top,top)))),acq(la(1,0),st(c(i(c(0,s(24,1,2),0,0),2),i(l(0,0,0),i(1,i(top,top)))),base(st(c(i(c(0,s(24,2,0),0,0),2),i(l(0,0,0),i(1,i(top,top)))),call1(st(c(i(c(0,s(18,0,0),0,0),2),i(l(0,0,0),i(1,i(top,top)))),nil(t(0,s(18,0,0)))))))))))))))))),st(c(i(c(0,s(16,1,0),0,1),0),i(l(0,0,0),i(0,i(bot,bot)))),ret))),st(c(i(c(0,s(10,1,2),0,0),0),i(l(1,3,2),i(1,i(top,top)))),base(st(c(i(c(0,s(10,2,0),0,0),0),i(l(1,3,2),i(1,i(top,top)))),base(st(c(i(c(0,s(10,2,1),0,0),0),i(l(1,3,2),i(1,i(top,top)))),acq(la(0,0),st(c(i(c(0,s(10,2,2),0,0),1),i(l(0,2,0),i(1,i(top,top)))),base(st(c(i(c(0,s(10,3,0),0,0),1),i(l(0,2,0),i(1,i(top,top)))),base(st(c(i(c(0,s(10,3,1),0,0),1),i(l(0,2,0),i(1,i(top,top)))),use(la(1,0),st(c(i(c(0,s(10,3,2),0,1),3),i(l(0,0,0),i(0,i(bot,bot)))),base(st(c(i(c(0,s(10,4,0),0,1),3),i(l(0,0,0),i(0,i(bot,bot)))),base(st(c(i(c(0,s(10,4,1),0,1),3),i(l(0,0,0),i(0,i(bot,bot)))),base(st(c(i(c(0,s(10,5,0),0,1),3),i(l(0,0,0),i(0,i(bot,bot)))),ret))))))),st(c(i(c(0,s(10,6,0),0,0),1),i(l(0,0,0),i(1,i(top,top)))),base(st(c(i(c(0,s(10,6,1),0,0),1),i(l(0,0,0),i(1,i(top,top)))),base(st(c(i(c(0,s(10,9,0),0,0),1),i(l(0,0,0),i(1,i(top,top)))),call1(st(c(i(c(0,s(18,0,0),0,0),1),i(l(0,0,0),i(1,i(top,top)))),nil(t(0,s(18,0,0))))))))))))))))))))))))))))))))))))))))))))))))))))))"
    val witness = FullWitnessParser.parseTree(lsTestTree)
    WTGraph.showTree(witness.get)
  }

  @Test
  def testWTGUI() {
    val (cg, pa, mr) = stuff(5)
    val nodes = cg.getNodes(mr)
    val res = SimpleAnalyses.runWitnessTSRCheck(cg, pa, getStackSymbols(cg, mr), getStackSymbols(cg, mr), nodes, true)

    assertFalse("There should be an Conflict", res == None)
    val switness = res.get
    val parser = FullWitnessParser
    val witness = parser.parseTree(switness)
    WTGraph.showTree(witness.get, cg)

  }

  def getStackSymbols(cg: CallGraph, mr: MethodReference): Set[StackSymbol] = {
    val nodes = cg.getNodes(mr)
    var res = Set[StackSymbol]()
    nodes.foreach({ res += StackSymbol(_, 0, 0) })
    return res
  }

}