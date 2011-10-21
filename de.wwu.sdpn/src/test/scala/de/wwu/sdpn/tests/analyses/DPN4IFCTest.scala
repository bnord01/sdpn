package de.wwu.sdpn.tests.analyses
import org.junit.Test
import de.wwu.sdpn.util.PreAnalysis
import org.junit.BeforeClass
import de.wwu.sdpn.analysis.MyPreAnalysis
import org.junit.AfterClass
import de.wwu.sdpn.analysis.SDPNProps
import de.wwu.sdpn.analysis.DPN4IFCAnalysis
import com.ibm.wala.classLoader.IMethod
import scala.collection.JavaConversions._
import de.wwu.sdpn.dpn.explicit.StackSymbol
import com.ibm.wala.util.strings.StringStuff
import org.junit.Assert._

object DPN4IFCTest {
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

class DPN4IFCTest {
  val analysis = DPN4IFCTest.analysis
  import analysis.{ cg, pa, cha }

  @Test
  def testIndexMappingWithDPN4IFC {
    val dia = new DPN4IFCAnalysis(analysis.cg, analysis.pa)
    val im = mrr("bnord.unittests.Main.p2()V")
    val nodes = analysis.cg.getNodes(im.getReference())
    assert(nodes.size == 1)
    val node = nodes.first
    val expResult = StackSymbol(node, 1, 1)
    val realResult = dia.getSS4NodeAndIndex(node, 3)
    println(realResult)
    assertTrue("Got wrong StackSymbol got " + realResult + "\t expected" + expResult, expResult == realResult)

  }

  def mrr(methodSig: String): IMethod = {
    val mr = StringStuff.makeMethodReference(methodSig)

    val m = cha.resolveMethod(mr);
    assertNotNull("IMethod Null", m)
    return m

  }

}