package de.wwu.sdpn.test.pfg
import de.wwu.sdpn.wala.util.PreAnalysis
import de.wwu.sdpn.wala.analyses.MyPreAnalysis
import org.junit.BeforeClass
import de.wwu.sdpn.pfg.wala.PFGFactory
import org.junit.Test
import de.wwu.sdpn.pfg.wala.WalaPFG
import de.wwu.sdpn.pfg.wala.BaseEdge
import de.wwu.sdpn.pfg.wala.SpawnEdge
import de.wwu.sdpn.pfg.wala.CallEdge
import com.ibm.wala.util.strings.StringStuff
import scala.collection.JavaConversions._
import de.wwu.sdpn.pfg.wala.Edge
import org.junit.Assert._
import de.wwu.sdpn.pfg.genkill.PFGForwardGenKillSolver
import de.wwu.sdpn.pfg.wala.Node
import de.wwu.sdpn.pfg.wala.N
import de.wwu.sdpn.pfg.wala.CFGPoint
import de.wwu.sdpn.pfg.lattices.LMap
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import de.wwu.sdpn.pfg.wala.SSAAction
import com.ibm.wala.ssa.SSAPutInstruction
import com.ibm.wala.types.FieldReference
import DefUseUtil._
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.callgraph.CallGraph
import de.wwu.sdpn.pfg.lattices.genkill.GenKill
import de.wwu.sdpn.pfg.lattices.TopMap
import de.wwu.sdpn.pfg.lattices.BottomMap
import de.wwu.sdpn.pfg.lattices.Lattice
import de.wwu.sdpn.pfg.wala.E
import de.wwu.sdpn.pfg.wala.DefUse

class DefUseTest {

    @Test
    def defUseTest01 {
        import de.wwu.sdpn.wala.analyses.SDPNTestProps
        val cp = SDPNTestProps.get.classPath
        val mc = "Lbnord/unittests/defuse/Test01"
        val preAnalysis = MyPreAnalysis.getStd(cp, mc)
        val(pa,cg) = (preAnalysis.pa, preAnalysis.cg)
        val du = new DefUse(cg,pa)
        
        du.solve()
        
        println(du.printResults)
        //todo check something here ;)
    }
    

}