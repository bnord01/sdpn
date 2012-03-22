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
import de.wwu.sdpn.wala.util.DefUseUtil
import com.ibm.wala.types.ClassLoaderReference

class DefUseTest {

    def runDUTest(className:String,expectedNumberOfDepsInApplication:Int) {
        import de.wwu.sdpn.wala.analyses.SDPNTestProps
        import System.{currentTimeMillis => now}
        val start = now
        println()
        println(" -------------- RUNNING FOR " + className + " ----------------")
        val cp = SDPNTestProps.get.classPath
        val mc = "Lbnord/unittests/defuse/" + className
        val preAnalysis = MyPreAnalysis.getStd(cp, mc)
        val (cg, pa) = (preAnalysis.cg, preAnalysis.pa)
        val du = new DefUse(cg, pa)
        val startSolve = now
        val timeInit = startSolve - start
        printf("Time wala + init: %d.%03ds%n", timeInit/1000 ,timeInit%1000)
        
        du.solve()

        val startPrint = now
        val timeSolve = startPrint - startSolve
        printf("Time solve      : %d.%03ds%n", timeSolve/1000 ,timeSolve%1000)
        
        	
        val duu = new DefUseUtil(cg, pa)
        var number = 0
        
        println(" -------------- RESULTS FOR " + className + " ----------------")
        
        for (
            ((srcNode, srcIdx), (snkNode, snkIdx)) <- duu.possibleDeps if srcNode.getMethod().getDeclaringClass().getClassLoader().getReference() == ClassLoaderReference.Application
        ) if (du.flowPossible(srcNode, srcIdx, snkNode, snkIdx)) {
            println("Possible flow: ")
            println("  " + srcNode.getMethod().getSignature() + "    " + srcNode)
            println("     " + srcNode.getIR().getInstructions()(srcIdx))
            println(" --->")
            println("  " + snkNode.getMethod().getSignature() + "    " + snkNode)
            println("     " + snkNode.getIR().getInstructions()(snkIdx))
            number += 1
        }
        println(" -------- END OF RESULTS FOR " + className + " --------------")
        val end = now
        val timePrint = end - startPrint
        val timeTotal = end - start
        printf("Time print      : %d.%03ds%n", timePrint/1000 ,timePrint%1000)
        printf("Time total      : %d.%03ds%n", timeTotal/1000 ,timeTotal%1000)        
        println("Number of Statements: " + du.getNumberOfStatements)
        println("--------- FINISHED " + className + " -------------")
        assertEquals("Didn't found expected number of dependencies",expectedNumberOfDepsInApplication,number)
    }
    
    @Test
    def defUseTest01 {
        runDUTest("Test01",0)
    }
    
    @Test
    def defUseTest02 {
        runDUTest("Test02",1)
    }
    @Test
    def defUseTest03 {
        runDUTest("Test03",2)
    }
    @Test
    def defUseTest04 {
        runDUTest("Test04",3)
    }
    
//    @Test // This hasn't finished yet
    def defUseTestPrintln01 {
        runDUTest("TestPrintln01",0)
    }

}