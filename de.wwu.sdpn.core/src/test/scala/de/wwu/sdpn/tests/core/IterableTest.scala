package de.wwu.sdpn.tests.core
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.FunSuite
import org.scalatest.Suites
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import de.wwu.sdpn.core.textual._
import de.wwu.sdpn.core.analyses.IRTask
import de.wwu.sdpn.core.ta.xsb.iterable._
import de.wwu.sdpn.core.dpn.monitor.MonitorDPN
import de.wwu.sdpn.core.ta.xsb.IntersectionEmptinessCheck
import de.wwu.sdpn.core.ta.xsb.IntersectionTA
import de.wwu.sdpn.core.ta.xsb.XSBInterRunner
import de.wwu.sdpn.core.ta.xsb.FullWitnessIntersectionEmptinessCheck
import de.wwu.sdpn.core.ta.xsb.witness.iterable._
import org.junit.Test
import de.wwu.sdpn.core.ta.xsb.ScriptTreeAutomata
import de.wwu.sdpn.core.dpn.monitor.DPNRule

@RunWith(classOf[JUnitRunner])
class IterableTest extends FunSuite with ShouldMatchers with TableDrivenPropertyChecks {
//    override def nestedSuites = List(ActionLockSensetive)
    override def nestedSuites = List(LockInsensetive, LockSensetive,ActionLockInsensetive, ActionLockSensetive)
    override def suiteName = "Testing the iterable analyses"

    import TextualTestDPNs.dpn

    def S = List

    def lockInsensetiveTestData = Table(
        ("dpn number", "symbollist", "expectedResult"),
        (1, S("1", "2"), true),
        (1, S("2", "1"), false),
        (2, S("1", "2", "3"), true),
        (2, S("3", "2", "1"), false),
        (2, S("1", "2", "3", "4"), true),
        (2, S("3", "2", "1", "4"), false),
        (3, S("a1", "a2", "a3", "a4"), true),
        (4, S("a1", "a4"), true),
        (4, S("a2", "a4"), true),
        (4, S("a2", "a3"), true),
        (4, S("a1", "a2", "a4"), true),
        (4, S("a2", "a3", "a4"), true),
        (4, S("a1", "a2", "a3", "a4"), true),
        (5, S("a1", "a4", "a4"), true),
        (5, S("a1", "a3", "a4"), true),
        (5, S("a3", "a4"), true),
        (6, S("a2", "a3"), true),
        (7, S("d1", "b2"), true),
        (9, S("b1","d1"), true)
    )

    def lockSensetiveTestData = Table(
        ("Number of the DPN", "List of symbols", "Expected result"),
//        (1, S("1", "2"), true), // There are no locks so the same analysis is run as above!
//        (1, S("2", "1"), false),
//        (2, S("1", "2", "3"), true),
//        (2, S("3", "2", "1"), false),
//        (2, S("1", "2", "3", "4"), true),
//        (2, S("3", "2", "1", "4"), false),
//        (3, S("a1", "a2", "a3", "a4"), true),
//        (4, S("a1", "a4"), true),
//        (4, S("a2", "a4"), true),
//        (4, S("a2", "a3"), true),
//        (4, S("a1", "a2", "a4"), true),
//        (4, S("a2", "a3", "a4"), true),
//        (4, S("a1", "a2", "a3", "a4"), true),
//        (5, S("a1", "a4", "a4"), true),
//        (5, S("a1", "a3", "a4"), true),
//        (5, S("a3", "a4"), true),
//        (6, S("a2", "a3"), true),
        // new stuff
        (7, S("a1", "a2"), true),
        (7, S("a1", "b1"), true),
        (7, S("d1"), true),
        (7, S("b1"), true),
        (7, S("d1", "b2"), false),
        (8, S("a0","a1"), true),
        (8, S("a1","a2"), true),
        (9, S("b1","d1"), true)
    )

    object LockSensetive extends FunSuite {
        override def suiteName = "Lock sensetive"
        forAll(lockSensetiveTestData) {
            (nr: Int, syms: List[String], expectedResult: Boolean) =>
                val testname = "DPN number: %d expected %s conflict on symbols %s ".format(nr, if (expectedResult) "a" else "no", syms.mkString(","))
                test(testname) {
                    val (result, owitness) = runAnalysis(syms, dpn(nr), true)
                    if (result)
                        println(testname + "\nWitness:\n" + owitness.get.printTreeStates)
                    result should equal(expectedResult)
                    owitness.isDefined should equal (expectedResult)
                }
        }
    }
    object LockInsensetive extends FunSuite {
        override def suiteName = "Lock insensetive"
        forAll(lockInsensetiveTestData) {
            (nr: Int, syms: List[String], expectedResult: Boolean) =>
                val testname = "DPN number: %d expected %s conflict on symbols %s ".format(nr, if (expectedResult) "a" else "no", syms.mkString(","))
                test(testname) {
                    val (result, owitness) = runAnalysis(syms, dpn(nr), false)
                    if (result)
                        println(testname + "\nWitness:\n" + owitness.get.printTreeStates)
                    result should equal(expectedResult)
                    owitness.isDefined should equal (expectedResult)
                }
        }
    }
    
    object ActionLockSensetive extends FunSuite {
        override def suiteName = "Action lock sensetive"
        forAll(lockSensetiveTestData) {
            (nr: Int, syms: List[String], expectedResult: Boolean) =>
                val testname = "DPN number: %d expected %s conflict on symbols %s ".format(nr, if (expectedResult) "a" else "no", syms.mkString(","))
                test(testname) {
                    val (result, owitness) = runActionAnalysis(syms, dpn(nr), true)
                    if (result)
                        println(testname + "\nWitness:\n" + owitness.get.printTreeStates)
                    result should equal(expectedResult)
                    owitness.isDefined should equal (expectedResult)
                }
        }
    }
    object ActionLockInsensetive extends FunSuite {
        override def suiteName = "Action lock insensetive"
        forAll(lockInsensetiveTestData) {
            (nr: Int, syms: List[String], expectedResult: Boolean) =>
                val testname = "DPN number: %d expected %s conflict on symbols %s ".format(nr, if (expectedResult) "a" else "no", syms.mkString(","))
                test(testname) {
                    val (result, owitness) = runActionAnalysis(syms, dpn(nr), false)
                    if (result)
                        println(testname + "\nWitness:\n" + owitness.get.printTreeStates)
                    result should equal(expectedResult)
                    owitness.isDefined should equal (expectedResult)
                }
        }
    }

    
    
    def runAnalysis(symsS: List[String], dpnString: String, lockSens: Boolean = false): (Boolean, Option[WitnessTree]) = {
        val pr = DPNParser.parseDPN(dpnString)
        if (!pr.successful)
            println(pr)
        val (dpn, naming) = pr.get

        symsS should not be ('empty)

        val syms = symsS.map(x => Set(naming.sname(x)))

        val task = IRTask(dpn, syms, lockSens)

        val result = task.run
        val witness = task.runWitness

        (result, witness)
    }
    
    def runActionAnalysis(symsS: List[String], dpnString: String, lockSens: Boolean = false): (Boolean, Option[WitnessTree]) = {
        val pr = DPNParser.parseDPN(dpnString)
        if (!pr.successful)
            println(pr)
        val (dpn, naming) = pr.get

        symsS should not be ('empty)

        val syms = symsS.map(x => Set(naming.sname(x)))
        val fst::tail = syms
        val confs: List[(Set[DPNRule[Control,Stack,Action]],Set[Stack])] = tail.map(x => (Set[DPNRule[Control,Stack,Action]](),x))

        import de.wwu.sdpn.core.analyses.AIRTask
        
        val task = AIRTask(dpn, fst,confs, lockSens)

        val result = task.run
        val witness = task.runWitness

        (result, witness)
    }

}