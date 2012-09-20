package de.wwu.sdpn.tests
import org.junit.runner.RunWith
import org.junit.runners.Suite
import org.junit.runners.Suite.SuiteClasses
import de.wwu.sdpn.tests.analyses.DataraceAnalysisTest
import de.wwu.sdpn.tests.analyses.DPN4IFCTest
import de.wwu.sdpn.tests.analyses.SimpleAnalysesTest
import de.wwu.sdpn.tests.randomisolation.SimpleRITest
import de.wwu.sdpn.tests.util.UtilTest
import de.wwu.sdpn.tests.analyses.DPN4IFCAnalysisMFTest

/**
 * Suit to run all wala specific sdpn tests.
 * @author Benedikt Nordhoff <b.n (at) wwu.de>
 *
 */
@RunWith(classOf[Suite])
@SuiteClasses(Array(
    classOf[SimpleAnalysesTest],
    classOf[DataraceAnalysisTest],
    classOf[DPN4IFCTest],
    classOf[DPN4IFCAnalysisMFTest],
    classOf[SimpleRITest],
    classOf[UtilTest]
))
class AllsDPNWalaTests {

}