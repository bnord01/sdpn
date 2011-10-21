package sdpn.runner

import sdpn.dpn.datalog.ExplicitTranslator
import sdpn.dpn.explicit.example.DPNFactory

object DatalogTest {

    def main(args: Array[String]): Unit = {
        val analysis = MyPreAnalysis.getStd        
        val dpnfac = new MyPreAnalysis(analysis) with DPNFactory 
        val trans = new ExplicitTranslator(dpnfac.getDPN)
        trans.writeOutput
    }

}