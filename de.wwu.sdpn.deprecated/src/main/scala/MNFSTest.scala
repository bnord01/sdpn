package sdpn.runner

import sdpn.util.BackwardSliceFilter
import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import java.io.PrintWriter
import sdpn.mnfs.MNFS2Prolog
import java.io.IOException
import java.io.PrintStream
import java.io.PipedOutputStream
import java.io.PipedInputStream
import sdpn.mnfs.ConflictMNFSGenerator
import sdpn.dpn.datalog.ExplicitTranslator
import sdpn.dpn.explicit.example._
import sdpn.mnfs.MNFS2Datalog
import sdpn.util.MNFSPrinter
import sdpn.mnfs.MNFS
import scala.collection.Set
import scala.collection.JavaConversions._

object MNFSTest {

    def main(args: Array[String]): Unit = {
//        example
//        explicitMNFS
        
//        genProlog
        
    	val f = new File("target/datalog")
    	if(!f.exists)
    		f.mkdir()
    		
    	genRunSymbolicDatalog	
    	genSymbolicProlog
    	genPrologTA
    	time(_ => runProlog("[ta],conflict."))
    	time(_ => runProlog("[dpns],conflict."))
        
        
//    	genRunSymbolicDatalog
    	
    	
    	// run with xsb -e "[ta],conflict,halt." --nobanner --quietload --noprompt
    }
    
    def time (action: Unit => Any) {
    	val t = System.currentTimeMillis
    	action()
    	val total = System.currentTimeMillis - t
    	println(total + "ms")    	
    }
    
    def runProlog(command: String) {
    	
    	val runcommand = "/u/b_nord01/local/XSB/bin/xsb -e \"" + command + "\" --nobanner --quietload --noprompt"
    	
    	val rt = Runtime.getRuntime().exec(runcommand,null,new File("target/datalog"))
    	
    	val in = rt.getInputStream
    	
    	val x = scala.io.Source.fromInputStream(in)
    	try {
    		for (line <- x.getLines){
    			if (line.startsWith("no")) {
    				println("NO!  : " + command)
    				rt.destroy()
    			}
    			if (line.startsWith("yes")) {
    				println("YES!  : " + command)
    				rt.destroy()
    			}    			
    		}            			
    	} catch {
    	case e: IOException =>  
    	}
    	
    }

    def genProlog() {
        val time1 = System.currentTimeMillis
        val analysis = MyPreAnalysis.getStd
        import analysis._

        val timewala = System.currentTimeMillis

        val entryNode = cg.getFakeRootNode
        val dpnfac = new MyPreAnalysis(analysis) with DPNFactory 
        
        val trans = new ExplicitTranslator(dpnfac.getDPN)

        val timedpn = System.currentTimeMillis

        val dpnrules = trans.transRules
        val pmap = trans.getGMap
        val gmap = trans.getSMap
        val pset = dpnfac.getDPN.getControlSymbols;
        val gset = dpnfac.getDPN.getStackSymbols;
        val methodSig = "bnord.testapps.Main.excludeMe()V"

        val cset = ConflictMNFSGenerator.generateConflictSet4Method(cg, methodSig)

        val mnfs = ConflictMNFSGenerator.generateMNFS(pset, gset, cset, cset)

        mnfs.setStartConf(NState, StackSymbol(entryNode, 0, 0));
        val automata = MNFS2Prolog.generateProlog(mnfs, pmap, gmap)
        val out = new PrintWriter(new BufferedWriter(new FileWriter("target/datalog/dpn.P")));
        out.println(dpnrules.toString)
        out.println(automata.toString)
        out.close();

        val timewrite = System.currentTimeMillis
        //        net.sf.bddbddb.Solver.main(Array("target/datalog/mnfs.datalog"))
        //        val timesolve = System.currentTimeMillis
        println("WALA:  " + (timewala - time1) + "ms")
        println("sDPN:  " + (timedpn - timewala) + "ms")
        println("write: " + (timewrite - timedpn) + "ms")
        //        println("bdd:   " + (timesolve - timewrite) + "ms")
        println("Total: " + (timewrite - time1) + "ms")

    }
    
    def genSymbolicProlog() {
        val time1 = System.currentTimeMillis
        val analysis = MyPreAnalysis.getStd
        import analysis._

        val timewala = System.currentTimeMillis

        val entryNode = cg.getFakeRootNode()
            
        val dpnfac = new MyPreAnalysis(analysis) with DPNFactory
            
        val trans = new ExplicitTranslator(dpnfac.getDPN)

        val timedpn = System.currentTimeMillis

        val dpnrules = trans.transRules
        val gmap = trans.getGMap
        val smap = trans.getSMap
        
        val methodSig = "bnord.testapps.Main.excludeMe()V"

        val cset = ConflictMNFSGenerator.generateConflictSet4Method2(cg, methodSig)

        val mnfs = ConflictMNFSGenerator.generateSymbolicMNFS(gmap, smap, cset,(NState, StackSymbol(entryNode, 0, 0)))

        val out = new PrintWriter(new BufferedWriter(new FileWriter("target/datalog/dpns.P")));
        out.println(dpnrules.toString)
        out.println(mnfs.toString)
        out.close();

        val timewrite = System.currentTimeMillis
        //        net.sf.bddbddb.Solver.main(Array("target/datalog/mnfs.datalog"))
        //        val timesolve = System.currentTimeMillis
        println("WALA:  " + (timewala - time1) + "ms")
        println("sDPN:  " + (timedpn - timewala) + "ms")
        println("write: " + (timewrite - timedpn) + "ms")
        //        println("bdd:   " + (timesolve - timewrite) + "ms")
        println("Total: " + (timewrite - time1) + "ms")

    }
    
    def genPrologTA() {
        val time1 = System.currentTimeMillis
        val analysis = MyPreAnalysis.getStd
        import analysis._

        val timewala = System.currentTimeMillis

        val entryNode = cg.getFakeRootNode()
        val dpnfac = new MyPreAnalysis(analysis) with DPNFactory 
        val trans = new ExplicitTranslator(dpnfac.getDPN)

        val timedpn = System.currentTimeMillis

        val dpnrules = trans.transRules
        val gmap = trans.getGMap
        val smap = trans.getSMap
        
        val methodSig = "bnord.testapps.Main.excludeMe()V"

        val cset = ConflictMNFSGenerator.generateConflictSet4Method2(cg, methodSig)

        val ta = ConflictMNFSGenerator.generateTA(gmap, smap, cset,(NState, StackSymbol(entryNode, 0, 0)))

        val out = new PrintWriter(new BufferedWriter(new FileWriter("target/datalog/ta.P")));
        out.println(dpnrules.toString)
        out.println(ta.toString)
        out.close();

        val timewrite = System.currentTimeMillis
        //        net.sf.bddbddb.Solver.main(Array("target/datalog/mnfs.datalog"))
        //        val timesolve = System.currentTimeMillis
        println("WALA:  " + (timewala - time1) + "ms")
        println("sDPN:  " + (timedpn - timewala) + "ms")
        println("write: " + (timewrite - timedpn) + "ms")
        //        println("bdd:   " + (timesolve - timewrite) + "ms")
        println("Total: " + (timewrite - time1) + "ms")

    }
    

    def genRunDatalog() {
        val time1 = System.currentTimeMillis
        val analysis = MyPreAnalysis.getStd
        import analysis._

        val timewala = System.currentTimeMillis

        val entryNode = cg.getFakeRootNode
        val dpnfac = new MyPreAnalysis(analysis) with DPNFactory 
        val trans = new ExplicitTranslator(dpnfac.getDPN)

        val timedpn = System.currentTimeMillis

        trans.writeOutput
        val pmap = trans.getGMap
        val gmap = trans.getSMap
        val pset = dpnfac.getDPN.getControlSymbols;
        val gset = dpnfac.getDPN.getStackSymbols;
        val methodSig = "bnord.testapps.Main.excludeMe()V"

        val cset = ConflictMNFSGenerator.generateConflictSet4Method(cg, methodSig)

        val mnfs = ConflictMNFSGenerator.generateMNFS(pset, gset, cset, cset)

        mnfs.setStartConf(NState, StackSymbol(entryNode, 0, 0));

        //            MNFSPrinter.dotify(mnfs)
        MNFS2Datalog.writeDatalogFile(mnfs, pmap, gmap)

        //            val solver = new net.sf.bddbddb.BDDSolver()            
        //            val in = new PipedInputStream()
        //            val out = new PipedOutputStream(in)
        //            
        //            val x = scala.io.Source.fromInputStream(in)
        //            new Thread {
        //            	override def run() {
        //            		try {
        //            			for (line <- x.getLines if line.contains("query")){
        //            				println(line)
        //            			}            			
        //            		} catch {
        //            			case e: IOException => 
        //            		}
        //            	}
        //            }.start()
        //            
        //            solver.out = new PrintStream(out)
        //            solver.err = new PrintStream(out) 
        //            
        //            solver.load("target/datalog/mnfs.datalog")
        //            solver.solve();
        val timewrite = System.currentTimeMillis
        net.sf.bddbddb.Solver.main(Array("target/datalog/mnfs.datalog"))
        val timesolve = System.currentTimeMillis
        println("WALA:  " + (timewala - time1) + "ms")
        println("sDPN:  " + (timedpn - timewala) + "ms")
        println("write: " + (timewrite - timedpn) + "ms")
        println("bdd:   " + (timesolve - timewrite) + "ms")
        println("Total: " + (timesolve - time1) + "ms")
    }
    
    def genRunSymbolicDatalog() {
        val time1 = System.currentTimeMillis
        val analysis = MyPreAnalysis.getStd
        import analysis._

        val timewala = System.currentTimeMillis

        val entryNode = cg.getFakeRootNode
        val dpnfac = new MyPreAnalysis(analysis) with DPNFactory 
        val trans = new ExplicitTranslator(dpnfac.getDPN)

        val timedpn = System.currentTimeMillis

        trans.writeOutput
        
        val gmap = trans.getGMap
        val smap = trans.getSMap
        
        val methodSig = "bnord.testapps.Main.excludeMe()V"

        val cset = ConflictMNFSGenerator.generateConflictSet4Method2(cg, methodSig)

        val mnfs = ConflictMNFSGenerator.generateSymbolicDatalogMNFS(gmap, smap, cset,(NState, StackSymbol(entryNode, 0, 0)))

        val out = new PrintWriter(new BufferedWriter(new FileWriter("target/datalog/smnfs.datalog")));
        out.println(mnfs.toString)
        out.close();
        
        val timewrite = System.currentTimeMillis
        net.sf.bddbddb.Solver.main(Array("target/datalog/smnfs.datalog"))
        val timesolve = System.currentTimeMillis
        println("WALA:  " + (timewala - time1) + "ms")
        println("sDPN:  " + (timedpn - timewala) + "ms")
        println("write: " + (timewrite - timedpn) + "ms")
        println("bdd:   " + (timesolve - timewrite) + "ms")
        println("Total: " + (timesolve - time1) + "ms")
    }

    def example() {
        val pmap = Map("N" -> 0, "E" -> 1)
        val gmap = Map("g1" -> 0, "g2" -> 1, "G" -> 2)
        val pset: Set[String] = pmap.keySet
        val gset: Set[String] = gmap.keySet

        generateMNFS[String, String](pset, gset, "N", "g1", "N", "g2", pmap, gmap)
    }

    def explicitMNFS {
        val P = Set("N", "E")
        val G = Set("g1", "g2", "G", "G'", "Gg1", "Gg2")
        val mnfs = new MNFS[String, String, Int, Int](P, G)
        mnfs.addSPSEdge(0, "N", 0)
        mnfs.addSPSEdge(0, "E", 0)
        mnfs.addSPTEdge(0, "N", "G'", 1)
        mnfs.addSPTEdge(0, "N", "g1", 2)
        mnfs.addSPTEdge(0, "N", "g2", 3)
        mnfs.addSPTEdge(0, "E", "G", 4)
        mnfs.addTTEdge(1, "G", 1)
        mnfs.addTTEdge(2, "G", 2)
        mnfs.addTTEdge(3, "G", 3)
        mnfs.addTTEdge(4, "G", 4)
        mnfs.addTSEdge(1, 0)
        mnfs.addTSEdge(4, 0)

        mnfs.addTSEdge(2, 1)
        mnfs.addSPTEdge(1, "N", "Gg1", 5)
        mnfs.addTTEdge(5, "G", 5)
        mnfs.addTSEdge(5, 1)
        mnfs.addSPTEdge(1, "E", "G", 6)
        mnfs.addTTEdge(6, "G", 6)
        mnfs.addTSEdge(6, 1)

        mnfs.addTSEdge(3, 2)
        mnfs.addSPTEdge(2, "N", "Gg2", 7)
        mnfs.addTTEdge(7, "G", 7)
        mnfs.addTSEdge(7, 2)
        mnfs.addSPTEdge(2, "E", "G", 8)
        mnfs.addTTEdge(8, "G", 8)
        mnfs.addTSEdge(8, 2)

        mnfs.addSPTEdge(1, "N", "g2", 9)
        mnfs.addSPTEdge(2, "N", "g1", 9)

        mnfs.addTTEdge(9, "G", 9)

        mnfs.addTSEdge(9, 3)

        mnfs.addSPSEdge(3, "N", 3)
        mnfs.addSPSEdge(3, "E", 3)

        mnfs.addSPTEdge(3, "N", "G", 9)
        mnfs.addSPTEdge(3, "E", "G", 9);

        MNFSPrinter.dotify(mnfs)
    }

    def generateMNFS[P, G](pset: Set[P], gset: Set[G], p1: P, g1: G, p2: P, g2: G, pmap: Map[P, Int], gmap: Map[G, Int]) {
        val mnfs = new MNFS[P, G, Int, Int](pset, gset)
        assert(pset(p1) && pset(p2) && gset(g1) && gset(g2))

        mnfs.setStart(0)

        for (p <- pset) {
            //empty transition from S0_p back to S0
            mnfs.addSPSEdge(0, p, 0)
            for (g <- gset) {
                if (p == p1 && g == g1) {
                    //g1 transition from S0_p1 to T2
                    mnfs.addSPTEdge(0, p1, g1, 2)
                } else if (p == p2 && g == g2) {
                    //g2 transition from S0_p2 to T3
                    mnfs.addSPTEdge(0, p2, g2, 3)
                } else {
                    //g transition from SP_p to T1
                    mnfs.addSPTEdge(0, p, g, 1)
                }
            }
        }

        //G loops for T1,T2,T3
        for (g <- gset) {
            mnfs.addTTEdge(1, g, 1)
            mnfs.addTTEdge(2, g, 2)
            mnfs.addTTEdge(3, g, 3)
        }

        //empty transition from T1 back to S0
        mnfs.addTSEdge(1, 0)

        //now waiting for (p2,g2) in S1
        mnfs.addTSEdge(2, 1)
        for (p <- pset) {
            //empty transition from S1_p back to S1
            mnfs.addSPSEdge(1, p, 1)
            for (g <- gset) {
                if (p == p2 && g == g2) {
                    //g2 transition from S1_p2 to T6
                    mnfs.addSPTEdge(1, p2, g2, 6)
                } else {
                    //g transition from S1_p to T4
                    mnfs.addSPTEdge(1, p, g, 4)
                }
            }
        }
        for (g <- gset) {
            mnfs.addTTEdge(4, g, 4)
        }
        //empty transition from T4 back to S1
        mnfs.addTSEdge(4, 1)

        //waiting for (p1,g1) in S2
        //empty transition from T3 to S2
        mnfs.addTSEdge(3, 2)
        for (p <- pset) {
            //empty transition from S2_p back to S2
            mnfs.addSPSEdge(2, p, 2)
            for (g <- gset) {
                if (p == p1 && g == g1) {
                    //g1 transition from S2_p1 to T6
                    mnfs.addSPTEdge(2, p1, g1, 6)
                } else {
                    //g transition from S2_p to T5
                    mnfs.addSPTEdge(2, p, g, 5)
                }
            }
        }
        for (g <- gset) {
            mnfs.addTTEdge(5, g, 5)
        }
        //empty transition from T5 back to S2
        mnfs.addTSEdge(5, 2)

        for (g <- gset) {
            mnfs.addTTEdge(6, g, 6)
        }

        //empty transition from T6 to S3
        mnfs.addTSEdge(6, 3)

        for (p <- pset) {
            //empty transition from S3_p back to S3
            mnfs.addSPSEdge(3, p, 3)
            //set S3_p final
            mnfs.setFinalSP(3, p)
            for (g <- gset) {
                //g transition from S3_p to T6
                mnfs.addSPTEdge(3, p, g, 6)

            }
        }
        mnfs.setFinalT(6);

        MNFSPrinter.dotify(mnfs)
    }

}