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

object WalaPFGTest {
    var pa :PreAnalysis = null
    @BeforeClass
    def setUp {
        pa = MyPreAnalysis.getStd
    }
}

class WalaPFGTest {
    import WalaPFGTest._
    
    @Test
    def testFactory1{
        val fac = new PFGFactory(pa)
        val pfg = fac.getPFG
        println("Number of Edges: " + pfg.edges.size)
        println("Number of Nodes: " + pfg.nodes.size)
        import pfg._
        for (edge <- edges) {
            println(edge)
            assert(nodes.contains(edge.src),"Edge src not contained in nodes set.")
            
        }
        for (node <- pfg.nodes) {
            println(node)
        }
        
        println ("""
        ---------- HERE COMES DOT!------------
                
                
        """)
        println(pfg2Dot(pfg))
        
        
        println ("""
                
                
                
        ---------- HERE ENDES DOT!------------
                
                
        """)
        
        val mr = StringStuff.makeMethodReference("bnord.testapps.Main.p2()V")
        val im = pa.cg.getNodes(mr)
        assert (im.size() == 1)
        println(im.first.getIR())
    }
    
    def pfg2Dot (pfg: WalaPFG) : String = {
        val bld = new StringBuilder () 
        def pln(n:String){bld.append(n + "\n")}
        pln ( "digraph pfg { " )
        
        def quote(n:Any) = "\"" + n.toString() + "\""
        
        for (edge <- pfg.edges) {
            edge match {
                case BaseEdge(src,a,snk) =>
                    pln( quote(src) + " -> " + quote(snk) + "[label=\"" + a + "\"]")
                case SpawnEdge(src,proc,snk) =>
                    pln( quote(src) + " -> " + quote(snk)+ "[label=\"Spawn: " + proc.getMethod().getDeclaringClass().getName() + "." + proc.getMethod().getName() + "\"]")
                case CallEdge(src,proc,returns) => 
                    for((r,snk) <- returns) {
                    	pln( quote(src) + " -> " + quote(snk) + "[label=\"" + r + " : " + proc.getMethod().getDeclaringClass().getName() + "." + proc.getMethod().getName() + "\"]")
                    }
            }             
        }
        
        pln("}")
        
        bld.toString()
    }
    
    
}