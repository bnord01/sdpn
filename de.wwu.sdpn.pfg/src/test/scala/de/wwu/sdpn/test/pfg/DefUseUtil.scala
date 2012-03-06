package de.wwu.sdpn.test.pfg
import de.wwu.sdpn.pfg.lattices.LMap
import de.wwu.sdpn.pfg.lattices.BottomMap
import de.wwu.sdpn.pfg.lattices.TopMap
import org.junit.Assert._

object DefUseUtil {
    
    def printRes[A,B,C](res:Map[A,LMap[B,LMap[C,Boolean]]]): String = {
        (for((n,r) <- res) yield "At " + n  +" : \n"+ printSingleRes(r)).mkString("\n\n") 
    }
    
    def printSingleRes[B,C](res:LMap[B,LMap[C,Boolean]]): String = {
        res match {
            case BottomMap(entrys) => "  All none but" +
                (for((variable,value) <- entrys) yield "\n    " + variable + ": \n" + printValRes(value)).mkString
            case TopMap(entrys) => "  All all but" +
                (for((variable,value) <- entrys) yield "\n    " + variable + ": \n" + printValRes(value)).mkString
                
        }
    }
    
    def printValRes[C](sr:LMap[C,Boolean]):String = {
        sr match {
            case BottomMap(entrys) =>
                (for((node,value) <- entrys;if value) yield "        " + node).mkString("\n")
            case TopMap(entrys) =>
                if(entrys isEmpty)
                    "        All"
                else
                	"        All" + (for((node,value) <- entrys;if !value) yield "            "  + node).mkString(" but: ","\n","")
        }
    }
    
     def checkResult[A,B,C](res:Map[A,LMap[B,LMap[C,Boolean]]])  {
         for ((node,nodeResult) <- res) {
             nodeResult match {
                 case BottomMap(entrys) =>
                     for ( (variable, defs) <- entrys) {
                         defs match {
                             case BottomMap(_) =>
                             case TopMap(_) =>
                                 fail("TopMap for variable " + variable + " at " + node)
                         }
                     }
                 case TopMap(entrys) =>
                     fail("TopMap as result at: " + node)
             }
         }
     }

}