package de.wwu.sdpn.zzz.ta
import de.wwu.sdpn.ta.ScriptTreeAutomata

/**
 * Intersection tree automata for general script tree automata.
 * Intended for datalog interpretation, doesn't use nested terms.
 * @deprecated
 * @author Benedikt Nordhoff
 */
class PrologIntersectionTreeAutomata(ta1: ScriptTreeAutomata, ta2: ScriptTreeAutomata) extends ScriptTreeAutomata {
    require(ta1.name != ta2.name)
    val name = "inter_" + ta1.name + "_" + ta2.name
    require(ta1.alphabet == ta2.alphabet)
    val alphabet = ta1.alphabet
    val stateSize = ta1.stateSize + ta2.stateSize
    val isForwardRule = ta1.isForwardRule.intersect(ta2.isForwardRule)

    def genScript: String = {
        val buf = new StringBuilder()
        import buf.append
        append("%%% Intersection of " + ta1.name + " and " + ta2.name + "\n")
        append("%%% BEGIN Definition of: " + ta1.name + "\n")
        append(ta1.genScript)
        append("%%% END Definition of: " + ta1.name + "\n\n")
        append("%%% BEGIN Definition of: " + ta2.name + "\n")
        append(ta2.genScript)
        append("%%% END Definition of: " + ta2.name + "\n\n")

        append("%%% BEGIN Definition of: " + name + "\n")
        
        for ((func, v) <- alphabet) {
        	append("%% Intersection of " + func + "rules\n")        	
            val (annots, states) = v
            //append(":- table(" + name + "_" + func + "/" + (annots + stateSize*states) + ").\n")
            val Annotations = valString("A", 1, annots)
            val t1States = new Array[String](states)
            val t2States = new Array[String](states)
            for (i <- 0 until states){
            	t1States(i) = valString("T1_"+(i+1)+"_", 1, ta1.stateSize)
            	t2States(i) = valString("T2_"+(i+1)+"_", 1, ta2.stateSize)
            }
            
            if (annots > 0) {
                append(name + "_" + func + "(")
                append(Annotations)
                for(i<- 0 until states){
                	append(" , ")                
                	append(t1States(i))
                	append(",")
                	append(t2States(i))
                }
                append(") :- ")
                append(ta1.name + "_" + func + "(")
                append(Annotations)
                append(",")
                append(t1States.mkString(" , "))
                append(") , ")
                append(ta2.name + "_" + func + "(")
                append(Annotations)
                append(",")
                append(t2States.mkString(" , "))
                append(") .\n\n")
            } else {
                append(name + "_" + func + "(")
                	append(t1States(0))
                	append(",")
                	append(t2States(0))
                for(i<- 1 until states){
                	append(" , ")                
                	append(t1States(i))
                	append(",")
                	append(t2States(i))
                }
                append(") :- ")
                append(ta1.name + "_" + func + "(")
                append(t1States.mkString(" , "))
                append(") , ")
                append(ta2.name + "_" + func + "(")
                append(t2States.mkString(" , "))
                append(") .\n\n")
            }

        }

        append("\n%%% Definition of final states\n")

        val StateT1 = valString("T1_", 1, ta1.stateSize)
        val StateT2 = valString("T2_", 1, ta2.stateSize)

        append(name + "_final(")
        append(StateT1)
        append(" , ")
        append(StateT2)
        append(") :- ")
        append(ta1.name + "_final(")
        append(StateT1)
        append(") , ")
        append(ta2.name + "_final(")
        append(StateT2)
        append(") .\n")

        append("%%% END Definition of: " + name + "\n\n")
        buf.toString
    }

    def valString(x: String, min: Int, max: Int) = {
        x + (min to max).mkString("," + x)
    }

}