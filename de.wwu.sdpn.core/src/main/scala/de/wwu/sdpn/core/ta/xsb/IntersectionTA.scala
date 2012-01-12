package de.wwu.sdpn.core.ta.xsb

/**
 * Intersection tree automata of the tree automata ta1 and ta2.
 * 
 * @author Benedikt Nordhoff
 */
class IntersectionTA(ta1: ScriptTreeAutomata, ta2: ScriptTreeAutomata) extends ScriptTreeAutomata {
    require(ta1.name != ta2.name)
    require(ta1.stateSize == 1)
    require(ta2.stateSize == 1)
    val name = "inter_" + ta1.name + "_" + ta2.name
    require(ta1.alphabet == ta2.alphabet)
    val alphabet = ta1.alphabet
    val stateSize = 1

    /**
     * Set of function symbols which are to be interpreted top down
     * @todo change to boolean.
     */
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
            append("%% Intersection of " + func + " rules\n")
            val (annots, states) = v
            //append(":- table(" + name + "_" + func + "/" + (annots + stateSize*states) + ").\n")
            val Annotations = valString("Annot", 1, annots)
            val t1States = new Array[String](states)
            val t2States = new Array[String](states)
            for (i <- 0 until states) {
                t1States(i) = "A" + (i + 1)
                t2States(i) = "B" + (i + 1)
            }

            if (annots > 0) {
                append(name + "_" + func + "(")
                append(Annotations)
                for (i <- 0 until states) {
                    append(" , i(")
                    append(t1States(i))
                    append(",")
                    append(t2States(i))
                    append(")")
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
                append("i(")
                append(t1States(0))
                append(",")
                append(t2States(0))
                append(")")
                for (i <- 1 until states) {
                    append(" , i(")
                    append(t1States(i))
                    append(",")
                    append(t2States(i))
                    append(")")
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

        append(name + "_final( i(A,B) ) :- " + ta1.name + "_final(A), " + ta2.name + "_final(B).\n")

        append("%%% END Definition of: " + name + "\n\n")
        buf.toString
    }

    def valString(x: String, min: Int, max: Int) = {
        x + (min to max).mkString("," + x)
    }

}
