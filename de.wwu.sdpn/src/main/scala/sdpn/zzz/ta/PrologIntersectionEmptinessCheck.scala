package sdpn.zzz.ta
import sdpn.ta.ScriptTreeAutomata

/**
 * Intersection EmptinessCheck for general script tree automata.
 * Intended for datalog interpretation, doesn't use nested terms.
 * @deprecated
 * @author Benedikt Nordhoff
 */
class PrologIntersectionEmptinessCheck(ta1: ScriptTreeAutomata, ta2: ScriptTreeAutomata) {
    require(ta1.alphabet == ta2.alphabet, "Tried to intersect tree automata with different alphabets.")

    val alphabet = ta1.alphabet
    val name = "interemp_" + ta1.name + "_" + ta2.name
    val stateSize = ta1.stateSize + ta2.stateSize

    def emptiness: String = {
        val buffer = new StringBuilder()
        import buffer.{ append => out }

        def nl = out("\n")
        def outln(s: String) { out(s); nl }

        outln(ta1.genScript)
        outln(ta2.genScript)

        
        def ne(num: Int) = name + "_ne(" + state("T" + num + "_") + ")"
        def hasRule(tanum: Int, rule: String) {
            val ta = if (tanum == 1) ta1 else ta2
            out(ta.name + "_" + rule + "(")
            if (alphabet(rule)._1 > 0)
                out(state("A", 1, alphabet(rule)._1) + ",")
            val min = if (tanum == 1) 1 else ta1.stateSize + 1
            val max = if (tanum == 1) ta1.stateSize else ta1.stateSize + ta2.stateSize
            val nums = 1 to alphabet(rule)._2
            val sts = nums map ((x: Int) => state("T" + x + "_", min, max));
            out(sts.mkString(" , "))
            out(")")
        }
        
        outln(":- table(" + name + "_ne/" + stateSize +").")
        for ((rule, (annots, states)) <- alphabet) {
            nl
            outln(ne(states) + " :- ")
            var needKomma = false
            if (ta1.isForwardRule(rule)) {
                out("\t")
                hasRule(1, rule)
                needKomma = true
            }
            if (ta2.isForwardRule(rule)) {
                if (needKomma)
                    outln(",")
                out("\t")
                hasRule(2, rule)
                needKomma = true
            }

            for (i <- 1 until states) {
                if (needKomma)
                    outln(",")
                out("\t")
                out(ne(i))
                needKomma = (i < states - 1 || !ta1.isForwardRule(rule) || !ta2.isForwardRule(rule))
            }
            if (!ta1.isForwardRule(rule)) {
                if (needKomma)
                    outln(",")
                out("\t")
                hasRule(1, rule)
                needKomma = true
            }
            if (!ta2.isForwardRule(rule)) {
                if (needKomma)
                    outln(",")
                out("\t")
                hasRule(2, rule)
                needKomma = true
            }
            outln(".")

        }

        nl
        
        val ta1fin = ta1.name+"_final("+state("T", 1, ta1.stateSize) + ")"
        val ta2fin = ta2.name+"_final("+state("T", ta1.stateSize + 1, stateSize) + ")"
        val nefin = name+"_ne("+state("T")+")"
        
        
        
        val arr = new Array[String](3)
        
        if (ta1.isForwardRule("final")){
        	arr(0) = ta1fin
        	if(ta2.isForwardRule("final")){
        		arr(1) = ta2fin
        		arr(2) = nefin
        	}else {
        		arr(1) = nefin
        		arr(2) = ta2fin
        	}
        }else{
        	if(ta2.isForwardRule("final")){
        		arr(0) = ta2fin
        		arr(1) = nefin
        		arr(2) = ta1fin
        	}else {
        		arr(0) = nefin
        		arr(1) = ta1fin
        		arr(2) = ta2fin
        	}
        }
        
        out("%%% IS TA EMPTY?\n")
        out(name)
        out("_notEmpty :- ")
        out(arr.mkString(" , "))
        out(".\n")

        out(name + "_runCheck :-  cputime(X0), (" + name + "_notEmpty -> write('" + name + " is not empty!'),nl;write('" + name + " is empty!'),nl), cputime(X1), X is X1 - X0,")
        out("write('cputime: '), write(X), nl.\n")

        buffer.toString
    }

    def state(s: String, min: Int, max: Int): String =
        s + (min to max).mkString("," + s)

    def state(s: String): String = state(s, 1, stateSize)

}