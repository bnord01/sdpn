package de.wwu.sdpn.ta

/**
 * 
 * An emptiness check for the intersection of ta1 and ta2 
 * where ta1 is checked top down and ta2 is checked bottom up
 * and a witness is generated.
 * 
 * The Method emptiness generates the full XSB-Script which 
 * contains the predicate name_runCheck/0 which runs the 
 * check and prints 
 * 
 * "name is empty!"
 * 
 *  or 
 *  
 *  "name is not empty!
 *  
 *  witness: some witness"
 *   
 * plus the used cputime to stdout.  
 * 
 * @param ta1 the tree automata to be checked top down
 * @param ta2 the tree automata to be checked bottom up
 * @todo generalize the kind of generated witness, like ignoring some nodes and annotations.
 * @todo check why the generated script is very expensive to evaluate some times.
 * @author Benedikt Nordhoff
 */
class WitnessIntersectionEmptinessCheck(ta1: ScriptTreeAutomata, ta2: ScriptTreeAutomata) {
    require(ta1.alphabet == ta2.alphabet, "Tried to intersect tree automata with different alphabets.")
    require(ta1.stateSize == 1)
    require(ta2.stateSize == 1)
    
    val alphabet = ta1.alphabet
    val name = "interemp_" + ta1.name + "_" + ta2.name

    def emptiness: String = {
        val buffer = new StringBuilder()
        import buffer.{ append => out }

        def nl = out("\n")
        def outln(s: String) { out(s); nl }

        out("%%% BEGIN Definition of: " + ta1.name + "\n")
        out(ta1.genScript)
        out("%%% END Definition of: " + ta1.name + "\n\n")
        out("%%% BEGIN Definition of: " + ta2.name + "\n")
        out(ta2.genScript)
        out("%%% END Definition of: " + ta2.name + "\n\n")
        
        outln("%%Emptiness check for the intersection of " + ta1.name + " and " + ta2.name);nl
        
        out("""
:- dynamic name_witness/3.
""".replace("name",name))
        
        def ne(num:Int) = name + "_ne(A" + num + ",B" + num + ")"
        
        def assertWitness(num:Int,rule:String) { 
            out("assert(" + name + "_witness(A" + num + ",B" + num + ",")
            hasRule(3,rule)
            out("))")
        }
        
        def witness(num:Int) = name + "_witness(A" + num + ",B" + num + ",W" + num + ")"
        def nowitness(num:Int) = "not(" + name + "_witness(A" + num + ",B" + num + ",_))"
        
        def hasRule(tanum: Int, rule: String) {
            val (taname,ab,size) = 
                tanum match {
                case 1 => (ta1.name + "_","A",alphabet(rule)._2) 
                case 2 => (ta2.name + "_","B",alphabet(rule)._2)
                case 3 => ("","W",alphabet(rule)._2-1)
            }
            out(taname + rule)
            if(alphabet(rule)._1 > 0 || size > 0) out("(")
            if (alphabet(rule)._1 > 0) {
                out(state("Annot", 1, alphabet(rule)._1))
                if(size>0)
                    out(",")
            }
            
            val nums = 1 to size
            val sts = nums map (ab + _);
            out(sts.mkString(" , "))
            if(alphabet(rule)._1 > 0 || size > 0) out(")")
        }
        
        outln(":- table(" + name + "_ne/2).")
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
            if(states == 1) {
                outln(",")
                out("\t")
                assertWitness(states,rule)
            } else {
                outln(",")
                out("\t(")
                out(nowitness(states) + " ->\n\t\t ")
                
                out (1 until states map (witness(_)) mkString ",\n\t\t")
                out(",\n\t\t")
                assertWitness(states,rule)
                out("\n\t\t;true)")
            }
            
            outln(".")

        }

        nl
        
        val ta1fin = ta1.name+"_final(A)"
        val ta2fin = ta2.name+"_final(B)"
        val nefin = name+"_ne(A,B)"
        
        
        
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
        out("_notEmpty(A,B) :- ")
        out(arr.mkString(" , "))
        out(".\n")

        out("""
name_runCheck :-  
    cputime(X0), 
    (name_notEmpty(A,B) -> 
        write('name is not empty!'),nl,
        write('witness:'),nl,
        name_witness(A,B,W),
        write(W),nl
     ;
        write('name is empty!'),nl
    ), 
    cputime(X1), 
    X is X1 - X0,
    write('cputime: '), 
    write(X), nl.
""".replace("name",name))

        buffer.toString
    }

    def state(s: String, min: Int, max: Int): String =
        s + (min to max).mkString("," + s)

}
