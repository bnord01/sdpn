package sdpn.zzz.ta
import sdpn.ta.ScriptTreeAutomata

/**
 * Bottom Up EmptinessCheck for general script tree automata.
 * Intended for datalog interpretation, doesn't use nested terms.
 * @deprecated
 * @author Benedikt Nordhoff
 */
trait PrologBUEmptinessCheck extends ScriptTreeAutomata{
	def emptiness:String = {
		val buffer = new StringBuilder()
		import buffer.append		
		
		def appendState(s:String) {
			for(n <- 1 until stateSize)
				append(s +"_"+ n + ",")
			append(s+"_" + stateSize)
		}
		
		def appendBlanks(count:Int){
			for(i <- 1 until count)
				append("_,")
			if (count > 0)
				append("_")
		}
		
		append("%Emptinesscheck for " + name+ "\n")
		append(":- table("+name+"_ne/"+stateSize+").\n")
		
		for ((func,v) <- alphabet) {
			val (annots,states) = v
			append(name)
			append("_ne(")
			appendState("T_"+states)
			append(") :- ")
			
			//switch this
			for(n <- 1 until states){
				append (name)
				append ("_ne(")
				appendState("T_"+n)
				append(")")
				append (" , ")
			
			}
			//end this
			//with this
			append(name)
			append("_")
			append(func)
			append("(")
			appendBlanks(annots)
			if(annots > 0) 
				append(" , ")			
			for(n <- 1 until states){
				appendState("T_"+n)
				append(" , ")
			}
			appendState("T_"+states)
			append(") ")		
			
			
			//end this
			//so it can't be evaluated but in this order it doesn't terminate
			append(".\n")
		}
		append("%%% IS TA EMPTY?\n")
		append(name)
		append("_notEmpty :- ")
		append(name)
		append("_ne(")
		appendState("T")
		append("), ")
		append(name)
		append("_final(")
		appendState("T")
		append(").\n")	
		
		append("%%% accepting states\n")
		append(name)
		append("_acc(")
		appendState("T")				
		append(") :- ")
		append(name)
		append("_ne(")
		appendState("T")		
		append("), ")
		append(name)
		append("_final(")
		appendState("T")
		append(").\n")
		
		append (name + "_runCheck :-  cputime(X0), (" + name + "_notEmpty -> write('" + name + " is not empty!'),nl;write('" + name + " is empty!'),nl), cputime(X1), X is X1 - X0,")
        append ("write('cputime: '), write(X), nl.\n")
		
		buffer.toString
	}

}