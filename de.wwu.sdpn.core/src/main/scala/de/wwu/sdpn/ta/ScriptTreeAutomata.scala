package de.wwu.sdpn.ta

/**
 * A simple ScriptTreeAutomata for XSB which contains information 
 * needed to create intersection tree automata and emptiness checks.
 * @author Benedikt Nordhoff
 */
trait ScriptTreeAutomata {
	/**
	 * The name of the tree automata, the script should contain name_func(....) predicates.
	 */
	def name:String
	
	/**
	 * The alphabet of the tree automata, maps function symbols to the number of annotations
	 * and number of states.
	 */
	def alphabet:scala.collection.Map[String,(Int,Int)]
	/**
	 * Generate a XSB script for this script tree automata
	 */
	def genScript:String	
	/**
	 * The number of variables used for a single state
	 * @deprecated This should always be one for XSB tree automata.
	 */
	def stateSize:Int
	/**
	 * The set of rules which should be evaluated top down. 
	 * @todo This should be changed to a single boolean whether this 
	 * tree automata is to be evaluated top down or bottom up
	 * as this gives no measurable effect.  
	 */
	def isForwardRule:scala.collection.Set[String]
	
}