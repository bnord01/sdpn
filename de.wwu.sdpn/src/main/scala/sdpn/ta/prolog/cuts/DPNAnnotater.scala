package sdpn.ta.prolog.cuts


import sdpn.dpn.explicit._

/**
 * A trait used to define a Method which is used to annotate nodes in execution trees
 * with information extracted from a dpn rule.
 * 
 * @author Benedikt Nordhoff
 */
trait DPNAnnotater {
	type RuleAnnotation
	def annotateRule(b:DPNRule[GlobalState, StackSymbol, DPNAction]):RuleAnnotation
}