package de.wwu.sdpn.core.ta.xsb.iterable


import de.wwu.sdpn.core.dpn.monitor.DPNRule

/**
 * A trait used to define a Method which is used to annotate nodes in execution trees
 * with information extracted from a dpn rule.
 * 
 * @author Benedikt Nordhoff
 */
trait DPNAnnotater[GlobalState, StackSymbol, DPNAction] {
	type RuleAnnotation
	def annotateRule(b:DPNRule[GlobalState, StackSymbol, DPNAction]):RuleAnnotation
}