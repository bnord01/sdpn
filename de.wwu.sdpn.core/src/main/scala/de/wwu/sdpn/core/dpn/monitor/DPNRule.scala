package de.wwu.sdpn.core.dpn.monitor

/**
 * A general DPN Rule, Base, Call, Ret or Spawn
 * @tparam ControlSymbol
 * @tparam StackSymbol
 * @tparam Action
 * @author Benedikt Nordhoff
 */
sealed trait DPNRule[ControlSymbol, StackSymbol, Action] {
    val inState: ControlSymbol
    val inSymbol: StackSymbol
    val action: Action
    def accept(visitor: RuleVisitor[ControlSymbol, StackSymbol, Action])
}

case class SpawnRule[ControlSymbol, StackSymbol, Action](
    override val inState: ControlSymbol,
    override val inSymbol: StackSymbol,
    override val action: Action,
    outState: ControlSymbol, outSymbol: StackSymbol,
    spawnState: ControlSymbol, spawnSymbol: StackSymbol)
    extends DPNRule[ControlSymbol, StackSymbol, Action] {
    
	override def toString = {
        "Spawn:\t(" + inState + ", " + inSymbol + ")\n\t --" +
            action + "-->\n\t\t(" +
            outState + ", " + outSymbol + ")\n\t>>>>(" +
            spawnState + "," + spawnSymbol + ")"
    }
	override def accept(visitor: RuleVisitor[ControlSymbol, StackSymbol, Action]) {visitor.visitSpawnRule(this)}
}

case class PushRule[ControlSymbol, StackSymbol, Action](
    override val inState: ControlSymbol,
    override val inSymbol: StackSymbol,
    override val action: Action,
    outState: ControlSymbol, outSymbol1: StackSymbol, outSymbol2: StackSymbol)
    extends DPNRule[ControlSymbol, StackSymbol, Action] {
    
	override def toString = {
        "Push:\t(" + inState + ", " + inSymbol + ")\n\t --" +
            action + "-->\n\t\t(" +
            outState + ", [" + outSymbol1 + ", " + outSymbol2 + "])"
    }
	override def accept(visitor: RuleVisitor[ControlSymbol, StackSymbol, Action]) {visitor.visitPushRule(this)}
}

case class PopRule[ControlSymbol, StackSymbol, Action](
    override val inState: ControlSymbol, 
    override val inSymbol: StackSymbol,
    override val action: Action,
    outState: ControlSymbol)
    extends DPNRule[ControlSymbol, StackSymbol, Action] {
    
	override def toString = {
        "Pop:\t(" + inState + ", " + inSymbol + ")\n\t --" +
            action + "-->\n\t\t(" +
            outState + ")"
    }
	override def accept(visitor: RuleVisitor[ControlSymbol, StackSymbol, Action]) {visitor.visitPopRule(this)}
}

case class BaseRule[ControlSymbol, StackSymbol, Action](
    override val inState: ControlSymbol, 
    override val inSymbol: StackSymbol,
    override val action: Action,
    outState: ControlSymbol, outSymbol: StackSymbol)
    extends DPNRule[ControlSymbol, StackSymbol, Action] {
    
	override def toString = {
        "Base:\t(" + inState + ", " + inSymbol + ")\n\t --" +
            action + "-->\n\t\t(" +
            outState + ", " + outSymbol + ")"
    }
	override def accept(visitor: RuleVisitor[ControlSymbol, StackSymbol, Action]) {visitor.visitBaseRule(this)}

}

trait RuleVisitor[ControlSymbol,StackSymbol,Action] {
    def visitBaseRule(rule: BaseRule[ControlSymbol,StackSymbol,Action])
    def visitPopRule(rule: PopRule[ControlSymbol,StackSymbol,Action])
    def visitPushRule(rule: PushRule[ControlSymbol,StackSymbol,Action])
    def visitSpawnRule(rule: SpawnRule[ControlSymbol,StackSymbol,Action])
}