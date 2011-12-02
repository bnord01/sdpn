package de.wwu.sdpn.dpn.explicit

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

}