package de.wwu.sdpn.core.ta.xsb.witness


sealed trait WitnessTree {
  def state: State
  override def toString(): String = {
    var cn = this.getClass().getCanonicalName()
    cn = cn.substring(0, cn.length() - 4)
    cn = cn.split('.').last
    return cn + "(" + state.ss.cg + ", " + state.ss.bb + ", " + state.ss.instr + ")"
  }
}

case class BaseTree(state: State, child: WitnessTree) extends WitnessTree

case class Call1Tree(state: State, child: WitnessTree) extends WitnessTree

case class AcqTree(state: State, lock: Int, reentrant: Boolean, child: WitnessTree) extends WitnessTree {
  override def toString = {
    (if (reentrant) "Re" else "") +
      "Acq(" + lock + "," + state.ss.cg + "," + state.ss.bb + "," + state.ss.instr + ")"
  }
}

case class Call2Tree(state: State, called: WitnessTree, next: WitnessTree) extends WitnessTree

case class UseTree(state: State, lock: Int, reentrant: Boolean, called: WitnessTree, next: WitnessTree) extends WitnessTree {
  override def toString = {
    (if (reentrant) "Re" else "") +
      "Use(" + lock + ",(" + state.ss.cg + "," + state.ss.bb + "," + state.ss.instr + "))"
  }
}

case class SpawnTree(state: State, spawned: WitnessTree, next: WitnessTree) extends WitnessTree

case class NilTree(state: State, globalState: GlobalState, stackSymbol: StackSymbol) extends WitnessTree

case class RetTree(state: State) extends WitnessTree

case class State(g: GlobalState, ss: StackSymbol, gf: GlobalState, term: Boolean, conflictState: CState) {
  def this(cf: CFState, cstate: CState) = this(cf.g, cf.ss, cf.gf, cf.term, cstate)
}

case class CFState(g: GlobalState, ss: StackSymbol, gf: GlobalState, term: Boolean)

case class LSState(g0: GlobalState,
  ss0: StackSymbol,
  gf0: GlobalState,
  term0: Boolean,
  lockSet: Long, as: AcquisitionStructure, conflictState0: CState) extends State(g0, ss0, gf0, term0, conflictState0) {
  def this(cf: CFState, lockSet: Long, as: AcquisitionStructure, cstate: CState) = this(cf.g, cf.ss, cf.gf, cf.term, lockSet, as, cstate)
}

case class AcquisitionStructure(acq: Long, use: Long, graph: Long)

case class GlobalState(num: Int)

case class StackSymbol(cg: Int, bb: Int, instr: Int)

trait CState {
  def isConflict:Boolean
  def possibleConflict:Boolean
}

case class TSRCState(num: Int, reachedOne: Boolean, reachedTwo: Boolean) extends CState {
  def isConflict = (num == 2 && reachedOne && reachedTwo)  
  def possibleConflict = (num > 0)
}

case class SSRCState(num: Int) extends CState {
  def isConflict = (num == 2)
  def possibleConflict = (num > 0)

}
