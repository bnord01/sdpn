package de.wwu.sdpn.core.ta.xsb.witness.iterable


sealed trait WitnessTree extends Product{
  def state: State
  def printTree:String = WitnessTree.printTree(this)
}

object WitnessTree{
    def pruneBase(tree:WitnessTree) :WitnessTree = {
        tree match {
            case CutTree(a,s,c) => CutTree(a,s,pruneBase(c))
            case BaseTree(_,_,c) => pruneBase(c)
            case Call1Tree(s,c) => Call1Tree(s,pruneBase(c))
            case AcqTree(a,s,c) => AcqTree(a,s,pruneBase(c))
            case Call2Tree(s,c1,c2) => Call2Tree(s,pruneBase(c1),pruneBase(c2))
            case UseTree(a,s,c1,c2) => UseTree(a,s,pruneBase(c1),pruneBase(c2))
            case SpawnTree(s,c1,c2) => SpawnTree(s,pruneBase(c1),pruneBase(c2))
            case NilTree(a,s) => NilTree(a,s)
            case RetTree(s) => RetTree(s)
        }
    }
    
    
    def printTree(tree:WitnessTree,prefix:String = "") :String= {
        tree match {
            case CutTree(a,s,c) => prefix + "Cut(" + a + ") -- " + s + "\n" + printTree(c,prefix + " ")
            case BaseTree(a,s,c) => prefix + "Base(" + a + ") -- " + s + "\n" + printTree(c,prefix + " ")
            case Call1Tree(s,c) => prefix + "Call1 -- " + s + "\n" + printTree(c,prefix + " ")
            case AcqTree(a,s,c) => prefix + "Acq(" + a + ") -- " + s + "\n" + printTree(c,prefix + " ")
            case Call2Tree(s,c1,c2) => prefix + "Call2 -- " + s + "\n" + printTree(c1,prefix + " ") + "\n" + printTree(c2,prefix + " ")
            case UseTree(a,s,c1,c2) => prefix + "Use(" + a + ") -- " + s + "\n" + printTree(c1,prefix + " ") + "\n" + printTree(c2,prefix + " ")
            case SpawnTree(s,c1,c2) => prefix + "Spawn -- " + s + "\n" + printTree(c1,prefix + " ") + "\n" + printTree(c2,prefix + " ")
            case NilTree(a,s) => prefix + "Nil(" + a + ") -- " + s
            case RetTree(s) => prefix + "Ret -- " + s
        }
    }
}

case class BaseTree(annot: Annot, state: State, child: WitnessTree) extends WitnessTree

case class CutTree(annot: Annot, state: State, child: WitnessTree) extends WitnessTree

case class Call1Tree(state: State, child: WitnessTree) extends WitnessTree

case class AcqTree(annot: Annot, state: State, child: WitnessTree) extends WitnessTree 

case class Call2Tree(state: State, called: WitnessTree, next: WitnessTree) extends WitnessTree

case class UseTree(annot: Annot, state: State, called: WitnessTree, next: WitnessTree) extends WitnessTree 

case class SpawnTree(state: State, spawned: WitnessTree, next: WitnessTree) extends WitnessTree

case class NilTree(annot:Annot, state: State) extends WitnessTree

case class RetTree(state: State) extends WitnessTree

sealed trait Annot

case class UnparsedAnnot(annot:String) extends Annot {
    override def toString = annot
}
case object NoAnnot extends Annot

sealed trait State

case class UnparsedState(state:String) extends State{
    override def toString = state
}



