package de.wwu.sdpn.core.ta.xsb.witness.iterable


sealed trait WitnessTree extends Product{
  def state: State
  def printTree:String = WitnessTree.printTree(this)
  def printTreeStates:String = WitnessTree.printTreeStates(this)
  def equalModStates(other:WitnessTree) = WitnessTree.equalModStates(this, other)
}

object WitnessTree{
    
    def equalModStates(t1:WitnessTree, t2:WitnessTree) : Boolean = {
        (t1,t2) match {
           	case (CutTree(a1,_,c1),CutTree(a2,_,c2)) => a1 == a2 && equalModStates(c1, c2)
           	case (BaseTree(a1,_,c1),BaseTree(a2,_,c2)) => a1 == a2 && equalModStates(c1, c2)
           	case (Call1Tree(a1,_,c1),Call1Tree(a2,_,c2)) => a1 == a2 && equalModStates(c1, c2)
           	case (AcqTree(a1,_,c1),AcqTree(a2,_,c2)) => a1 == a2 && equalModStates(c1, c2)
           	case (Call2Tree(a1,_,c11,c12),Call2Tree(a2,_,c21,c22)) => a1 == a2 && equalModStates(c11, c21) && equalModStates(c12, c22)
           	case (UseTree(a1,_,c11,c12),UseTree(a2,_,c21,c22)) => a1 == a2 && equalModStates(c11, c21) && equalModStates(c12, c22)
           	case (SpawnTree(a1,_,c11,c12),SpawnTree(a2,_,c21,c22)) => a1 == a2 && equalModStates(c11, c21) && equalModStates(c12, c22)
           	case (NilTree(a1,_),NilTree(a2,_)) => a1 == a2
           	case (RetTree(a1,_),RetTree(a2,_)) => a1 == a2
           	case (_,_) => false
        }
    }
    
    def pruneBase(tree:WitnessTree) :WitnessTree = {
        tree match {
            case CutTree(a,s,c) => CutTree(a,s,pruneBase(c))
            case BaseTree(_,_,c) => pruneBase(c)
            case Call1Tree(a,s,c) => Call1Tree(a,s,pruneBase(c))
            case AcqTree(a,s,c) => AcqTree(a,s,pruneBase(c))
            case Call2Tree(a,s,c1,c2) => Call2Tree(a,s,pruneBase(c1),pruneBase(c2))
            case UseTree(a,s,c1,c2) => UseTree(a,s,pruneBase(c1),pruneBase(c2))
            case SpawnTree(a,s,c1,c2) => SpawnTree(a,s,pruneBase(c1),pruneBase(c2))
            case NilTree(a,s) => NilTree(a,s)
            case RetTree(a,s) => RetTree(a,s)
        }
    }
    
    
    def printTreeStates(tree:WitnessTree,prefix:String = "") :String= {
        tree match {
            case CutTree(a,s,c) => prefix + "Cut(" + a + ") -- " + s + "\n" + printTreeStates(c,prefix + " ")
            case BaseTree(a,s,c) => prefix + "Base(" + a + ") -- " + s + "\n" + printTreeStates(c,prefix + " ")
            case Call1Tree(a,s,c) => prefix + "Call1(" + a + ") -- " + s + "\n" + printTreeStates(c,prefix + " ")
            case AcqTree(a,s,c) => prefix + "Acq(" + a + ") -- " + s + "\n" + printTreeStates(c,prefix + " ")
            case Call2Tree(a,s,c1,c2) => prefix + "Call2(" + a + ") -- " + s + "\n" + printTreeStates(c1,prefix + " ") + "\n" + printTreeStates(c2,prefix + " ")
            case UseTree(a,s,c1,c2) => prefix + "Use(" + a + ") -- " + s + "\n" + printTreeStates(c1,prefix + " ") + "\n" + printTreeStates(c2,prefix + " ")
            case SpawnTree(a,s,c1,c2) => prefix + "Spawn(" + a + ") -- " + s + "\n" + printTreeStates(c1,prefix + " ") + "\n" + printTreeStates(c2,prefix + " ")
            case NilTree(a,s) => prefix + "Nil(" + a + ") -- " + s
            case RetTree(a,s) => prefix + "Ret(" + a + ") -- " + s
        }
    }
    
    def printTree(tree:WitnessTree,prefix:String = "") :String= {
        tree match {
            case CutTree(a,s,c) => prefix + "Cut(" + a + ")\n" + printTree(c,prefix + " ")
            case BaseTree(a,s,c) => prefix + "Base(" + a + ")\n" + printTree(c,prefix + " ")
            case Call1Tree(a,s,c) => prefix + "Call1(" + a + ")\n" + printTree(c,prefix + " ")
            case AcqTree(a,s,c) => prefix + "Acq(" + a + ")\n" + printTree(c,prefix + " ")
            case Call2Tree(a,s,c1,c2) => prefix + "Call2(" + a + ")\n" + printTree(c1,prefix + " ") + "\n" + printTree(c2,prefix + " ")
            case UseTree(a,s,c1,c2) => prefix + "Use(" + a + ")\n" + printTree(c1,prefix + " ") + "\n" + printTree(c2,prefix + " ")
            case SpawnTree(a,s,c1,c2) => prefix + "Spawn(" + a + ")\n" + printTree(c1,prefix + " ") + "\n" + printTree(c2,prefix + " ")
            case NilTree(a,s) => prefix + "Nil(" + a + ")"
            case RetTree(a,s) => prefix + "Ret(" + a + ")"
        }
    }
}

case class BaseTree(annot: Annot, state: State, child: WitnessTree) extends WitnessTree

case class CutTree(annot: Annot, state: State, child: WitnessTree) extends WitnessTree

case class Call1Tree(annot: Annot,state: State, child: WitnessTree) extends WitnessTree

case class AcqTree(annot: Annot, state: State, child: WitnessTree) extends WitnessTree 

case class Call2Tree(annot:Annot, state: State, called: WitnessTree, next: WitnessTree) extends WitnessTree

case class UseTree(annot: Annot, state: State, called: WitnessTree, next: WitnessTree) extends WitnessTree 

case class SpawnTree(annot: Annot,state: State, spawned: WitnessTree, next: WitnessTree) extends WitnessTree

case class NilTree(annot:Annot, state: State) extends WitnessTree

case class RetTree(annot: Annot,state: State) extends WitnessTree

sealed trait Annot

case class UnparsedAnnot(annot:String) extends Annot {
    override def toString = annot
}
case object NoAnnot extends Annot

sealed trait State

case class UnparsedState(state:String) extends State{
    override def toString = state
}



