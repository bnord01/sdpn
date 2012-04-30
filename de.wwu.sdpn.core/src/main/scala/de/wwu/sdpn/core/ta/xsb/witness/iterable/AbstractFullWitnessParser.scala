package de.wwu.sdpn.core.ta.xsb.witness.iterable

import scala.util.parsing.combinator.JavaTokenParsers


/**
 * Parser for full witnesses of two set reachability analyses with or without locks
 *
 * @author Benedikt Nordhoff
 */
object AbstractFullWitnessParser extends JavaTokenParsers {

  // The Parser 
  // format: OFF
      
  def xsbAtom: Parser[String] = "[0-9][0-9]*".r | "[a-z][a-zA-Z0-9_]*".r
  def xsbTerm: Parser[String] = (xsbAtom ~ opt(xsbTuple) ^^ {case a~None => a; case a~Some(t) => a + t}) | xsbTuple
  def xsbTuple: Parser[String] = "("~>repsep(xsbTerm,",")<~")"^^{case ls => ls.mkString("(",",",")")}

  def state: Parser[State] = xsbTerm^^{case x => UnparsedState(x)}
  def annot: Parser[Annot] = xsbTerm^^{case x => UnparsedAnnot(x)}
    
    def fullTree: Parser[WitnessTree] = "st("~state~","~ptree~")"^^{
    case "st("~state~","~ptree~")" => {
      ptree match {
        case PNilTree(annot) => NilTree(annot,state)
        case PBaseTree(annot,next) => BaseTree(annot,state, next)
        case PCutTree(annot,next) => CutTree(annot,state, next)
        case PCall1Tree(next) => Call1Tree(state, next)
        case PCall2Tree(called, next) => Call2Tree(state, called, next)
        case PSpawnTree(spawned, next) => SpawnTree(state, spawned, next)
        case PAcqTree(annot,next) => AcqTree(annot,state,next)
        case PUseTree(annot,called,next) => UseTree(annot,state,called,next)
        case PRetTree => RetTree(state)
      }
    }
  }

  def ptree: Parser[PTree] = baseTree | nilTree | call1Tree | call2Tree | spawnTree | retTree | acqTree | useTree | cutTree
  def cutTree = "cut(" ~> annot~","~fullTree <~ ")" ^^ { case a~","~x => PCutTree(a,x) }
  def baseTree = "base(" ~> annot~","~fullTree <~ ")" ^^ { case a~","~x => PBaseTree(a,x) }
  def call1Tree = "call1(" ~> fullTree <~ ")" ^^ { x => PCall1Tree(x) }
  def acqTree = "acq("~>annot~","~fullTree<~")" ^^ { 
      case la~","~fullTree => PAcqTree(la,fullTree) 
  }
  def call2Tree = "call2("~fullTree~","~fullTree~")" ^^ {
    case "call2("~called~","~next~")" => PCall2Tree(called, next)
  }
  def useTree = "use("~>annot~","~fullTree~","~fullTree<~")" ^^ {
    case lockAnnot~","~called~","~next => PUseTree(lockAnnot,called, next)
  }
  def spawnTree = "spawn("~>fullTree~","~fullTree<~")" ^^ {
    case spawned~","~next => PSpawnTree(spawned, next)
  }
  def nilTree = "nil("~>annot<~")" ^^ {x => PNilTree(x)}
  def retTree = "ret" ^^ { _ => PRetTree }
  
  def fullTreeWithName: Parser[WitnessTree] = xsbAtom~"("~>fullTree<~")"
  
  def anyFullTree: Parser[WitnessTree] = fullTree | fullTreeWithName
  // format: ON

  // Partial Trees without State
  sealed trait PTree
  case class PNilTree(annot:Annot) extends PTree
  case class PBaseTree(annot:Annot,next: WitnessTree) extends PTree
  case class PCutTree(annot:Annot,next: WitnessTree) extends PTree
  case class PCall1Tree(next: WitnessTree) extends PTree
  case class PAcqTree(annot:Annot, next: WitnessTree) extends PTree
  case class PCall2Tree(called: WitnessTree, next: WitnessTree) extends PTree
  case class PUseTree(annot:Annot, called: WitnessTree, next: WitnessTree) extends PTree
  case class PSpawnTree(spawned: WitnessTree, next: WitnessTree) extends PTree
  case object PRetTree extends PTree

  def parseTree(str: String) = parseAll(anyFullTree, str)

}