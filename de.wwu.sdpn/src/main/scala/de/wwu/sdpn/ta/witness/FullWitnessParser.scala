package de.wwu.sdpn.ta.witness

import scala.util.parsing.combinator.JavaTokenParsers


/**
 * Parser for full witnesses of two set reachability analyses with or without locks
 *
 * @author Benedikt Nordhoff
 */
object FullWitnessParser extends JavaTokenParsers {

  // The Parser 
  // format: OFF
  
  
  def lnr = wholeNumber ^^ (_.toLong)
  def inr = wholeNumber ^^ (_.toInt)
  def tb = "top" ^^ (_ => true) | "bot" ^^ (_ => false)
  def tf10 = "1" ^^ (_ => true) | "0" ^^ (_ => false)
  def lockAnnot = "la("~inr~","~tf10~")" ^^ {case "la("~inr~","~tf10~")" => (inr,tf10)}

  def state: Parser[State] = 
    (
        "c("~cfState~","~conflictState~")" ^^ {
    		case "c("~cfState~","~cState~")" =>
    			new State(cfState, cState)
        }
    | 
    
    	"c(i("~cfState~","~lnr~"),i("~acqStruct~","~conflictState~"))" ^^ {
    		case "c(i("~cfState~","~lnr~"),i("~acqStruct~","~conflictState~"))" =>
    			new LSState(cfState, lnr, acqStruct, conflictState)
        }
    )
    
  def acqStruct = "l("~lnr~","~lnr~","~lnr~")" ^^ {case "l("~a~","~u~","~gr~")" =>  AcquisitionStructure(a,u,gr)}
  
  def conflictState:Parser[CState] =  tsrState | ssrState
  
  def tsrState:Parser[TSRCState] =  "i("~inr~",i("~tb~","~tb~"))" ^^ {
    case "i("~nr~",i("~r1~","~r2~"))" => TSRCState(nr, r1, r2)
  }
  
  def ssrState:Parser[SSRCState] =  inr ^^ (SSRCState(_))
  
  
  def cfState = "c("~inr~","~stackSym~","~inr~","~tf10~")" ^^ {case "c("~g~","~ss~","~gf~","~t~")" => CFState(GlobalState(g),ss,GlobalState(gf),t)}
  

  def stackSym: Parser[StackSymbol] = "s("~inr~","~inr~","~inr~")"^^ { 
    case "s("~cg~","~bb~","~nr~")" => StackSymbol(cg, bb, nr) 
  }

  def fullTree: Parser[WitnessTree] = "st("~state~","~ptree~")"^^{
    case "st("~state~","~ptree~")" => {
      ptree match {
        case PNilTree(gs, ss) => NilTree(state, gs, ss)
        case PBaseTree(next) => BaseTree(state, next)
        case PCall1Tree(next) => Call1Tree(state, next)
        case PCall2Tree(called, next) => Call2Tree(state, called, next)
        case PSpawnTree(spawned, next) => SpawnTree(state, spawned, next)
        case PAcqTree((lock,re),next) => AcqTree(state,lock,re,next)
        case PUseTree((lock,re),called,next) => UseTree(state,lock,re,called,next)
        case PRetTree => RetTree(state)
      }
    }
  }

  def ptree: Parser[PTree] = baseTree | nilTree | call1Tree | call2Tree | spawnTree | retTree | acqTree | useTree
  def baseTree = "base(" ~> fullTree <~ ")" ^^ { x => PBaseTree(x) }
  def call1Tree = "call1(" ~> fullTree <~ ")" ^^ { x => PCall1Tree(x) }
  def acqTree = "acq("~lockAnnot~","~fullTree~")" ^^ { 
    case "acq("~la~","~fullTree~")" => PAcqTree(la,fullTree)
  }
  def call2Tree = "call2("~fullTree~","~fullTree~")" ^^ {
    case "call2("~called~","~next~")" => PCall2Tree(called, next)
  }
  def useTree = "use("~lockAnnot~","~fullTree~","~fullTree~")" ^^ {
    case "use("~lockAnnot~","~called~","~next~")" => PUseTree(lockAnnot,called, next)
  }
  def spawnTree = "spawn("~fullTree~","~fullTree~")" ^^ {
    case "spawn("~spawned~","~next~")" => PSpawnTree(spawned, next)
  }
  def nilTree = "nil(t("~inr~","~stackSym~"))" ^^ { 
    case "nil(t("~inr~","~stackSym~"))" => PNilTree(GlobalState(inr), stackSym) 
  }
  def retTree = "ret" ^^ { _ => PRetTree }
  // format: ON

  // Partial Trees without State
  sealed trait PTree
  case class PNilTree(gs: GlobalState, ss: StackSymbol) extends PTree
  case class PBaseTree(next: WitnessTree) extends PTree
  case class PCall1Tree(next: WitnessTree) extends PTree
  case class PAcqTree(lock: (Int, Boolean), next: WitnessTree) extends PTree
  case class PCall2Tree(called: WitnessTree, next: WitnessTree) extends PTree
  case class PUseTree(lock: (Int, Boolean), called: WitnessTree, next: WitnessTree) extends PTree
  case class PSpawnTree(spawned: WitnessTree, next: WitnessTree) extends PTree
  case object PRetTree extends PTree

  def parseTree(str: String) = parseAll(fullTree, str)

}