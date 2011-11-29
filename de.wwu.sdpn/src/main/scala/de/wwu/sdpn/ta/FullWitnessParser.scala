package de.wwu.sdpn.ta

import scala.util.parsing.combinator._
import scala.swing.MainFrame
import scala.swing.Component
import org.eclipse.zest.core.widgets.Graph
import org.eclipse.swt.widgets.Shell
import org.eclipse.zest.core.widgets.GraphNode
import org.eclipse.swt.SWT
import org.eclipse.zest.core.widgets.GraphConnection
import org.eclipse.swt.widgets.Display
import org.eclipse.swt.layout.FillLayout
import org.eclipse.zest.layouts.algorithms.TreeLayoutAlgorithm
import org.eclipse.zest.layouts.algorithms.SpaceTreeLayoutAlgorithm
import org.eclipse.draw2d.geometry.Dimension
import org.eclipse.swt.graphics.Color
import org.eclipse.zest.core.widgets.ZestStyles

/**
 * Parser for full witnesses of two set reachability analyses with or without locks
 *
 * @author Benedikt Nordhoff
 */
class FullWitnessParser extends JavaTokenParsers {

  // The Parser 
  // format: OFF
  
  
  def lnr = wholeNumber ^^ (_.toLong)
  def inr = wholeNumber ^^ (_.toInt)
  def tb = "top" ^^ (_ => true) | "bot" ^^ (_ => false)
  def tf10 = "1" ^^ (_ => true) | "0" ^^ (_ => false)
  def lockAnnot = "la("~inr~","~tf10~")" ^^ {case "la("~inr~","~tf10~")" => (inr,tf10)}

  def state: Parser[State] = 
    (
        "c(c("~inr~","~stackSym~","~inr~","~tf10~"),i("~inr~",i("~tb~","~tb~")))" ^^ {
    		case "c(c("~g~","~ss~","~gf~","~t~"),i("~nr~",i("~r1~","~r2~")))" =>
    			State(GlobalState(g), ss, GlobalState(gf), t, CState(nr, r1, r2))
        }
    | 
    	"c(i(c("~inr~","~stackSym~","~inr~","~tf10~"),"~lnr~"),i(l("~lnr~","~lnr~","~lnr~"),i("~inr~",i("~tb~","~tb~"))))" ^^ {
    		case "c(i(c("~g~","~ss~","~gf~","~t~"),"~lnr~"),i(l("~a~","~u~","~gr~"),i("~nr~",i("~r1~","~r2~"))))" =>
    			LSState(GlobalState(g), ss, GlobalState(gf), t,lnr,AcquisitionStructure(a,u,gr), CState(nr, r1, r2)) 
       	}
    )
  

  def stackSym: Parser[StackSymbol] = "s("~inr~","~inr~","~inr~")"^^ { 
    case "s("~cg~","~bb~","~nr~")" => StackSymbol(cg, bb, nr) 
  }

  def fullTree: Parser[Tree] = "st("~state~","~ptree~")"^^{
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

  // Data structures for output
  // Maybe move these out of this class
  case class State(g: GlobalState, ss: StackSymbol, gf: GlobalState, term: Boolean, conflictState: CState)
  
  case class LSState(g0: GlobalState, 
      ss0: StackSymbol, 
      gf0: GlobalState, 
      term0: Boolean, 
      lockSet: Long, as: AcquisitionStructure, conflictState0: CState
      ) extends State(g0, ss0, gf0, term0, conflictState0)
  case class AcquisitionStructure(acq: Long, use: Long, graph: Long)
  case class GlobalState(num: Int)
  case class StackSymbol(cg: Int, bb: Int, instr: Int)
  case class CState(num: Int, reachedOne: Boolean, reachedTwo: Boolean)

  trait Tree {
    def state: State

    def addToGraph(graph: Graph): GraphNode
    def toGraph(shell: Shell): Graph = {
      val graph = new Graph(shell, SWT.NONE)
      graph.setConnectionStyle(ZestStyles.CONNECTIONS_DIRECTED);
      val v = addToGraph(graph)
      return graph
    }

    override def toString(): String = {
      var cn = this.getClass().getName()
      cn = cn.substring(0, cn.length() - 4)
      cn = cn.split('$').last
      return cn + "(" + state.ss.cg + ", " + state.ss.bb + ", " + state.ss.instr + ")"
    }
  }
  case class BaseTree(state: State, child: Tree) extends Tree {
    def addToGraph(graph: Graph): GraphNode = {
      val v2 = child.addToGraph(graph)
      val v1 = new GraphNode(graph, SWT.None, this)
      v1.setText(toString)
      val con = new GraphConnection(graph, SWT.None, v1, v2)
      return v1
    }
  }
  case class Call1Tree(state: State, child: Tree) extends Tree {
    def addToGraph(graph: Graph): GraphNode = {
      val v2 = child.addToGraph(graph)
      val v1 = new GraphNode(graph, SWT.None, this)
      v1.setText(toString)
      val con = new GraphConnection(graph, SWT.None, v1, v2)
      return v1
    }
  }
  case class AcqTree(state: State, lock:Int,reentrant:Boolean, child: Tree) extends Tree {
    def addToGraph(graph: Graph): GraphNode = {
      val v2 = child.addToGraph(graph)
      val v1 = new GraphNode(graph, SWT.None, this)
      v1.setText(toString)
      if(!reentrant){
      	val color = new Color(graph.getDisplay(),255,80,80)
      	v1.setBackgroundColor(color)
      }
      val con = new GraphConnection(graph, SWT.None, v1, v2)
      return v1
    }
    override def toString = {
      (if(reentrant) "Re" else "") +
      "Acq(" + lock + ","+ state.ss.cg + "," + state.ss.bb + "," + state.ss.instr +")" 
    }
  }
  case class Call2Tree(state: State, called: Tree, next: Tree) extends Tree {
    def addToGraph(graph: Graph): GraphNode = {
      val v2 = called.addToGraph(graph)
      val v3 = next.addToGraph(graph)
      val v1 = new GraphNode(graph, SWT.None, this)
      v1.setText(toString)
      val conCall = new GraphConnection(graph, SWT.None, v1, v2)
      conCall.setText("call")
      val conRet = new GraphConnection(graph, SWT.None, v1, v3)
      conRet.setText("return")
      return v1
    }
  }
  case class UseTree(state: State,lock:Int,reentrant:Boolean, called: Tree, next: Tree) extends Tree {
    def addToGraph(graph: Graph): GraphNode = {
      val v2 = called.addToGraph(graph)
      val v3 = next.addToGraph(graph)
      val v1 = new GraphNode(graph, SWT.None, this)
      v1.setText(toString)
      if(!reentrant){
      	val color = new Color(graph.getDisplay(),255,80,80)
      	v1.setBackgroundColor(color)
      }
      val conCall = new GraphConnection(graph, SWT.None, v1, v2)
      conCall.setText("use")
      val conRet = new GraphConnection(graph, SWT.None, v1, v3)
      conRet.setText("return")
      return v1
    }
    override def toString = {
      (if(reentrant) "Re" else "") +
      "Use(" + lock + ",("+ state.ss.cg + "," + state.ss.bb + "," + state.ss.instr +"))" 
    }
  }
  case class SpawnTree(state: State, spawned: Tree, next: Tree) extends Tree {
    def addToGraph(graph: Graph): GraphNode = {
      val v2 = spawned.addToGraph(graph)
      val v3 = next.addToGraph(graph)
      val v1 = new GraphNode(graph, SWT.None, this)
      v1.setText(toString)
      val color = graph.getDisplay().getSystemColor(SWT.COLOR_GREEN)
      v1.setBackgroundColor(color)
      val conSpawn = new GraphConnection(graph, SWT.None, v1, v2)
      conSpawn.setText("spawn")
      val conRet = new GraphConnection(graph, SWT.None, v1, v3)
      conRet.setText("return")
      return v1
    }
  }
  case class NilTree(state: State, globalState: GlobalState, stackSymbol: StackSymbol) extends Tree {
    def addToGraph(graph: Graph): GraphNode = {
      val v1 = new GraphNode(graph, SWT.None, this)
      v1.setText(toString)
      val color = graph.getDisplay().getSystemColor(SWT.COLOR_CYAN)
      v1.setBackgroundColor(color)
      return v1
    }
  }

  case class RetTree(state: State) extends Tree {
    def addToGraph(graph: Graph): GraphNode = {
      val v1 = new GraphNode(graph, SWT.None, this)
      v1.setText(toString)
      return v1
    }
  }

  trait PTree
  case class PNilTree(gs: GlobalState, ss: StackSymbol) extends PTree
  case class PBaseTree(next: Tree) extends PTree
  case class PCall1Tree(next: Tree) extends PTree
  case class PAcqTree(lock:(Int,Boolean),next: Tree) extends PTree
  case class PCall2Tree(called: Tree, next: Tree) extends PTree
  case class PUseTree(lock:(Int,Boolean),called: Tree, next: Tree) extends PTree
  case class PSpawnTree(spawned: Tree, next: Tree) extends PTree
  case object PRetTree extends PTree

  def parseTree(str: String) = parseAll(fullTree, str)

  val testTree = "st(c(c(0,s(0,0,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,0,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,1,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,1,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,2,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,2,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,3,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,3,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,4,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,4,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,5,0),0,0),i(2,i(top,top))),call1(st(c(c(0,s(5,0,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(5,1,0),0,0),i(2,i(top,top))),call1(st(c(c(0,s(10,0,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(10,1,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(10,1,1),0,0),i(2,i(top,top))),call2(st(c(c(0,s(16,0,0),0,1),i(1,i(top,top))),spawn(st(c(c(0,s(20,0,0),0,0),i(1,i(top,top))),base(st(c(c(0,s(20,1,0),0,0),i(1,i(top,top))),call1(st(c(c(0,s(23,0,0),0,0),i(1,i(top,top))),base(st(c(c(0,s(23,1,0),0,0),i(1,i(top,top))),call1(st(c(c(0,s(17,0,0),0,0),i(1,i(top,top))),nil(t(0,s(17,0,0)))))))))))),st(c(c(0,s(16,1,0),0,1),i(0,i(bot,bot))),ret))),st(c(c(0,s(10,1,2),0,0),i(1,i(top,top))),base(st(c(c(0,s(10,2,0),0,0),i(1,i(top,top))),call1(st(c(c(0,s(17,0,0),0,0),i(1,i(top,top))),nil(t(0,s(17,0,0))))))))))))))))))))))))))))))))))))))))"
  val lsTestTree = "st(c(i(c(0,s(0,0,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,0,1),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,1,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,1,1),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,2,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,2,1),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,3,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,3,1),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,4,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,4,1),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(0,5,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),call1(st(c(i(c(0,s(5,0,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(5,1,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),call1(st(c(i(c(0,s(10,0,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(10,1,0),0,0),0),i(l(3,3,2),i(2,i(top,top)))),base(st(c(i(c(0,s(10,1,1),0,0),0),i(l(3,3,2),i(2,i(top,top)))),call2(st(c(i(c(0,s(16,0,0),0,1),0),i(l(2,2,0),i(1,i(top,top)))),spawn(st(c(i(c(0,s(21,0,0),0,0),0),i(l(2,2,0),i(1,i(top,top)))),base(st(c(i(c(0,s(21,1,0),0,0),0),i(l(2,2,0),i(1,i(top,top)))),call1(st(c(i(c(0,s(24,0,0),0,0),0),i(l(2,2,0),i(1,i(top,top)))),base(st(c(i(c(0,s(24,1,0),0,0),0),i(l(2,2,0),i(1,i(top,top)))),base(st(c(i(c(0,s(24,1,1),0,0),0),i(l(2,2,0),i(1,i(top,top)))),acq(la(1,0),st(c(i(c(0,s(24,1,2),0,0),2),i(l(0,0,0),i(1,i(top,top)))),base(st(c(i(c(0,s(24,2,0),0,0),2),i(l(0,0,0),i(1,i(top,top)))),call1(st(c(i(c(0,s(18,0,0),0,0),2),i(l(0,0,0),i(1,i(top,top)))),nil(t(0,s(18,0,0)))))))))))))))))),st(c(i(c(0,s(16,1,0),0,1),0),i(l(0,0,0),i(0,i(bot,bot)))),ret))),st(c(i(c(0,s(10,1,2),0,0),0),i(l(1,3,2),i(1,i(top,top)))),base(st(c(i(c(0,s(10,2,0),0,0),0),i(l(1,3,2),i(1,i(top,top)))),base(st(c(i(c(0,s(10,2,1),0,0),0),i(l(1,3,2),i(1,i(top,top)))),acq(la(0,0),st(c(i(c(0,s(10,2,2),0,0),1),i(l(0,2,0),i(1,i(top,top)))),base(st(c(i(c(0,s(10,3,0),0,0),1),i(l(0,2,0),i(1,i(top,top)))),base(st(c(i(c(0,s(10,3,1),0,0),1),i(l(0,2,0),i(1,i(top,top)))),use(la(1,0),st(c(i(c(0,s(10,3,2),0,1),3),i(l(0,0,0),i(0,i(bot,bot)))),base(st(c(i(c(0,s(10,4,0),0,1),3),i(l(0,0,0),i(0,i(bot,bot)))),base(st(c(i(c(0,s(10,4,1),0,1),3),i(l(0,0,0),i(0,i(bot,bot)))),base(st(c(i(c(0,s(10,5,0),0,1),3),i(l(0,0,0),i(0,i(bot,bot)))),ret))))))),st(c(i(c(0,s(10,6,0),0,0),1),i(l(0,0,0),i(1,i(top,top)))),base(st(c(i(c(0,s(10,6,1),0,0),1),i(l(0,0,0),i(1,i(top,top)))),base(st(c(i(c(0,s(10,9,0),0,0),1),i(l(0,0,0),i(1,i(top,top)))),call1(st(c(i(c(0,s(18,0,0),0,0),1),i(l(0,0,0),i(1,i(top,top)))),nil(t(0,s(18,0,0))))))))))))))))))))))))))))))))))))))))))))))))))))))"

}

//DELETE ME!!!
object TestFullWitnessParser {
  def main(arg: Array[String]) {
    val parser = new FullWitnessParser
    val p = parser.parseAll(parser.state, "c(c(0,s(0,0,0),0,0),i(2,i(top,top)))")

    println(p)

    val p2 = parser.parseTree(parser.lsTestTree)

    println(p2)

    val d = new Display();
    val shell = new Shell(d);
    shell.setText("GraphSnippet1");
    shell.setLayout(new FillLayout());
    shell.setSize(400, 800);
    val graph = p2.get.toGraph(shell)
    val layout = new TreeLayoutAlgorithm()
    layout.setNodeSpace(new Dimension(140, 40))
    graph.setLayoutAlgorithm(layout, true);
    shell.open();
    while (!shell.isDisposed()) {
      while (!d.readAndDispatch()) {
        d.sleep();
      }
    }

  }

}