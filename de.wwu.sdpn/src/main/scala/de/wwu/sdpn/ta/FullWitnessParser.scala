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

class FullWitnessParser extends JavaTokenParsers with Serializable {

  def lnr = wholeNumber ^^ (_.toLong)
  def inr = wholeNumber ^^ (_.toInt)
  def tb = "top" ^^ (_ => true) | "bot" ^^ (_ => false)
  def tf10 = "1" ^^ (_ => true) | "0" ^^ (_ => false)

  // format: OFF
  def state: Parser[State] = 
    "c(c("~inr~","~stackSym~","~inr~","~tf10~"),i("~inr~",i("~tb~","~tb~")))" ^^ {
    	case "c(c("~g~","~ss~","~gf~","~t~"),i("~nr~",i("~r1~","~r2~")))" =>
    		State(GlobalState(g), ss, GlobalState(gf), t, CState(nr, r1, r2))
  }
  // format: ON

  def stackSym: Parser[StackSymbol] = "s(" ~ inr ~ "," ~ inr ~ "," ~ inr ~ ")" ^^ { case "s(" ~ cg ~ "," ~ bb ~ "," ~ nr ~ ")" => StackSymbol(cg, bb, nr) }

  def fullTree: Parser[Tree] = "st(" ~ state ~ "," ~ ptree ~ ")" ^^ {
    case "st(" ~ state ~ "," ~ ptree ~ ")" => {
      ptree match {
        case PNilTree(gs, ss) => NilTree(state, gs, ss)
        case PBaseTree(next) => BaseTree(state, next)
        case PCall1Tree(next) => Call1Tree(state, next)
        case PCall2Tree(called, next) => Call2Tree(state, called, next)
        case PSpawnTree(spawned, next) => SpawnTree(state, spawned, next)
        case PRetTree => RetTree(state)
      }
    }
  }

  def ptree: Parser[PTree] = baseTree | nilTree | call1Tree | call2Tree | spawnTree | retTree
  def baseTree = "base(" ~> fullTree <~ ")" ^^ { x => PBaseTree(x) }
  def call1Tree = "call1(" ~> fullTree <~ ")" ^^ { x => PCall1Tree(x) }
  def call2Tree = "call2(" ~ fullTree ~ "," ~ fullTree ~ ")" ^^ {
    case "call2(" ~ called ~ "," ~ next ~ ")" => PCall2Tree(called, next)
  }
  def spawnTree = "spawn(" ~ fullTree ~ "," ~ fullTree ~ ")" ^^ {
    case "spawn(" ~ spawned ~ "," ~ next ~ ")" => PSpawnTree(spawned, next)
  }
  def nilTree = "nil(t(" ~ inr ~ "," ~ stackSym ~ "))" ^^ { case "nil(t(" ~ inr ~ "," ~ stackSym ~ "))" => PNilTree(GlobalState(inr), stackSym) }
  def retTree = "ret" ^^ { _ => PRetTree }

  case class State(g: GlobalState, ss: StackSymbol, gf: GlobalState, term: Boolean, conflictState: CState)
  case class GlobalState(num: Int)
  case class StackSymbol(cg: Int, bb: Int, instr: Int)
  case class CState(num: Int, reachedOne: Boolean, reachedTwo: Boolean)

  trait Tree {
    def state: State

    def addToGraph(graph: Graph): GraphNode
    def toGraph(shell: Shell): Graph = {
      val graph = new Graph(shell, SWT.NONE)
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
      con.setDirected(true)
      return v1
    }
  }
  case class Call1Tree(state: State, child: Tree) extends Tree {
    def addToGraph(graph: Graph): GraphNode = {
      val v2 = child.addToGraph(graph)
      val v1 = new GraphNode(graph, SWT.None, this)
      v1.setText(toString)
      val con = new GraphConnection(graph, SWT.None, v1, v2)
      con.setDirected(true)
      return v1
    }
  }
  case class Call2Tree(state: State, called: Tree, next: Tree) extends Tree {
    def addToGraph(graph: Graph): GraphNode = {
      val v2 = called.addToGraph(graph)
      val v3 = next.addToGraph(graph)
      val v1 = new GraphNode(graph, SWT.None, this)
      v1.setText(toString)
      val conCall = new GraphConnection(graph, SWT.None, v1, v2)
      conCall.setDirected(true)
      conCall.setText("call")
      val conRet = new GraphConnection(graph, SWT.None, v1, v3)
      conRet.setDirected(true)
      conRet.setText("return")
      return v1
    }
  }
  case class SpawnTree(state: State, spawned: Tree, next: Tree) extends Tree {
    def addToGraph(graph: Graph): GraphNode = {
      val v2 = spawned.addToGraph(graph)
      val v3 = next.addToGraph(graph)
      val v1 = new GraphNode(graph, SWT.None, this)
      v1.setText(toString)
      val conSpawn = new GraphConnection(graph, SWT.None, v1, v2)
      conSpawn.setDirected(true)
      conSpawn.setText("spawn")
      val conRet = new GraphConnection(graph, SWT.None, v1, v3)
      conRet.setDirected(true)
      conRet.setText("return")
      return v1
    }
  }
  case class NilTree(state: State, globalState: GlobalState, stackSymbol: StackSymbol) extends Tree {
    def addToGraph(graph: Graph): GraphNode = {
      val v1 = new GraphNode(graph, SWT.None, this)
      v1.setText(toString)
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
  case class PCall2Tree(called: Tree, next: Tree) extends PTree
  case class PSpawnTree(spawned: Tree, next: Tree) extends PTree
  case object PRetTree extends PTree

  def parseTree(str: String) = parseAll(fullTree, str)

  val testTree = "st(c(c(0,s(0,0,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,0,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,1,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,1,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,2,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,2,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,3,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,3,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,4,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,4,1),0,0),i(2,i(top,top))),base(st(c(c(0,s(0,5,0),0,0),i(2,i(top,top))),call1(st(c(c(0,s(5,0,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(5,1,0),0,0),i(2,i(top,top))),call1(st(c(c(0,s(10,0,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(10,1,0),0,0),i(2,i(top,top))),base(st(c(c(0,s(10,1,1),0,0),i(2,i(top,top))),call2(st(c(c(0,s(16,0,0),0,1),i(1,i(top,top))),spawn(st(c(c(0,s(20,0,0),0,0),i(1,i(top,top))),base(st(c(c(0,s(20,1,0),0,0),i(1,i(top,top))),call1(st(c(c(0,s(23,0,0),0,0),i(1,i(top,top))),base(st(c(c(0,s(23,1,0),0,0),i(1,i(top,top))),call1(st(c(c(0,s(17,0,0),0,0),i(1,i(top,top))),nil(t(0,s(17,0,0)))))))))))),st(c(c(0,s(16,1,0),0,1),i(0,i(bot,bot))),ret))),st(c(c(0,s(10,1,2),0,0),i(1,i(top,top))),base(st(c(c(0,s(10,2,0),0,0),i(1,i(top,top))),call1(st(c(c(0,s(17,0,0),0,0),i(1,i(top,top))),nil(t(0,s(17,0,0))))))))))))))))))))))))))))))))))))))))"
}

//DELETE ME!!!
object TestFullWitnessParser {
  def main(arg: Array[String]) {
    val parser = new FullWitnessParser
    val p = parser.parseAll(parser.state, "c(c(0,s(0,0,0),0,0),i(2,i(top,top)))")

    println(p)

    val p2 = parser.parseTree(parser.testTree)

    println(p2)

    val d = new Display();
    val shell = new Shell(d);
    shell.setText("GraphSnippet1");
    shell.setLayout(new FillLayout());
    shell.setSize(400, 800);
    val graph = p2.get.toGraph(shell)
    val layout = new TreeLayoutAlgorithm()
    layout.setNodeSpace(new Dimension(140,40))
    graph.setLayoutAlgorithm(layout, true);
    shell.open();
    while (!shell.isDisposed()) {
      while (!d.readAndDispatch()) {
        d.sleep();
      }
    }

  }

}