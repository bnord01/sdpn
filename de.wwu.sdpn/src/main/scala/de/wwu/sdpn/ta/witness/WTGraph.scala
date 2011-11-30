package de.wwu.sdpn.ta.witness

import org.eclipse.swt.widgets.Composite
import org.eclipse.zest.core.widgets.Graph
import org.eclipse.zest.core.widgets.ZestStyles
import org.eclipse.zest.core.widgets.GraphNode
import org.eclipse.zest.core.widgets.GraphConnection
import org.eclipse.swt.SWT
import org.eclipse.swt.graphics.Color
import org.eclipse.zest.layouts.algorithms.TreeLayoutAlgorithm
import org.eclipse.draw2d.geometry.Dimension
import org.eclipse.swt.widgets.Display
import org.eclipse.swt.widgets.Shell
import org.eclipse.swt.layout.FillLayout
import com.ibm.wala.ipa.callgraph.CallGraph

class WTGraph(tree: WitnessTree, parent: Composite, decorator: WitnessTree => String = _.toString, style: Int = 0) extends Graph(parent, style) {
  setConnectionStyle(ZestStyles.CONNECTIONS_DIRECTED)
  addTree(tree)

  private val tlayout = new TreeLayoutAlgorithm()
  tlayout.setNodeSpace(new Dimension(140, 40))
  setLayoutAlgorithm(tlayout, true);

  def addTree(t: WitnessTree): GraphNode = {
    t match {
      case BaseTree(state, child) =>
        val v2 = addTree(child)
        val v1 = new GraphNode(this, SWT.None, t)
        v1.setText(decorator(t))
        val con = new GraphConnection(this, SWT.None, v1, v2)
        return v1

      case Call1Tree(state, child) =>
        val v2 = addTree(child)
        val v1 = new GraphNode(this, SWT.None, t)
        v1.setText(decorator(t))
        val con = new GraphConnection(this, SWT.None, v1, v2)
        return v1

      case AcqTree(state, lock, reentrant, child) =>
        val v2 = addTree(child)
        val v1 = new GraphNode(this, SWT.None, t)
        v1.setText(decorator(t))
        if (!reentrant) {
          val color = new Color(this.getDisplay(), 255, 80, 80)
          v1.setBackgroundColor(color)
        }
        val con = new GraphConnection(this, SWT.None, v1, v2)
        return v1

      case Call2Tree(state, called, next) =>
        val v2 = addTree(called)
        val v3 = addTree(next)
        val v1 = new GraphNode(this, SWT.None, t)
        v1.setText(decorator(t))
        val conCall = new GraphConnection(this, SWT.None, v1, v2)
        conCall.setText("call")
        val conRet = new GraphConnection(this, SWT.None, v1, v3)
        conRet.setText("return")
        return v1

      case UseTree(state, lock, reentrant, called, next) =>
        val v2 = addTree(called)
        val v3 = addTree(next)
        val v1 = new GraphNode(this, SWT.None, t)
        v1.setText(decorator(t))
        if (!reentrant) {
          val color = new Color(this.getDisplay(), 255, 80, 80)
          v1.setBackgroundColor(color)
        }
        val conCall = new GraphConnection(this, SWT.None, v1, v2)
        conCall.setText("use")
        val conRet = new GraphConnection(this, SWT.None, v1, v3)
        conRet.setText("return")
        return v1

      case SpawnTree(state, spawned, next) =>
        val v2 = addTree(spawned)
        val v3 = addTree(next)
        val v1 = new GraphNode(this, SWT.None, t)
        v1.setText(decorator(t))
        val color = this.getDisplay().getSystemColor(SWT.COLOR_GREEN)
        v1.setBackgroundColor(color)
        val conSpawn = new GraphConnection(this, SWT.None, v1, v2)
        conSpawn.setText("spawn")
        val conRet = new GraphConnection(this, SWT.None, v1, v3)
        conRet.setText("return")
        return v1

      case NilTree(state, globalState, stackSymbol) =>
        val v1 = new GraphNode(this, SWT.None, t)
        v1.setText(decorator(t))
        val color = this.getDisplay().getSystemColor(SWT.COLOR_CYAN)
        v1.setBackgroundColor(color)
        return v1

      case RetTree(state) =>
        val v1 = new GraphNode(this, SWT.None, t)
        v1.setText(decorator(t))
        return v1

    }
  }

}

object WTGraph {
  def showTree(tree: WitnessTree,decorator:WitnessTree => String = _.toString) {
    val d = new Display();
    val shell = new Shell(d);
    shell.setText("GraphSnippet1");
    shell.setLayout(new FillLayout());
    shell.setSize(400, 800);
    val graph = new WTGraph(tree, shell , decorator = decorator)
    shell.open();
    while (!shell.isDisposed()) {
      while (!d.readAndDispatch()) {
        d.sleep();
      }
    }
  }
  
  def showTree(tree:WitnessTree, cg:CallGraph) {
    val decorator = (t:WitnessTree) => {
      val ss = t.state.ss
      val nr = ss.cg
      val node = cg.getNode(nr)
      val name = node.getMethod().getName()
      var cn = t.getClass().getCanonicalName()
      cn = cn.dropRight(4)
      cn = cn split('.') last; 
      
      "%s in  %s  at (%d,%d)".format(cn,name,ss.bb,ss.instr)
    }
    showTree(tree,decorator)
    
  }
  
  
  
}