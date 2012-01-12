package de.wwu.sdpn.gui.dpn.zest

import org.eclipse.zest.core.widgets.Graph
import de.wwu.sdpn.core.dpn.monitor.MonitorDPN
import de.wwu.sdpn.core.dpn.monitor.DPNRule
import de.wwu.sdpn.core.dpn.monitor.PushRule
import de.wwu.sdpn.core.dpn.monitor.BaseRule
import de.wwu.sdpn.core.dpn.monitor.SpawnRule
import de.wwu.sdpn.core.dpn.monitor.PopRule
import org.eclipse.swt.widgets.Composite
import org.eclipse.zest.core.widgets.ZestStyles
import org.eclipse.zest.core.widgets.GraphNode
import org.eclipse.zest.core.widgets.GraphConnection
import org.eclipse.swt.widgets.Display
import org.eclipse.swt.widgets.Shell
import org.eclipse.swt.layout.FillLayout
import org.eclipse.zest.layouts.algorithms.DirectedGraphLayoutAlgorithm
import org.eclipse.zest.layouts.algorithms.SpringLayoutAlgorithm
import org.eclipse.zest.layouts.algorithms.RadialLayoutAlgorithm

class MDPNGraph[G, S, A, L](
  mdpn: MonitorDPN[G, S, A, L],
  parent: Composite,
  decorator: DPNDecorator[G, S, A, L] = ToStringDecorator.get,
  style: Int = 0)
  extends Graph(parent, style) {
  setConnectionStyle(ZestStyles.CONNECTIONS_DIRECTED)
  import decorator._
  val nodeMap = scala.collection.mutable.Map[S, GraphNode]()
  for (s <- mdpn.stackSymbols)
    nodeMap += s -> new GraphNode(this, 0, decorateStack(s))

  for (rule <- mdpn.transitions) {
    rule match {
      case PushRule(_, s, a, _, sc, sr) =>
        val c = new GraphConnection(this, 0, nodeMap(s), nodeMap(sr))
        c.setText(decorateRule(rule))
      case BaseRule(_, s, a, _, sn) =>
        val c = new GraphConnection(this, 0, nodeMap(s), nodeMap(sn))
        c.setText(decorateRule(rule))
      case PopRule(_, s, a, _) =>
        val c = new GraphConnection(this, 0, nodeMap(s), nodeMap(s))
        c.setText(decorateRule(rule))
      case SpawnRule(_, s, a, _, ss, _, sn) =>
        val cs = new GraphConnection(this, 0, nodeMap(s), nodeMap(ss))
        cs.setText("Spawn: " + decorateRule(rule))
        val cn = new GraphConnection(this, 0, nodeMap(s), nodeMap(ss))
        cn.setText("Next: " + decorateRule(rule))
    }
  }

  val tlayout = new RadialLayoutAlgorithm()
  //tlayout.setSpringLength(100f)
  //tlayout.setIterations(10000)
  this.setLayoutAlgorithm(tlayout, true)

}

trait DPNDecorator[G, S, A, L] {
  def decorateGlobal(g: G): String
  def decorateStack(s: S): String
  def decorateRule(a: DPNRule[G, S, A]): String
  def decorateState(g: G, s: S): String
  def decorateLock(l: L): String
}

object ToStringDecorator {
  def get[G, S, A, L]: DPNDecorator[G, S, A, L] = new DPNDecorator[G, S, A, L] {
    def decorateGlobal(g: G): String = g.toString
    def decorateStack(s: S): String = s.toString
    def decorateRule(a: DPNRule[G, S, A]): String = a.toString
    def decorateState(g: G, s: S): String = "(%s,%s)".format(g, s)
    def decorateLock(l: L): String = l.toString
  }
}

object MDPNGraph {
  def showMDPN[G, S, A, L](mdpn: MonitorDPN[G, S, A, L], decorator: DPNDecorator[G, S, A, L] = ToStringDecorator.get) {
    val d = new Display();
    val shell = new Shell(d);
    shell.setText("MDPN View");
    shell.setLayout(new FillLayout());
    shell.setSize(1920, 1000);
    val graph = new MDPNGraph(mdpn, shell, decorator = decorator)
    shell.open();
    while (!shell.isDisposed()) {
      while (!d.readAndDispatch()) {
        d.sleep();
      }
    }
  }
}