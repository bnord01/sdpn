package de.wwu.sdpn.core.gui

import scala.swing.event.ButtonClicked
import scala.swing._
import de.wwu.sdpn.core.dpn.monitor._
/**
 * Explore a MonitorDPN by simulation, locks are ignored.
 * @param dpn the MonitorDPN to explore
 * @author Benedikt Nordhoff
 */
class MonitorDPNView[C, S, A, L](dpn: MonitorDPN[C, S, A, L], var state: C, var stack: List[(S, Option[L])], val tname: String, root: MonitorDPNView[C, S, A, L]) extends BorderPanel {

    def this(dpn: MonitorDPN[C, S, A, L]) = {
        this(dpn, dpn.initialState, List((dpn.initialStack, None)), "Main", null)
        spawnedProcesses = List(this)
    }

    private var history = List[(C, List[(S, Option[L])])]()

    private var spawncount = 0

    private var spawnedProcesses: List[MonitorDPNView[C, S, A, L]] = Nil

    val stackl = new ListView(stack)
    val statel = new Label("State: " + state.toString)
    val rules = new BoxPanel(Orientation.Vertical) { border = Swing.EmptyBorder(5) }
    val undob = new Button("Undo")
    undob.enabled = false

    add(new BorderPanel {
        add(statel, BorderPanel.Position.North)
        add(stackl, BorderPanel.Position.Center)
    }, BorderPanel.Position.West)
    add(new BoxPanel(Orientation.Vertical) {
        contents += undob
        contents += rules
    }, BorderPanel.Position.Center)

    border = Swing.EmptyBorder(30)

    listenTo(undob)

    updateTrans()

    reactions += {
        case ButtonClicked(b) =>
            b match {
                case RuleButton(rule) =>
                    history = (state, stack) :: history
                    undob.enabled = true
                    rule match {
                        case BaseRule(p, s, a, p1, s1) =>
                            state = p1
                            stack = (s1, stack.head._2) :: stack.tail
                            stackl.listData = stack
                            statel.text = "State: " + state.toString
                        case PushRule(p, s, a, p1, s1, s2) =>
                            state = p1
                            stack = (s1, dpn.usedLock(rule)) :: (s2, stack.head._2) :: stack.tail
                            stackl.listData = stack
                            statel.text = "State: " + state.toString
                            if(dpn.usedLock(rule).isDefined)
                                noticePop()
                        case PopRule(p, s, a, p1) =>
                            state = p1
                            stack = stack.tail
                            stackl.listData = stack
                            statel.text = "State: " + state.toString
                            noticePop()
                        case SpawnRule(p, s, a, p1, s1, p2, s2) =>
                            spawncount += 1
                            val dpnview = new MonitorDPNView(dpn, p2, List((s2, None)), tname + "|" + spawncount, getRoot)
                            new Frame {
                                title = "DPN Explorer " + tname + "|" + spawncount
                                contents = dpnview
                                visible = true
                                pack()
                            }
                            registerSpawn(dpnview)
                            state = p1
                            stack = (s1, stack.head._2) :: stack.tail
                            stackl.listData = stack
                            statel.text = "State: " + state.toString
                    }

                case `undob` =>
                    if (!history.isEmpty) {
                        val (p, s) = history.head
                        history = history.tail
                        state = p
                        stack = s
                        stackl.listData = stack
                        statel.text = "State: " + state.toString
                        if (history.isEmpty)
                            undob.enabled = false
                    }

                case _ =>

            }
            updateTrans
    }

    def updateTrans() {
        for (r <- rules.contents)
            deafTo(r)
        rules.contents.clear()
        for (rule <- nextRules) {
            val b = RuleButton(rule)
            rules.contents += b
            listenTo(b)
        }
        rules.revalidate()
    }

    def nextRules: Set[DPNRule[C, S, A]] = {
        if (stack.isEmpty)
            return Set()
        val tmap = dpn.getTransMap
        val key = (state, stack.head._1)
        val blockedLocks = allHeldLocks -- heldLocks
        if (tmap.contains(key))
            tmap(key).filter(r => dpn.usedLock(r).forall(!blockedLocks(_)))
        else
            Set()

    }

    def noticePop() {
        if (root != null)
            root.noticePop()
        else
            for (x <- spawnedProcesses)
                x.updateTrans
    }

    def getRoot = if (root == null) this else root

    def heldLocks = stack.flatMap { case (_, Some(x)) => Set(x) case _ => Set[L]() }

    def allHeldLocks: Set[L] = {
        if (root != null)
            root.allHeldLocks
        else
            Set() ++ spawnedProcesses.flatMap(_.heldLocks)
    }

    def registerSpawn(sp: MonitorDPNView[C, S, A, L]) {
        if (root != null)
            root.registerSpawn(sp)
        else
            spawnedProcesses ::= sp
    }

    def toHtml(rule: DPNRule[C, S, A]): String = {
        rule match {
            case SpawnRule(inState, inSymbol, action, outState, outSymbol, spawnState, spawnSymbol) =>
                "<html>Spawn" +
                    "<br>&nbsp;&nbsp;" + inState + ",&nbsp;&nbsp; " + inSymbol +
                    "<br>&nbsp;&nbsp;&nbsp;&nbsp; --" +
                    action + "-->" +
                    "<br>&nbsp;&nbsp;" +
                    outState + ", &nbsp;&nbsp;" + outSymbol + "" +
                    "<br>&nbsp;>>>> " +
                    spawnState + ",&nbsp;&nbsp; " + spawnSymbol + "</html>"

            case PushRule(inState, inSymbol, action, outState, outSymbol1, outSymbol2) =>
                dpn.usedLock(rule) match {
                    case None =>
                        "<html>Push" +
                            "<br>&nbsp;&nbsp;" + inState + ",&nbsp;&nbsp; " + inSymbol +
                            "<br>&nbsp;&nbsp;&nbsp;&nbsp; --" +
                            action + "-->" +
                            "<br>&nbsp;&nbsp;" +
                            outState + ", <br>&nbsp;&nbsp;&nbsp;&nbsp;" + outSymbol1 + "," +
                            "<br>&nbsp;&nbsp;&nbsp;&nbsp;" + outSymbol2 + "</html>"
                    case Some(l) =>
                        "<html>Acq" +
                            "<br>&nbsp;&nbsp;" + inState + ",&nbsp;&nbsp; " + inSymbol +
                            "<br>&nbsp;&nbsp;&nbsp;&nbsp; --" +
                            action + " -- lock: " + l + "-->" +
                            "<br>&nbsp;&nbsp;" +
                            outState + ", <br>&nbsp;&nbsp;&nbsp;&nbsp;" + outSymbol1 + "," +
                            "<br>&nbsp;&nbsp;&nbsp;&nbsp;" + outSymbol2 + "</html>"
                }

            case PopRule(inState, inSymbol, action, outState) =>
                "<html>Pop" +
                    "<br>&nbsp;&nbsp;" + inState + ",&nbsp;&nbsp; " + inSymbol +
                    "<br>&nbsp;&nbsp;&nbsp;&nbsp; --" +
                    action + "-->" +
                    "<br>&nbsp;&nbsp;" +
                    outState + "</html>"

            case BaseRule(inState, inSymbol, action, outState, outSymbol) =>
                "<html>Base" +
                    "<br>&nbsp;&nbsp;" + inState + ", &nbsp;&nbsp;" + inSymbol +
                    "<br>&nbsp;&nbsp;&nbsp;&nbsp; --" +
                    action + "-->" +
                    "<br>&nbsp;&nbsp;" +
                    outState + ", &nbsp;&nbsp;" + outSymbol + " </html>"
        }
    }

    case class RuleButton(rule: DPNRule[C, S, A]) extends Button(toHtml(rule))
}

object MonitorDPNView {
    def show[C, S, A, L](dpn: MonitorDPN[C, S, A, L], exitOnClose: Boolean = false) {
        if (exitOnClose) {
            val gui = new MainFrame {
                title = "DPN Explorer"
                contents = new MonitorDPNView(dpn)
                maximize()
                visible = true
            }
        } else {
            val gui = new Frame {
                title = "DPN Explorer"
                contents = new MonitorDPNView(dpn)
                maximize()
                visible = true
            }
        }

    }
}