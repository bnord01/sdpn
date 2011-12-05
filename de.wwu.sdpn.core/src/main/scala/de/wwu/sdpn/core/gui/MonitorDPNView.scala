package de.wwu.sdpn.core.gui

import scala.swing.event.ButtonClicked
import scala.swing._
import de.wwu.sdpn.core.dpn.monitor._
/**
 * Explore a MonitorDPN by simulation, locks are ignored.
 * @param dpn the MonitorDPN to explore
 * @author Benedikt Nordhoff
 */
class MonitorDPNView[C, S, A, L](dpn: MonitorDPN[C, S, A, L], var state: C, var stack: List[S], val tname: String) extends BorderPanel {

    def this(dpn: MonitorDPN[C, S, A, L]) = {
        this(dpn, dpn.initialState, List(dpn.initialStack), "Main")
    }

    var history = List[(C, List[S])]()

    var spawncount = 0

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

    step()

    reactions += {
        case ButtonClicked(b) =>
            b match {
                case RuleButton(rule) =>
                    history = (state, stack) :: history
                    undob.enabled = true
                    rule match {
                        case BaseRule(p, s, a, p1, s1) =>
                            state = p1
                            stack = s1 :: stack.tail
                            stackl.listData = stack
                            statel.text = "State: " + state.toString
                        case PushRule(p, s, a, p1, s1, s2) =>
                            state = p1
                            stack = s1 :: s2 :: stack.tail
                            stackl.listData = stack
                            statel.text = "State: " + state.toString
                        case PopRule(p, s, a, p1) =>
                            state = p1
                            stack = stack.tail
                            stackl.listData = stack
                            statel.text = "State: " + state.toString
                        case SpawnRule(p, s, a, p1, s1, p2, s2) =>
                            spawncount += 1
                            new Frame {
                                title = "DPN Explorer " + tname + "|" + spawncount
                                contents = new MonitorDPNView(dpn, p2, List(s2), tname + "|" + spawncount)
                                visible = true
                                pack()
                            }
                            state = p1
                            stack = s1 :: stack.tail
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
            step()
    }

    def step() {
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
        val key = (state, stack.head)
        if (tmap.contains(key))
            tmap(key)
        else
            Set()

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
                "<html>Push" +
                    "<br>&nbsp;&nbsp;" + inState + ",&nbsp;&nbsp; " + inSymbol +
                    "<br>&nbsp;&nbsp;&nbsp;&nbsp; --" +
                    action + "-->" +
                    "<br>&nbsp;&nbsp;" +
                    outState + ", <br>&nbsp;&nbsp;&nbsp;&nbsp;" + outSymbol1 + "," +
                    "<br>&nbsp;&nbsp;&nbsp;&nbsp;" + outSymbol2 + "</html>"

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