package de.wwu.sdpn.core.dpn.monitor

import de.wwu.sdpn.core.ta.xsb.{ HasTermRepresentation => HTR }

object DPNUtil {

    def createMDPNfromRules[C, S, A, L](init: (C, S), rset: Set[DPNRule[C, S, A]], lockMap: DPNRule[C, S, A] => Option[L]): MonitorDPN[C, S, A, L] = {
        var ta = Set[A]()
        var tc = Set[C]()
        var ts = Set[S]()
        var tt = rset
        var ttm = Map[(C, S), Set[DPNRule[C, S, A]]]().withDefaultValue(Set())
        var tl = Set[L]()
        for (r <- rset) {
            r match {
                case BaseRule(c, s, a, c1, s1) =>
                    ta += a
                    tc += c
                    tc += c1
                    ts += s
                    ts += s1
                    ttm += (c, s) -> (ttm((c, s)) + r)
                case PopRule(c, s, a, c1) =>
                    ta += a
                    tc += c
                    tc += c1
                    ts += s
                    ttm += (c, s) -> (ttm((c, s)) + r)
                case SpawnRule(c, s, a, cr, sr, cs, ss) =>
                    ta += a
                    tc += c
                    tc += cr
                    tc += cs
                    ts += s
                    ts += sr
                    ts += ss
                    ttm += (c, s) -> (ttm((c, s)) + r)
                case PushRule(c, s, a, cc, sc, sr) =>
                    ta += a
                    tc += c
                    tc += cc
                    ts += s
                    ts += sc
                    ts += sr
                    ttm += (c, s) -> (ttm((c, s)) + r)
                    lockMap(r).foreach(tl += _)
            }
        }

        return new MonitorDPN[C, S, A, L] {
            val initialState = init._1
            val initialStack = init._2
            val actions = ta
            val controlSymbols = tc
            val stackSymbols = ts
            val transitions = tt
            val transmap = ttm
            val locks = tl
            def usedLock(rule: DPNRule[C, S, A]) = lockMap(rule)
        }
    }


    def printNumberedDPN[C,S,A,L](dpn:MonitorDPN[C,S,A,L]): String = {
        val ca = Map() ++ dpn.controlSymbols.zipWithIndex
        val sa = Map() ++ dpn.stackSymbols.zipWithIndex
        val aa = Map() ++ dpn.actions.zipWithIndex
        val la = Map() ++ dpn.locks.zipWithIndex
        implicit def c2htr(c:C) = new HTR{ def toTerm = ca(c).toString}
        implicit def s2htr(c:S) = new HTR{ def toTerm = sa(c).toString}
        implicit def a2htr(c:A) = new HTR{ def toTerm = aa(c).toString}
        implicit def l2htr(c:L) = new HTR{ def toTerm = la(c).toString}
        printMDPN(dpn)
    }
    
    def printMDPN[C <% HTR, S <% HTR, A <% HTR, L <% HTR](dpn: MonitorDPN[C, S, A, L]): String = {
        val buf = new StringBuilder
        var idnt = 0
        def out(x: Any) { buf.append("  " * idnt); buf.append(x); buf.append("\n") }
        def id(x: String)(body: => Unit) {
            out(x + " {")
            idnt += 1
            body
            idnt -= 1
            out("}")
        }

        id("dpn") {
            id("controls") {
                for (x <- dpn.getControlSymbols) {
                    out(x.toTerm + " : \"" + x.toString + "\"")
                }
            }
            id("stacks") {
                for (x <- dpn.getStackSymbols) {
                    out(x.toTerm + " : \"" + x.toString + "\"")
                }
            }
            id("actions") {
                for (x <- dpn.getActions) {
                    out(x.toTerm + " : \"" + x.toString + "\"")
                }
            }
            id("locks") {
                for (x <- dpn.locks) {
                    out(x.toTerm + " : \"" + x.toString + "\"")
                }
            }

            out("initial( " + dpn.initialState.toTerm + " , " + dpn.initialStack.toTerm + " )")

            id("rules") {
                for (x <- dpn.getTransitions) {
                    x match {
                        case BaseRule(c, s, a, c1, s1) =>
                            out("(%s,%s)--%s-->(%s,%s)".format(c.toTerm, s.toTerm, a.toTerm, c1.toTerm, s1.toTerm))
                        case PopRule(c, s, a, c1) =>
                            out("(%s,%s)--%s-->(%s)".format(c.toTerm, s.toTerm, a.toTerm, c1.toTerm))
                        case SpawnRule(c, s, a, c1, s1, cs, ss) =>
                            out("(%s,%s)--%s-->(%s,%s:>%s,%s)".format(c.toTerm, s.toTerm, a.toTerm, c1.toTerm, s1.toTerm, cs.toTerm, ss.toTerm))
                        case PushRule(c, s, a, c1, sc, sr) =>
                            dpn.usedLock(x) match {
                                case None =>
                                    out("(%s,%s)--%s-->(%s,%s,%s)".format(c.toTerm, s.toTerm, a.toTerm, c1.toTerm, sc.toTerm, sr.toTerm))
                                case Some(l) =>
                                    out("(%s,%s)--%s-%s-->(%s,%s,%s)".format(c.toTerm, s.toTerm, a.toTerm, l.toTerm, c1.toTerm, sc.toTerm, sr.toTerm))
                            }
                    }
                }
            }

        }
        return buf.toString

    }
}