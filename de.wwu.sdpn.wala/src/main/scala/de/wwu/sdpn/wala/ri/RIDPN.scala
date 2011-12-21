package de.wwu.sdpn.wala.ri

import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ssa.SSAAbstractInvokeInstruction
import com.ibm.wala.ssa.SSAMonitorInstruction

import de.wwu.sdpn.core.dpn.monitor.MonitorDPN
import de.wwu.sdpn.core.dpn.monitor.BaseRule
import de.wwu.sdpn.core.dpn.monitor.DPNRule
import de.wwu.sdpn.core.dpn.monitor.PopRule
import de.wwu.sdpn.core.dpn.monitor.PushRule
import de.wwu.sdpn.core.dpn.monitor.SpawnRule
import de.wwu.sdpn.wala.dpngen.symbols.DPNAction
import de.wwu.sdpn.wala.dpngen.symbols.MonitorEnter
import de.wwu.sdpn.wala.dpngen.symbols.SyncMethodEnter

import de.wwu.sdpn.core.ta.xsb.{ HasTermRepresentation => HTR }

class RIDPN[GS <% HTR, SS <% HTR <% CGNode](
    dpn: MonitorDPN[GS, SS, DPNAction, InstanceKey],
    isolationKey: InstanceKey,
    ikTerm: String,
    pa: PointerAnalysis)
        extends MonitorDPN[GS, RISymbol[InstanceKey, SS], DPNAction, InstanceKey] {

    type RS = RISymbol[InstanceKey, SS]
    type Lock = InstanceKey

    private implicit def ikToTerm(ik: InstanceKey) = new HTR {
        def toTerm = {
            assert(ik == isolationKey, "Tried to convert instance key other than the one beeing isolated")
            ikTerm
        }
    }

    private val tinitial: (GS, RS) = (dpn.initialState, NotIsolated(dpn.initialStack))
    private var tactions = Set[DPNAction]()
    private var tcontrolSymbols = Set[GS]()
    private var tstackSymbols = Set[RISymbol[InstanceKey, SS]]()
    private var ttransitions = Set[DPNRule[GS, RS, DPNAction]]()
    private var ttransmap = Map[(GS, RS), Set[DPNRule[GS, RS, DPNAction]]]()
    private var tlocks = dpn.locks

    for (rule <- dpn.getTransitions) {
        rule match {
            case BaseRule(g1, s1, a, g2, s2) => {
                if (canBeIsolated(s1)) {
                    addTransition(BaseRule(g1, Isolated(isolationKey, s1), a, g2, Isolated(isolationKey, s2)))
                    addTransition(BaseRule(g1, Summary(s1), a, g2, Summary(s2)))
                } else {
                    addTransition(BaseRule(g1, NotIsolated(s1), a, g2, NotIsolated(s2)))
                }
            }
            case PopRule(g1, s1, a, g2) => {
                if (canBeIsolated(s1)) {
                    addTransition(PopRule(g1, Isolated(isolationKey, s1), a, g2))
                    addTransition(PopRule(g1, Summary(s1), a, g2))
                } else {
                    addTransition(PopRule(g1, NotIsolated(s1), a, g2))
                }
            }
            case PushRule(g1, s1, a, g2, sc, sr) => {
                if (canBeIsolated(s1)) {
                    if (isCallOnSameObject(s1, a)) {
                        if (canBeIsolated(sc)) {
                            if (isLockOnThisPointer(a, sc)) {
                                tlocks += isolationKey
                                addTransition(PushRule(g1, Isolated(isolationKey, s1),
                                    actionForThisLock(a),
                                    g2, Isolated(isolationKey, sc),
                                    Isolated(isolationKey, sr)))
                            } else {
                                addTransition(PushRule(g1, Isolated(isolationKey, s1), a, g2, Isolated(isolationKey, sc), Isolated(isolationKey, sr)))
                            }
                            addTransition(PushRule(g1, Summary(s1), a, g2, Summary(sc), Summary(sr)))
                        } else {
                            addTransition(PushRule(g1, Summary(s1), a, g2, NotIsolated(sc), Summary(sr)))
                        }
                    } else {
                        if (canBeIsolated(sc)) {
                            if (isLockOnThisPointer(a, sc)) {
                                addTransition(PushRule(g1, Isolated(isolationKey, s1),
                                    actionForThisLock(a),
                                    g2, Isolated(isolationKey, sc),
                                    Isolated(isolationKey, sr)))
                                addTransition(PushRule(g1, Summary(s1),
                                    actionForThisLock(a),
                                    g2, Isolated(isolationKey, sc),
                                    Summary(sr)))
                            } else {
                                addTransition(PushRule(g1, Isolated(isolationKey, s1), a, g2, Isolated(isolationKey, sc), Isolated(isolationKey, sr)))
                                addTransition(PushRule(g1, Summary(s1), a, g2, Isolated(isolationKey, sc), Summary(sr)))
                            }
                            addTransition(PushRule(g1, Isolated(isolationKey, s1), a, g2, Summary(sc), Isolated(isolationKey, sr)))
                            addTransition(PushRule(g1, Summary(s1), a, g2, Summary(sc), Summary(sr)))

                        } else {
                            addTransition(PushRule(g1, Isolated(isolationKey, s1), a, g2, NotIsolated(sc), Isolated(isolationKey, sr)))
                            addTransition(PushRule(g1, Summary(s1), a, g2, NotIsolated(sc), Summary(sr)))
                        }

                    }
                } else {
                    if (canBeIsolated(sc)) {
                        addTransition(PushRule(g1, NotIsolated(s1), a, g2, Isolated(isolationKey, sc), NotIsolated(sr)))
                        addTransition(PushRule(g1, NotIsolated(s1), a, g2, Summary(sc), NotIsolated(sr)))
                    } else {
                        addTransition(PushRule(g1, NotIsolated(s1), a, g2, NotIsolated(sc), NotIsolated(sr)))
                        addTransition(PushRule(g1, NotIsolated(s1), a, g2, NotIsolated(sc), NotIsolated(sr)))
                    }
                }
            }
            case SpawnRule(g1, s1, a, gr, sr, gs, ss) => {
                if (canBeIsolated(s1)) {
                    if (isCallOnSameObject(s1, a)) {
                        if (canBeIsolated(ss)) {
                            addTransition(
                                SpawnRule(g1, Isolated(isolationKey, s1),
                                    a,
                                    gr, Isolated(isolationKey, sr),
                                    gs, Isolated(isolationKey, ss)))
                            addTransition(SpawnRule(g1, Summary(s1),
                                a,
                                gr, Summary(sr),
                                gs, Summary(ss)))
                        } else {
                            addTransition(SpawnRule(g1, Summary(s1),
                                a,
                                gr, Summary(sr),
                                gs, NotIsolated(ss)))
                        }
                    } else {
                        if (canBeIsolated(ss)) {
                            addTransition(SpawnRule(g1, Isolated(isolationKey, s1),
                                a,
                                gr, Isolated(isolationKey, sr),
                                gs, Isolated(isolationKey, ss)))
                            addTransition(SpawnRule(g1, Summary(s1),
                                a,
                                gr, Summary(sr),
                                gs, Summary(ss)))
                            addTransition(SpawnRule(g1, Isolated(isolationKey, s1),
                                a,
                                gr, Isolated(isolationKey, sr),
                                gs, Summary(ss)))
                            addTransition(SpawnRule(g1, Summary(s1),
                                a,
                                gr, Summary(sr),
                                gs, Isolated(isolationKey, ss)))
                        } else {
                            addTransition(SpawnRule(g1, Isolated(isolationKey, s1),
                                a,
                                gr, Isolated(isolationKey, sr),
                                gs, NotIsolated(ss)))
                            addTransition(SpawnRule(g1, Summary(s1),
                                a,
                                gr, Summary(sr),
                                gs, NotIsolated(ss)))
                        }
                    }
                } else {
                    if (canBeIsolated(ss)) {
                        addTransition(SpawnRule(g1, NotIsolated(s1),
                            a,
                            gr, NotIsolated(sr),
                            gs, Isolated(isolationKey, ss)))
                        addTransition(SpawnRule(g1, NotIsolated(s1),
                            a,
                            gr, NotIsolated(sr),
                            gs, Summary(ss)))
                    } else {
                        addTransition(SpawnRule(g1, NotIsolated(s1),
                            a,
                            gr, NotIsolated(sr),
                            gs, NotIsolated(ss)))
                        addTransition(SpawnRule(g1, NotIsolated(s1),
                            a,
                            gr, NotIsolated(sr),
                            gs, NotIsolated(ss)))
                    }
                }
            }
        }
    }

    def initialState: GS = dpn.initialState
    def initialStack: RS = NotIsolated(dpn.initialStack)
    def actions: Set[DPNAction] = tactions
    def controlSymbols: Set[GS] = tcontrolSymbols
    def stackSymbols: Set[RS] = tstackSymbols
    def transitions: Set[DPNRule[GS, RS, DPNAction]] = ttransitions
    def transmap: Map[(GS, RS), Set[DPNRule[GS, RS, DPNAction]]] = ttransmap

    /**
     * the set of locks used by this MonitorDPN
     */
    def locks: Set[Lock] = tlocks
    /**
     * The lock used by rule
     * @param rule a DPN rule only returns non None results for call rules
     * @return Some[lock] iff the call rule acquires the stated lock
     */
    def usedLock(rule: DPNRule[GS, RS, DPNAction]): Option[Lock] = {
        rule match {
            case PushRule(_, _, a, _, _, _) =>
                a match {
                    case SyncMethodEnter(_, ik) => Some(ik)
                    case MonitorEnter(_, ik) => Some(ik)
                    case _ => None
                }
            case _ => None
        }
    }

    /* 
     * Helper Methods
     */
    private def canBeIsolated(s: SS): Boolean = {
        val ir = s.getIR
        val method = s.getMethod()

        {
            !method.isStatic()
        } && {

            ir != null
        } && {
            val vn = ir.getSymbolTable().getParameter(0)
            val pk = pa.getHeapModel().getPointerKeyForLocal(s, vn)
            val iks = pa.getPointsToSet(pk)
            iks.contains(isolationKey)
        }
    }

    private def isLockOnThisPointer(a: DPNAction, target: CGNode): Boolean = {
        a.getSSAInstruction match {
            case None => false
            case Some(sa) =>
                sa match {
                    case ii: SSAAbstractInvokeInstruction =>
                        target.getMethod().isSynchronized()
                    case mi: SSAMonitorInstruction =>
                        val vn = target.getIR.getSymbolTable().getParameter(0)
                        mi.isMonitorEnter() && mi.getRef() == vn

                }
        }
    }

    private def actionForThisLock(a: DPNAction): DPNAction = {
        a.getSSAInstruction match {
            case ii: SSAAbstractInvokeInstruction => SyncMethodEnter(ii, isolationKey)
            case mi: SSAMonitorInstruction => MonitorEnter(mi, isolationKey)
            case _ => sys.error("Tried to convert non Invoke nor MonitorInstruction to LockAction")
        }

    }

    private def isCallOnSameObject(node: CGNode, a: DPNAction): Boolean = {
        a.getSSAInstruction match {
            case None => true
            case Some(sa) =>
                sa match {
                    case ii: SSAAbstractInvokeInstruction =>
                        val vn = node.getIR.getSymbolTable().getParameter(0)
                        vn == ii.getReceiver()
                    case _ => true
                }
        }
    }

    private def addTransition(rule: DPNRule[GS, RS, DPNAction]) {
        ttransitions += rule
        val (p, s) = (rule.inState, rule.inSymbol)
        if (!ttransmap.contains((p, s)))
            ttransmap += (p, s) -> Set(rule)
        else
            ttransmap += (p, s) -> (ttransmap((p, s)) + rule)
        rule match {
            case SpawnRule(p, s, a, p1, w1, p2, w2) =>
                tactions += a
                tstackSymbols ++= Set(s, w1, w2)
                tcontrolSymbols ++= Set(p, p1, p2)
            case PushRule(p, s, a, p1, w1, w2) =>
                tactions += a
                tstackSymbols ++= Set(s, w1, w2)
                tcontrolSymbols ++= Set(p, p1)
            case PopRule(p, s, a, p1) =>
                tactions += a
                tstackSymbols += s
                tcontrolSymbols ++= Set(p, p1)
            case BaseRule(p, s, a, p1, s1) =>
                tactions += a
                tstackSymbols ++= Set(s, s1)
                tcontrolSymbols ++= Set(p, p1)
        }

    }

}

