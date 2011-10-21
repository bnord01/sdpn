package sdpn.dpn.explicit.example

import sdpn.dpn.explicit.DPNRule
import sdpn.dpn.explicit.PushRule
import sdpn.dpn.explicit.PopRule
import sdpn.dpn.explicit.SpawnRule
import sdpn.dpn.explicit.BaseRule

/**
 * A general DPN this is to be replaced by MonitorDPN but still used by DPN2CutTA
 * @tparam CSymbol
 * @tparam SSymbol
 * @tparam Action
 * 
 * @deprecated
 * 
 * @author Benedikt Nordhoff
 */
class DPN[CSymbol, SSymbol, Action](val initialState: CSymbol, val initialStack: SSymbol) {
    private var actions = Set[Action]()
    private var controlSymbols = Set[CSymbol]()
    private var stackSymbols = Set[SSymbol]()
    private var transitions = Set[DPNRule[CSymbol, SSymbol, Action]]()
    private var transmap = Map[(CSymbol, SSymbol), Set[DPNRule[CSymbol, SSymbol, Action]]]()

    def getActions: Set[Action] = actions
    def getControlSymbols: Set[CSymbol] = controlSymbols
    def getStackSymbols: Set[SSymbol] = stackSymbols

    def getTransitions: Set[DPNRule[CSymbol, SSymbol, Action]] = transitions
    def getTransMap = transmap

    def addTransition(rule: DPNRule[CSymbol, SSymbol, Action]) {
        transitions += rule
        val (p, s) = (rule.inState, rule.inSymbol)
        if (!transmap.contains((p, s)))
            transmap += (p, s) -> Set(rule)
        else
            transmap += (p, s) -> (transmap((p, s)) + rule)
        rule match {
            case SpawnRule(p, s, a, p1, w1, p2, w2) =>
                actions += a
                stackSymbols ++= Set(s, w1, w2)
                controlSymbols ++= Set(p, p1, p2)
            case PushRule(p, s, a, p1, w1, w2) =>
                actions += a
                stackSymbols ++= Set(s, w1, w2)
                controlSymbols ++= Set(p, p1)
            case PopRule(p, s, a, p1) =>
                actions += a
                stackSymbols += s
                controlSymbols ++= Set(p, p1)
            case BaseRule(p, s, a, p1, s1) =>
                actions += a
                stackSymbols ++= Set(s, s1)
                controlSymbols ++= Set(p, p1)
        }

    }

    def addSpawnRule(p: CSymbol, s: SSymbol, a: Action, p1: CSymbol, w1: SSymbol, p2: CSymbol, w2: SSymbol) {
        addTransition(SpawnRule(p, s, a, p1, w1, p2, w2))
    }
    def addPushRule(p: CSymbol, s: SSymbol, a: Action, p1: CSymbol, w1: SSymbol, w2: SSymbol) {
        addTransition(PushRule(p, s, a, p1, w1, w2))
    }
    def addPopRule(p: CSymbol, s: SSymbol, a: Action, p1: CSymbol) {
        addTransition(PopRule(p, s, a, p1))
    }
    def addBaseRule(p: CSymbol, s: SSymbol, a: Action, p1: CSymbol, w1: SSymbol) {
        addTransition(BaseRule(p, s, a, p1, w1))
    }

}