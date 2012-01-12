package de.wwu.sdpn.core.dpn.monitor

/**
 * A generalized MonitorDPN which consists of a set of rules and a map from rules to locks 
 * and some methods to access these.
 * Plus a initial state.
 *
 * @tparam CSymbol ControllSymbols or (thread) global State
 * @tparam SSymbol StackSymbols or program positions
 * @tparam Action Actions of the program
 * @tparam Lock Monitors used by the program 
 * 
 * @author Benedikt Nordhoff
 */
trait MonitorDPN[CSymbol, SSymbol, Action,Lock] {
    def initialState: CSymbol
    def initialStack: SSymbol 
    def actions : Set[Action]
    def controlSymbols : Set[CSymbol]
    def stackSymbols : Set[SSymbol]
    def transitions : Set[DPNRule[CSymbol, SSymbol, Action]]
    def transmap : Map[(CSymbol, SSymbol), Set[DPNRule[CSymbol, SSymbol, Action]]]
    
    /**
     * the set of locks used by this MonitorDPN
     */
    def locks : Set[Lock]
    /**
     * The lock used by rule
     * @param rule a DPN rule only returns non None results for call rules
     * @return Some[lock] iff the call rule acquires the stated lock  
     */
    def usedLock(rule:DPNRule[CSymbol,SSymbol,Action]) : Option[Lock]
    

    def getActions: Set[Action] = actions
    def getControlSymbols: Set[CSymbol] = controlSymbols
    def getStackSymbols: Set[SSymbol] = stackSymbols

    def getTransitions: Set[DPNRule[CSymbol, SSymbol, Action]] = transitions
    def getTransMap = transmap

}