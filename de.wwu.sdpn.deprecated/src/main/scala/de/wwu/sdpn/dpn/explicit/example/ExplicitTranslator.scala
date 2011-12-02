package de.wwu.sdpn.wala.mnfs

import java.io.FileWriter
import java.io.BufferedWriter
import java.io.File
import java.io.PrintWriter

/**
 * Translates a dpn to a bddbddb program
 * @author Benedikt Nordhoff
 * @deprecated
 */
class ExplicitTranslator(val dpn: DPN[GlobalState, StackSymbol, DPNAction]) {
    type Rule = DPNRule[GlobalState, StackSymbol, DPNAction]
    type Dpn = DPN[GlobalState, StackSymbol, DPNAction]
    val dpnout = new PrintWriter(new BufferedWriter(new FileWriter("target/datalog/dpn.datalog")));
    val globalout = new PrintWriter(new BufferedWriter(new FileWriter("target/datalog/global.map")));
    val stackout = new PrintWriter(new BufferedWriter(new FileWriter("target/datalog/stack.map")));
    val actionsout = new PrintWriter(new BufferedWriter(new FileWriter("target/datalog/actions.map")));

    private var gmap = Map[GlobalState, Int]()
    private var smap = Map[StackSymbol, Int]()
    private var amap = Map[DPNAction, Int]()
    def getGMap = gmap
    def getSMap = smap

    def genMaps() {
        var index = 0
        for (state <- dpn.getControlSymbols) {
            gmap += state -> index
            index += 1
        }

        index = 0
        for (stack <- dpn.getStackSymbols) {
            smap += stack -> index
            index += 1
        }

        index = 0
        for (action <- dpn.getActions) {
            amap += action -> index
            index += 1
        }

    }

    def defVars {
        var index = 0
        for (state <- dpn.getControlSymbols) {
            gmap += state -> index
            globalout.println(state)
            index += 1
        }
        dpnout.println("GLOBAL " + index + " global.map")
        globalout.close();
        index = 0
        for (stack <- dpn.getStackSymbols) {
            smap += stack -> index
            stackout.println(stack)
            index += 1
        }
        dpnout.println("STACK " + index + " stack.map")
        stackout.close();
        index = 0
        for (action <- dpn.getActions) {
            amap += action -> index
            actionsout.println(action)
            index += 1
        }
        dpnout.println("ACTION " + index + " actions.map")
        actionsout.close();
    }
    def defRels = {
        dpnout.println("base_rule(p:GLOBAL,s:STACK,a:ACTION,p':GLOBAL,s':STACK)")
        dpnout.println("push_rule(p:GLOBAL,s:STACK,a:ACTION,p':GLOBAL,s':STACK,s'':STACK)")
        dpnout.println("pop_rule(p:GLOBAL,s:STACK,a:ACTION,p':GLOBAL)")
        dpnout.println("spawn_rule(p:GLOBAL,s:STACK,a:ACTION,p':GLOBAL,s':STACK,p'':GLOBAL,s'':STACK)")
    }
    def translateRule(rule: Rule): StringBuffer = {
        val result = new StringBuffer();
        import result.{ append => out }

        rule match {
            case BaseRule(p, s, a, p1, s1) =>
                out("base_rule(" +
                    gmap(p) + ", " +
                    smap(s) + ", " +
                    amap(a) + ", " +
                    gmap(p1) + ", " +
                    smap(s1) + ").")
            case SpawnRule(p, s, a, p1, w1, p2, w2) =>
                out("spawn_rule(" +
                    gmap(p) + ", " +
                    smap(s) + ", " +
                    amap(a) + ", " +
                    gmap(p1) + ", " +
                    smap(w1) + ", " +
                    gmap(p2) + ", " +
                    smap(w2) + ").")
            case PushRule(p, s, a, p1, w1, w2) =>
                out("push_rule(" +
                    gmap(p) + ", " +
                    smap(s) + ", " +
                    amap(a) + ", " +
                    gmap(p1) + ", " +
                    smap(w1) + ", " +
                    smap(w2) + ").")
            case PopRule(p, s, a, p1) =>
                out("pop_rule(" +
                    gmap(p) + ", " +
                    smap(s) + ", " +
                    amap(a) + ", " +
                    gmap(p1) + ").")
        }
        return result
    }

    def transRules: StringBuffer = {
        genMaps()
        val result = new StringBuffer()
        result.append("%%% Defining dpn transitions %%%\n")
        for (rule <- dpn.getTransitions)
            result.append(translateRule(rule).toString + "\n")
        return result
    }

    def writeOutput {
        dpnout.println("## DEFINING VARS ##")
        defVars
        dpnout.println()
        dpnout.println("## DEFINING RELATIONS ##")
        defRels
        dpnout.println()
        dpnout.println("## DEFINING RULES ##")
        for (rule <- dpn.getTransitions)
            dpnout.println(translateRule(rule).toString)
        dpnout.close()
    }

}