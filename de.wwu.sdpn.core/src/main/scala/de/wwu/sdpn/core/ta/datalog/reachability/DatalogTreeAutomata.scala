package de.wwu.sdpn.core.ta.datalog.reachability

import de.wwu.sdpn.core.ta.xsb.ScriptTreeAutomata

trait DatalogTreeAutomata extends ScriptTreeAutomata {
    def alphabet = Map(
        "nil" -> (1, 1),
        "ret" -> (0, 1),
        "base" -> (0, 2),
        "call1" -> (0, 2),
        "call2" -> (0, 3),
//        "use" -> (1, 3),
//        "acq" -> (1, 2),
        "spawn" -> (0, 3))
}
