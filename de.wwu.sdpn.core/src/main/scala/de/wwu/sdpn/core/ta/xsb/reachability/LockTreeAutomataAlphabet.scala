package de.wwu.sdpn.core.ta.xsb.reachability

import de.wwu.sdpn.core.ta.xsb.ScriptTreeAutomata

/**
 * The alphabet used for simple reachablility analyses.
 *
 * alphabet = Map(
 * "nil" -> (1,1),
 * "ret" -> (0,1),
 * "base" -> (0,2),
 * "call1" -> (0,2),
 * "call2" -> (0,3),
 * "use" -> (1,3),
 * "acq" -> (1,2),
 * "spawn" -> (0,3)
 * )
 *
 * @author Benedikt Nordhoff
 */
trait LockTreeAutomataAlphabet extends ScriptTreeAutomata {
    def alphabet = Map(
        "nil" -> (1, 1),
        "ret" -> (0, 1),
        "base" -> (0, 2),
        "call1" -> (0, 2),
        "call2" -> (0, 3),
        "use" -> (1, 3),
        "acq" -> (1, 2),
        "spawn" -> (0, 3))
}
