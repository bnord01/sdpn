package de.wwu.sdpn.ta.prolog.cuts

import de.wwu.sdpn.ta.ScriptTreeAutomata

/**
 * The alphabet of the tree automata for iterated reachability analyses. 
 * 
 * alphabet = Map(
 * "cut" -> (1,2),
 * "nil" -> (1,1),
 * "ret" -> (0,1),
 * "base" -> (1,2),
 * "call1" -> (0,2),
 * "call2" -> (0,3),
 * "use" -> (1,3),
 * "acq" -> (1,2),
 * "spawn" -> (0,3)
 * )
 *
 * @author Benedikt Nordhoff
 */
trait CutLockTreeAutomataAlphabet extends ScriptTreeAutomata {
    override val alphabet = Map(
        "cut" -> (1, 2),
        "nil" -> (1, 1),
        "ret" -> (0, 1),
        "base" -> (1, 2),
        "call1" -> (0, 2),
        "call2" -> (0, 3),
        "use" -> (1, 3),
        "acq" -> (1, 2),
        "spawn" -> (0, 3))
    override final val stateSize = 1
}
