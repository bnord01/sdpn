package de.wwu.sdpn.core.dpn.monitor

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

}