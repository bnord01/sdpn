package de.wwu.sdpn.core.ta.xsb.iterable

import de.wwu.sdpn.core.ta.xsb.IntersectionTA
import de.wwu.sdpn.core.ta.xsb.HasTermRepresentation

class TwoSetConflictTA[StackSymbol <% HasTermRepresentation](override val name: String, cset1: Set[StackSymbol], cset2: Set[StackSymbol])
        extends IntersectionTA(new SingleSetConflictTA(name + "cnfl12", cset1 union cset2),
            new IntersectionTA(new TopOfStackTA(name + "_cfl1", cset1), new TopOfStackTA(name + "_cfl2", cset2), name + "confl_1_2"), name) {
}