package de.wwu.sdpn.pfg.lattices

package object genkill {
	implicit def some2GKLatticeElem[L: GenKillLattice](self: GenKill[L]) = new GenKillLatticeElem[L] {
        val lat = implicitly[GenKillLattice[L]]
        def andThen(other:GenKill[L]) = lat.andThen(self,other)
        def appliedTo(elem:L) = lat.appliedTo(self,elem)

    }
	implicit def getGKLattice[L:Lattice]:GenKillLattice[L] = new GenKillLattice[L]
}