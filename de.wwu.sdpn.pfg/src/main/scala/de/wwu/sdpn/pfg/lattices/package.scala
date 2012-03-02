package de.wwu.sdpn.pfg

import scala.collection.BitSet
package object lattices {
    implicit def some2LatticeElem[L: Lattice](self: L) = new LatticeElem[L] {
        val lat = implicitly[Lattice[L]]
        def join(other: L) = lat.join(self, other)
        def meet(other: L) = lat.meet(self, other)
        def isBottom = lat.isBottom(self)
        def isTop = lat.isTop(self)
        def tryCompareTo(other: L) = lat.tryCompare(self, other)
        def <(other: L) = lat.lt(self, other)
        def >(other: L) = lat.gt(self, other)
        def <=(other: L) = lat.lteq(self, other)
        def >=(other: L) = lat.gteq(self, other)
        def ⊔(other: L) = lat.join(self, other)
        def ⊓(other: L) = lat.meet(self, other)
        def equiv(other: L) = lat.equiv(self,other)

    }

    implicit object BooleanLattice extends Lattice[Boolean] {
        def join(e1: Boolean, e2: Boolean): Boolean = e1 || e2
        def meet(e1: Boolean, e2: Boolean): Boolean = e1 && e2
        def isBottom(e: Boolean): Boolean = !e
        def isTop(e: Boolean): Boolean = e
        def bottom: Boolean = false
        def top: Boolean = true
        def lteq(e1: Boolean, e2: Boolean): Boolean = e2 || !e1
        def tryCompare(e1: Boolean, e2: Boolean) =
            (e1, e2) match {
                case (true, true)   => Some(0)
                case (false, true)  => Some(-1)
                case (true, false)  => Some(1)
                case (false, false) => Some(0)
            }
    }   
    
    implicit def getMapLattice[N,L:Lattice]:MapLattice[N,L] = new MapLattice[N,L]
    
}