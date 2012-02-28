package de.wwu.sdpn.pfg.lattices

trait Lattice[L] extends PartialOrdering[L] {
    def join(e1: L, e2: L): L
    def meet(e1: L, e2: L): L
    def isBottom(e: L): Boolean
    def isTop(e: L): Boolean
    def bottom: L
    def top: L
}

trait LatticeElem[L] {
    def join(other: L): L
    def meet(other: L): L
    def isBottom: Boolean
    def isTop: Boolean
    def tryCompareTo(other: L): Option[Int]
    def <(other: L): Boolean
    def >(other: L): Boolean
    def <=(other: L): Boolean
    def >=(other: L): Boolean
    def ⊔(other: L): L
    def ⊓(other: L): L
}