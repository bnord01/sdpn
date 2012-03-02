package de.wwu.sdpn.pfg.lattices

trait Lattice[L] extends PartialOrdering[L] {
    def join(e1: L, e2: L): L
    def meet(e1: L, e2: L): L
    def isBottom(e: L): Boolean
    def isTop(e: L): Boolean
    def bottom: L
    def top: L

    /**
     * Result of comparing <code>x</code> with operand <code>y</code>.
     *  Returns <code>None</code> if operands are not comparable.
     *  If operands are comparable, returns <code>Some(r)</code> where
     *  <code>r &lt; 0</code>    iff    <code>x &lt; y</code>
     *  <code>r == 0</code>   iff    <code>x == y</code>
     *  <code>r &gt; 0</code>    iff    <code>x &gt; y</code>
     */

    def tryCompare(x: L, y: L): Option[Int]

    /**
     * Returns <code>true</code> iff <code>x</code> comes before
     *  <code>y</code> in the ordering.
     */
    def lteq(x: L, y: L): Boolean
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
    def equiv(other: L): Boolean
}