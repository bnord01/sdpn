package de.wwu.sdpn.pfg.lattices.genkill
import de.wwu.sdpn.pfg.lattices._

class GenKillLattice[L: Lattice] extends Lattice[GenKill[L]] {
    
    def andThen(e1: GenKill[L],e2:GenKill[L]) :GenKill[L] = {
        GenKill(e1.gen ⊓ e2.kill ⊔ e2.gen,e1.kill ⊔ e2.kill)
    }

    def appliedTo(e1:GenKill[L],l: L): L = {
        l ⊓ e1.kill ⊔ e1.gen
    }
    
    def join(e1: GenKill[L], e2: GenKill[L]): GenKill[L] = {
        GenKill(e1.gen ⊔ e2.gen, e1.kill ⊔ e2.kill)
    }
    def meet(e1: GenKill[L], e2: GenKill[L]): GenKill[L] = {
        GenKill(e1.gen ⊓ e2.gen, e1.kill ⊓ e2.kill)
    }
    def isBottom(e: GenKill[L]): Boolean = {
        e.kill.isBottom && e.gen.isBottom
    }
    def isTop(e: GenKill[L]): Boolean = {
        e.gen isTop
    }
    def isId(e: GenKill[L]): Boolean = {
        e.gen.isBottom && e.kill.isTop
    }
    
    def id: GenKill[L] = GenKill(implicitly[Lattice[L]].bottom, implicitly[Lattice[L]].top)
    def bottom: GenKill[L] = GenKill(implicitly[Lattice[L]].bottom, implicitly[Lattice[L]].bottom)
    def top: GenKill[L] = GenKill(implicitly[Lattice[L]].top, implicitly[Lattice[L]].bottom)

    /**
     * Result of comparing <code>x</code> with operand <code>y</code>.
     *  Returns <code>None</code> if operands are not comparable.
     *  If operands are comparable, returns <code>Some(r)</code> where
     *  <code>r &lt; 0</code>    iff    <code>x &lt; y</code>
     *  <code>r == 0</code>   iff    <code>x == y</code>
     *  <code>r &gt; 0</code>    iff    <code>x &gt; y</code>
     */
    def tryCompare(x: GenKill[L], y: GenKill[L]): Option[Int] = {
        //TODO Check this!
        if (lteq(x, y)) {
            if (lteq(y, x))
                Some(0)
            else
                Some(-1)
        } else {
            if (lteq(y, x))
                Some(1)
            else
                None
        }

    }

    //TODO Check this!
    /**
     * Returns <code>true</code> iff <code>x</code> comes before
     *  <code>y</code> in the ordering.
     */
    def lteq(x: GenKill[L], y: GenKill[L]): Boolean = x.gen <= y.gen && x.kill <= y.kill //&& x.kill <= (y.kill ⊔ y.gen)

}

trait GenKillLatticeElem[L] {
    def andThen(other:GenKill[L]):GenKill[L]
    def appliedTo(elem:L):L
    def isId:Boolean
}

// TODO Normalize?
case class GenKill[L](gen: L, kill: L)
