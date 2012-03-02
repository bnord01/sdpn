package de.wwu.sdpn.pfg.lattices

class FlatLattice[T] extends Lattice[FLE[T]] {
    def join(e1: FLE[T], e2: FLE[T]): FLE[T] = {
        (e1,e2) match {
            case(Bottom,Bottom) => Bottom
            case(Elem(x),Elem(y)) if x == y => Elem(x) 
            case _ => Top
        }
    }    
    def meet(e1: FLE[T], e2: FLE[T]): FLE[T] = {
        (e1,e2) match {
            case(Top,Top) => Top
            case(Elem(x),Elem(y)) if x == y => Elem(x) 
            case _ => Bottom
        }
    }    
    def isBottom(e: FLE[T]): Boolean = e == Bottom
    def isTop(e: FLE[T]): Boolean = e == Top
    def bottom: FLE[T] = Bottom
    def top: FLE[T] = Top

    /**
     * Result of comparing <code>x</code> with operand <code>y</code>.
     *  Returns <code>None</code> if operands are not comparable.
     *  If operands are comparable, returns <code>Some(r)</code> where
     *  <code>r &lt; 0</code>    iff    <code>x &lt; y</code>
     *  <code>r == 0</code>   iff    <code>x == y</code>
     *  <code>r &gt; 0</code>    iff    <code>x &gt; y</code>
     */

    def tryCompare(x: FLE[T], y: FLE[T]): Option[Int] = {
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

    /**
     * Returns <code>true</code> iff <code>x</code> comes before
     *  <code>y</code> in the ordering.
     */
    def lteq(x: FLE[T], y: FLE[T]): Boolean = {
        (x,y) match {
            case(_,Top) => true
            case(Bottom,_) => true
            case(Elem(a),Elem(b)) => a == b 
            case _ => false
        }
    }
}

sealed trait FLE[+T]
case object Bottom extends FLE[Nothing]
case object Top extends FLE[Nothing]
case class Elem[+T](x: T) extends FLE[T]