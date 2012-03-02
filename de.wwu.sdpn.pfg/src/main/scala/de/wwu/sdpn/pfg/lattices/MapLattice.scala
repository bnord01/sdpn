package de.wwu.sdpn.pfg.lattices

/**
 * Lattice over maps with default values into an arbitrary lattice  
 */
class MapLattice[N, L: Lattice] extends Lattice[LMap[N, L]] {

    def join(e1: LMap[N, L], e2: LMap[N, L]): LMap[N, L] = {
        if (e1.isInstanceOf[TopMap[_, _]] || e2.isInstanceOf[TopMap[_, _]]) {
            var m: Map[N, L] = Map()
            for (x <- e1.elems.keySet ++ e2.elems.keySet) {
                val v = e1(x) ⊔ e2(x)
                if (!v.isTop)
                    m += x -> v
            }
            TopMap(m)
        } else {
            var m: Map[N, L] = Map()
            for (x <- e1.elems.keySet ++ e2.elems.keySet) {
                val v = e1(x) ⊔ e2(x)
                if (!v.isBottom)
                    m += x -> v
            }
            BottomMap(m)
        }
    }

    def meet(e1: LMap[N, L], e2: LMap[N, L]): LMap[N, L] = {
        if (e1.isInstanceOf[BottomMap[_, _]] || e2.isInstanceOf[BottomMap[_, _]]) {
            var m: Map[N, L] = Map()
            for (x <- e1.elems.keySet ++ e2.elems.keySet) {
                val v = e1(x) ⊔ e2(x)
                if (!v.isBottom)
                    m += x -> v
            }
            BottomMap(m)
        } else {
            var m: Map[N, L] = Map()
            for (x <- e1.elems.keySet ++ e2.elems.keySet) {
                val v = e1(x) ⊔ e2(x)
                if (!v.isTop)
                    m += x -> v
            }
            TopMap(m)
        }
    }

    def isBottom(e: LMap[N, L]): Boolean = {
        e match {
            case TopMap(_)        => false
            case BottomMap(elems) => elems.forall { case (_, b) => b.isBottom }
        }
    }

    def isTop(e: LMap[N, L]): Boolean = {
        e match {
            case BottomMap(_)  => false
            case TopMap(elems) => elems.forall { case (_, b) => b.isTop }
        }
    }

    def bottom: LMap[N, L] = BottomMap(Map())

    def top: LMap[N, L] = TopMap(Map())

    def tryCompare(x: LMap[N, L], y: LMap[N, L]): Option[Int] = {
        throw new UnsupportedOperationException("No tryCompare for MapLattices")
    }

    def lteq(x: LMap[N, L], y: LMap[N, L]): Boolean = {
        if (x.isInstanceOf[TopMap[_, _]] && y.isInstanceOf[BottomMap[_, _]])
            return (implicitly[Lattice[L]].top <= implicitly[Lattice[L]].bottom)
        return (x.elems.keySet ++ y.elems.keySet) forall { n => x(n) <= y(n) }
    }

}

sealed trait LMap[N, L] {
    def apply(n: N): L
    def elems: Map[N, L]
}
case class BottomMap[N, L: Lattice](elems: Map[N, L]) extends LMap[N, L] {
    val lat = implicitly[Lattice[L]]
    def apply(n: N): L = elems.getOrElse(n, lat.bottom)
}
case class TopMap[N, L: Lattice](elems: Map[N, L]) extends LMap[N, L] {
    val lat = implicitly[Lattice[L]]
    def apply(n: N): L = elems.getOrElse(n, lat.top)
}

