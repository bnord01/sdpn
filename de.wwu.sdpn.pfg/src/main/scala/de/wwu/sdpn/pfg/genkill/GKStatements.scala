package de.wwu.sdpn.pfg.genkill
import de.wwu.sdpn.pfg.lattices._
import de.wwu.sdpn.pfg.fixedpoint._
import de.wwu.sdpn.pfg.lattices.genkill.GenKill
import de.wwu.sdpn.pfg.lattices.genkill._

//todo implement hash code and equals for variables?
sealed abstract class PFGVar[L] {
    def isTop: Boolean
}
object PFGVar{
    private var curHash = 0
    def nextHash = { curHash += 1; curHash }
}

sealed class LVar[L: Lattice] extends PFGVar[L] {
    var elem: L = implicitly[Lattice[L]].bottom

    def isTop = elem.isTop

    def joinWith(other: => L): ChangeInfo = {
        if (isTop)
            return NotChangedAndFixed
        val newelem = elem ⊔ other
        if (newelem > elem) {
            elem = newelem
            if (newelem.isTop)
                return ChangedAndFixed
            else
                return Changed
        } else
            return NotChanged
    }

    def ⊒(expr: LExpr[L]) = new LStatement(this, expr)
    
    override val hashCode = PFGVar.nextHash
    override def equals(other: Any) = this eq (other.asInstanceOf[AnyRef])

}
/**
 * Variables for the lattice of monotone functions f:L -> L
 * where f(l) = (l meet kill) join gen
 */
sealed class GKVar[L: GenKillLattice: Lattice] extends PFGVar[L] {
    val lat = implicitly[Lattice[L]]
    val gklat = implicitly[GenKillLattice[L]]
    var elem: GenKill[L] = gklat.bottom
    def gen: L = elem.gen
    def kill: L = elem.kill

    def isTop = elem.isTop

    def joinWith(update: => GenKill[L]): ChangeInfo = {
        if (isTop)
            return NotChangedAndFixed
        val newelem = elem ⊔ update
        if (newelem > elem) {
            elem = newelem
            if (isTop)
                return ChangedAndFixed
            else
                return Changed
        } else {
            return NotChanged
        }
        //        val ugen = update.gen
        //        val ukill = update.kill
        //        val newkill = kill ⊔ ukill
        //        val newgen = gen ⊔ ugen
        //        if (newgen > gen) {
        //           if (newgen.isTop) {
        //               elem = GenKill(newgen,lat.bottom)
        //               return ChangedAndFixed
        //            }
        //           elem = GenKill(newgen,newkill)
        //           return Changed
        //        }
        //        if (kill < newkill) {
        //            elem = GenKill(gen,newkill)
        //            return Changed
        //        }
        //        return NotChanged
    }

    def ⊒(expr: GKExpr[L]) = new GKStatement(this, expr)
    
    override val hashCode = PFGVar.nextHash
    override def equals(other: Any) = this eq (other.asInstanceOf[AnyRef])

}

sealed trait LGKStatement[L] extends Statement[PFGVar[L]]

sealed class LStatement[L: Lattice](lh: LVar[L], rh: LExpr[L]) extends LGKStatement[L] {

    def evaluate(): ChangeInfo = lh joinWith (rh.value)
    def lhs: PFGVar[L] = lh

    lazy val rhs: List[PFGVar[L]] = rh.vars

    def hasVariable(v: PFGVar[L]): Boolean = lhs :: rhs contains v

    override val hashCode = GKStatement.nextHash
    override def equals(other: Any) = this eq (other.asInstanceOf[AnyRef])

}

sealed class GKStatement[L: Lattice](lh: GKVar[L], rh: GKExpr[L]) extends LGKStatement[L] {

    def evaluate(): ChangeInfo = lh joinWith (rh.value)
    def lhs: PFGVar[L] = lh

    lazy val rhs: List[PFGVar[L]] = rh.vars

    def hasVariable(v: PFGVar[L]): Boolean = lhs :: rhs contains v

    override val hashCode = GKStatement.nextHash
    override def equals(other: Any) = this eq (other.asInstanceOf[AnyRef])

}
object GKStatement {
    private var curHash = 0
    def nextHash = { curHash += 1; curHash }
}

sealed trait LExpr[L] {
    def vars: List[PFGVar[L]]
    def value: L
    def join(other: LExpr[L])(implicit lat: Lattice[L]) = LJoinExpr(this, other)
    def ⊔(other: LExpr[L])(implicit lat: Lattice[L]) = LJoinExpr(this, other)
    def andThen(trans: GKExpr[L])(implicit lat: GenKillLattice[L]) = LAppliedToExpr(trans, this)    
}

case class LJoinExpr[L: Lattice](lh: LExpr[L], rh: LExpr[L]) extends LExpr[L] {
    lazy val vars = lh.vars ::: rh.vars
    def value = lh.value ⊔ rh.value
}

case class LAppliedToExpr[L: GenKillLattice](trans: GKExpr[L], elem: LExpr[L]) extends LExpr[L] {
    lazy val vars = elem.vars ::: trans.vars
    def value = trans.value appliedTo elem.value
}

sealed trait GKExpr[L] {
    def vars: List[PFGVar[L]]
    def value: GenKill[L]

    def andThen(other: GKExpr[L])(implicit lat: GenKillLattice[L]) = GKAndThenExpr(this, other)
    def andThen(other: GenKill[L])(implicit lat: GenKillLattice[L]) = GKAndThenExpr(this, ConstGKExpr[L](other))
    def before(other: GKExpr[L])(implicit lat: GenKillLattice[L]) = GKAndThenExpr(other,this)
    def before(other: GenKill[L])(implicit lat: GenKillLattice[L]) = GKAndThenExpr(ConstGKExpr[L](other),this)
    def before(other: LExpr[L])(implicit lat: GenKillLattice[L]) = LAppliedToExpr(this,other)
}
object GKExpr {
    implicit def gkVar2Expr[L](gkvar: GKVar[L]): GKExpr[L] = new GKExpr[L] { def vars = List(gkvar); def value = gkvar.elem }
    implicit def gk2Expr[L](elem: GenKill[L]): GKExpr[L] = new GKExpr[L] { def vars = Nil; def value = elem }
    implicit def lVar2Expr[L](lvar: LVar[L]): LExpr[L] = new LExpr[L] { def vars = Nil; def value = lvar.elem }
    implicit def l2Expr[L: Lattice](elem: L): LExpr[L] = new LExpr[L] { def vars = Nil; def value = elem }
}

case class GKAndThenExpr[L: GenKillLattice](lh: GKExpr[L], rh: GKExpr[L]) extends GKExpr[L] {
    def vars = lh.vars ::: rh.vars
    def value = lh.value andThen rh.value
}
case class ConstGKExpr[L](gk: GenKill[L]) extends GKExpr[L] {
    def vars: List[PFGVar[L]] = Nil
    def value = gk
}