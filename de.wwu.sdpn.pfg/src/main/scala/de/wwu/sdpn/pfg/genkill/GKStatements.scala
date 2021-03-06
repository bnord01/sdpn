package de.wwu.sdpn.pfg.genkill
import de.wwu.sdpn.pfg.lattices._
import de.wwu.sdpn.pfg.fixedpoint._
import de.wwu.sdpn.pfg.lattices.genkill.GenKill
import de.wwu.sdpn.pfg.lattices.genkill._

sealed abstract class PFGVar[L] {
    def isTop: Boolean
}
object PFGVar {
    private var curHash = 0
    def nextHash = { curHash += 1; curHash }
}

sealed class LVar[L: Lattice] extends PFGVar[L] {
    @volatile
    var elem: L = implicitly[Lattice[L]].bottom

    def isTop = elem.isTop

    def joinWith(other: => L): ChangeInfo = synchronized {
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
    def debugGE(expr: LExpr[L]) = new DebuggingLStatement(this, expr)

    override val hashCode = PFGVar.nextHash
    override def equals(other: Any) = this eq (other.asInstanceOf[AnyRef])
    override def toString: String = "LVar"+hashCode+"(" + elem + ")"

}
/**
 * Variables for the lattice of monotone functions f:L -> L
 * where f(l) = (l meet kill) join gen
 */
sealed class GKVar[L: GenKillLattice: Lattice] extends PFGVar[L] {
    val lat = implicitly[Lattice[L]]
    val gklat = implicitly[GenKillLattice[L]]
    @volatile
    var elem: GenKill[L] = gklat.bottom
    def gen: L = elem.gen
    def kill: L = elem.kill

    def isTop = elem.isTop

    def joinWith(update: => GenKill[L]): ChangeInfo = synchronized {
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
    }

    def ⊒(expr: GKExpr[L]) = new GKStatement(this, expr)
    def debugGE(expr: GKExpr[L]) = new DebuggingGKStatement(this, expr)

    override val hashCode = PFGVar.nextHash
    override def equals(other: Any) = this eq (other.asInstanceOf[AnyRef])
    override def toString: String = "GKVar" + hashCode + "(" + elem + ")"
}

sealed trait LGKStatement[L] extends Statement[PFGVar[L]] with IdentityCheck[PFGVar[L]] with LHSSubstitution[PFGVar[L],LGKStatement[L]]

sealed class LStatement[L: Lattice](lh: LVar[L], rh: LExpr[L]) extends LGKStatement[L] {

    def evaluate(): ChangeInfo = lh joinWith (rh.value)
    def lhs: PFGVar[L] = lh

    lazy val rhs: List[PFGVar[L]] = rh.vars

    def hasVariable(v: PFGVar[L]): Boolean = lhs :: rhs contains v

    override val hashCode = GKStatement.nextHash
    override def equals(other: Any) = this eq (other.asInstanceOf[AnyRef])

    override def toString: String = lh + " ⊑ " + rh
    
    def isId = rh.getIdVar.isDefined
    override def getIdVar = rh.getIdVar
    
    override def substituteLHS(nlhs:PFGVar[L]) :LStatement[L] = nlhs match {
        case nlh: LVar[_] => new LStatement(nlh,rh)
        case _ => throw new IllegalArgumentException("Can substitute LHS of LStatement only by LVar but got: " + nlhs)
    }

}

sealed class DebuggingLStatement[L: Lattice](lh: LVar[L], rh: LExpr[L]) extends LStatement[L](lh, rh) {
    override def evaluate(): ChangeInfo = {
        val oldLH = lh.elem
        System.err.println("Evaluating: " + rh)
        System.err.println("RH: " + rh.value)
        System.err.println("Old LHS: " + oldLH)
        val res = lh joinWith (rh.value)
        System.err.println("New LHS: " + lh.elem)
        res
    }

}

sealed class GKStatement[L: Lattice](lh: GKVar[L], rh: GKExpr[L]) extends LGKStatement[L] {

    def evaluate(): ChangeInfo = lh joinWith (rh.value)
    def lhs: PFGVar[L] = lh

    lazy val rhs: List[PFGVar[L]] = rh.vars

    def hasVariable(v: PFGVar[L]): Boolean = lhs :: rhs contains v

    override val hashCode = GKStatement.nextHash
    override def equals(other: Any) = this eq (other.asInstanceOf[AnyRef])

    override def toString: String = lh + " ⊒ " + rh
    
    def isId = rh.getIdVar.isDefined
    override def getIdVar = rh.getIdVar
    
    override def substituteLHS(nlhs:PFGVar[L]) :GKStatement[L] = nlhs match {
        case nlh: GKVar[_] => new GKStatement(nlh,rh)
        case _ => throw new IllegalArgumentException("Can substitute LHS of GKStatement only by GKVar but got: " + nlhs)
    }
}
sealed class DebuggingGKStatement[L: Lattice](lh: GKVar[L], rh: GKExpr[L]) extends GKStatement[L](lh, rh) {
    override def evaluate(): ChangeInfo = {
        val oldLH = lh.elem
        System.err.println("Evaluating: " + rh)
        System.err.println("RH: " + rh.value)
        System.err.println("Old LHS: " + oldLH)
        val res = lh joinWith (rh.value)
        System.err.println("New LHS: " + lh.elem)
        res
    }

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
    def getIdVar:Option[LVar[L]]
    def isConstTop:Boolean
}

case class LJoinExpr[L: Lattice](lh: LExpr[L], rh: LExpr[L]) extends LExpr[L] {
    lazy val vars = lh.vars ::: rh.vars
    def value = lh.value ⊔ rh.value
    def getIdVar = if (lh.isConstTop) rh.getIdVar else if(rh.isConstTop) lh.getIdVar else None
    def isConstTop = lh.isConstTop && rh.isConstTop
}

case class LAppliedToExpr[L: GenKillLattice](trans: GKExpr[L], elem: LExpr[L]) extends LExpr[L] {
    lazy val vars = elem.vars ::: trans.vars
    def value = trans.value appliedTo elem.value
    def isConstTop = trans.isConstIdTransfer && elem.isConstTop
    def getIdVar = if(trans.isConstIdTransfer) elem.getIdVar else None
}

sealed trait GKExpr[L] {
    def vars: List[PFGVar[L]]
    def value: GenKill[L]
    /**
     * Is this expression constant and the identity transformer?
     */
    def isConstIdTransfer :Boolean
    /**
     * Is this expression equivalent to some var? 
     */
    def getIdVar:Option[GKVar[L]]
    
    def andThen(other: GKExpr[L])(implicit lat: GenKillLattice[L]) = GKAndThenExpr(this, other)
    def andThen(other: GenKill[L])(implicit lat: GenKillLattice[L]) = GKAndThenExpr(this, GKConst[L](other))
    def after(other: GKExpr[L])(implicit lat: GenKillLattice[L]) = GKAndThenExpr(other, this)
    def after(other: GenKill[L])(implicit lat: GenKillLattice[L]) = GKAndThenExpr(GKConst[L](other), this)
    def appliedTo(other: LExpr[L])(implicit lat: GenKillLattice[L]) = LAppliedToExpr(this, other)
}
object GKExpr {
    implicit def gkVar2Expr[L](gkvar: GKVar[L]): GKExpr[L] = GKVarExpr(gkvar)
    implicit def gk2Expr[L: GenKillLattice](elem: GenKill[L]): GKExpr[L] = GKConst(elem)
    implicit def lVar2Expr[L](lvar: LVar[L]): LExpr[L] = LVarExpr(lvar)
    implicit def l2Expr[L: Lattice](elem: L): LExpr[L] = LConst(elem)
}

case class GKAndThenExpr[L: GenKillLattice](lh: GKExpr[L], rh: GKExpr[L]) extends GKExpr[L] {
    def vars = lh.vars ::: rh.vars
    def value = lh.value andThen rh.value
    def isConstIdTransfer = lh.isConstIdTransfer && rh.isConstIdTransfer    
    def getIdVar = if(lh.isConstIdTransfer) rh.getIdVar else None 
}
case class GKConst[L: GenKillLattice](gk: GenKill[L]) extends GKExpr[L] {
    def vars: List[PFGVar[L]] = Nil
    def value = gk
    lazy val isConstIdTransfer = gk.isId
    val getIdVar = None
}
case class LConst[L:Lattice](l: L) extends LExpr[L] {
    def vars: List[PFGVar[L]] = Nil
    def value = l
    def isConstTop = l.isTop
    def getIdVar = None
}
case class GKVarExpr[L](gk: GKVar[L]) extends GKExpr[L] {
    def vars: List[PFGVar[L]] = List(gk)
    def value = gk.elem
    def isConstIdTransfer = false
    def getIdVar = Some(gk)
}
case class LVarExpr[L](l: LVar[L]) extends LExpr[L] {
    def vars: List[PFGVar[L]] = List(l)
    def value = l.elem
    def isConstTop = false
    def getIdVar = Some(l)
}

