package de.wwu.sdpn.pfg

import com.ibm.wala.fixpoint.IVariable
import com.ibm.wala.fixpoint.AbstractVariable
import com.ibm.wala.fixedpoint.impl.DefaultFixedPointSolver
import com.ibm.wala.fixpoint.UnaryStatement
import com.ibm.wala.fixpoint.UnaryOperator
import com.ibm.wala.fixpoint.FixedPointConstants._
import lattices._
import com.ibm.wala.fixpoint.AbstractOperator
import com.ibm.wala.fixpoint.IFixedPointStatement
import com.ibm.wala.util.graph.impl.NodeWithNumber
import com.ibm.wala.fixpoint.AbstractStatement

class PFGGenKillSolver[L: Lattice, P, N, BA, R, E <: Edge[P, N, BA, R]](pfg: ParFlowGraph[P, N, BA, R, E], trans: GenKillFunction[E, L]) extends DefaultFixedPointSolver[PFGVar[L, N]] {

    type LV = LVar[L, N]
    type GKV = GKVar[L, N]
    type GV = PFGVar[L, N]
    type GKOP = GenKillOperator[L, N]
    val GKOP = GenKillOperator
    type HOGKOP = HOGenKillOperator[L, N]
    val HOGKOP = HOGenKillOperator
    type HOGKOP2 = HOGenKillOperator2[L, N]
    val HOGKOP2 = HOGenKillOperator2

    import scala.collection.mutable.{ Map => MMap }
    val rvar: MMap[N, LVar[L, N]] = MMap()
    val svar: MMap[N, GKVar[L, N]] = MMap()

    // Set up variables
    override def initializeVariables {
        for (node <- pfg.nodes) {
            rvar += node -> new LVar[L, N](node)
            svar += node -> new GKVar[L, N](node)
        }
    }

    override def initializeWorkList {

        // Set up statements for edges
        for (edge <- pfg.edges) {
            edge match {
                case be: BaseEdge[P, N, BA, R] =>
                    val ssrc = svar(be.src)
                    val ssnk = svar(be.snk)
                    newGKVStatement(ssnk, HOGKOP(trans(edge)), ssrc)
                case ce: CallEdge[P, N, BA, R] =>
                    for ((rval, snk) <- ce.returns) {
                        for (rnode <- pfg.retNodes(ce.proc)(rval)) {
                            val ssrc = svar(ce.src)
                            val ssnk = svar(snk)
                            val sret = svar(rnode)
                            newGKV2Statement(ssnk, HOGKOP2(trans(edge)), ssrc, sret)
                        }
                    }
                case se: SpawnEdge[P, N, BA, R] =>
                    val ssrc = svar(se.src)
                    val ssnk = svar(se.snk)
                    newGKVStatement(ssnk, HOGKOP(trans(edge)), ssrc)
            }
        }
    }

    def newRVStatement(lhs: LV, op: GKOP, rhs: LV) {
        newStatement(lhs, op, rhs, true, false)
    }

    def newGKVStatement(lhs: GKV, op: HOGKOP, rhs: GKV) {
        newStatement(lhs, op, rhs, true, false)
    }

    def newGKV2Statement(lhs: GKV, op: HOGKOP2, rhs1: GKV, rhs2: GKV) {
        newStatement(lhs, op, rhs1, rhs2, true, false)
    }
    
    def addStatement(st: GKStatement[L,N]) {
        
    }

    //TODO change this?
    override def makeStmtRHS(size: Int): Array[PFGVar[L, N]] = new Array[PFGVar[L,N]](size)

    def printResults: String = {
        val buf = new StringBuffer
        def outln(s: Any) = { buf.append(s.toString()); buf.append("\n") }
        def out(s: Any) = buf.append(s)
        for ((n, s) <- svar) {
            out("λl. ( l ⊓ " + s.kill + " ) ⊔ " + s.gen)
            out(" for : ")
            outln(n)
        }

        buf.toString()
    }

}

trait GenKill[L] {
    def kill: L
    def gen: L
    def apply(l: L)(implicit lat: Lattice[L]): L = {
        l ⊓ kill ⊔ gen
    }

    /**
     * Returns a new GenKill with the effect of other o this
     */
    def andThen(other: GenKill[L])(implicit lat: Lattice[L]) = {
        val rkill = this.kill ⊔ other.kill
        val rgen = this.gen ⊓ other.kill ⊔ other.gen
        new GenKill[L] { val gen = rgen; val kill = rkill }
    }

    def appliedTo(l: L)(implicit lat: Lattice[L]): L = {
        l ⊓ kill ⊔ gen
    }

}

trait GenKillFunction[E, L] {
    def apply(edge: E): GenKill[L]
}

case class GenKillOperator[L: Lattice, N](e: GenKill[L]) extends UnaryOperator[PFGVar[L, N]] {
    def evaluate(lhs: PFGVar[L, N], rhs: PFGVar[L, N]): Byte = {
        require(lhs.isInstanceOf[LVar[L, N]], "GenKillOperator can only be applied to instances of LVar as LHS not: " + lhs)
        require(rhs.isInstanceOf[LVar[L, N]], "GenKillOperator can only be applied to instances of LVar as RHS not: " + rhs)
        val lh = lhs.asInstanceOf[LVar[L, N]]
        val rh = rhs.asInstanceOf[LVar[L, N]]
        evaluate(lh, rh)

    }
    def evaluate(lhs: LVar[L, N], rhs: LVar[L, N]): Byte = {
        return lhs joinWith (e appliedTo rhs)
    }
}

/**
 * Implements S[lhs] ⊒ S[rhs];gk
 */
case class HOGenKillOperator[L: Lattice, N](gk: GenKill[L]) extends UnaryOperator[PFGVar[L, N]] {
    def evaluate(lhs: PFGVar[L, N], rhs: PFGVar[L, N]): Byte = {
        require(lhs.isInstanceOf[GKVar[L, N]], "GenKillOperator can only be applied to instances of GKVar as LHS not: " + lhs)
        require(rhs.isInstanceOf[GKVar[L, N]], "GenKillOperator can only be applied to instances of GKVar as RHS not: " + rhs)
        val lh = lhs.asInstanceOf[GKVar[L, N]]
        val rh = rhs.asInstanceOf[GKVar[L, N]]
        evaluate(lh, rh)
    }
    def evaluate(lhs: GKVar[L, N], rhs: GKVar[L, N]): Byte = {
        if (lhs.isTop)
            return NOT_CHANGED_AND_FIXED
        return lhs joinWith (rhs andThen gk)
    }
}

/**
 * Implements S[lhs] ⊒ S[rhs1];e;S[rhs2]
 */
case class HOGenKillOperator2[L: Lattice, N](e: GenKill[L]) extends AbstractOperator[PFGVar[L, N]] {
    def evaluate(lhs: PFGVar[L, N], rhs: Array[PFGVar[L, N]]): Byte = {
        require(lhs.isInstanceOf[GKVar[L, N]], "GenKillOperator can only be applied to instances of GKVar as LHS not: " + lhs)
        require(rhs.length == 2, "Need exactly 2 RHS variables found: " + rhs.length)
        val rh1 = rhs(0)
        val rh2 = rhs(1)
        require(rh1.isInstanceOf[GKVar[L, N]], "GenKillOperator can only be applied to instances of GKVar as RHS not: " + rh1)
        require(rh2.isInstanceOf[GKVar[L, N]], "GenKillOperator can only be applied to instances of GKVar as RHS not: " + rh2)
        val v = lhs.asInstanceOf[GKVar[L, N]]
        val u = rh1.asInstanceOf[GKVar[L, N]]
        val r = rh2.asInstanceOf[GKVar[L, N]]

        evaluate(v, u, r)

    }

    def evaluate(lhs: GKVar[L, N], rhs1: GKVar[L, N], rhs2: GKVar[L, N]): Byte = {
        if (lhs.isTop)
            return NOT_CHANGED_AND_FIXED
        return lhs joinWith (rhs1 andThen e andThen rhs2)
    }

}

sealed abstract class PFGVar[L, N] extends AbstractVariable[PFGVar[L, N]] {
    def isTop: Boolean
}
sealed class LVar[L: Lattice, N](node: N) extends PFGVar[L, N] {
    var elem: L = implicitly[Lattice[L]].bottom
    def copyState(src: PFGVar[L, N]) {
        src match {
            case rv: LVar[L, N] => elem = rv.elem
        }
    }
    def isTop = elem.isTop

    def joinWith(other: => L): Byte = {
        if (isTop)
            return NOT_CHANGED_AND_FIXED
        val newelem = elem ⊔ other
        if (newelem > elem) {
            elem = newelem
            if (newelem.isTop)
                return CHANGED_AND_FIXED
            else
                return CHANGED
        } else
            return NOT_CHANGED
    }

}
object LVar {
    implicit def unwrapL[L: Lattice, N](lvar: LVar[L, N]): L = lvar.elem
}
/**
 * Variables for the lattice of monotone functions f:L -> L
 * where f(l) = (l meet kill) join gen
 */
sealed class GKVar[L: Lattice, N](node: N) extends PFGVar[L, N] with GenKill[L] {
    val lat = implicitly[Lattice[L]]
    import lat._
    var gen: L = bottom
    var kill: L = bottom
    def copyState(src: PFGVar[L, N]) {
        src match {
            case sv: GKVar[L, N] => gen = sv.gen; kill = sv.kill
        }
    }

    def isTop = gen.isTop

    def joinWith(update: => GenKill[L]): Byte = {
        if (isTop)
            return NOT_CHANGED_AND_FIXED
        val ugen = update.gen
        val ukill = update.kill
        val newkill = kill ⊓ ukill
        val newgen = gen ⊔ ugen
        if (newgen > gen) {
            gen = newgen
            if (newgen.isTop) {
                kill = bottom
                return CHANGED_AND_FIXED
            }
            kill = newkill
            return CHANGED
        }
        if (newkill < kill) {
            kill = newkill
            return CHANGED
        }
        return NOT_CHANGED
    }
}

sealed class GKStatement[L: Lattice, N](lhs: GKVar[L, N], rhs: GKExpr[L, N]) extends AbstractStatement[PFGVar[L, N],Nothing] {

    /**
     * Evaluate this statement, setting a new value for the left-hand side. The
     * return value is one of the following:
     * <ul>
     * {@link FixedPointConstants#CHANGED},
     * {@link FixedPointConstants#CHANGED_AND_FIXED},
     * {@link FixedPointConstants#NOT_CHANGED}or
     * {@link FixedPointConstants#NOT_CHANGED_AND_FIXED}.
     * </ul>
     */
    def evaluate(): Byte = lhs joinWith (rhs.value)
    def getLHS(): PFGVar[L, N] = lhs

    def getRHS(): Array[PFGVar[L, N]] = (lhs :: rhs.vars).toArray

    def hasVariable(v: PFGVar[L, N]): Boolean = lhs :: rhs.vars contains v
    
    override val hashCode = GKStatement.nextHash
    override def equals(other:Any) = this eq (other.asInstanceOf[AnyRef])
    
    def getOperator = throw new UnsupportedOperationException("GKStatments don't have an Operator!")
    
}
object GKStatement{
    private var curHash = 0
    def nextHash = {curHash += 1; curHash}
}

sealed trait GKExpr[L, N] {
    def vars: List[PFGVar[L, N]]
    def value: GenKill[L]
}
object GKExpr {
    implicit def gkVar2Expr[L, N](gkvar: GKVar[L, N]): GKExpr[L, N] = new GKExpr[L, N] { def vars = List(gkvar); def value = gkvar }
}

class GKAndThenExpr[L: Lattice, N](lh: GKExpr[L, N], rh: GKExpr[L, N]) extends GKExpr[L, N] {
    def vars = lh.vars ::: rh.vars
    def value = lh.value andThen rh.value
}
class SaticGKExpr[L,N](gk:GenKill[L]) extends GKExpr[L,N]{
    def vars:List[PFGVar[L,N]] = Nil
    def value = gk
}




