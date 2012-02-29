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

class PFGGenKillSolver[L: Lattice, P, N, BA, R, E <: Edge[P, N, BA, R]](pfg: ParFlowGraph[P, N, BA, R, E], trans: GenKillFunction[E, L]) {

    type LV = LVar[L, N]
    type GKV = GKVar[L, N]
    type GV = PFGVar[L, N]
    type GKStmt = GKStatement[L,N] 
        
    import scala.collection.mutable.{ Map => MMap, Set => MSet }
    val rvar: MMap[N, LVar[L, N]] = MMap()
    val svar: MMap[N, GKVar[L, N]] = MMap()
    
    private val dependendStatements:MMap[GV,MSet[GKStmt]] = MMap().withDefaultValue(MSet())
    private val wl:MSet[GKStmt] = MSet()

    // Set up variables
    def initializeVariables {
        for (node <- pfg.nodes) {
            rvar += node -> new LVar[L, N](node)
            svar += node -> new GKVar[L, N](node)
        }
    }

    def initializeWorkList {
        import GKExpr._
        // Set up statements for initial nodes
        for(eNode <- pfg.entryNode.values) {
            add ( svar(eNode) ⊒ ConstGKExpr[L,N](GenKill.id))
        }
            
        
        // Set up statements for edges
        for (edge <- pfg.edges) {
            edge match {
                case be: BaseEdge[P, N, BA, R] =>
                    val ssrc = svar(be.src)
                    val ssnk = svar(be.snk)
                    add( ssnk ⊒ (ssrc beforeConst trans(edge)) )
                case ce: CallEdge[P, N, BA, R] =>
                    for ((rval, snk) <- ce.returns) {
                        for (rnode <- pfg.retNodes(ce.proc)(rval)) {
                            val ssrc = svar(ce.src)
                            val ssnk = svar(snk)
                            val sret = svar(rnode)
                            add( ssnk ⊒ ( ssrc beforeConst trans(edge) before sret ))                            
                        }
                    }
                case se: SpawnEdge[P, N, BA, R] =>
                    val ssrc = svar(se.src)
                    val ssnk = svar(se.snk)
                    add( ssnk ⊒ ( ssrc beforeConst trans(edge) ) )
            }
        }
    }

    def add(st: GKStatement[L, N]) {
        for(v <- st.getRHS()){
            dependendStatements.getOrElseUpdate(v,MSet()) += st
        }
        wl += st
    }

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

    def solve(canceled: => Boolean = false) {
        initializeVariables
        initializeWorkList
        while(!wl.isEmpty){
            if(canceled)
                throw new RuntimeException("Canceled!")
            val st = wl.first
            wl -= st
            val res = st.evaluate()
            if(res == CHANGED || res == CHANGED_AND_FIXED)
                for(s <- dependendStatements(st.getLHS()))
                    wl += s
        }

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
object GenKill{
    def id[L](implicit lat:Lattice[L]) = new GenKill[L]{
        val gen = lat.bottom
        val kill = lat.top
    }
}



trait GenKillFunction[E, L] {
    def apply(edge: E): GenKill[L]
}

case class GenKillOperator[L: Lattice, N](e: GenKill[L]) {
    def evaluate(lhs: LVar[L, N], rhs: LVar[L, N]): Byte = {
        return lhs joinWith (e appliedTo rhs)
    }
}

/**
 * Implements S[lhs] ⊒ S[rhs];gk
 */
case class HOGenKillOperator[L: Lattice, N](gk: GenKill[L]) {
    def evaluate(lhs: GKVar[L, N], rhs: GKVar[L, N]): Byte = {
        if (lhs.isTop)
            return NOT_CHANGED_AND_FIXED
        return lhs joinWith (rhs andThen gk)
    }
}

/**
 * Implements S[lhs] ⊒ S[rhs1];e;S[rhs2]
 */
case class HOGenKillOperator2[L: Lattice, N](e: GenKill[L]) {

    def evaluate(lhs: GKVar[L, N], rhs1: GKVar[L, N], rhs2: GKVar[L, N]): Byte = {
        if (lhs.isTop)
            return NOT_CHANGED_AND_FIXED
        return lhs joinWith (rhs1 andThen e andThen rhs2)
    }

}

sealed abstract class PFGVar[L, N] {
    def isTop: Boolean
}
sealed class LVar[L: Lattice, N](node: N) extends PFGVar[L, N] {
    var elem: L = implicitly[Lattice[L]].bottom

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

    def isTop = gen.isTop

    def joinWith(update: => GenKill[L]): Byte = {
        if (isTop)
            return NOT_CHANGED_AND_FIXED
        val ugen = update.gen
        val ukill = update.kill
        val newkill = kill ⊔ ukill
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
        if (kill < newkill) {
            kill = newkill
            return CHANGED
        }
        return NOT_CHANGED
    }

    def ⊒(expr: GKExpr[L, N]) = new GKStatement(this, expr)

}

sealed class GKStatement[L: Lattice, N](lhs: GKVar[L, N], rhs: GKExpr[L, N]) {

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

    def getRHS(): List[PFGVar[L, N]] = rhs.vars

    def hasVariable(v: PFGVar[L, N]): Boolean = lhs :: rhs.vars contains v

    override val hashCode = GKStatement.nextHash
    override def equals(other: Any) = this eq (other.asInstanceOf[AnyRef])

}
object GKStatement {
    private var curHash = 0
    def nextHash = { curHash += 1; curHash }
}

sealed trait GKExpr[L, N] {
    def vars: List[PFGVar[L, N]]
    def value: GenKill[L]

    def before(other: GKExpr[L, N])(implicit lat: Lattice[L]) = GKAndThenExpr(this, other)
    def beforeConst(other: GenKill[L])(implicit lat: Lattice[L]) = GKAndThenExpr(this, ConstGKExpr[L, N](other))
}
object GKExpr {
    implicit def gkVar2Expr[L, N](gkvar: GKVar[L, N]): GKExpr[L, N] = new GKExpr[L, N] { def vars = List(gkvar); def value = gkvar }
}

case class GKAndThenExpr[L: Lattice, N](lh: GKExpr[L, N], rh: GKExpr[L, N]) extends GKExpr[L, N] {
    def vars = lh.vars ::: rh.vars
    def value = lh.value andThen rh.value
}
case class ConstGKExpr[L, N](gk: GenKill[L]) extends GKExpr[L, N] {
    def vars: List[PFGVar[L, N]] = Nil
    def value = gk
}




