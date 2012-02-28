package de.wwu.sdpn.pfg

import com.ibm.wala.fixpoint.IVariable
import com.ibm.wala.fixpoint.AbstractVariable
import com.ibm.wala.fixedpoint.impl.DefaultFixedPointSolver
import com.ibm.wala.fixpoint.UnaryStatement
import com.ibm.wala.fixpoint.UnaryOperator
import com.ibm.wala.fixpoint.FixedPointConstants._
import lattices._
import com.ibm.wala.fixpoint.AbstractOperator

class PFGGenKillSolver[L: Lattice, P, N, BA, R, E <: Edge[P, N, BA, R]](pfg: ParFlowGraph[P, N, BA, R, E], trans: GenKillFunction[E, L]) extends DefaultFixedPointSolver[PFGVar[L, N]] {

    type RV = RVar[L, N]
    type SV = SVar[L, N]
    type GV = PFGVar[L, N]
    type GKOP = GenKillOperator[L, N]
    val GKOP = GenKillOperator
    type HOGKOP = HOGenKillOperator[L, N]
    val HOGKOP = HOGenKillOperator

    import scala.collection.mutable.{ Map => MMap }
    val rvar: MMap[N, RVar[L, N]] = MMap()
    val svar: MMap[N, SVar[L, N]] = MMap()

    // Set up variables
    override def initializeVariables {
        for (node <- pfg.nodes) {
            rvar += node -> RVar[L, N](node)
            svar += node -> SVar[L, N](node)
        }
    }

    override def initializeWorkList {

        // Set up statements for edges
        for (edge <- pfg.edges) {
            edge match {
                case be: BaseEdge[P, N, BA, R] =>
                    val ssrc = svar(be.src)
                    val ssnk = svar(be.snk)
                    newSVStatement(ssnk, HOGKOP(trans(edge)), ssrc)
                case ce: CallEdge[P,N,BA,R] =>
                    for((rval,snk)<-ce.returns){
                        for(rnode <- pfg.retNodes(ce.proc)(rval)) {
                        	
                        }
                    }
                case se: SpawnEdge[P,N,BA,R] =>
                    val ssrc = svar(se.src)
                    val ssnk = svar(se.snk)
                    newSVStatement(ssnk, HOGKOP(trans(edge)), ssrc)
            }
        }
    }

    def newRVStatement(lhs: RV, op: GKOP, rhs: RV) {
        newStatement(lhs, op, rhs, true, false)
    }
    
    def newSVStatement(lhs: SV, op: HOGKOP, rhs: SV) {
        newStatement(lhs, op, rhs, true, false)
    }

    override def makeStmtRHS(size: Int): Array[PFGVar[L, N]] = throw new UnsupportedOperationException("Cant make statement rhs")
    
    def printResults: String = {
        val buf = new StringBuffer
        def outln(s:Any) = {buf.append(s.toString());buf.append("\n")}
        def out(s:Any) = buf.append(s)
        for ((n,s) <- svar){
            out("λl. ( l ⊓ " + s.kill + " ) ⊔ " + s.gen)
            out (" for : ")
            outln (n)
        }
        
        buf.toString()
    }

}

trait GenKill[L] {
    def kill: L
    def gen: L
}

trait GenKillFunction[E, L] {
    def apply(edge: E): GenKill[L]
}

case class GenKillOperator[L: Lattice, N](gk: GenKill[L]) extends UnaryOperator[PFGVar[L, N]] {
    def evaluate(lhs: PFGVar[L, N], rhs: PFGVar[L, N]): Byte = {
        require(lhs.isInstanceOf[RVar[L, N]], "GenKillOperator can only be applied to instances of RVar as LHS not: " + lhs)
        require(rhs.isInstanceOf[RVar[L, N]], "GenKillOperator can only be applied to instances of RVar as RHS not: " + rhs)
        val lh = lhs.asInstanceOf[RVar[L, N]]
        val rh = rhs.asInstanceOf[RVar[L, N]]
        if (lh.elem isTop)
            return NOT_CHANGED_AND_FIXED

        val newelem = lh.elem ⊔ (rh.elem ⊓ gk.kill ⊔ gk.gen)
        if (newelem > lh.elem) {
            lh.elem = newelem
            if (newelem.isTop)
                return CHANGED_AND_FIXED
            else
                return CHANGED
        } else
            return NOT_CHANGED
    }
}

/**
 * Implements S[lhs] ⊒ S[rhs];gk
 */
case class HOGenKillOperator[L: Lattice, N](gk: GenKill[L]) extends UnaryOperator[PFGVar[L, N]] {
    def evaluate(lhs: PFGVar[L, N], rhs: PFGVar[L, N]): Byte = {
        require(lhs.isInstanceOf[SVar[L, N]], "GenKillOperator can only be applied to instances of SVar as LHS not: " + lhs)
        require(rhs.isInstanceOf[SVar[L, N]], "GenKillOperator can only be applied to instances of SVar as RHS not: " + rhs)
        val lh = lhs.asInstanceOf[SVar[L, N]]
        val rh = rhs.asInstanceOf[SVar[L, N]]

        if (lh.isTop)            
            return NOT_CHANGED_AND_FIXED
        
        val rgen = rh.gen
        val rkill = rh.kill

        val egen = gk.gen
        val ekill = gk.kill
        
        val ukill = rkill ⊔ ekill
        val ugen = rgen ⊓ ekill ⊔ egen

        return lh.update(ugen, ukill)
    }
}

/**
 * Implements S[lhs] ⊒ S[rhs];gk
 */
case class HOGenKillOperator2[L: Lattice, N](e: GenKill[L]) extends AbstractOperator[PFGVar[L, N]] {
    def evaluate(lhs: PFGVar[L, N], rhs: Array[PFGVar[L, N]]): Byte = {
        require(lhs.isInstanceOf[SVar[L, N]], "GenKillOperator can only be applied to instances of SVar as LHS not: " + lhs)
        require(rhs.length == 2, "Need exactly 2 RHS variables found: " + rhs.length)
        val rh1 = rhs(0)
        val rh2 = rhs(1)
        require(rh1.isInstanceOf[SVar[L, N]], "GenKillOperator can only be applied to instances of SVar as RHS not: " + rh1)
        require(rh2.isInstanceOf[SVar[L, N]], "GenKillOperator can only be applied to instances of SVar as RHS not: " + rh2)
        val v = lhs.asInstanceOf[SVar[L, N]]
        val u = rh1.asInstanceOf[SVar[L, N]]
        val r = rh2.asInstanceOf[SVar[L, N]]

        if (v.isTop)            
            return NOT_CHANGED_AND_FIXED
        /// TODO CONTINUE HERE!
        val ugen = u.gen
        val ukill = u.kill

        val egen = e.gen
        val ekill = e.kill        
               3
    }
}

sealed trait PFGVar[L, N] extends AbstractVariable[PFGVar[L, N]] {
    def isTop: Boolean
}
case class RVar[L: Lattice, N](node: N) extends PFGVar[L, N] {
    var elem: L = implicitly[Lattice[L]].bottom
    def copyState(src: PFGVar[L, N]) {
        src match {
            case rv: RVar[L, N] => elem = rv.elem
        }
    }
    def isTop = elem.isTop
}
case class SVar[L: Lattice, N](node: N) extends PFGVar[L, N] with GenKill[L] {
    val lat = implicitly[Lattice[L]]
    import lat._
    var gen: L = bottom
    var kill: L = bottom
    def copyState(src: PFGVar[L, N]) {
        src match {
            case sv: SVar[L, N] => gen = sv.gen; kill = sv.kill
        }
    }
    
    def isTop = gen.isTop
    
    def update(ugen: L, ukill: L): Byte = {
        if (isTop)
            return NOT_CHANGED_AND_FIXED
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


