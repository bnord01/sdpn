package de.wwu.sdpn.wala.util

import com.ibm.wala.ssa.SSACFG
import com.ibm.wala.ssa.ISSABasicBlock
import com.ibm.wala.ssa.SSAMonitorInstruction

import com.ibm.wala.ssa.SSAInstruction
import com.ibm.wala.ipa.callgraph.CGNode
import scala.collection.JavaConversions._
import scala.collection.Set

/**
 * A trait to be mixed into a DPNFactory 
 * mapping monitorenter instructions to the corresponding monitorexits
 * 
 * @author Benedikt Nordhoff
 */
trait MonitorMatcher {
    
    def getExits(node: CGNode, bbNr: Int): Set[ISSABasicBlock] = {
        val ir = node.getIR
        require(ir != null, "got null IR from CGNode")
        var res = MonitorMatcher.getExits(ir.getControlFlowGraph, bbNr)
        assert(!res.isEmpty, "no exits found")
        res
    }
    
    /**
     * get the MonitorExits corresponding to the MonitorEnter in cfg at bbNr.
     * @return A tuple of two BasicBlocks where the first represents the normal MonitorExit and the second the exception handler
     */
    def getExitTuple(node: CGNode, bbNr: Int): (ISSABasicBlock, ISSABasicBlock) = {
    	val ir = node.getIR
        require(ir != null, "got null IR from CGNode")
        var exits = MonitorMatcher.getExits(ir.getControlFlowGraph, bbNr)        
        assert(exits.size == 2, "Found " + exits.size + " exits for " + bbNr + " expected 2!")
        val it = exits.iterator
        val a = it.next
        val b = it.next
        val cfg = node.getIR.getControlFlowGraph
        import cfg.{ getNormalSuccessors => nSucc, getExceptionalSuccessors => eSucc }
        val aex = eSucc(a).contains(a)
        val bex = eSucc(b).contains(b)
        if (aex && !bex) {
        	val nex = nSucc(b)
        	//require(nex.size == 1)
        	val eex = nSucc(a)
        	//require(eex.size == 1)
            return (nex.first, eex.first)
        }
        if (bex && !aex){
        	val nex = nSucc(a)
        	//require(nex.size == 1)
        	val eex = nSucc(b)
        	//require(nex.size == 1)
            return (nex.first, eex.first)
        }
        
        throw new IllegalArgumentException("Didn't find one handler and one non handler block!")
    }
}

object MonitorMatcher {

    

    def getExits(cfg: SSACFG, bbNr: Int): Set[ISSABasicBlock] = {
        //TODO: This assumes there are no cycles except for monitorExits
        //TODO: does it?
        val bb = cfg.getBasicBlock(bbNr)
        val visited = scala.collection.mutable.Set[ISSABasicBlock]()
        val vnr = getMI(bb) match {
            case Some(enter) =>
                if (!enter.isMonitorEnter)
                    throw new IllegalArgumentException("bb contains Monitor Exit as first instruction!")
                enter.getRef
            case None =>
                throw new IllegalArgumentException("bb contains no Monitor Enter as first instruction!")
        }

        def getExits(bb: ISSABasicBlock, counter: Int): Iterable[ISSABasicBlock] = {
            if (visited(bb)) {
                return Iterable()
            }
            visited += bb

            import cfg.{ getNormalSuccessors => nSucc, getExceptionalSuccessors => eSucc }

            getMI(bb) match {
                case Some(monitorInstr) if (monitorInstr.getRef == vnr) =>
                    if (monitorInstr.isMonitorEnter) {
                        return nSucc(bb).flatMap(getExits(_, counter + 1)) ++
                            eSucc(bb).flatMap(getExits(_, counter))
                    } else {
                        if (counter > 0)
                            return nSucc(bb).flatMap(getExits(_, counter - 1)) ++
                                eSucc(bb).flatMap(getExits(_, counter))
                        else
                            return eSucc(bb).flatMap(getExits(_, counter)) += bb
                    }
                case _ =>
                    return nSucc(bb).flatMap(getExits(_, counter)) ++
                        eSucc(bb).flatMap(getExits(_, counter))
            }
        }

        return Set() ++ getExits(bb, -1);
    }

    def getMI(bb: ISSABasicBlock): Option[SSAMonitorInstruction] = {
        bb.find(_.isInstanceOf[SSAMonitorInstruction]).map(_.asInstanceOf[SSAMonitorInstruction])
    }

    def getEnters(cfg: SSACFG): Set[ISSABasicBlock] = {
        var res = Set[ISSABasicBlock]()

        for (bb <- cfg.iterator) {
            getMI(bb) match {
                case Some(mi) =>
                    if (mi.isMonitorEnter)
                        res += bb
                case _ =>
            }
        }
        res
    }

}
