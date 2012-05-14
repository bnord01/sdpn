package de.wwu.sdpn.wala.util

import com.ibm.wala.ssa.SSACFG
import com.ibm.wala.ssa.ISSABasicBlock
import com.ibm.wala.ssa.SSAMonitorInstruction

import com.ibm.wala.ssa.SSAInstruction
import com.ibm.wala.ipa.callgraph.CGNode
import scala.collection.JavaConversions._
import scala.collection.Set

/**
 * A helper used at DPN construction
 * mapping monitorenter instructions to the corresponding monitorexits
 *
 * @author Benedikt Nordhoff
 */
object MonitorMatcher {

    def getExits(cfg: SSACFG, bbNr: Int): Set[(SSACFG#BasicBlock, Int)] = {
        //TODO: This assumes there are no cycles except for monitorExits
        //TODO: does it?
        val bb = cfg.getBasicBlock(bbNr)
        val visited = scala.collection.mutable.Set[SSACFG#BasicBlock]()
        val vnr = getMI(bb) match {
            case Some(enter) =>
                if (!enter.isMonitorEnter)
                    throw new IllegalArgumentException("bb contains Monitor Exit as first instruction!")
                enter.getRef
            case None =>
                throw new IllegalArgumentException("bb contains no Monitor Enter as first instruction!")
        }

        def getExits(bb: SSACFG#BasicBlock, counter: Int): Iterable[(SSACFG#BasicBlock, Int)] = {
            if (visited(bb)) {
                return Iterable()
            }
            visited += bb

            
            def nSucc(bb:SSACFG#BasicBlock) = cfg.getNormalSuccessors(bb).map({
                case b: SSACFG#BasicBlock => b
                case x => throw new IllegalArgumentException ("Didn't expect non SSACFG#BasicBlock in SSACFG! " + x)
            })
            
            def eSucc(bb:SSACFG#BasicBlock) = cfg.getExceptionalSuccessors(bb)map({
                case b: SSACFG#BasicBlock => b
                case x => throw new IllegalArgumentException ("Didn't expect non SSACFG#BasicBlock in SSACFG! " + x)
            })
            
            getMI(bb) match {
                case Some(monitorInstr) if (monitorInstr.getRef == vnr) =>
                    if (monitorInstr.isMonitorEnter) {
                        return nSucc(bb).flatMap(getExits(_, counter + 1)) ++
                            eSucc(bb).flatMap(getExits(_, counter))
                    } else {
                        if (counter > 0)
                            return nSucc(bb).flatMap(getExits(_, counter - 1)) ++
                                eSucc(bb).flatMap(getExits(_, counter))
                        else {
                            val index = findMIIndex(bb)
                            return eSucc(bb).flatMap(getExits(_, counter)) += ((bb, index))
                        }

                    }
                case _ =>
                    return nSucc(bb).flatMap(getExits(_, counter)) ++
                        eSucc(bb).flatMap(getExits(_, counter))
            }
        }

        return Set() ++ getExits(bb, -1);
    }

    def getMI(bb: SSACFG#BasicBlock): Option[SSAMonitorInstruction] = {
        // if this is removed need to check findMIIndex
        assert(bb.filter(_.isInstanceOf[SSAMonitorInstruction]).size < 2, "Can't handle more than 1 monitor instruction per basic block.")
        bb.find(_.isInstanceOf[SSAMonitorInstruction]).map(_.asInstanceOf[SSAMonitorInstruction])
    }

    def findMIIndex(bb: SSACFG#BasicBlock): Int = {
        var index = 0
        bb.iteratePhis().foreach(_ => index += 1)
        if (bb.isCatchBlock())
            index += 1
        for (ins <- bb.iterateNormalInstructions) {
            if (ins.isInstanceOf[SSAMonitorInstruction]) // assuming there is only one as guaranteed by getMI 
                return index
            index += 1
        }
        throw new IllegalArgumentException("Basic block didn't contain any SSAMonitorInstruction " + bb)
    }

    def getEnters(cfg: SSACFG): Set[SSACFG#BasicBlock] = {
        var res = Set[SSACFG#BasicBlock]()

        for (bb <- cfg.iterator) {
            assert(bb.isInstanceOf[SSACFG#BasicBlock], "Didn't expext non SSACFG#BasicBlock in SSACFG!" + bb)
            val bs = bb.asInstanceOf[SSACFG#BasicBlock]
            getMI(bs) match {
                case Some(mi) =>
                    if (mi.isMonitorEnter)
                        res += bs
                case _ =>
            }
        }
        res
    }

}
