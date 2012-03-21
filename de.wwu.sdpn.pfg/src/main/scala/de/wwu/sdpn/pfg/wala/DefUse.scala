package de.wwu.sdpn.pfg.wala
import scala.collection.JavaConversions.iterableAsScalaIterable
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ssa.SSAPutInstruction
import com.ibm.wala.types.FieldReference
import de.wwu.sdpn.pfg.genkill.PFGForwardGenKillSolver
import de.wwu.sdpn.pfg.lattices.genkill.GenKill
import de.wwu.sdpn.pfg.lattices.BooleanLattice
import de.wwu.sdpn.pfg.lattices.getMapLattice
import de.wwu.sdpn.pfg.lattices.BottomMap
import de.wwu.sdpn.pfg.lattices.LMap
import de.wwu.sdpn.pfg.lattices.Lattice
import de.wwu.sdpn.pfg.lattices.TopMap
import de.wwu.sdpn.wala.util.UniqueInstanceLocator
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ssa.SSAGetInstruction

/**
 * We calculate def/use dependencies on wala parallel flow graphs.
 *
 * For every CFGPoint we calculate for every FieldReference for every FieldReference
 * which defs (represented by BaseEdges (which contain a SSAPutInstruction)) may flow there.
 *
 */
class DefUse(cg: CallGraph, pa: PointerAnalysis, interpretKill: Boolean = true) {

    private var p_result: Map[Node, LMap[(InstanceKey, FieldReference), LMap[BaseEdge, Boolean]]] = null

    private lazy val uniqueInstances: Set[InstanceKey] = if (interpretKill) UniqueInstanceLocator.instances(cg, pa) else Set()

    private lazy val pfg = PFGFactory.getPFG(cg)

    private lazy val solver = new PFGForwardGenKillSolver(pfg, getGenKill(pa, uniqueInstances))

    def solve(canceled: => Boolean) {
        solver.solve(canceled)
        p_result = solver.results
    }

    def result = {assert(p_result != null, "Constraint system not solved yet! Call DefUse.solve first!");p_result}
    
    def flowPossible(srcNode:CGNode, srcIdx:Int, snkNode:CGNode,snkIdx:Int) : Boolean = {        
        val srcPnt = PFGFactory.getCFGPoint4NodeAndIndex(srcNode,srcIdx)
        val snkPnt = PFGFactory.getCFGPoint4NodeAndIndex(snkNode,snkIdx)
        
        val srcInstr = srcNode.getIR().getInstructions()(srcIdx)
        val snkInstr = snkNode.getIR().getInstructions()(snkIdx)
        
        (srcInstr,snkInstr) match {
            case (put:SSAPutInstruction,get:SSAGetInstruction) =>
                // TODO continue here!
            case _ => throw new IllegalArgumentException("Instructions (src,snk) not corresponding to put and get: " + (srcInstr,snkInstr))
        }
        
        for (node <- List(Node(N,snkPnt),Node(E,snkPnt))) {
            // TODO continue here!
        }
        
        return true
    }

    /**
     * Provides the gen/kill transfer for an edge in a WalaPFG
     * This is supposed to be partially applied and handed to the gen/kill solver.
     *
     * @param pa the corresponding pointer analysis used to resolve field accesses
     * @param isUnique a predicate use to identify unique instance keys on which killings are interpreted
     * @param edge an edge of a WalaPFG which corresponds to the given pointer analysis
     */
    def getGenKill(pa: PointerAnalysis, isUnique: InstanceKey => Boolean = _ => false)(edge: Edge): GenKill[LMap[(InstanceKey, FieldReference), LMap[BaseEdge, Boolean]]] = {
        val lat = implicitly[Lattice[LMap[(InstanceKey, FieldReference), LMap[BaseEdge, Boolean]]]]
        val hm = pa.getHeapModel()
        edge match {
            case be@BaseEdge(Node(_, src @ CFGPoint(cgnode, _, _)), SSAAction(act: SSAPutInstruction), Node(nstate, _)) =>
                val field = act.getDeclaredField()
                val iks: Iterable[InstanceKey] =
                    if (act.isStatic()) {
                        val declClass = act.getDeclaredField().getDeclaringClass()
                        val ik = hm.getInstanceKeyForClassObject(declClass)
                        Set(ik)
                    } else {
                        val refNr = act.getRef()
                        val pk = hm.getPointerKeyForLocal(cgnode, refNr)
                        pa.getPointsToSet(pk)
                    }

                val defs = Map() ++ (for (ik <- iks) yield ((ik, field) -> BottomMap(Map(be -> true))))
                val kill = nstate match {
                    case N => TopMap[(InstanceKey, FieldReference), LMap[BaseEdge, Boolean]](defs filterKeys { case (ik, _) => isUnique(ik) })
                    case E => lat.top
                }
                GenKill(
                    BottomMap[(InstanceKey, FieldReference), LMap[BaseEdge, Boolean]](defs),
                    kill
                )

            case _ => GenKill(lat.bottom, lat.top)
        }

    }
    
    def printResults: String = {
        val buf = new StringBuffer
        def outln(s: Any) = { buf.append(s.toString()); buf.append("\n") }
        def out(s: Any) = buf.append(s)
        for (n <- pfg.nodes) {
            out(result(n))
            out(" for : ")
            outln(n)
        }
        buf.toString()
    }

}


