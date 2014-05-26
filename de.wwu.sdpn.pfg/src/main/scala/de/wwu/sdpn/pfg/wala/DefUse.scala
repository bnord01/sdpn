package de.wwu.sdpn.pfg.wala
import scala.collection.JavaConversions._
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ssa.SSAPutInstruction
import de.wwu.sdpn.pfg.genkill.PFGForwardGenKillSolver
import de.wwu.sdpn.pfg.fixedpoint.BasicFixedpointSolver
import de.wwu.sdpn.pfg.lattices.genkill.GenKill
import de.wwu.sdpn.pfg.genkill.PFGVar
import de.wwu.sdpn.pfg.lattices.BooleanLattice
import de.wwu.sdpn.pfg.lattices.getMapLattice
import de.wwu.sdpn.pfg.lattices.BottomMap
import de.wwu.sdpn.pfg.lattices.LMap
import de.wwu.sdpn.pfg.lattices.Lattice
import de.wwu.sdpn.pfg.lattices.TopMap
import de.wwu.sdpn.wala.util.UniqueInstanceLocator
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ssa.SSAGetInstruction
import com.ibm.wala.ssa.SSAFieldAccessInstruction
import de.wwu.sdpn.pfg.fixedpoint.ConcurrentFixedpointSolver
import com.ibm.wala.types.ClassLoaderReference
import de.wwu.sdpn.pfg.fixedpoint.FixedpointSolver
import de.wwu.sdpn.pfg.genkill.LGKStatement
import com.ibm.wala.classLoader.IField

/**
 * We calculate def/use dependencies on wala parallel flow graphs.
 *
 * For every CFGPoint we calculate for every IField for every IField
 * which defs (represented by BaseEdges (which contain a SSAPutInstruction)) may flow there.
 *
 */
class DefUse(cg: CallGraph, pa: PointerAnalysis[InstanceKey], interpretKill: Boolean = true, subSolver: FixedpointSolver[PFGVar[LMap[(InstanceKey, IField), LMap[BaseEdge, Boolean]]],LGKStatement[LMap[(InstanceKey, IField), LMap[BaseEdge, Boolean]]]] = null, onlyApplication:Boolean=true) {
	type Facts = LMap[(InstanceKey, IField), LMap[BaseEdge, Boolean]]
    type DUVar = PFGVar[Facts]
	
    
    private var p_result: Map[Node, LMap[(InstanceKey, IField), LMap[BaseEdge, Boolean]]] = null

    private lazy val isUnique: Set[InstanceKey] = if (interpretKill) UniqueInstanceLocator.instances(cg, pa) else Set()

    private lazy val pfg = PFGFactory.getPFG(cg)

    private lazy val solver = if(subSolver == null)
    	new PFGForwardGenKillSolver[Facts,CGNode,Node,Action,State,Edge](pfg, getGenKill)
    else 
        new PFGForwardGenKillSolver[Facts,CGNode,Node,Action,State,Edge](pfg, getGenKill,Some(subSolver))

    private lazy val hm = pa.getHeapModel()
    private lazy val cha = cg.getClassHierarchy()
    
    def solve(canceled: => Boolean = false) {
        solver.solve(canceled)
        p_result = solver.results
    }

    def result = { assert(p_result != null, "Constraint system not solved yet! Call DefUse.solve first!"); p_result }

    def flowPossible(srcNode: CGNode, srcIdx: Int, snkNode: CGNode, snkIdx: Int): Boolean = {
        val srcPnt = PFGFactory.getCFGPoint4NodeAndIndex(srcNode, srcIdx)
        val snkPnt = PFGFactory.getCFGPoint4NodeAndIndex(snkNode, snkIdx)

        val srcInstr = srcNode.getIR().getInstructions()(srcIdx)
        val snkInstr = snkNode.getIR().getInstructions()(snkIdx)

        (srcInstr, snkInstr) match {
            case (put: SSAPutInstruction, get: SSAGetInstruction) =>
                val fieldRef = put.getDeclaredField()                
                if(onlyApplication && fieldRef.getDeclaringClass().getClassLoader() != ClassLoaderReference.Application)
                    return true;
                
                val putField = cha.resolveField(put.getDeclaredField())
                val getField = cha.resolveField(get.getDeclaredField())
                
                if (getField != putField) {
                    System.err.println("Fields don't match: " + (put, get))
                    return false
                }           
                
                val field = putField
                
                val putIks = getIKS4FieldInstr(srcNode, put)
                val getIks = getIKS4FieldInstr(snkNode, get)
                val iks = putIks intersect getIks

                if (iks.isEmpty) {
                    System.err.println("Field put and get instruktions share no common instance key: " + (put, get) + " iks: " + (putIks, getIks))
                    return false
                }
                
                

                for {
                    node <- List(Node(N, snkPnt), Node(E, snkPnt));
                    ik <- iks;
                    res <- result.get(node);
                    (edge, true) <- res((ik, field)).elems
                } {
                    if (edge.src.proc == srcNode) {
                        edge match {
                            case BaseEdge(_, SSAAction(instr), _) =>
                                if (instr.iindex == put.iindex)
                                    return true
                            case _ =>
                        }

                    }

                }

            // TODO continue here!
            case _ => throw new IllegalArgumentException("Instructions (src,snk) not corresponding to put and get: " + (srcInstr, snkInstr))
        }

        return false
    }

    /**
     * Provides the gen/kill transfer for an edge in a WalaPFG
     * This is supposed to be partially applied and handed to the gen/kill solver.
     *
     * @param pa the corresponding pointer analysis used to resolve field accesses
     * @param isUnique a predicate use to identify unique instance keys on which killings are interpreted
     * @param edge an edge of a WalaPFG which corresponds to the given pointer analysis
     */
    def getGenKill(edge: Edge): GenKill[LMap[(InstanceKey, IField), LMap[BaseEdge, Boolean]]] = {
        val lat = implicitly[Lattice[LMap[(InstanceKey, IField), LMap[BaseEdge, Boolean]]]]
        edge match {
            case be @ BaseEdge(Node(_, src @ CFGPoint(cgnode, _, _)), SSAAction(put: SSAPutInstruction), Node(nstate, _)) =>
                val field = cha.resolveField(put.getDeclaredField())
                
                if(onlyApplication && field.getDeclaringClass().getClassLoader() != ClassLoaderReference.Application)
                    return GenKill(lat.bottom, lat.top)
                    
                val iks = getIKS4FieldInstr(cgnode, put)

                val defs = Map() ++ (for (ik <- iks) yield ((ik, field) -> BottomMap(Map(be -> true))))
                val kill = nstate match {
                    case N => TopMap[(InstanceKey, IField), LMap[BaseEdge, Boolean]](defs filterKeys { case (ik, _) => isUnique(ik) })
                    case E => lat.top
                }
                GenKill(
                    BottomMap[(InstanceKey, IField), LMap[BaseEdge, Boolean]](defs),
                    kill
                )

            case _ => GenKill(lat.bottom, lat.top)
        }

    }

    private def getIKS4FieldInstr(cgnode: CGNode, instr: SSAFieldAccessInstruction): Set[InstanceKey] = {
        val field = instr.getDeclaredField()
        if (instr.isStatic()) {
            val declClass = instr.getDeclaredField().getDeclaringClass()
            val ik = hm.getInstanceKeyForClassObject(declClass)
            Set(ik)
        } else {
            val refNr = instr.getRef()
            val pk = hm.getPointerKeyForLocal(cgnode, refNr)
            Set() ++ pa.getPointsToSet(pk)
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
    def getNumberOfStatements = solver.getNumberOfStatements

}


