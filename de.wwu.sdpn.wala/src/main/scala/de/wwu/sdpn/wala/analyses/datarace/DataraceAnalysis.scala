package de.wwu.sdpn.wala.analyses.datarace
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.callgraph.CallGraph
import de.wwu.sdpn.wala.dpngen.symbols.StackSymbol
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.util.strings.Atom
import scala.collection.JavaConversions._
import com.ibm.wala.types.ClassLoaderReference
import scala.util.control.Exception._
import com.ibm.wala.ssa.SSAFieldAccessInstruction
import com.ibm.wala.ssa.SSAGetInstruction
import de.wwu.sdpn.wala.util.Converters
import com.ibm.wala.ipa.callgraph.propagation.PointerKey
import com.ibm.wala.types.FieldReference
import de.wwu.sdpn.wala.analyses.SimpleAnalyses
import de.wwu.sdpn.core.result._
import com.ibm.wala.ipa.callgraph.CGNode
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.NullProgressMonitor

object DataraceAnalysis {
    type DRBaseResult = BaseResult[InstanceKey, Set[(CGNode, SSAFieldAccessInstruction)]]
    type DRFieldResult = DisjunctiveResult[FieldReference, Unit, InstanceKey, DRBaseResult, Set[(CGNode, SSAFieldAccessInstruction)], Nothing, Nothing]
    type DRResult = DisjunctiveRootResult[Unit, FieldReference, DRFieldResult, Unit, InstanceKey, DRBaseResult]
}

class DataraceAnalysis(cg: CallGraph, pa: PointerAnalysis, ops: DRAOptions) {
    import DataraceAnalysis._

    def this(cg: CallGraph, pa: PointerAnalysis) = this(cg, pa, new DRAOptions)
    ops.seal
    import ops.applicationOnly
    var fieldMap: Map[(InstanceKey, Atom), (Set[StackSymbol], Set[StackSymbol], Set[(CGNode, SSAFieldAccessInstruction)])] = Map().withDefaultValue((Set(), Set(), Set()))
    var fieldRefMap: Map[FieldReference, Set[(InstanceKey, Atom)]] = Map().withDefaultValue(Set())

    buildFieldMap()

    def possibleRaceOnField(ik: InstanceKey, atom: Atom): Boolean = {
        val (rs, ws, _) = fieldMap((ik, atom))
        if (ws.isEmpty)
            return false;
        return !SimpleAnalyses.runTSRCheck(cg, pa, ws, ws ++ rs, true)
    }

    def possibleRaceOnField(ref: FieldReference): Boolean = {
        val ikas = fieldRefMap(ref)
        if (ikas.isEmpty)
            throw new IllegalArgumentException("No corresponding fields found!")
        for ((ik, atom) <- ikas) {
            if (possibleRaceOnField(ik, atom)) {
                return true
            }
        }
        return false

    }

    def anyRacePossible: Boolean = {
        for ((ik, atom) <- fieldMap.keys) {
            if (possibleRaceOnField(ik, atom)) {
                return true
            }
        }
        return false
    }

    def fullDetailedAnalysis(fireUpdate: DRResult => Unit = { _ => () }, pm: IProgressMonitor = new NullProgressMonitor): DRResult = {
        import de.wwu.sdpn.core.util.ProgressMonitorUtil._
        try {
            val instances = fieldMap.size
            var current = 1
            pm.beginTask("Preparing to run " + instances + "Analyes", instances)
            val subResults = for ((fr, iks) <- fieldRefMap) yield {
                val subResults = for ((ik, atom) <- iks) yield {
                    ik -> BaseResult[InstanceKey, Set[(CGNode, SSAFieldAccessInstruction)]](ik, fieldMap((ik, atom))._3, Undecidet)
                }
                fr -> (DisjunctiveResult(fr, (), Map() ++ subResults): DRFieldResult)
            }
            val mainResult = DisjunctiveRootResult((), subResults): DRResult
            fireUpdate(mainResult)

            for ((fr, iks) <- fieldRefMap) {
                for ((ik, atom) <- iks) {
                    val path = List(fr, ik)
                    if (possibleRaceOnField(ik, atom))
                        mainResult.updateValue(path, Positive)
                    else
                        mainResult.updateValue(path, Negative)
                    pm worked 1
                    pm subTask ("Finished data race analyis " + current + " of " + instances + ".")
                    current += 1
                    fireUpdate(mainResult)
                }
            }
            mainResult
        } finally {
            pm done
        }
    }

    protected def buildFieldMap() {
        for (
            node <- cg if !applicationOnly ||
                failAsValue(classOf[NullPointerException])(false)(node.getMethod().getReference().getDeclaringClass().getClassLoader().equals(ClassLoaderReference.Application))
        ) {
            for (instr <- node.getIR().iterateNormalInstructions()) {
                instr match {
                    case fai: SSAFieldAccessInstruction =>
                        val iks: Set[InstanceKey] = if (fai.getRef() != -1)
                            Set() ++ pa.getPointsToSet(pa.getHeapModel().getPointerKeyForLocal(node, fai.getRef()))
                        else {
                            Set(pa.getHeapModel().getInstanceKeyForClassObject(fai.getDeclaredField().getDeclaringClass()))
                        }
                        val ref = fai.getDeclaredField()
                        val fields = iks map ((_, ref.getName()))
                        if (!fields.isEmpty)
                            fieldRefMap += ref -> (fieldRefMap(ref) ++ fields)
                        val isRead = fai.isInstanceOf[SSAGetInstruction]
                        val ss = Converters.getSS4NodeAndIndex(node, instr.iindex)
                        for (field <- fields) {
                            val (or, ow, is) = fieldMap(field)
                            val nr = if (isRead) or + ss else or
                            val nw = if (!isRead) ow + ss else ow
                            fieldMap += field -> ((nr, nw, is + ((node, fai))))
                        }
                    case _ =>
                }
            }
        }
    }

}

class DRAOptions {
    private var ao = true
    private var isSealed = false;

    def applicationOnly: Boolean = ao
    def applicationOnly_=(t: Boolean) = { checkSealed; ao = t }

    def seal {
        isSealed = true;
    }
    def checkSealed {
        if (isSealed)
            throw new IllegalStateException("Tried to update sealed DRAOptions!")
    }
} 