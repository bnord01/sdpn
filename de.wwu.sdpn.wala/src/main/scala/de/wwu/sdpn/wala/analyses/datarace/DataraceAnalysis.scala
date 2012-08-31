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
import de.wwu.sdpn.wala.analyses.SimpleTSRResult
import de.wwu.sdpn.core.result._
import com.ibm.wala.ipa.callgraph.CGNode
import de.wwu.sdpn.core.util.IProgressMonitor
import de.wwu.sdpn.core.util.NullProgressMonitor
import de.wwu.sdpn.core.dpn.monitor.MonitorDPN
import de.wwu.sdpn.wala.dpngen.symbols.DPNAction
import de.wwu.sdpn.wala.dpngen.symbols.GlobalState
import de.wwu.sdpn.wala.ri.RIDPN
import de.wwu.sdpn.wala.ri.RISymbol
import de.wwu.sdpn.wala.ri.Isolated
import de.wwu.sdpn.wala.analyses.MyPreAnalysis
import de.wwu.sdpn.wala.util.WaitMap
import de.wwu.sdpn.wala.dpngen.MonitorDPNFactory
import de.wwu.sdpn.core.analyses.TSRTask
import scala.util.parsing.combinator.JavaTokenParsers

object DataraceAnalysis {

}

class DataraceAnalysis(cg: CallGraph, pa: PointerAnalysis, ops: DRAOptions) {
    type MDPN = MonitorDPN[GlobalState, StackSymbol, DPNAction, InstanceKey]
    type RIMDPN = RIDPN[GlobalState, StackSymbol]

    def this(cg: CallGraph, pa: PointerAnalysis) = this(cg, pa, new DRAOptions)
    ops.seal
    import ops.applicationOnly
    var fieldMap: Map[(InstanceKey, Atom), (Set[StackSymbol], Set[StackSymbol], Set[(CGNode, SSAFieldAccessInstruction)])] = Map().withDefaultValue((Set(), Set(), Set()))
    var fieldRefMap: Map[FieldReference, Set[(InstanceKey, Atom)]] = Map().withDefaultValue(Set())

    buildFieldMap()
    val uniqueLocks = SimpleAnalyses.filterByClassLoader(SimpleAnalyses.getPossibleLocks(cg, pa))

    def possibleRaceOnField(ik: InstanceKey, atom: Atom): Boolean = {
        val (rs, ws, _) = fieldMap((ik, atom))
        if (ws.isEmpty)
            return false;
        return !SimpleAnalyses.runTSRCheck(cg, pa, ws, ws ++ rs, true)
    }

    def possibleRaceOnFieldDetailed(ik: InstanceKey, atom: Atom): SimpleTSRResult = {
        val (rs, ws, _) = fieldMap((ik, atom))
        if (ws.isEmpty)
            return new SimpleTSRResult(null, null, null, null, null, null, false, Negative);
        return SimpleAnalyses.runDetailedTSRCheck(cg, pa, ws, ws ++ rs, true)
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
                    ik -> new DRBaseResult(ik, atom)
                }
                fr -> (new DRFieldResult(fr, Map() ++ subResults))
            }
            val mainResult = new DRResult(subResults)
            fireUpdate(mainResult)

            for ((_, fr) <- mainResult.subResults; (_, br) <- fr.subResults) {
                br.run
                pm worked 1
                pm subTask ("Finished data race analyis " + current + " of " + instances + ".")
                current += 1
                fireUpdate(mainResult)
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

    class DRBaseResult(val ik: InstanceKey,
                       val atom: Atom,
                       val rs: Set[StackSymbol],
                       val ws: Set[StackSymbol],
                       val nodes: Set[(CGNode, SSAFieldAccessInstruction)])
            extends BaseResult[(InstanceKey, Set[(CGNode, SSAFieldAccessInstruction)], SimpleTSRResult)]((ik, nodes, null), Undecidet) {
        def this(ik0: InstanceKey, a0: Atom) {
            this(ik0, a0, fieldMap(ik0, a0)._1, fieldMap(ik0, a0)._2, fieldMap(ik0, a0)._3)
        }
        type RIStack = RISymbol[InstanceKey, StackSymbol]
        import de.wwu.sdpn.core.ta.xsb.{ HasTermRepresentation => HTR }
        def ikTerm = "fobj"
        private implicit def ikToTerm(ik0: InstanceKey) = new HTR {
            def toTerm = {
                assert(ik0 == ik, "Tried to convert instance key other than the one beeing isolated")
                ikTerm
            }
        }

        lazy val dpn: MDPN = {
            val sliceSet = nodes.map(_._1)
            var analysis = MyPreAnalysis(cg.getClassHierarchy(), cg, pa)
            analysis += sliceSet
            val wm = new WaitMap(analysis, uniqueLocks)
            analysis = new MyPreAnalysis(analysis) {
                override def safeLock(lock: InstanceKey, node: CGNode) = uniqueLocks.contains(lock) && (ops.ignoreWait || !wm(node)(lock))
            }
            val factory = new MonitorDPNFactory(analysis)
            factory.getDPN
        }
        lazy val ridpn: RIMDPN = new RIDPN(dpn, ik, ikTerm, pa)
        lazy val riRs: Set[RIStack] = ridpn.getIsolated(rs)
        lazy val riWs: Set[RIStack] = ridpn.getIsolated(ws)

        lazy val task = if (ops.randomIsolation) TSRTask.withoutActions(ridpn, riRs union riWs, riWs, !ridpn.locks.isEmpty) else
            TSRTask.withoutActions(dpn, rs union ws, ws, !dpn.locks.isEmpty)

        lazy val possible = !ws.isEmpty && task.run
        lazy val witness = if (possible) task.runWitness else None

        def run {
            if (possible) {
                this.value = Positive
            } else {
                this.value = Negative
            }
        }

        def getCG = cg

        def runSimulator {
            if (ops.randomIsolation)
                de.wwu.sdpn.core.gui.MonitorDPNView.show(ridpn)
            else
                de.wwu.sdpn.core.gui.MonitorDPNView.show(dpn)
        }

    }
    class DRFieldResult(fr: FieldReference, srm: Map[InstanceKey, DRBaseResult])
        extends DisjunctiveResult[FieldReference, InstanceKey, DRBaseResult, (InstanceKey, Set[(CGNode, SSAFieldAccessInstruction)], SimpleTSRResult), Nothing, Nothing](fr, srm)
    class DRResult(srm: Map[FieldReference, DRFieldResult]) extends DisjunctiveResult[Unit, FieldReference, DRFieldResult, FieldReference, InstanceKey, DRBaseResult]((), srm)

}

class DRAOptions {
    private var ao = true
    private var isSealed = false;
    private var ri = false
    private var igWait = false

    def applicationOnly: Boolean = ao
    def applicationOnly_=(t: Boolean) = { checkSealed; ao = t }
    def randomIsolation: Boolean = ri
    def randomIsolation_=(t: Boolean) = { checkSealed; ri = t }
    def ignoreWait: Boolean = igWait
    def ignoreWait_=(t: Boolean) = { checkSealed; igWait = t }

    def seal {
        isSealed = true;
    }
    def checkSealed {
        if (isSealed)
            throw new IllegalStateException("Tried to update sealed DRAOptions!")
    }
}

object DRStateParser extends JavaTokenParsers {
    def xsbAtomOrVar: Parser[String] = "[a-zA-Z0-9_]+".r
    def xsbTerm: Parser[String] = (xsbAtomOrVar ~ opt(xsbTuple) ^^ { case a ~ None => a; case a ~ Some(t) => a + t }) | xsbTuple
    def xsbTuple: Parser[String] = "(" ~> repsep(xsbTerm, ",") <~ ")" ^^ { case ls => ls.mkString("(", ",", ")") }
    def lnr = wholeNumber ^^ (_.toLong)
    def inr = wholeNumber ^^ (_.toInt)
    def tb = "top" ^^ (_ => true) | "bot" ^^ (_ => false)
    def tf10 = "1" ^^ (_ => true) | "0" ^^ (_ => false)
    def state: Parser[State] =
        (
            "c(" ~> cfState <~ ("," ~ xsbTerm ~ ")")
            |
            "c(i(" ~> cfState <~ ("," ~ xsbTerm ~ ")," ~ xsbTerm ~ ")")
        )

    def cfState = "c(" ~ inr ~ "," ~ stackSym ~ "," ~ inr ~ "," ~ tf10 ~ ")" ^^ {
        case "c(" ~ g ~ "," ~ ss ~ "," ~ gf ~ "," ~ t ~ ")" =>
            State(GlobalState(g), ss, GlobalState(gf), t)
    }

    def stackSym: Parser[StackSymbol] = "s("~inr~","~inr~","~inr~")" ^^ {
        case "s(" ~ cg ~ "," ~ bb ~ "," ~ nr ~ ")" => BasicStackSymbol(cg, bb, nr)
    } |  "ri("~xsbTerm~",s("~inr~","~inr~","~inr~"))" ^^ {
        case "ri("~ key ~ ",s(" ~ cg ~ "," ~ bb ~ "," ~ nr ~ "))" => Isolated(key,cg, bb, nr)
    } |  "nri(s("~inr~","~inr~","~inr~"))" ^^ {
        case "nri(s(" ~ cg ~ "," ~ bb ~ "," ~ nr ~ "))" => NotIsolated(cg, bb, nr)
    } |  "rs(s("~inr~","~inr~","~inr~"))" ^^ {
        case "rs(s(" ~ cg ~ "," ~ bb ~ "," ~ nr ~ "))" => Summary(cg, bb, nr)
    }

    case class State(
        g: GlobalState,
        ss: StackSymbol,
        gf: GlobalState,
        term: Boolean)

    case class GlobalState(num: Int)

    trait StackSymbol{def cg:Int;def bb:Int;def instr:Int}
    
    case class BasicStackSymbol(cg: Int, bb: Int, instr: Int) extends StackSymbol
    case class Isolated(key:String, cg: Int, bb: Int, instr: Int) extends StackSymbol
    case class NotIsolated(cg: Int, bb: Int, instr: Int) extends StackSymbol
    case class Summary(cg: Int, bb: Int, instr: Int) extends StackSymbol

    def parseState(u: String): State = {
        parseAll(state, u).get
    }

}