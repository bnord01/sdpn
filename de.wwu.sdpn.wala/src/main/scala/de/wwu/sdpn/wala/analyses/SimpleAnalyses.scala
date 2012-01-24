package de.wwu.sdpn.wala.analyses
import scala.collection.Set
import com.ibm.wala.classLoader.IClass
import com.ibm.wala.ipa.callgraph.impl.Util
import com.ibm.wala.ipa.callgraph.propagation.ConstantKey
import com.ibm.wala.ipa.callgraph.propagation.InstanceKey
import com.ibm.wala.ipa.callgraph.propagation.PointerAnalysis
import com.ibm.wala.ipa.callgraph.propagation.SSAPropagationCallGraphBuilder
import com.ibm.wala.ipa.callgraph.AnalysisCache
import com.ibm.wala.ipa.callgraph.AnalysisOptions
import com.ibm.wala.ipa.callgraph.CGNode
import com.ibm.wala.ipa.callgraph.CallGraph
import com.ibm.wala.ipa.cha.ClassHierarchy
import com.ibm.wala.types.ClassLoaderReference
import com.ibm.wala.util.config.AnalysisScopeReader
import com.ibm.wala.util.io.FileProvider
import de.wwu.sdpn.core.analyses.SingleSetReachability
import de.wwu.sdpn.core.analyses.TwoSetReachability
import de.wwu.sdpn.core.dpn.monitor.MonitorDPN
import de.wwu.sdpn.core.ta.xsb.FullWitnessIntersectionEmptinessCheck
import de.wwu.sdpn.core.ta.xsb.IntersectionEmptinessCheck
import de.wwu.sdpn.core.ta.xsb.XSBInterRunner
import de.wwu.sdpn.core.util.IProgressMonitor
import de.wwu.sdpn.core.util.ProgressMonitorUtil
import de.wwu.sdpn.wala.dpngen.symbols.DPNAction
import de.wwu.sdpn.wala.dpngen.symbols.GlobalState
import de.wwu.sdpn.wala.dpngen.symbols.StackSymbol
import de.wwu.sdpn.wala.dpngen.MonitorDPNFactory
import de.wwu.sdpn.wala.util.LockWithOriginLocator
import de.wwu.sdpn.wala.util.ThreadSensContextSelector
import de.wwu.sdpn.wala.util.WaitMap
import de.wwu.sdpn.core.util.SubProgressMonitor

object SimpleAnalyses {
  import ProgressMonitorUtil._
  val runner = XSBInterRunner
  type MDPN = MonitorDPN[GlobalState, StackSymbol, DPNAction, InstanceKey]

  /**
   * Method to run a SingleSetReachability analysis and return the result of the
   * emptiness check.
   *
   * This Analysis generates a DPN from the given CallGraph
   * and PointerAnalysis which uses the given sliceSet to reduce
   * the size of the generated DPN. Only methods from which
   * a Method from sliceSet is reachable are considered.
   * To be sound all Methods of confSet must be considered e.g.
   * should be contained in sliceSet.
   * If sliceSet is null or empty no slicing is used.
   *
   * If lockSens is set to true a lock sensitive analysis is used.
   * Unique locks are identified but only those with
   * class loader "Application" are considered.  There should be at most
   * eight such locks on a 64bit system and at most five on a 32bit system.
   *
   * The DPN uses the "FakeRootMethod" of the call graph as start configuration.
   *
   * An conflict exists if there are at least two processes which can simultaneously
   * reach an stack symbol of confSet.  So confSet could be some critical section.
   *
   * The method returns the result of the emptiness check. That means it returns
   * true iff no conflict can exists.
   *
   * @param cg A call graph for the analysis
   * @param pa The corresponding PointerAnalysis obtained from the builder used to create the call graph
   * @param confSet A set of StackSymbols which should be checked for mutual exclusion.
   * @param sliceSet A set of CGNodes which are used for slicing. E.g. the methods of confSet plus locking positions.
   * @param lockSens A flag which decieds if this analysis should be locksensitive.
   * @return true iff no conflict can exist.
   */
  def runSSRCheck(cg: CallGraph,
    pa: PointerAnalysis,
    confSet: Set[StackSymbol],
    sliceSet: Set[CGNode],
    lockSens: Boolean,
    pm: IProgressMonitor = null): Boolean = {
    val dpn = getMDPN(cg, pa, sliceSet, lockSens)
    val ss = dpn.getStackSymbols
    require(confSet.subsetOf(ss), "Some symbols of confSet are not contained in the DPN!")
    val (td, bu) = SingleSetReachability.genAutomata(dpn, confSet, lockSens)
    val check = SingleSetReachability.genCheck(td, bu)
    return runner.runCheck(check, pm)
  }

  def runStdLibSSRCheck(cg: CallGraph,
    pa: PointerAnalysis,
    confSet: Set[StackSymbol],
    sliceSet: Set[CGNode],
    pm: IProgressMonitor = null): Boolean = {
    val dpn = getMDPN(cg, pa, sliceSet, true)
    val ss = dpn.getStackSymbols
    require(confSet.subsetOf(ss), "Some symbols of confSet are not contained in the DPN!")
    val (td, bu) = SingleSetReachability.genStdLibAutomata(dpn, confSet)
    val check = SingleSetReachability.genCheck(td, bu)
    return runner.runCheck(check, pm)
  }

  /**
   * Method to run a WitnessTwoSetReachability analysis and return the result of the
   * emptiness check.
   *
   * Same as runSSRCheck but two sets of conflict stack symbols are used.
   * Here there must exist two processes where the first reaches a stack symbol
   * of confSet1 and the second a stack symbol of confSet2.
   *
   * If one want's to check for data races on a variable  confSet1 could be all positions where
   * a variable is written and confSet2 all positions where the variable is written or read.
   *
   *
   * @param cg A call graph for the analysis
   * @param pa The corresponding PointerAnalysis obtained from the builder used to create the call graph
   * @param confSet1 A set of StackSymbols which should be checked for mutual exclusion with confSet2.
   * @param confSet2 A set of StackSymbols which should be checked for mutual exclusion with confSet1.
   * @param sliceSet A set of CGNodes which are used for slicing. E.g. the methods of confSet plus locking positions.
   * @param lockSens A flag which decieds if this analysis should be locksensitive.
   * @return None iff no conflict can exist Some(witness) iff an conflict exists with the given witness
   */
  def runWitnessTSRCheck(cg: CallGraph, pa: PointerAnalysis, confSet1: Set[StackSymbol], confSet2: Set[StackSymbol], sliceSet: Set[CGNode], lockSens: Boolean): Option[String] = {
    val dpn = getMDPN(cg, pa, sliceSet, lockSens)
    val ss = dpn.getStackSymbols
    require(confSet1.subsetOf(ss), "Some symbols of confSet1 are not contained in the DPN!")
    require(confSet2.subsetOf(ss), "Some symbols of confSet2 are not contained in the DPN!")
    val (td, bu) = TwoSetReachability.genAutomata(dpn, confSet1, confSet2, lockSens)
    val check = new FullWitnessIntersectionEmptinessCheck(td, bu)
    return runner.runFullWitnessCheck(check)
  }

  /**
   * Method to run a TwoSetReachability analysis and return the result of the
   * emptiness check.
   *
   * Same as runSSRCheck but two sets of conflict stack symbols are used.
   * Here there must exist two processes where the first reaches a stack symbol
   * of confSet1 and the second a stack symbol of confSet2.
   *
   * If one want's to check for data races on a variable  confSet1 could be all positions where
   * a variable is written and confSet2 all positions where the variable is written or read.
   *
   *
   * @param cg A call graph for the analysis
   * @param pa The corresponding PointerAnalysis obtained from the builder used to create the call graph
   * @param confSet1 A set of StackSymbols which should be checked for mutual exclusion with confSet2.
   * @param confSet2 A set of StackSymbols which should be checked for mutual exclusion with confSet1.
   * @param sliceSet A set of CGNodes which are used for slicing. E.g. the methods of confSet plus locking positions.
   * @param lockSet A set of locks which can be abstracted into the DPN.
   * @return true iff no conflict can exist
   */
  def runTSRCheck(cg: CallGraph, pa: PointerAnalysis, confSet1: Set[StackSymbol], confSet2: Set[StackSymbol], sliceSet: Set[CGNode], lockSens: Boolean, pm: IProgressMonitor): Boolean = {
    try {
      beginTask(pm, "Running two set reachability check.", 4)
      subTask(pm, "Generating DPN and tree automata.")
      val dpn = getMDPN(cg, pa, sliceSet, lockSens)
      val ss = dpn.getStackSymbols
      require(confSet1.subsetOf(ss), "Some symbols of confSet1 are not contained in the DPN!")
      require(confSet2.subsetOf(ss), "Some symbols of confSet2 are not contained in the DPN!")
      val (td, bu) = TwoSetReachability.genAutomata(dpn, confSet1, confSet2, lockSens)
      val check = new IntersectionEmptinessCheck(td, bu)
      worked(pm, 1)
      if (isCanceled(pm)) throw new RuntimeException("Canceled")
      subTask(pm, "Running XSB based emptiness check")
      val pms = new SubProgressMonitor(pm, 3)
      return runner.runCheck(check, pms)
    } finally {
      done(pm)
    }
  }
  def runTSRCheck(cg: CallGraph, pa: PointerAnalysis, confSet1: Set[StackSymbol], confSet2: Set[StackSymbol], lockSens: Boolean, pm0: IProgressMonitor = null): Boolean =
    runTSRCheck(cg, pa, confSet1, confSet2, confSet1.map(_.node) ++ confSet2.map(_.node), lockSens, pm0)
  def runTSRCheck(cg: CallGraph, pa: PointerAnalysis, confSet1: Set[StackSymbol], confSet2: Set[StackSymbol], sliceSet: Set[CGNode], lockSens: Boolean): Boolean =
    runTSRCheck(cg, pa, confSet1, confSet2, sliceSet, lockSens, null)

  /**
   * Method to run a SingleSetReachability analysis and return the result of the
   * emptiness check.
   *
   * This Analysis generates a DPN from the given CallGraph
   * and PointerAnalysis which uses the given sliceSet to reduce
   * the size of the generated DPN. Only methods from which
   * a Method from sliceSet is reachable are considered.
   * To be sound all Methods of confSet must be considered e.g.
   * should be contained in sliceSet.
   * If sliceSet is null or empty no slicing is used.
   *
   * If lockSens is set to true a lock sensitive analysis is used.
   * Unique locks are identified but only those with
   * class loader "Application" are considered.  There should be at most
   * eight such locks on a 64bit system and at most five on a 32bit system.
   *
   * The DPN uses the "FakeRootMethod" of the call graph as start configuration.
   *
   * An conflict exists if there are at least two processes which can simultaneously
   * reach an stack symbol of confSet.  So confSet could be some critical section.
   *
   * The method returns the result of the emptiness check. That means it returns
   * true iff no conflict can exists.
   *
   * @param cg A call graph for the analysis
   * @param pa The corresponding PointerAnalysis obtained from the builder used to create the call graph
   * @param confSet A set of StackSymbols which should be checked for mutual exclusion.
   * @param sliceSet A set of CGNodes which are used for slicing. E.g. the methods of confSet plus locking positions.
   * @param lockSet A set of locks which can be abstracted into the DPN.
   * @return true iff no conflict can exist.
   */
  def runSSRCheck(cg: CallGraph, pa: PointerAnalysis, confSet: Set[StackSymbol], sliceSet: Set[CGNode], lockSet: Set[InstanceKey]): Boolean = {
    val dpn = getMDPN(cg, pa, sliceSet, lockSet)
    val lockSens = !dpn.locks.isEmpty
    val ss = dpn.getStackSymbols
    require(confSet.subsetOf(ss), "Some symbols of confSet are not contained in the DPN!")
    val (td, bu) = SingleSetReachability.genAutomata(dpn, confSet, lockSens)
    val check = SingleSetReachability.genCheck(td, bu)
    return runner.runCheck(check)
  }

  /**
   * Method to run a WitnessTwoSetReachability analysis and return the result of the
   * emptiness check.
   *
   * Same as runSSRCheck but two sets of conflict stack symbols are used.
   * Here there must exist two processes where the first reaches a stack symbol
   * of confSet1 and the second a stack symbol of confSet2.
   *
   * If one want's to check for data races on a variable  confSet1 could be all positions where
   * a variable is written and confSet2 all positions where the variable is written or read.
   *
   *
   * @param cg A call graph for the analysis
   * @param pa The corresponding PointerAnalysis obtained from the builder used to create the call graph
   * @param confSet1 A set of StackSymbols which should be checked for mutual exclusion with confSet2.
   * @param confSet2 A set of StackSymbols which should be checked for mutual exclusion with confSet1.
   * @param sliceSet A set of CGNodes which are used for slicing. E.g. the methods of confSet plus locking positions.
   * @param lockSet A set of locks which can be abstracted into the DPN.
   * @return None iff no conflict can exist Some(witness) iff an conflict exists with the given witness
   */
  def runWitnessTSRCheck(cg: CallGraph, pa: PointerAnalysis, confSet1: Set[StackSymbol], confSet2: Set[StackSymbol], sliceSet: Set[CGNode], lockSet: Set[InstanceKey]): Option[String] = {
    val dpn = getMDPN(cg, pa, sliceSet, lockSet)
    val lockSens = !dpn.locks.isEmpty
    val ss = dpn.getStackSymbols
    require(confSet1.subsetOf(ss), "Some symbols of confSet1 are not contained in the DPN!")
    require(confSet2.subsetOf(ss), "Some symbols of confSet2 are not contained in the DPN!")
    val (td, bu) = TwoSetReachability.genAutomata(dpn, confSet1, confSet2, lockSens)
    val check = new FullWitnessIntersectionEmptinessCheck(td, bu)
    return runner.runFullWitnessCheck(check)
  }

  /**
   * Method to run a TwoSetReachability analysis and return the result of the
   * emptiness check.
   *
   * Same as runSSRCheck but two sets of conflict stack symbols are used.
   * Here there must exist two processes where the first reaches a stack symbol
   * of confSet1 and the second a stack symbol of confSet2.
   *
   * If one want's to check for data races on a variable  confSet1 could be all positions where
   * a variable is written and confSet2 all positions where the variable is written or read.
   *
   *
   * @param cg A call graph for the analysis
   * @param pa The corresponding PointerAnalysis obtained from the builder used to create the call graph
   * @param confSet1 A set of StackSymbols which should be checked for mutual exclusion with confSet2.
   * @param confSet2 A set of StackSymbols which should be checked for mutual exclusion with confSet1.
   * @param sliceSet A set of CGNodes which are used for slicing. E.g. the methods of confSet plus locking positions.
   * @param lockSens A flag which decieds if this analysis should be locksensitive.
   * @return true iff no conflict can exist
   */
  def runTSRCheck(cg: CallGraph, pa: PointerAnalysis, confSet1: Set[StackSymbol], confSet2: Set[StackSymbol], sliceSet: Set[CGNode], lockSet: Set[InstanceKey]): Boolean = {
    val dpn = getMDPN(cg, pa, sliceSet, lockSet)
    val lockSens = !dpn.locks.isEmpty
    val ss = dpn.getStackSymbols
    require(confSet1.subsetOf(ss), "Some symbols of confSet1 are not contained in the DPN!")
    require(confSet2.subsetOf(ss), "Some symbols of confSet2 are not contained in the DPN!")
    val (td, bu) = TwoSetReachability.genAutomata(dpn, confSet1, confSet2, lockSens)
    val check = new IntersectionEmptinessCheck(td, bu)
    return runner.runCheck(check)
  }

  /**
   * Generates a CallGraph and PointerAnalysis using the given parameters,
   * a scope based on {{primordial.txt}} with
   * exclusions found in {{Java60RegressionExclusions.txt}}.
   * And a VanillaZeroOneCFA extended by [[de.wwu.sdpn.util.ThreadSensContextSelector]]
   * @param cp the class path to analyze
   * @param mc the main class to analyze
   * @return the PreAnalysis
   */
  def getCGandPAfromCP(cp: String, mc: String): (CallGraph, PointerAnalysis) = {

    val scope = AnalysisScopeReader.makeJavaBinaryAnalysisScope(
      cp,
      FileProvider.getFile("Java60RegressionExclusions.txt"))

    val cha = ClassHierarchy.make(scope);

    val entrypoints = Util.makeMainEntrypoints(scope, cha, mc);

    val options = new AnalysisOptions(scope, entrypoints);

    val cache = new AnalysisCache();

    val cgbuilder: SSAPropagationCallGraphBuilder =
      Util.makeVanillaZeroOneCFABuilder(options, cache, cha, scope, new ThreadSensContextSelector(), null);

    val cg = cgbuilder.makeCallGraph(options)

    val pa = cgbuilder.getPointerAnalysis

    return (cg, pa)
  }

  /**
   * Method to generate a monitor DPN from a given call graph cg an associated pointer analysis pa.
   * The DPN only contains methods from which a method contained in sliceSet can be reached.
   * The DPN is lock sensitive iff lockSens is set to true.  If so abstractable locks
   * are identified and only those with class loader "Application" are considered.
   *
   * @param cg A call graph representing the control flow.
   * @param pa The associated pointer analysis of cg.
   * @param sliceSet An set of nodes which will be used to reduce the size of the DPN.
   * @param lockSens A flag which determines if the DPN should be lock sensitive
   * @return A monitor DPN representing the control flow of cg.
   */
  def getMDPN(cg: CallGraph, pa: PointerAnalysis, sliceSet: Set[CGNode], lockSens: Boolean): MDPN = {
    var analysis = MyPreAnalysis(cg.getClassHierarchy(), cg, pa)
    if (sliceSet != null && !sliceSet.isEmpty) {
      analysis += sliceSet
    }
    if (lockSens) {
      val pl = getPossibleLocks(cg, pa)
      val ul = filterByClassLoader(pl) //LockWithOriginLocator.uniqueLocks(cg, pa).filter(appIKFilter)
      val wm = new WaitMap(analysis, ul)
      analysis = new MyPreAnalysis(analysis) {
        override def safeLock(lock: InstanceKey, node: CGNode) = ul(lock) && !wm(node)(lock)
      }
    }

    val factory = new MonitorDPNFactory(analysis)
    return factory.getDPN
  }

  /**
   * Method to generate a monitor DPN from a given call graph cg an associated pointer analysis pa.
   * The DPN only contains methods from which a method contained in sliceSet can be reached.
   * The DPN will contain locks identified by lockSet
   *
   * @param cg A call graph representing the control flow.
   * @param pa The associated pointer analysis of cg.
   * @param sliceSet An set of nodes which will be used to reduce the size of the DPN.
   * @param lockSet A set of InstanceKeys which should be treated as locks
   * @return A monitor DPN representing the control flow of cg.
   */
  def getMDPN(cg: CallGraph, pa: PointerAnalysis, sliceSet: Set[CGNode], lockSet: Set[InstanceKey]): MDPN = {
    require(lockSet.size < 9, "Can handle at most 8 Locks but got " + lockSet.size)
    var analysis = MyPreAnalysis(cg.getClassHierarchy(), cg, pa)
    if (sliceSet != null && !sliceSet.isEmpty) {
      analysis += sliceSet
    }

    val wm = new WaitMap(analysis, lockSet.contains _)
    analysis = new MyPreAnalysis(analysis) {
      override def safeLock(lock: InstanceKey, node: CGNode) = lockSet(lock) && !wm(node)(lock)
    }

    val fact = new MonitorDPNFactory(analysis)
    return fact.getDPN
  }

  val appIKFilter: InstanceKey => Boolean = {
    case x: ConstantKey[_] =>
      x.getValue() match {
        case x: IClass => ClassLoaderReference.Application.equals(x.getClassLoader().getReference())
        case _ => false
      }
    case x: InstanceKey =>
      ClassLoaderReference.Application.equals(x.getConcreteType().getClassLoader().getReference())
    case _ => false
  }

  /**
   * This method calculates an under approximation of the set of InstanceKeys
   * which can safely be translated to locks in a monitor DPN representing
   * the control flow of the given call graph.
   *
   * Note that not all usages of such instances will be translated into locks
   * in the DPN as monitors inside which a wait() call is possible are ignored.
   *
   * @param cg A call graph representing control flow
   * @param pa The associated pointer analysis of the cg
   * @return A set of instance keys which can be abstracted as locks in a monitor DPN
   */
  def getPossibleLocks(cg: CallGraph, pa: PointerAnalysis): Set[InstanceKey] = {
    val locks = LockWithOriginLocator.uniqueLocks(cg, pa)
    return locks
  }

  /**
   * A helper method filtering out instance keys which don't have "Application"
   * as class loader.
   * @param iks A set of instance keys.
   * @return The set of all x in iks for which
   * ClassLoaderReference.Application.equals(x.getConcreteType().getClassLoader().getReference()) holds.
   */
  def filterByClassLoader(iks: Set[InstanceKey]) = iks.filter(appIKFilter)

}