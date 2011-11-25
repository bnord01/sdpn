package de.wwu.sdpn.util
import com.ibm.wala.util.graph.Graph
import com.ibm.wala.dataflow.graph.DataflowSolver
import com.ibm.wala.fixpoint.BitVectorVariable
import com.ibm.wala.util.intset.OrdinalSetMapping
import com.ibm.wala.util.intset.MutableMapping
import com.ibm.wala.util.collections.Iterator2Collection
import com.ibm.wala.dataflow.graph.ITransferFunctionProvider
import com.ibm.wala.fixpoint.UnaryOperator
import com.ibm.wala.dataflow.graph.BitVectorUnionConstant
import com.ibm.wala.dataflow.graph.BitVectorIdentity
import com.ibm.wala.dataflow.graph.AbstractMeetOperator
import com.ibm.wala.dataflow.graph.BitVectorUnion
import com.ibm.wala.dataflow.graph.BitVectorFramework
import com.ibm.wala.util.graph.impl.GraphInverter
import com.ibm.wala.dataflow.graph.BitVectorSolver
import com.ibm.wala.util.MonitorUtil.IProgressMonitor

class GraphCycleFinder[T] (graph: Graph[T]) {  
  
  private var solver :  DataflowSolver[T, BitVectorVariable] = null;

  /**
   * domain
   */
  private val domain: OrdinalSetMapping[T] = new MutableMapping((Iterator2Collection.toSet(graph.iterator)).toArray());

    /**
   * @param n a node in the given graph
   * @return true iff there exists a real path from node to node e.g. node is reachable from node 
   */
  def inCycle(n: T): Boolean =  {
    if (solver == null) {
      throw new IllegalStateException("must call solve() before calling getReachableSet()");
    }
    val v = solver.getIn(n);
    assert (v != null , "null variable for node " + n)
    val index = domain.getMappedIndex(n)
    if (v.getValue() == null) {
      return false
    } else {
      return v.getValue().contains(index)
    }
  }

  /**
   * @return true iff the evaluation of some equation caused a change in the value of some variable.
   */
  def solve( monitor: IProgressMonitor) :Boolean =  {

  val functions = new ITransferFunctionProvider[T, BitVectorVariable]() {

      def  getNodeTransferFunction(n:T) : UnaryOperator[BitVectorVariable] = {
        assert(false,"Called getEdgeTransferFunction but hasEdgeTransferFunctions == false")
        return null;
      }

      def hasNodeTransferFunctions() : Boolean = false

      def getEdgeTransferFunction(from : T, to: T) :  UnaryOperator[BitVectorVariable]  = {
        val index = domain.getMappedIndex(from);
        assert( index > -1, "Got unmapped index for noded " + from)
          return new BitVectorUnionConstant(index);
        
        
      }

      def hasEdgeTransferFunctions() : Boolean = true

      def getMeetOperator() :  AbstractMeetOperator[BitVectorVariable] = {
        return BitVectorUnion.instance();
      }
    };

    val f = new BitVectorFramework[T, T](GraphInverter.invert(graph), functions, domain);
    solver = new BitVectorSolver(f);
    return solver.solve(monitor);
  }
}