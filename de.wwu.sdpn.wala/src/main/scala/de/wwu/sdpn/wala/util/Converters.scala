package de.wwu.sdpn.wala.util
import com.ibm.wala.ipa.callgraph.CGNode
import de.wwu.sdpn.wala.dpngen.symbols.StackSymbol
import scala.collection.JavaConversions._

object Converters {
  /**
   * Obtains the StackSymbol representing the point just ''before'' the
   * instruction corresponding to the given instructionIndex.
   *
   * @param node A CGNode
   * @param instructionIndex An index of an instruction within the array node.getIR.getInstructions
   * @return A corresponding stack symbol
   */
  def getSS4NodeAndIndex(node: CGNode, instructionIndex: Int): StackSymbol = {
    val ir = node.getIR
    val cfg = ir.getControlFlowGraph

    val bb = cfg.getBlockForInstruction(instructionIndex)
    //val bb = cfg.filter(x => x.getFirstInstructionIndex <= instructionIndex && instructionIndex <= x.getLastInstructionIndex).first

    var index = 0
    for (instr <- bb.iteratePhis()) {
      index += 1
    }
    if (bb.isCatchBlock())
      index += 1
    val start = bb.getFirstInstructionIndex

    val instrArr = ir.getInstructions
    for (i <- start until instructionIndex) {
      if (instrArr(i) != null) {
        index += 1
      }
    }
    return StackSymbol(node, bb.getNumber, index)
  }

}