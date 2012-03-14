package de.wwu.sdpn.core.ta.xsb

import de.wwu.sdpn.core.util.IProgressMonitor

/**
 * This Object contains helper functions to run an XSB process and interpret the result.
 * These methos rely on an well formed check and the xsbExe and tempDir specified
 * in [[de.wwu.sdpn.analysis.SDPNProps]].
 *
 * @author Benedikt Nordhoff
 */
trait XSBRunner {
  def runCheck(check: IntersectionEmptinessCheck, pm: IProgressMonitor): Boolean 
  def runCheck(check: IntersectionEmptinessCheck): Boolean

  def runFullWitnessCheck(check: FullWitnessIntersectionEmptinessCheck, pm: IProgressMonitor): Option[String] 
  def runFullWitnessCheck(check: FullWitnessIntersectionEmptinessCheck): Option[String]

  def shutdown() :Unit
}