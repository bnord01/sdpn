package de.wwu.sdpn.core.util

trait IProgressMonitor {

  /**
   * Sets the main task of this progress monitor. 
   * This method should be called only once!
   */
  def beginTask(name: String, totalWork: Int): Unit

  def done(): Unit

  def isCanceled(): Boolean

  def setCanceled(value: Boolean): Unit

  def subTask(name: String)
  
  def worked(work: Int): Unit
  
  def worked(work: Double): Unit

}