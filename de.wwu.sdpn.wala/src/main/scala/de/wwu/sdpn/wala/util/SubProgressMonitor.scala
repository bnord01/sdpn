package de.wwu.sdpn.wala.util

import com.ibm.wala.util.MonitorUtil.IProgressMonitor

class SubProgressMonitor(parent: IProgressMonitor, ticksToSend: Int) extends IProgressMonitor {
  private var sentTicks = 0
  private var scale = 0d
  private var realTicksDone = 0d

  def beginTask(task: String, totalWork: Int) {
    if (parent == null)
      return
    parent.subTask(task)
    sentTicks = 0
    realTicksDone = 0d
    scale = if (totalWork <= 0) 0 else ticksToSend.asInstanceOf[Double] / totalWork.asInstanceOf[Double]
  }

  def subTask(subTask: String) {
    if (parent == null)
      return
    parent.subTask(subTask)
  }

  def isCanceled: Boolean = {
    if (parent == null)
      false
    else
      parent.isCanceled()
  }
  
  def cancel() {
    if (parent != null)
      parent.cancel()
  }

  def done() {
    if (parent == null)
	    return
    val toSent = ticksToSend - sentTicks
    if (toSent > 0)
      parent.worked(toSent)
    parent.subTask("")
  }

  def worked(units: Int) {
    if (parent == null)
	    return
    realTicksDone += (units * scale)
    val newTicks = (realTicksDone - sentTicks).asInstanceOf[Int]
    if (newTicks > 0) {
      parent.worked(newTicks)
      sentTicks += newTicks
    }
  }
}