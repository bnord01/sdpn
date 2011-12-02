package de.wwu.sdpn.tests.analyses

import org.eclipse.core.runtime.IProgressMonitor
import System.{ currentTimeMillis => now }

class PrintingPM extends IProgressMonitor {
  var total: Double = -1
  var doneWork: Double = -1
  var mainTask = "none"
  var subTask = ""
  var start: Long = -1
  var last: Long = -1
  var canceled = false

  def beginTask(name: String, totalWork: Int): Unit = {
    mainTask = name
    total = totalWork
    doneWork = 0
    start = now
    last = start
    println("Begun main task: " + mainTask)
  }

  def done(): Unit = {
    val ttime = now - start
    println("Finished: " + mainTask)
    println("Total time used: " + ttime + "ms")
    total = -1
    doneWork = -1
    mainTask = "none"
    subTask = ""
    start - 1
    last = -1
    canceled = false

  }

  def internalWorked(work: Double): Unit = {
    val t = now
    val used = t - last
    last = t
    doneWork += work
    println("Worked " + used + "ms (" + doneWork + "/" + total + ") : " + mainTask + " --- " + subTask)
  }

  def isCanceled(): Boolean = { canceled }

  def setCanceled(value: Boolean): Unit = { canceled = value }

  def setTaskName(name: String): Unit = {
    mainTask = name
    println(" Started working on main task: " + mainTask)
  }

  def subTask(name: String): Unit = {
    subTask = name
    println(" Started working on sub task:  " + subTask)
  }

  def worked(work: Int): Unit = {
    val t = now
    val used = t - last
    last = t
    doneWork += work
    println("Worked " + used + "ms (" + doneWork + "/" + total + ") : " + mainTask + " --- " + subTask)
  }

}