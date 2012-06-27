package de.wwu.sdpn.eclipse.util

import de.wwu.sdpn.core.util.IProgressMonitor

/**
 * Wrapper to convert an '''org.eclipse.core.runtime.IProgressMonitor''' into a 
 * '''de.wwu.sdpn.core.util.IProgressMonitor''' using reflection.
 * If delegate isn't an instance of '''org.eclipse.core.runtime.IProgressMonitor'''
 * an '''IllegalArgumentException''' is thrown. 
 * 
 * @author Benedikt Nordhoff
 */
class EPMWrapper(delegate: Any) extends IProgressMonitor {
  val epmc = Class.forName("org.eclipse.core.runtime.IProgressMonitor")
  require(epmc.isInstance(delegate))

  def beginTask(name: String, totalWork: Int): Unit = {
    val meth = epmc.getMethod("beginTask", classOf[java.lang.String], java.lang.Integer.TYPE)
    meth.invoke(delegate, name, totalWork: java.lang.Integer)
  }

  def done(): Unit = {
    epmc.getMethod("done").invoke(delegate)
  }

  def worked(work: Double): Unit = {
    val meth = epmc.getMethod("internalWorked",java.lang.Double.TYPE)
    meth.invoke(delegate,work:java.lang.Double)
  }

  def isCanceled(): Boolean = { 
    val res = epmc.getMethod("isCanceled").invoke(delegate)
    return res.asInstanceOf[Boolean]
  }

  def setCanceled(value: Boolean): Unit = {
    val meth = epmc.getMethod("setCanceled",java.lang.Boolean.TYPE)
    meth.invoke(delegate,value:java.lang.Boolean)
  }

  def subTask(name: String): Unit = {
    val meth = epmc.getMethod("subTask",classOf[java.lang.String])
    meth.invoke(delegate,name)
  }

  def worked(work: Int): Unit = {
    val meth = epmc.getMethod("worked",java.lang.Integer.TYPE)
    meth.invoke(delegate,work:java.lang.Integer)
  }

}