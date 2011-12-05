package de.wwu.sdpn.core.util

/**
 * Wrapper to wrap an com.ibm.wala.util.MonitorUtil.IProgressMonitor into a
 * de.wwu.sdpn.core.util.IProgressMonitor using reflection.
 * If delegate isn't an instance of {{com.ibm.wala.util.MonitorUtil.IProgressMonitor}}
 * an IllegalArgumentException is thrown.
 *
 * @author Benedikt Nordhoff
 */
class WPMWrapper(delegate: Any) extends IProgressMonitor {
  val epmc = Class.forName("com.ibm.wala.util.MonitorUtil$IProgressMonitor")
  require(epmc.isInstance(delegate))

  private var internalWork: Double = 0d

  def beginTask(name: String, totalWork: Int): Unit = {
    val meth = epmc.getMethod("beginTask", classOf[java.lang.String], java.lang.Integer.TYPE)
    meth.invoke(delegate, name, totalWork: java.lang.Integer)
  }

  def done(): Unit = {
    epmc.getMethod("done").invoke(delegate)
  }

  def internalWorked(work: Double): Unit = {
    if(work <= 0)
      return;
    internalWork += work
    val toSend = internalWork.toInt
    if (toSend > 0) {
      worked(toSend)
      internalWork -= toSend
    }
  }

  def isCanceled(): Boolean = {
    val res = epmc.getMethod("isCanceled").invoke(delegate)
    return res.asInstanceOf[Boolean]
  }

  def setCanceled(value: Boolean): Unit = {
    if (value) {
      try {
        val meth = epmc.getMethod("setCanceled", java.lang.Boolean.TYPE)
        meth.invoke(delegate, value: java.lang.Boolean)
      } catch {
        case e: NoSuchMethodException =>
          throw new UnsupportedOperationException("Can't set canceled in vanilla Wala IProgressMonitor.")
      }
    } else {
      throw new UnsupportedOperationException("Can't unset canceled in Wala IProgressMonitor.")
    }
  }

  def setTaskName(name: String): Unit = { }

  def subTask(name: String): Unit = {
    try {
      val meth = epmc.getMethod("subTask", classOf[java.lang.String])
      meth.invoke(delegate, name)
    } catch {
      case e: NoSuchMethodException =>
    }
  }

  def worked(work: Int): Unit = {
    val meth = epmc.getMethod("worked", java.lang.Integer.TYPE)
    meth.invoke(delegate, work: java.lang.Integer)
  }

}